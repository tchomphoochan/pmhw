////////////////////////////////////////////////////////////////////////////////
//  Filename      : Renamer.bsv
//  Description   : Maps object addresses to a smaller address space.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import ClientServer::*;
import Connectable::*;
import GetPut::*;
import Vector::*;

import PmTypes::*;
import Shard::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef struct {
   TransactionId tid;
   Vector#(NumberTransactionObjects, ObjectAddress) readObjects;
   Vector#(NumberTransactionObjects, ObjectAddress) writeObjects;
} InputTransaction deriving(Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    ObjectSet readSet;
    ObjectSet writeSet;
} RenamedTransaction deriving(Bits, Eq, FShow);

typedef Server#(InputTransaction, RenamedTransaction) Renamer;

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer maxTransactions = valueOf(SizeRenamerBuffer);
Integer numShards = valueOf(NumberShards);
Integer objSetSize = valueOf(NumberTransactionObjects);

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Helper module to distribute requests to shards.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
interface RequestDistributor;
    interface Put#(InputTransaction) request;
    interface Vector#(NumberShards, Get#(ShardRenameRequest)) outputs;
endinterface

module mkRequestDistributor(RequestDistributor);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(Maybe#(InputBufferEntry)) entry <- mkReg(tagged Invalid);
    RWire#(InputBufferEntry) nextEntry <- mkRWire();
    RWire#(ShardIndex) shardIndex <- mkRWire();
    RWire#(ShardRenameRequest) shardRequest <- mkRWire();

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    // Send every object in the input transaction to the appropriate shard.
    rule progress if (isValid(entry));
        let newEntry = fromMaybe(?, entry);
        if (newEntry.readSetIndex < fromInteger(objSetSize)) begin
            let readObject = newEntry.inputTr.readObjects[newEntry.readSetIndex];
            newEntry.readSetIndex = newEntry.readSetIndex + 1;
            shardIndex.wset(getShard(readObject));
            shardRequest.wset(ShardRenameRequest{tid: newEntry.inputTr.tid, address: readObject, isWrittenObject: False});
        end else if (newEntry.writeSetIndex < fromInteger(objSetSize)) begin
            let writtenObject = newEntry.inputTr.writeObjects[newEntry.writeSetIndex];
            newEntry.writeSetIndex = newEntry.writeSetIndex + 1;
            shardIndex.wset(getShard(writtenObject));
            shardRequest.wset(ShardRenameRequest{tid: newEntry.inputTr.tid, address: writtenObject, isWrittenObject: True});
        end
        nextEntry.wset(newEntry);
    endrule

    rule finalize if (isValid(entry) && !isValid(shardIndex.wget()));
        entry <= tagged Invalid;
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    function makeOutputInterface(Integer i);
        return (
            interface Get
                method ActionValue#(ShardRenameRequest) get() if (isValid(entry) && isValid(shardIndex.wget()) && fromMaybe(?, shardIndex.wget()) == fromInteger(i));
                    entry <= tagged Valid fromMaybe(?, nextEntry.wget());
                    return fromMaybe(?, shardRequest.wget());
                endmethod
            endinterface
        );
    endfunction

    interface outputs = map(makeOutputInterface, genVector());

    interface Put request;
        method Action put(InputTransaction it) if (!isValid(entry));
            entry <= tagged Valid InputBufferEntry{inputTr: it, readSetIndex: 0, writeSetIndex: 0};
        endmethod
    endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Helper module to aggregate responses from shards.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
typedef Server#(ShardRenameResponse, RenamedTransaction) ResponseAggregator;

module mkResponseAggregator(ResponseAggregator);
    ////////////////////////////////////////////////////////////////////////////////
    /// Constants.
    ////////////////////////////////////////////////////////////////////////////////
    let defaultValue = SetBufferEntry{tid: ?, readSet: 0, writeSet: 0, readObjectCount: 0, writtenObjectCount: 0};

    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(SetBufferEntry) entry <- mkReg(defaultValue);
    Reg#(Bool) isDone <- mkReg(False);

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Put request;
        method Action put(ShardRenameResponse response) if (!isDone);
            SetBufferEntry newEntry = entry;
            newEntry.tid = response.tid;
            if (response.isWrittenObject) begin
                newEntry.writeSet = entry.writeSet | (1 << response.name);
                newEntry.writtenObjectCount = entry.writtenObjectCount + 1;
            end else begin
                newEntry.readSet = entry.readSet | (1 << response.name);
                newEntry.readObjectCount = entry.readObjectCount + 1;
            end
            entry <= newEntry;
            if (newEntry.readObjectCount == fromInteger(objSetSize)
                    && newEntry.writtenObjectCount == fromInteger(objSetSize)) begin
                isDone <= True;
            end
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(RenamedTransaction) get() if (isDone);
            entry <= defaultValue;
            isDone <= False;
            return RenamedTransaction{
                tid: entry.tid,
                readSet: entry.readSet,
                writeSet: entry.writeSet
            };
        endmethod
    endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
/// Internal types.
////////////////////////////////////////////////////////////////////////////////
typedef struct {
    InputTransaction inputTr;
    Bit#(TAdd#(LogNumberTransactionObjects, 1)) readSetIndex;
    Bit#(TAdd#(LogNumberTransactionObjects, 1)) writeSetIndex;
} InputBufferEntry deriving(Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    ObjectSet readSet;
    ObjectSet writeSet;
    Bit#(TAdd#(LogNumberTransactionObjects, 1)) readObjectCount;
    Bit#(TAdd#(LogNumberTransactionObjects, 1)) writtenObjectCount;
} SetBufferEntry deriving(Bits, Eq, FShow);

// Tells the arbiter that there is no need to route responses back.
instance ArbRequestTC#(RenamedTransaction);
   function Bool isReadRequest(a x) = False;
   function Bool isWriteRequest(a x) = True;
endinstance

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Renamer implementation.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkRenamer(Renamer);

    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    // Request distributors.
    Vector#(SizeRenamerBuffer, RequestDistributor) distributors <- replicateM(mkRequestDistributor);

    // Shards.
    Vector#(NumberShards, Shard) shards <- replicateM(mkShard);

    // Input buffer.
    Reg#(Bit#(LogSizeRenamerBuffer)) inputBufferEnd <- mkReg(0);

    // Connections from distributors to shards.
    Vector#(NumberShards, Arbiter#(SizeRenamerBuffer, ShardRenameRequest, ShardRenameResponse)) shardArbiters;
    for (Integer i = 0; i < numShards; i = i + 1) begin
        let arb <- mkRoundRobin;
        shardArbiters[i] <- mkArbiter(arb, 1);
        for (Integer j = 0; j < maxTransactions; j = j + 1) begin
            mkConnection(distributors[j].outputs[i], shardArbiters[i].users[j].request);
        end
        mkConnection(shardArbiters[i].master, shards[i]);
    end

    // Connections from shards to aggregators.
    Vector#(SizeRenamerBuffer, Arbiter#(NumberShards, ShardRenameResponse, void)) transactionArbiters;
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        let arb1 <- mkRoundRobin;
        transactionArbiters[i] <- mkArbiter(arb1, 1);
        for (Integer j = 0; j < numShards; j = j + 1) begin
            mkConnection(shardArbiters[j].users[i].response, transactionArbiters[i].users[j].request);
        end
    end

    // Output aggregators.
    Vector#(SizeRenamerBuffer, ResponseAggregator) aggregators;
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        aggregators[i] <- mkResponseAggregator;
        mkConnection(transactionArbiters[i].master.request, aggregators[i].request);
    end

    // Connections from aggregators to output.
    let arb2 <- mkRoundRobin;
    Arbiter#(SizeRenamerBuffer, RenamedTransaction, void) outputArbiter <- mkArbiter(arb2, 1);
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        mkConnection(aggregators[i].response, outputArbiter.users[i].request);
    end

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    // Add input transaction to (circular) buffer.
    interface Put request;
        method Action put(InputTransaction it);
            distributors[inputBufferEnd + 1].request.put(it);
            inputBufferEnd <= inputBufferEnd + 1;
        endmethod
    endinterface

    // Return computed result (implemented inside arbiter).
    interface Get response = outputArbiter.master.request;
endmodule

////////////////////////////////////////////////////////////////////////////////
// Renamer tests.
////////////////////////////////////////////////////////////////////////////////
module mkRenamerTestbench();
    Renamer myRenamer <- mkRenamer();

    Vector#(1, InputTransaction) testInputs = newVector;
    testInputs[0].tid = 0;
    for (Integer i = 0; i < objSetSize; i = i + 1) begin
        testInputs[0].readObjects[i] = fromInteger(i) * 8;
        testInputs[0].writeObjects[i] = (fromInteger(i) + 1) * 4;
    end

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < 1);
        counter <= counter + 1;
        myRenamer.request.put(testInputs[0]);
    endrule

    rule stream;
        let result <- myRenamer.response.get();
        $display(fshow(result));
    endrule
endmodule