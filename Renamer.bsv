// Transaction renamer. Maps object addresses to a smaller address space.
import Arbitrate::*;
import ClientServer::*;
import Connectable::*;
import GetPut::*;
import Vector::*;

import PmTypes::*;
import Shard::*;

interface Renamer;
    method Action putInputTransaction(InputTransaction it);
    method ActionValue#(RenamedTransaction) getRenamedTransaction();
endinterface

typedef Server#(ShardRenameResponse, RenamedTransaction) ResponseAggregator;

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

Integer maxTransactions = valueOf(SizeRenamerBuffer);
Integer numShards = valueOf(NumberShards);
Integer objSetSize = valueOf(NumberTransactionObjects);

// Helper module to aggregate responses from shards.
module mkResponseAggregator(ResponseAggregator);
    let defaultValue = SetBufferEntry{tid: ?, readSet: 0, writeSet: 0, readObjectCount: 0, writtenObjectCount: 0};

    Reg#(SetBufferEntry) entry <- mkReg(defaultValue);
    Reg#(Bool) isDone <- mkReg(False);

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

// Main renamer module.
module mkRenamer(Renamer);
    // Shards.
    Vector#(NumberShards, Shard) shards <- replicateM(mkShard);

    // Input buffer.
    Vector#(SizeRenamerBuffer, Reg#(Maybe#(InputBufferEntry))) inputBuffer <- replicateM(mkReg(tagged Invalid));
    Reg#(Bit#(LogSizeRenamerBuffer)) inputBufferStart <- mkReg(0);
    Reg#(Bit#(LogSizeRenamerBuffer)) inputBufferEnd <- mkReg(0);

    // Connections from input buffers to shards.
    Vector#(NumberShards, Arbiter#(SizeRenamerBuffer, ShardRenameRequest, ShardRenameResponse)) shardArbiters;
    for (Integer i = 0; i < numShards; i = i + 1) begin
        let arb <- mkRoundRobin;
        shardArbiters[i] <- mkArbiter(arb, 1);
        mkConnection(shardArbiters[i].master, shards[i]);
    end

    // Connections from shards to aggregators.
    Vector#(SizeRenamerBuffer, Arbiter#(NumberShards, ShardRenameResponse, void)) transactionArbiters;
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        let arb <- mkRoundRobin;
        transactionArbiters[i] <- mkArbiter(arb, 1);
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
    let arbi <- mkRoundRobin;
    Arbiter#(SizeRenamerBuffer, RenamedTransaction, void) outputArbiter <- mkArbiter(arbi, 1);
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        mkConnection(aggregators[i].response, outputArbiter.users[i].request);
    end

    // Send every object in the input transactions to the appropriate shard.
    rule scatter;
        for (Integer i = 0; i < maxTransactions; i = i + 1) begin
            let entry = inputBuffer[i];
            if (isValid(entry)) begin
                let newEntry = fromMaybe(?, entry);
                if (newEntry.readSetIndex < fromInteger(objSetSize)) begin
                    let readObject = newEntry.inputTr.readObjects[newEntry.readSetIndex];
                    newEntry.readSetIndex = newEntry.readSetIndex + 1;
                    let request = ShardRenameRequest{tid: newEntry.inputTr.tid, address: readObject, isWrittenObject: False};
                    shardArbiters[getShard(readObject)].users[i].request.put(request);
                    inputBuffer[i] <= tagged Valid newEntry;
                end else if (newEntry.writeSetIndex < fromInteger(objSetSize)) begin
                    let writtenObject = newEntry.inputTr.writeObjects[newEntry.writeSetIndex];
                    newEntry.writeSetIndex = newEntry.writeSetIndex + 1;
                    let request = ShardRenameRequest{tid: newEntry.inputTr.tid, address: writtenObject, isWrittenObject: True};
                    shardArbiters[getShard(writtenObject)].users[i].request.put(request);
                    inputBuffer[i] <= tagged Valid newEntry;
                end else begin
                    inputBuffer[i] <= tagged Invalid;
                end
            end
        end
    endrule

    // Add input transaction to (circular) buffer.
    method Action putInputTransaction(InputTransaction it) if (!isValid(inputBuffer[inputBufferEnd + 1]));
        let entry = InputBufferEntry{inputTr: it, readSetIndex: 0, writeSetIndex: 0};
        inputBuffer[inputBufferEnd + 1] <= tagged Valid entry;
        inputBufferEnd <= inputBufferEnd + 1;
    endmethod

    // Return computed result (implemented inside arbiter).
    method ActionValue#(RenamedTransaction) getRenamedTransaction = outputArbiter.master.request.get;
endmodule

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
        myRenamer.putInputTransaction(testInputs[0]);
    endrule

    rule stream;
        let result <- myRenamer.getRenamedTransaction();
        $display(fshow(result));
    endrule
endmodule