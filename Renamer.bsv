////////////////////////////////////////////////////////////////////////////////
//  Filename      : Renamer.bsv
//  Description   : Maps object addresses to a smaller address space.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import ClientServer::*;
import Connectable::*;
import FIFO::*;
import GetPut::*;
import SpecialFIFOs::*;
import Vector::*;

import Scheduler::*;
import Shard::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef 3 LogNumberTransactionObjects;
typedef 2 LogSizeRenamerBuffer;

typedef TAdd#(1, LogNumberTransactionObjects) TransactionObjectCount;

typedef TExp#(LogNumberTransactionObjects) NumberTransactionObjects;
typedef TExp#(LogSizeRenamerBuffer) SizeRenamerBuffer;

typedef Bit#(LogNumberTransactionObjects) TransactionObjectIndex;
typedef Bit#(LogSizeRenamerBuffer) RenamerBufferIndex;
typedef Bit#(TransactionObjectCount) TransactionObjectCounter;

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

// Tells the arbiter that we don't need to route responses back.
instance ArbRequestTC#(RenamedTransaction);
   function Bool isReadRequest(a x) = False;
   function Bool isWriteRequest(a x) = True;
endinstance

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
    interface Vector#(NumberShards, Get#(ShardRequest)) outputs;
endinterface

typedef enum { ReadSet, WriteSet } SetType deriving (Bits, Eq, FShow);

module mkRequestDistributor(RequestDistributor);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    FIFO#(InputTransaction) inputFifo <- mkBypassFIFO();
    Reg#(SetType) setType <- mkReg(ReadSet);
    Reg#(TransactionObjectIndex) setIndex <- mkReg(0);
    Wire#(ShardIndex) shardIndex <- mkWire();

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    // Calculate shard corresponding to current object.
    rule progress;
        InputTransaction inputTr = inputFifo.first();
        if (setType == ReadSet) begin
            let readObject = inputTr.readObjects[setIndex];
            shardIndex <= getShard(readObject);
        end else begin
            let writtenObject = inputTr.writeObjects[setIndex];
            shardIndex <= getShard(writtenObject);
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    function Get#(ShardRequest) makeOutputInterface(Integer i);
        return (
            interface Get
                // One get method per shard.
                // At most one of these is unblocked on each cycle.
                method ActionValue#(ShardRequest) get() if (shardIndex == fromInteger(i));
                    InputTransaction inputTr = inputFifo.first();
                    if (setType == ReadSet) begin
                        if (setIndex < fromInteger(objSetSize - 1)) begin
                            // Go to next read object.
                            setIndex <= setIndex + 1;
                        end else begin
                            // No more read objects, go to first write object.
                            setIndex <= 0;
                            setType <= WriteSet;
                        end
                    end else begin
                        if (setIndex < fromInteger(objSetSize - 1)) begin
                            // Go to next write object.
                            setIndex <= setIndex + 1;
                        end else begin
                            // No more write objects, transaction is processed.
                            setIndex <= 0;
                            setType <= ReadSet;
                            inputFifo.deq();
                        end
                    end
                    return tagged Rename {
                        tid: inputTr.tid,
                        address: setType == ReadSet ? inputTr.readObjects[setIndex] : inputTr.writeObjects[setIndex],
                        isWrittenObject: setType == WriteSet
                    };
                endmethod
            endinterface
        );
    endfunction

    interface outputs = map(makeOutputInterface, genVector());

    interface Put request = toPut(inputFifo);
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
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(TransactionId) tid <- mkReg(?);
    Reg#(ObjectSet) readSet <- mkReg(0);
    Reg#(ObjectSet) writeSet <- mkReg(0);
    Reg#(TransactionObjectCounter) readObjectCount <- mkReg(0);
    Reg#(TransactionObjectCounter) writtenObjectCount <- mkReg(0);

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function Bool isDone();
        return readObjectCount == fromInteger(objSetSize) && writtenObjectCount == fromInteger(objSetSize);
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Put request;
        method Action put(ShardRenameResponse response) if (!isDone());
            tid <= response.tid;
            if (!response.success) begin
                // TODO: mark transaction failed.
            end else if (response.isWrittenObject) begin
                writeSet <= writeSet | (1 << response.name);
                writtenObjectCount <= writtenObjectCount + 1;
            end else begin
                readSet <= readSet | (1 << response.name);
                readObjectCount <= readObjectCount + 1;
            end
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(RenamedTransaction) get() if (isDone());
            readSet <= 0;
            writeSet <= 0;
            readObjectCount <= 0;
            writtenObjectCount <= 0;
            return RenamedTransaction{ tid: tid, readSet: readSet, writeSet: writeSet };
        endmethod
    endinterface
endmodule

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
    Vector#(SizeRenamerBuffer, Reg#(Bool)) distributorReadyFlags <- replicateM(mkReg(True));
    Reg#(RenamerBufferIndex) resultIndex <- mkReg(?);

    // Shards.
    Vector#(NumberShards, Shard) shards <- replicateM(mkShard);

    // Response aggregators.
    Vector#(SizeRenamerBuffer, ResponseAggregator) aggregators <- replicateM(mkResponseAggregator);

    // Connections from distributors to shards.
    Vector#(NumberShards, Arbiter#(SizeRenamerBuffer, ShardRequest, ShardRenameResponse)) shardArbiters;
    for (Integer i = 0; i < numShards; i = i + 1) begin
        let arb1 <- mkRoundRobin;
        shardArbiters[i] <- mkArbiter(arb1, 1);
        for (Integer j = 0; j < maxTransactions; j = j + 1) begin
            mkConnection(distributors[j].outputs[i], shardArbiters[i].users[j].request);
        end
        mkConnection(shardArbiters[i].master, shards[i]);
    end

    // Connections from shards to aggregators.
    Vector#(SizeRenamerBuffer, Arbiter#(NumberShards, ShardRenameResponse, void)) transactionArbiters;
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        let arb2 <- mkRoundRobin;
        transactionArbiters[i] <- mkArbiter(arb2, 1);
        for (Integer j = 0; j < numShards; j = j + 1) begin
            mkConnection(shardArbiters[j].users[i].response, transactionArbiters[i].users[j].request);
        end
        mkConnection(transactionArbiters[i].master.request, aggregators[i].request);
    end

    // Connections from aggregators to output.
    let arb3 <- mkRoundRobin;
    Arbiter#(SizeRenamerBuffer, RenamedTransaction, void) outputArbiter <- mkArbiter(arb3, 1);
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        mkConnection(aggregators[i].response, outputArbiter.users[i].request);
    end

    rule updateResultIndex if (findElem(True, arb3.grant) matches tagged Valid .distributorId);
        resultIndex <= pack(distributorId);
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    // Add input transaction to first open slot (distributor).
    interface Put request;
        method Action put(InputTransaction it) if (findElem(True, readVReg(distributorReadyFlags)) matches tagged Valid .entryIndex);
            distributors[entryIndex].request.put(it);
            distributorReadyFlags[entryIndex] <= False;
        endmethod
    endinterface

    // Return computed result.
    interface Get response;
        method ActionValue#(RenamedTransaction) get();
            distributorReadyFlags[resultIndex] <= True;
            let result <- outputArbiter.master.request.get();
            return result;
        endmethod
    endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
// Renamer tests.
////////////////////////////////////////////////////////////////////////////////
typedef 5 NumberRenamerTests;

Integer numTests = valueOf(NumberRenamerTests);

function InputTransaction makeInputTr(TransactionId i, ObjectAddress r[], ObjectAddress w[]);
    return InputTransaction{tid: i, readObjects: arrayToVector(r), writeObjects: arrayToVector(w)};
endfunction

module mkRenamerTestbench();
    Renamer myRenamer <- mkRenamer();

    ObjectAddress reads[numTests][objSetSize] = {
        {'h000, 'h008, 'h010, 'h018, 'h020, 'h028, 'h030, 'h038},
        {'h840, 'h83E, 'h83C, 'h83A, 'h838, 'h836, 'h834, 'h832},
        {'h110, 'h111, 'h112, 'h113, 'h114, 'h115, 'h116, 'h117},
        {'h100, 'h102, 'h104, 'h108, 'h10A, 'h10C, 'h10E, 'h110},
        {'h820, 'h101, 'h20C, 'h0E2, 'h388, 'h414, 'h6BB, 'h502}
    };
    ObjectAddress writes[numTests][objSetSize] = {
        {'h004, 'h008, 'h00C, 'h010, 'h014, 'h018, 'h01C, 'h020},
        {'h000, 'h001, 'h002, 'h003, 'h004, 'h005, 'h006, 'h007},
        {'h120, 'h121, 'h122, 'h123, 'h124, 'h125, 'h126, 'h127},
        {'h001, 'h003, 'h005, 'h007, 'h010, 'h030, 'h050, 'h070},
        {'h310, 'h002, 'h202, 'hF10, 'h720, 'h101, 'h610, 'hC20}
    };
    Vector#(NumberRenamerTests, InputTransaction) testInputs = zipWith3(makeInputTr, genWith(fromInteger), arrayToVector(reads), arrayToVector(writes));

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < fromInteger(numTests));
        counter <= counter + 1;
        myRenamer.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result <- myRenamer.response.get();
        $display(fshow(result));
    endrule
endmodule