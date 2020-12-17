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

import PmCore::*;
import PmIfc::*;
import Shard::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef 2 LogSizeRenamerBuffer;

typedef TAdd#(1, LogNumberTransactionObjects) TransactionObjectCount;

typedef TExp#(LogSizeRenamerBuffer) SizeRenamerBuffer;

typedef Bit#(LogNumberTransactionObjects) TransactionObjectIndex;
typedef Bit#(LogSizeRenamerBuffer) RenamerBufferIndex;
typedef Bit#(TransactionObjectCount) TransactionObjectCounter;

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
/// Helper modules to distribute requests to shards.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
interface RequestDistributor#(type transaction_type);
    interface Put#(transaction_type) request;
    interface Vector#(NumberShards, Get#(ShardRequest)) outputs;
endinterface

typedef RequestDistributor#(InputTransaction) RenameRequestDistributor;
typedef RequestDistributor#(RenamedTransaction) DeleteRequestDistributor;

typedef enum { ReadObject, WrittenObject } ObjectType deriving (Bits, Eq, FShow);

typedef struct {
    ObjectType objType;
    ObjectName objName;
} RenamedObject;

module mkRenameRequestDistributor(RenameRequestDistributor);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    FIFO#(InputTransaction) inputFifo <- mkBypassFIFO();
    Reg#(ObjectType) objType <- mkReg(ReadObject);
    Reg#(TransactionObjectIndex) objIndex <- mkReg(0);

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    ShardIndex shardIndex = begin
        InputTransaction inputTr = inputFifo.first();
        let readObject = inputTr.readObjects[objIndex];
        let writtenObject = inputTr.writeObjects[objIndex];
        getShard(objType == ReadObject ? readObject : writtenObject);
    end;

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
                    if (objType == ReadObject) begin
                        if (objIndex < fromInteger(objSetSize - 1)) begin
                            // Go to next read object.
                            objIndex <= objIndex + 1;
                        end else begin
                            // No more read objects, go to first written object.
                            objIndex <= 0;
                            objType <= WrittenObject;
                        end
                    end else begin
                        if (objIndex < fromInteger(objSetSize - 1)) begin
                            // Go to next written object.
                            objIndex <= objIndex + 1;
                        end else begin
                            // No more written objects, transaction is processed.
                            objIndex <= 0;
                            objType <= ReadObject;
                            inputFifo.deq();
                        end
                    end
                    return tagged Rename {
                        tid: inputTr.tid,
                        address: objType == ReadObject ? inputTr.readObjects[objIndex] : inputTr.writeObjects[objIndex],
                        isWrittenObject: objType == WrittenObject
                    };
                endmethod
            endinterface
        );
    endfunction

    interface outputs = map(makeOutputInterface, genVector());

    interface Put request = toPut(inputFifo);
endmodule

module mkDeleteRequestDistributor(DeleteRequestDistributor);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    let emptyTransaction = RenamedTransaction { tid: ?, readSet: 'b0, writeSet: 'b0 };
    Reg#(RenamedTransaction) currentTr <- mkReg(emptyTransaction);

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    let readObject = countZerosLSB(currentTr.readSet);
    let writtenObject = countZerosLSB(currentTr.writeSet);
    Maybe#(RenamedObject) maybeCurrentObj = (
        readObject < fromInteger(maxLiveObjects)    ? tagged Valid RenamedObject {
            objType: ReadObject, objName: pack(truncate(readObject)) } :
        writtenObject < fromInteger(maxLiveObjects) ? tagged Valid RenamedObject {
            objType: WrittenObject, objName: pack(truncate(writtenObject)) } :
                                                      tagged Invalid);

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    function Get#(ShardRequest) makeOutputInterface(Integer i);
        return (
            interface Get
                // One get method per shard.
                // At most one of these is unblocked on each cycle.
                method ActionValue#(ShardRequest) get() if (
                    maybeCurrentObj matches tagged Valid .currentObj
                    &&& getShard(currentObj.objName) == fromInteger(i)
                );
                    case (currentObj.objType) matches
                        ReadObject : currentTr <= RenamedTransaction {
                            tid: currentTr.tid,
                            readSet: currentTr.readSet & ~(1 << currentObj.objName),
                            writeSet: currentTr.writeSet
                        };
                        WrittenObject : currentTr <= RenamedTransaction {
                            tid: currentTr.tid,
                            readSet: currentTr.readSet,
                            writeSet: currentTr.writeSet & ~(1 << currentObj.objName)
                        };
                    endcase
                    return tagged Delete { name: currentObj.objName };
                endmethod
            endinterface
        );
    endfunction

    interface outputs = map(makeOutputInterface, genVector());

    interface Put request;
        method Action put(RenamedTransaction newTr) if (maybeCurrentObj matches tagged Invalid);
            currentTr <= newTr;
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
interface ResponseAggregator;
    interface Put#(ShardRenameResponse) request;
    interface Get#(RenamedTransaction) response;
    interface Get#(RenamedTransaction) failure;
endinterface

module mkResponseAggregator(ResponseAggregator);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(TransactionId) tid <- mkReg(?);
    Reg#(ObjectSet) readSet <- mkReg(0);
    Reg#(ObjectSet) writeSet <- mkReg(0);
    Reg#(TransactionObjectCounter) readObjectCount <- mkReg(0);
    Reg#(TransactionObjectCounter) writtenObjectCount <- mkReg(0);
    Reg#(Bool) success <- mkReg(True);

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
            // Increment request count.
            if (response.isWrittenObject) begin
                writtenObjectCount <= writtenObjectCount + 1;
            end else begin
                readObjectCount <= readObjectCount + 1;
            end
            // Insert object into appropriate set or mark transaction failed.
            if (!response.success) begin
                success <= False;
            end else if (response.isWrittenObject) begin
                writeSet <= writeSet | (1 << response.name);
            end else begin
                readSet <= readSet | (1 << response.name);
            end
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(RenamedTransaction) get() if (isDone() && success);
            readSet <= 0;
            writeSet <= 0;
            readObjectCount <= 0;
            writtenObjectCount <= 0;
            return RenamedTransaction{ tid: tid, readSet: readSet, writeSet: writeSet };
        endmethod
    endinterface

    interface Get failure;
        method ActionValue#(RenamedTransaction) get() if (isDone() && !success);
            readSet <= 0;
            writeSet <= 0;
            readObjectCount <= 0;
            writtenObjectCount <= 0;
            success <= True;
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
    // Rename request distributors.
    Vector#(SizeRenamerBuffer, RenameRequestDistributor) distributors <- replicateM(mkRenameRequestDistributor);
    Vector#(SizeRenamerBuffer, Reg#(Bool)) distributorReadyFlags <- replicateM(mkReg(True));
    Reg#(RenamerBufferIndex) resultIndex <- mkReg(?);

    // Shards.
    Vector#(NumberShards, Shard) shards <- replicateM(mkShard);

    // Response aggregators.
    Vector#(SizeRenamerBuffer, ResponseAggregator) aggregators <- replicateM(mkResponseAggregator);

    // Failed transaction handler: routes objects back to the shards for deletion.
    DeleteRequestDistributor failedTransactionHandler <- mkDeleteRequestDistributor();

    // Connections from distributors to shards.
    Vector#(NumberShards, Arbiter#(TAdd#(SizeRenamerBuffer, 1), ShardRequest, ShardRenameResponse)) shardArbiters;
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

    // Connections from aggregators to failed transaction handler.
    let arb4 <- mkRoundRobin;
    Arbiter#(SizeRenamerBuffer, RenamedTransaction, void) failedTransactionArbiter <- mkArbiter(arb4, 1);
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        mkConnection(aggregators[i].failure, failedTransactionArbiter.users[i].request);
    end
    mkConnection(failedTransactionArbiter.master.request, failedTransactionHandler.request);

    // Connections from failed transaction handler back to the shards.
    for (Integer i = 0; i < numShards; i = i + 1) begin
        mkConnection(failedTransactionHandler.outputs[i], shardArbiters[i].users[maxTransactions].request);
    end

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
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