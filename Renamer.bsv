////////////////////////////////////////////////////////////////////////////////
//  Filename      : Renamer.bsv
//  Description   : Maps object addresses to a smaller address space.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import ClientServer::*;
import Connectable::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;
import Shard::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef struct {
    InputTransaction inputTr;
} RenameRequest deriving (Bits, Eq, FShow);

typedef struct {
    RenamedTransaction renamedTr;
} DeleteRequest deriving (Bits, Eq, FShow);

typedef struct {
    RenamedTransaction renamedTr;
    SchedulerTransaction schedulerTr;
} RenameResponse deriving (Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
} DeleteResponse deriving (Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
} FailResponse deriving (Bits, Eq, FShow);

interface Renamer;
    interface Server#(RenameRequest, RenameResponse) rename;
    interface Server#(DeleteRequest, DeleteResponse) delete;
    interface Get#(FailResponse) fail;
endinterface

////////////////////////////////////////////////////////////////////////////////
/// Internal types.
////////////////////////////////////////////////////////////////////////////////
typedef 3 LogSizeRenamerBuffer;

typedef TExp#(LogSizeRenamerBuffer) SizeRenamerBuffer;

typedef Bit#(LogSizeRenamerBuffer) RenamerBufferIndex;

typedef struct {
    RenamedTransaction renamedTr;
} FailedRename deriving(Bits, Eq, FShow);

////////////////////////////////////////////////////////////////////////////////
/// Typeclasses.
////////////////////////////////////////////////////////////////////////////////
instance DefaultValue#(RenameResponse);
    defaultValue = RenameResponse {
        renamedTr: defaultValue(),
        schedulerTr: defaultValue()
    };
endinstance

// Tells the arbiter whether we need to route responses back.
instance ArbRequestTC#(RenameRequest);
    function Bool isReadRequest(a x) = True;
    function Bool isWriteRequest(a x) = False;
endinstance

instance ArbRequestTC#(DeleteRequest);
    function Bool isReadRequest(a x) = False;
    function Bool isWriteRequest(a x) = True;
endinstance

instance ArbRequestTC#(RenameResponse);
    function Bool isReadRequest(a x) = False;
    function Bool isWriteRequest(a x) = True;
endinstance

instance ArbRequestTC#(DeleteResponse);
    function Bool isReadRequest(a x) = False;
    function Bool isWriteRequest(a x) = True;
endinstance

instance ArbRequestTC#(FailedRename);
    function Bool isReadRequest(a x) = False;
    function Bool isWriteRequest(a x) = True;
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer maxTransactions = valueOf(SizeRenamerBuffer);

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Helper modules to distribute requests to shards.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
interface Signal;
    method Action send();
endinterface

interface RequestDistributor#(type req_type);
    method Bool canPut();
    interface Put#(req_type) request;
    interface Vector#(NumberShards, Get#(ShardRequest)) distribute;
endinterface

typedef RequestDistributor#(InputTransaction) RenameRequestDistributor;
typedef RequestDistributor#(FailedRename) DeleteRequestDistributor;

typedef struct {
    ObjectType objType;
    ObjectName objName;
} RenamedObject deriving (Bits,Eq);

(* synthesize *)
module mkRenameRequestDistributor(RenameRequestDistributor);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(Maybe#(InputTransaction)) maybeInputTr <- mkReg(tagged Invalid);
    Reg#(ObjectType) objType <- mkReg(?);
    Reg#(TransactionObjectCounter) objIndex <- mkReg(0);
`ifdef DEBUG
    Reg#(Timestamp) cycle <- mkReg(0);
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function ShardIndex getIndex(InputTransaction inputTr);
        let readObject = inputTr.readObjects[objIndex];
        let writtenObject = inputTr.writtenObjects[objIndex];
        return getShard(objType == ReadObject ? readObject : writtenObject);
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
`ifdef DEBUG
    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    Vector#(NumberShards, Get#(ShardRequest)) distributeIfc;
    for (Integer i = 0; i < numShards; i = i + 1) begin
        distributeIfc[i] = (
            interface Get
                // One get method per shard.
                // At most one of these is unblocked on each cycle.
                method ActionValue#(ShardRequest) get() if (
                    maybeInputTr matches tagged Valid .inputTr
                    &&& getIndex(inputTr) == fromInteger(i)
                );
                    if (objType == ReadObject) begin
                        if (objIndex < inputTr.readObjectCount - 1) begin
                            // Go to next read object.
                            objIndex <= objIndex + 1;
                        end else begin
                            // No more read objects, go to first written object.
                            objIndex <= 0;
                            if (inputTr.writtenObjectCount == 0) begin
                                // Except if there are no written objects, then reset.
                                maybeInputTr <= tagged Invalid;
                            end else begin
                                objType <= WrittenObject;
                            end
                        end
`ifdef DEBUG
                    $display(
                        "[%8d] Renamer: renaming read object %0d from T#%h on shard %0d",
                        cycle, objIndex, inputTr.tid, getIndex(inputTr));
`endif
                    end else begin
                        if (objIndex < inputTr.writtenObjectCount - 1) begin
                            // Go to next written object.
                            objIndex <= objIndex + 1;
                        end else begin
                            // No more written objects, transaction is processed.
                            objIndex <= 0;
                            maybeInputTr <= tagged Invalid;
                        end
`ifdef DEBUG
                    $display(
                        "[%8d] Renamer: renaming written object %0d from T#%h on shard %0d",
                        cycle, objIndex, inputTr.tid, getIndex(inputTr));
`endif
                    end
                    return tagged Rename ShardRenameRequest {
                        address: objType == ReadObject ? inputTr.readObjects[objIndex]
                                                       : inputTr.writtenObjects[objIndex],
                        objType: objType
                    };
                endmethod
            endinterface
        );
    end

    method Bool canPut() = !isValid(maybeInputTr);

    interface Put request;
        method Action put(InputTransaction inputTr) if (!isValid(maybeInputTr));
            if (inputTr.readObjectCount > 0) begin
                maybeInputTr <= tagged Valid inputTr;
                objType <= ReadObject;
            end else if (inputTr.writtenObjectCount > 0) begin
                maybeInputTr <= tagged Valid inputTr;
                objType <= WrittenObject;
            end
        endmethod
    endinterface

    interface distribute = distributeIfc;
endmodule

(* synthesize *)
module mkDeleteRequestDistributor(DeleteRequestDistributor);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(RenamedTransaction) req[2] <- mkCReg(2, defaultValue());
`ifdef DEBUG
    Reg#(Timestamp) cycle <- mkReg(0);
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions and function-like variables.
    ////////////////////////////////////////////////////////////////////////////////
    let done0 = req[0].readObjectCount == 0 && req[0].writtenObjectCount == 0;
    let done1 = req[1].readObjectCount == 0 && req[1].writtenObjectCount == 0;
    let currentObj = req[0].readObjectCount != 0 ?
        RenamedObject {
            objType: ReadObject,
            objName: req[0].readObjects[req[0].readObjectCount - 1] } :
        RenamedObject {
            objType: WrittenObject,
            objName: req[0].writtenObjects[req[0].writtenObjectCount - 1] };

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
`ifdef DEBUG
    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    Vector#(NumberShards, Get#(ShardRequest)) distributeIfc;
    for (Integer i = 0; i < numShards; i = i + 1) begin
        distributeIfc[i] = (
            interface Get
                // One get method per shard.
                // At most one of these is unblocked on each cycle.
                method ActionValue#(ShardRequest) get() if (
                    !done0 &&& getShard(currentObj.objName) == fromInteger(i)
                );
                    case (currentObj.objType) matches
                        ReadObject : begin
                            req[0].readObjectCount <= req[0].readObjectCount - 1;
`ifdef DEBUG
                    $display("[%8d] Renamer: deleting read object %0d from T#%h", cycle,
                             req[0].readObjectCount - 1, req[0].tid);
`endif
                        end
                        WrittenObject : begin
                            req[0].writtenObjectCount <= req[0].writtenObjectCount - 1;
`ifdef DEBUG
                    $display("[%8d] Renamer: deleting written object %0d from T#%h",
                             cycle, req[0].writtenObjectCount - 1, req[0].tid);
`endif
                        end
                    endcase
                    return tagged Delete ShardDeleteRequest { name: currentObj.objName };
                endmethod
            endinterface
        );
    end

    method Bool canPut = done1;

    interface Put request;
        method Action put(FailedRename failedRename) if (done1);
            req[1] <= failedRename.renamedTr;
        endmethod
    endinterface

    interface distribute = distributeIfc;
endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Helper module to aggregate responses from shards.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
interface ResponseAggregator;
    method Bool canPut();
    interface Put#(InputTransaction) request;
    interface Put#(ShardRenameResponse) aggregate;
    interface Get#(RenameResponse) response;
    interface Get#(FailedRename) failure;
endinterface

module mkResponseAggregator(ResponseAggregator);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(Maybe#(RenamedTransaction)) maybeRenamedTr[2] <- mkCReg(2, tagged Invalid);
    Reg#(ObjectSet) readSet <- mkReg(0);
    Reg#(ObjectSet) writeSet <- mkReg(0);
    Reg#(TransactionObjectCounter) totalReadObjectCount <- mkReg(?);
    Reg#(TransactionObjectCounter) totalWrittenObjectCount <- mkReg(?);
    Reg#(TransactionObjectCounter) readObjectCount <- mkReg(0);
    Reg#(TransactionObjectCounter) writtenObjectCount <- mkReg(0);
`ifdef DEBUG
    Reg#(Timestamp) cycle <- mkReg(0);
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions and function-like variables.
    ////////////////////////////////////////////////////////////////////////////////
    let isDone = readObjectCount == totalReadObjectCount
                 && writtenObjectCount == totalWrittenObjectCount;

    function Bool isSuccess(RenamedTransaction renamedTr);
        return readObjectCount == renamedTr.readObjectCount
               && writtenObjectCount == renamedTr.writtenObjectCount;
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
`ifdef DEBUG
    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    method Bool canPut() = !isValid(maybeRenamedTr[1]);

    interface Put request;
        method Action put(InputTransaction inputTr) if (!isValid(maybeRenamedTr[1]));
            maybeRenamedTr[1] <= tagged Valid RenamedTransaction {
                tid: inputTr.tid,
                trType: inputTr.trType,
                readObjects: ?,
                writtenObjects: ?,
                readObjectCount: 0,
                writtenObjectCount: 0
            };
            readSet <= 0;
            writeSet <= 0;
            totalReadObjectCount <= inputTr.readObjectCount;
            totalWrittenObjectCount <= inputTr.writtenObjectCount;
            readObjectCount <= 0;
            writtenObjectCount <= 0;
        endmethod
    endinterface

    interface Put aggregate;
        method Action put(ShardRenameResponse response) if (
            maybeRenamedTr[0] matches tagged Valid .renamedTr &&& !isDone
        );
            let newTr = renamedTr;
            // Increment request count.
            case (response.request.objType) matches
                ReadObject: readObjectCount <= readObjectCount + 1;
                WrittenObject: writtenObjectCount <= writtenObjectCount + 1;
            endcase
            // If successful, insert object into appropriate set.
            if (response.name matches tagged Valid .name) begin
                case (response.request.objType) matches
                    ReadObject: begin
                        newTr.readObjects[newTr.readObjectCount] = name;
                        newTr.readObjectCount = newTr.readObjectCount + 1;
                        readSet <= readSet | (1 << name);
`ifdef DEBUG
            $display("[%8d] Renamer: renamed read object %0d", cycle, readObjectCount);
`endif
                    end
                    WrittenObject: begin
                        newTr.writtenObjects[newTr.writtenObjectCount] = name;
                        newTr.writtenObjectCount = newTr.writtenObjectCount + 1;
                        writeSet <= writeSet | (1 << name);
`ifdef DEBUG
            $display("[%8d] Renamer: renamed written object %0d", cycle, writtenObjectCount);
`endif
                    end
                endcase
            end
            maybeRenamedTr[0] <= tagged Valid newTr;
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(RenameResponse) get() if (
            maybeRenamedTr[0] matches tagged Valid .renamedTr &&& isDone
            && isSuccess(renamedTr)
        );
            maybeRenamedTr[0] <= tagged Invalid;
            return RenameResponse {
                renamedTr: renamedTr,
                schedulerTr: SchedulerTransaction { readSet: readSet, writeSet: writeSet }
            };
        endmethod
    endinterface

    interface Get failure;
        method ActionValue#(FailedRename) get() if (
            maybeRenamedTr[0] matches tagged Valid .renamedTr &&& isDone
            && !isSuccess(renamedTr)
        );
            maybeRenamedTr[0] <= tagged Invalid;
`ifdef DEBUG
            $display("[%8d] Renamer: failed to rename T#%h", cycle, renamedTr.tid);
`endif
            return FailedRename { renamedTr : renamedTr };
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
typedef TAdd#(TMul#(2, SizeRenamerBuffer), 1) NumShardAccessors;

(* synthesize *)
module mkRenamer(Renamer);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    // Rename request distributors.
    Vector#(SizeRenamerBuffer, RenameRequestDistributor) renameDistributors <-
        replicateM(mkRenameRequestDistributor);

    // Delete request distributors.
    Vector#(SizeRenamerBuffer, DeleteRequestDistributor) deleteDistributors <-
        replicateM(mkDeleteRequestDistributor);
    // Temporary storage to keep track of delete distributor state.
    Reg#(Vector#(SizeRenamerBuffer, TransactionId)) sentToDelDistr <- mkReg(?);
    Reg#(Vector#(SizeRenamerBuffer, Bool)) prevDelDistrCanPut <- mkReg(replicate(True));

    // Shards.
    Vector#(NumberShards, Shard) shards <- replicateM(mkShard);

    // Response aggregators.
    Vector#(SizeRenamerBuffer, ResponseAggregator) aggregators <-
        replicateM(mkResponseAggregator());

    // Failed transaction handler: routes objects back to the shards for deletion.
    DeleteRequestDistributor failedTransactionHandler <- mkDeleteRequestDistributor();

    // Connections from distributors to shards.
    Vector#(NumberShards, Arbitrate#(NumShardAccessors)) shardArbSources;
    Vector#(NumberShards, Arbiter#(NumShardAccessors, ShardRequest, ShardRenameResponse))
        shardArbiters;
    for (Integer i = 0; i < numShards; i = i + 1) begin
        shardArbSources[i] <- mkRoundRobin();
        shardArbiters[i] <- mkArbiter(shardArbSources[i], 1);
        for (Integer j = 0; j < maxTransactions; j = j + 1) begin
            mkConnection(
                renameDistributors[j].distribute[i], shardArbiters[i].users[j].request
            );
            mkConnection(
                deleteDistributors[j].distribute[i],
                shardArbiters[i].users[maxTransactions + j].request
            );
        end
        mkConnection(shardArbiters[i].master.request, shards[i].request);
        mkConnection(shards[i].response, shardArbiters[i].master.response);
    end

    // Connections from shards to aggregators.
    Vector#(SizeRenamerBuffer, Arbiter#(NumberShards, ShardRenameResponse, void))
        transactionArbiters;
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        let arb2 <- mkRoundRobin;
        transactionArbiters[i] <- mkArbiter(arb2, 1);
        for (Integer j = 0; j < numShards; j = j + 1) begin
            mkConnection(
                shardArbiters[j].users[i].response,
                transactionArbiters[i].users[j].request
            );
        end
        mkConnection(transactionArbiters[i].master.request, aggregators[i].aggregate);
    end

    // Connections from aggregators to output.
    let arb3 <- mkRoundRobin;
    Arbiter#(SizeRenamerBuffer, RenameResponse, void) renameOutputArbiter <-
        mkArbiter(arb3, 1);
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        mkConnection(aggregators[i].response, renameOutputArbiter.users[i].request);
    end

    // Connections from aggregators to failed transaction handler.
    let arb4 <- mkRoundRobin;
    Arbiter#(SizeRenamerBuffer, FailedRename, void) failedTransactionArbiter <-
        mkArbiter(arb4, 1);
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        mkConnection(aggregators[i].failure, failedTransactionArbiter.users[i].request);
    end

    // Connections from failed transaction handler back to the shards.
    for (Integer i = 0; i < numShards; i = i + 1) begin
        mkConnection(
            failedTransactionHandler.distribute[i],
            shardArbiters[i].users[2 * maxTransactions].request);
    end

    // Connections to delete response output.
    let arb5 <- mkRoundRobin;
    Arbiter#(SizeRenamerBuffer, DeleteResponse, void) deleteOutputArbiter <-
        mkArbiter(arb5, 1);

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions and function-like variables.
    ////////////////////////////////////////////////////////////////////////////////
    function Bool getCanPut(RequestDistributor#(a) rd) = rd.canPut();
    function Bool getCanPut2(ResponseAggregator ra) = ra.canPut();

    function Bool getIsReady(Shard s) = s.isReady();

    function Bool noGrant(Arbitrate#(size) arb) = !isValid(findElem(True, arb.grant));

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    (* fire_when_enabled *)
    rule sendDeleteResponses;
        let delDistrCanPut = map(getCanPut, deleteDistributors);
        prevDelDistrCanPut <= delDistrCanPut;
        for (Integer i = 0; i < maxTransactions; i = i + 1) begin
            if (!prevDelDistrCanPut[i] && delDistrCanPut[i]) begin
                deleteOutputArbiter.users[i].request.put(DeleteResponse {
                    tid: sentToDelDistr[i]
                });
            end
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Server rename;
        // Add input transaction to first open slot (rename distributor).
        interface Put request;
            method Action put(RenameRequest req) if (
                findIndex(getCanPut2, aggregators) matches tagged Valid .index
                &&& all(getIsReady, shards)
            );
                renameDistributors[index].request.put(req.inputTr);
                aggregators[index].request.put(req.inputTr);
            endmethod
        endinterface

        // Return computed result.
        interface Get response = renameOutputArbiter.master.request;
    endinterface

    interface Server delete;
        // Send renamed transaction to first free delete request distributor.
        interface Put request;
            method Action put(DeleteRequest req) if (
                findIndex(getCanPut, deleteDistributors) matches tagged Valid .index
                &&& all(getIsReady, shards)
            );
                deleteDistributors[index].request.put(FailedRename {
                    renamedTr: req.renamedTr
                });
                sentToDelDistr[index] <= req.renamedTr.tid;
            endmethod
        endinterface

        // Notify about delete.
        interface Get response = deleteOutputArbiter.master.request;
    endinterface

    interface Get fail;
        method ActionValue#(FailResponse) get();
            let failedRename <- failedTransactionArbiter.master.request.get();
            failedTransactionHandler.request.put(failedRename);
            return FailResponse { tid: failedRename.renamedTr.tid };
        endmethod
    endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
// Renamer tests.
////////////////////////////////////////////////////////////////////////////////
typedef 5 NumberRenamerTests;

Integer numTests = valueOf(NumberRenamerTests);

function RenameRequest makeRenameReq(
    TransactionId i, ObjectAddress r[], ObjectAddress w[]
);
    return RenameRequest { inputTr: InputTransaction {
        tid: i,
        trType: MessagePost,
        readObjects: arrayToVector(r),
        writtenObjects: arrayToVector(w),
        readObjectCount: fromInteger(objSetSize),
        writtenObjectCount: fromInteger(objSetSize)
    }};
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
    Vector#(NumberRenamerTests, RenameRequest) testInputs = zipWith3(
        makeRenameReq, genWith(fromInteger), arrayToVector(reads), arrayToVector(writes));

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < fromInteger(numTests));
        counter <= counter + 1;
        myRenamer.rename.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result <- myRenamer.rename.response.get();
        $display(fshow(result));
    endrule
endmodule
