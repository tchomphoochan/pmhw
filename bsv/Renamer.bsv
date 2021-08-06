////////////////////////////////////////////////////////////////////////////////
//  Filename      : Renamer.bsv
//  Description   : Maps object addresses to a smaller address space.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import BRAMFIFO::*;
import ClientServer::*;
import Connectable::*;
import FIFO::*;
import GetPut::*;
import Vector::*;

import PmConfig::*;
import PmCore::*;
import PmIfc::*;
import Shard::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef struct {
    InputTransaction inputTr;
    Timestamp startTime;
} RenameRequest deriving (Bits, Eq, FShow);

typedef struct {
    RenamedTransaction renamedTr;
    Timestamp startTime;
} DeleteRequest deriving (Bits, Eq, FShow);

typedef struct {
    RenamedTransaction renamedTr;
    SchedulerTransaction schedulerTr;
    Timestamp startTime;
} RenameResponse deriving (Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    Timestamp startTime;
} DeleteResponse deriving (Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    Timestamp startTime;
} FailResponse deriving (Bits, Eq, FShow);

interface Renamer;
    interface Server#(RenameRequest, RenameResponse) rename;
    interface Server#(DeleteRequest, DeleteResponse) delete;
    interface Get#(FailResponse) fail;
endinterface

////////////////////////////////////////////////////////////////////////////////
/// Internal types.
////////////////////////////////////////////////////////////////////////////////
typedef TExp#(LogNumberRenamerThreads) NumberRenamerThreads;
typedef TExp#(LogSizeRenamerBuffer) SizeRenamerBuffer;

////////////////////////////////////////////////////////////////////////////////
/// Typeclasses.
////////////////////////////////////////////////////////////////////////////////
instance DefaultValue#(RenameResponse);
    defaultValue = RenameResponse {
        renamedTr: defaultValue(),
        schedulerTr: defaultValue(),
        startTime: ?
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

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer maxTransactions = valueOf(NumberRenamerThreads);
Integer bufferSize = valueOf(SizeRenamerBuffer);

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
    method Maybe#(req_type) currentRequest();
    interface Put#(req_type) request;
    interface Vector#(NumberShards, Get#(ShardRequest)) distribute;
endinterface

typedef RequestDistributor#(RenameRequest) RenameRequestDistributor;
typedef RequestDistributor#(DeleteRequest) DeleteRequestDistributor;

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
    Reg#(Timestamp) startTime <- mkReg(?);
    Reg#(ObjectType) objType <- mkReg(?);
    Reg#(TransactionObjectCounter) objIndex <- mkReg(0);
`ifdef DEBUG_R
    Reg#(Timestamp) cycle <- mkReg(0);
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function ShardIndex getIndex(InputTransaction inputTr);
        let readObject = inputTr.readObjects[objIndex];
        let writtenObject = inputTr.writtenObjects[objIndex];
        return getShard(addressToName(objType == ReadObject ? readObject : writtenObject));
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
`ifdef DEBUG_R
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
`ifdef DEBUG_R
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
`ifdef DEBUG_R
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

    method Maybe#(RenameRequest) currentRequest() =
        maybeInputTr matches tagged Valid .inputTr ?
            tagged Valid RenameRequest {inputTr: inputTr, startTime: startTime} :
            tagged Invalid;

    interface Put request;
        method Action put(RenameRequest req) if (!isValid(maybeInputTr));
            if (req.inputTr.readObjectCount > 0) begin
                maybeInputTr <= tagged Valid req.inputTr;
                objType <= ReadObject;
            end else if (req.inputTr.writtenObjectCount > 0) begin
                maybeInputTr <= tagged Valid req.inputTr;
                objType <= WrittenObject;
            end
            startTime <= req.startTime;
        endmethod
    endinterface

    interface distribute = distributeIfc;
endmodule

(* synthesize *)
module mkDeleteRequestDistributor(DeleteRequestDistributor);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(RenamedTransaction) req[3] <- mkCReg(3, defaultValue());
    Reg#(Timestamp) startTime[2] <- mkCReg(2, ?);
`ifdef DEBUG_R
    Reg#(Timestamp) cycle <- mkReg(0);
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions and function-like variables.
    ////////////////////////////////////////////////////////////////////////////////
    Bool done[3];
    for (Integer i = 0; i < 3; i = i + 1) begin
        done[i] = req[i].readObjectCount == 0 && req[i].writtenObjectCount == 0;
    end
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
`ifdef DEBUG_R
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
                    !done[0] &&& getShard(currentObj.objName) == fromInteger(i)
                );
                    case (currentObj.objType) matches
                        ReadObject : begin
                            req[0].readObjectCount <= req[0].readObjectCount - 1;
`ifdef DEBUG_R
                    $display("[%8d] Renamer: deleting read object %0d from T#%h", cycle,
                             req[0].readObjectCount - 1, req[0].tid);
`endif
                        end
                        WrittenObject : begin
                            req[0].writtenObjectCount <= req[0].writtenObjectCount - 1;
`ifdef DEBUG_R
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

    method Bool canPut = done[1];

    method Maybe#(DeleteRequest) currentRequest() =
        done[2] ?
            tagged Invalid :
            tagged Valid DeleteRequest { renamedTr: req[2], startTime: startTime[1] };

    interface Put request;
        method Action put(DeleteRequest dreq) if (done[1]);
            req[1] <= dreq.renamedTr;
            startTime[0] <= dreq.startTime;
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
    interface Put#(RenameRequest) request;
    interface Put#(ShardRenameResponse) aggregate;
    interface Get#(RenameResponse) response;
    interface Get#(DeleteRequest) failure;
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
    Reg#(Timestamp) startTime <- mkReg(?);
`ifdef DEBUG_R
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
`ifdef DEBUG_R
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
        method Action put(RenameRequest req) if (!isValid(maybeRenamedTr[1]));
            maybeRenamedTr[1] <= tagged Valid RenamedTransaction {
                tid: req.inputTr.tid,
                trData: req.inputTr.trData,
                readObjects: ?,
                writtenObjects: ?,
                readObjectCount: 0,
                writtenObjectCount: 0
            };
            readSet <= 0;
            writeSet <= 0;
            totalReadObjectCount <= req.inputTr.readObjectCount;
            totalWrittenObjectCount <= req.inputTr.writtenObjectCount;
            readObjectCount <= 0;
            writtenObjectCount <= 0;
            startTime <= req.startTime;
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
`ifdef DEBUG_R
            $display("[%8d] Renamer: renamed read object %0d", cycle, readObjectCount);
`endif
                    end
                    WrittenObject: begin
                        newTr.writtenObjects[newTr.writtenObjectCount] = name;
                        newTr.writtenObjectCount = newTr.writtenObjectCount + 1;
                        writeSet <= writeSet | (1 << name);
`ifdef DEBUG_R
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
                schedulerTr: SchedulerTransaction { readSet: readSet, writeSet: writeSet },
                startTime: startTime
            };
        endmethod
    endinterface

    interface Get failure;
        method ActionValue#(DeleteRequest) get() if (
            maybeRenamedTr[0] matches tagged Valid .renamedTr &&& isDone
            && !isSuccess(renamedTr)
        );
            maybeRenamedTr[0] <= tagged Invalid;
`ifdef DEBUG_R
            $display("[%8d] Renamer: failed to rename T#%h", cycle, renamedTr.tid);
`endif
            return DeleteRequest { renamedTr : renamedTr, startTime: startTime };
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
typedef TAdd#(TMul#(2, NumberRenamerThreads), 1) NumShardAccessors;

(* synthesize *)
module mkRenamer(Renamer);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    // Input queue.
    FIFO#(RenameRequest) inputBuffer <- mkSizedBRAMFIFO(bufferSize);
    // Rename request distributors.
    Vector#(NumberRenamerThreads, RenameRequestDistributor) renameDistributors <-
        replicateM(mkRenameRequestDistributor);

    // Delete request distributors.
    Vector#(NumberRenamerThreads, DeleteRequestDistributor) deleteDistributors <-
        replicateM(mkDeleteRequestDistributor);
    // Temporary storage to keep track of delete distributor state.
    Vector#(NumberRenamerThreads, Reg#(Maybe#(DeleteRequest))) prevDelDistrReq <-
        replicateM(mkReg(tagged Invalid));

    // Shards.
    Vector#(NumberShards, Shard) shards <- replicateM(mkShard);

    // Response aggregators.
    Vector#(NumberRenamerThreads, ResponseAggregator) aggregators <-
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
    Vector#(NumberRenamerThreads, Arbiter#(NumberShards, ShardRenameResponse, void))
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
    Arbiter#(NumberRenamerThreads, RenameResponse, void) renameOutputArbiter <-
        mkArbiter(arb3, 1);
    for (Integer i = 0; i < maxTransactions; i = i + 1) begin
        mkConnection(aggregators[i].response, renameOutputArbiter.users[i].request);
    end

    // Connections from aggregators to failed transaction handler.
    let arb4 <- mkRoundRobin;
    Arbiter#(NumberRenamerThreads, DeleteRequest, void) failedTransactionArbiter <-
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
    Arbiter#(NumberRenamerThreads, DeleteResponse, void) deleteOutputArbiter <-
        mkArbiter(arb5, 1);

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions and function-like variables.
    ////////////////////////////////////////////////////////////////////////////////
    function Bool getCanPut(RequestDistributor#(a) rd) = rd.canPut();
    function Maybe#(a) getCurrentReq(RequestDistributor#(a) rd) = rd.currentRequest();

    function Bool getCanPut2(ResponseAggregator ra) = ra.canPut();

    function Bool getIsReady(Shard s) = s.isReady();

    function Bool noGrant(Arbitrate#(size) arb) = !isValid(findElem(True, arb.grant));

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    (* fire_when_enabled *)
    rule sendInputs if (
        findIndex(getCanPut2, aggregators) matches tagged Valid .index
        &&& all(getIsReady, shards)
    );
        let req <- toGet(inputBuffer).get();
        renameDistributors[index].request.put(req);
        aggregators[index].request.put(req);
    endrule

    (* fire_when_enabled *)
    rule sendDeleteResponses;
        let delDistrReq = map(getCurrentReq, deleteDistributors);
        for (Integer i = 0; i < maxTransactions; i = i + 1) begin
            if (prevDelDistrReq[i] matches tagged Valid .req
                &&& !isValid(delDistrReq[i])
                || fromMaybe(?, delDistrReq[i]).renamedTr.tid != req.renamedTr.tid) begin
                deleteOutputArbiter.users[i].request.put(DeleteResponse {
                    tid: req.renamedTr.tid,
                    startTime: req.startTime
                });
            end
            prevDelDistrReq[i] <= delDistrReq[i];
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Server rename;
        // Add input transaction to first open slot (rename distributor).
        interface Put request;
            method Action put(RenameRequest req);
                toPut(inputBuffer).put(req);
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
                deleteDistributors[index].request.put(req);
            endmethod
        endinterface

        // Notify about delete.
        interface Get response = deleteOutputArbiter.master.request;
    endinterface

    interface Get fail;
        method ActionValue#(FailResponse) get();
            let req <- failedTransactionArbiter.master.request.get();
            failedTransactionHandler.request.put(req);
            return FailResponse { tid: req.renamedTr.tid, startTime: req.startTime };
        endmethod
    endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
// Renamer tests.
////////////////////////////////////////////////////////////////////////////////
typedef 5 NumberRenamerTests;

Integer numTests = valueOf(NumberRenamerTests);

function ObjectAddress addOffset(ObjectAddress address);
    return address << addrOffset;
endfunction

function RenameRequest makeRenameReq(
    TransactionId i, ObjectAddress r[], ObjectAddress w[]
);
    return RenameRequest {
        inputTr: InputTransaction {
            tid: i,
            trData: extend(pack(MessagePost)),
            readObjects: map(addOffset, arrayToVector(r)),
            writtenObjects: map(addOffset, arrayToVector(w)),
            readObjectCount: fromInteger(objSetSize),
            writtenObjectCount: fromInteger(objSetSize)
        },
        startTime: truncate(i)
    };
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
