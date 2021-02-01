////////////////////////////////////////////////////////////////////////////////
//  Filename      : Puppetmaster.bsv
//  Description   : Top-level module. Accepts a stream of trancactions and sends
//                  them to puppets for execution in parallel, avoiding
//                  conflicts between concurrently executing transactions.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import ClientServer::*;
import Connectable::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;
import Puppet::*;
import Renamer::*;
import Scheduler::*;
import Shard::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef 3 LogNumberPuppets;

typedef TSub#(SizeSchedulingPool, 1) NumberPendingTransactions;
typedef TExp#(LogNumberPuppets) NumberPuppets;

typedef Bit#(TLog#(TAdd#(NumberPendingTransactions, 1))) PendingTransactionCount;

typedef InputTransaction PuppetmasterRequest;

typedef enum {
    Received,
    Renamed,
    Started,
    Finished,
    Freed,
    Failed
} TransactionStatus deriving (Bits, Eq, FShow);

typedef struct {
    TransactionId id;
    TransactionStatus status;
    Timestamp timestamp;
} PuppetmasterResponse deriving (Bits, Eq, FShow);

instance ArbRequestTC#(PuppetmasterResponse);
    function Bool isReadRequest(a x) = False;
    function Bool isWriteRequest(a x) = True;
endinstance

interface Puppetmaster;
    interface Put#(PuppetmasterRequest) request;
    interface Get#(PuppetmasterResponse) response;
    method Vector#(NumberPuppets, Maybe#(TransactionId)) pollPuppets();
    method Action setPuppetClockMultiplier(ClockMultiplier multiplier);
    method Action clearState();
endinterface

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer numPuppets = valueOf(NumberPuppets);
Integer maxPendingTransactions = valueOf(NumberPendingTransactions);

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Puppetmaster implementation.
///
/// Takes a stream of incoming transactions and renames each of them. Once
/// there are enough for a batch, it send that batch to the scheduler. Returns
/// the indices of transactions which the scheduler says are OK to run.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
(* synthesize *)
module mkPuppetmaster(Puppetmaster);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    // Renamed transactions while they are waiting to be sent to the scheduler or puppets.
    Vector#(NumberPendingTransactions, Reg#(RenameResponse)) pendingTrs <-
        replicateM(mkReg(?));
    // Number of (valid) transactions in the above vector.
    Reg#(PendingTransactionCount) pendingTrCount[2] <- mkCReg(2, 0);
    // For each valid transaction: 0 = waiting to be scheduled, 1 = already scheduled.
    Reg#(Bit#(NumberPendingTransactions)) pendingTrFlags <- mkReg(0);
    // Last transaction sent to each puppet.
    Reg#(Vector#(NumberPuppets, RenameResponse)) sentToPuppet <- mkReg(?);
    // Clock.
    Reg#(Timestamp) cycle <- mkReg(0);
    // Store previous puppet state to detect when transactions finish running.
    Reg#(Vector#(NumberPuppets, Bool)) prevPuppetsDone <- mkReg(replicate(True));
    // Whether we should execute all remaining pending transactions.
    Reg#(Bool) partialMode <- mkReg(?);

    // Submodules.
    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();
    Vector#(NumberPuppets, Puppet) puppets <- replicateM(mkPuppet());
    // Arbiter to serialize status messages.
    let arb1 <- mkRoundRobin;
    Arbiter#(TAdd#(NumberPuppets, 4), PuppetmasterResponse, void) msgArbiter <-
        mkArbiter(arb1, 1);
    // Arbiter to serialize transaction deletion requests.
    let arb2 <- mkRoundRobin;
    Arbiter#(NumberPuppets, DeleteRequest, void) reqArbiter <- mkArbiter(arb2, 1);

    // Connect deletion request arbiter to renamer.
    mkConnection(reqArbiter.master.request, renamer.delete.request);

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function SchedulerTransaction getSchedTr(RenameResponse resp) = resp.schedulerTr;

    function SchedulerTransaction getSchedTrIfValid(
        RenameResponse resp, Integer i, PendingTransactionCount trCount
    );
        let emptySchedulerTr = SchedulerTransaction { readSet: 0, writeSet : 0 };
        return fromInteger(i) < trCount ? resp.schedulerTr : emptySchedulerTr;
    endfunction

    function TransactionId getTid(RenameResponse resp) = resp.renamedTr.tid;

    function Bool getIsDone(Puppet puppet) = puppet.isDone();

    function Maybe#(a) ifPuppetBusy(a value, Puppet puppet) =
        puppet.isDone() ? tagged Invalid : tagged Valid value;

    function SchedulerTransaction maybeTrUnion(
            Vector#(vsize, Maybe#(SchedulerTransaction)) maybeTrs);
        SchedulerTransaction result;
        result.readSet = 0;
        result.writeSet =  0;
        for (Integer i = 0; i < valueOf(vsize); i = i + 1) begin
            if (maybeTrs[i] matches tagged Valid .tr) begin
                result.readSet = result.readSet | tr.readSet;
                result.writeSet = result.writeSet | tr.writeSet;
            end
        end
        return result;
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule

    // Move renamed transactions to pending.
    rule getRenamed if (
        pendingTrCount[1] < fromInteger(maxPendingTransactions) && !partialMode
    );
        let result <- renamer.rename.response.get();
        pendingTrs[pendingTrCount[1]] <= result;
        pendingTrCount[1] <= pendingTrCount[1] + 1;
        msgArbiter.users[numPuppets + 1].request.put(PuppetmasterResponse {
            id: result.renamedTr.tid,
            status: Renamed,
            timestamp: cycle
        });
    endrule

    // Notify caller about failed transactions.
    rule getFailed;
        let result <- renamer.fail.get();
        msgArbiter.users[numPuppets + 2].request.put(PuppetmasterResponse {
            id: result.tid,
            status: Failed,
            timestamp: cycle
        });
    endrule

    // Notify caller about freed transactions.
    rule getFreed;
        let result <- renamer.delete.response.get();
        msgArbiter.users[numPuppets + 3].request.put(PuppetmasterResponse {
            id: result.tid,
            status: Freed,
            timestamp: cycle
        });
    endrule

    // When pending transaction buffer is full, send scheduling request.
    rule doSchedule if (
        pendingTrCount[1] == fromInteger(maxPendingTransactions) && pendingTrFlags == 0
    );
        let sentTransactions = map(getSchedTr, sentToPuppet);
        let runningTransactions = zipWith(ifPuppetBusy, sentTransactions, puppets);
        let runningTrSet = maybeTrUnion(runningTransactions);
        let transactions = readVReg(pendingTrs);
        let converted = map(getSchedTr, transactions);
        let toSchedule = cons(runningTrSet, converted);
        scheduler.request.put(toSchedule);
`ifdef DEBUG
        $display("[%8d] Puppetmaster: starting scheduler", cycle);
`endif
    endrule

    // When pending transation buffer is not full in partial mode, send scheduling request.
    rule doPartialSchedule if (
        0 < pendingTrCount[1] && pendingTrCount[1] < fromInteger(maxPendingTransactions)
        && pendingTrFlags == 0 && partialMode
    );
        let sentTransactions = map(getSchedTr, sentToPuppet);
        let runningTransactions = zipWith(ifPuppetBusy, sentTransactions, puppets);
        let runningTrSet = maybeTrUnion(runningTransactions);
        let transactions = readVReg(pendingTrs);
        let converted = zipWith3(
            getSchedTrIfValid, transactions, genVector(), replicate(pendingTrCount[1])
        );
        let toSchedule = cons(runningTrSet, converted);
        scheduler.request.put(toSchedule);
`ifdef DEBUG
        $display("[%8d] Puppetmaster: starting scheduler with partial buffer", cycle);
`endif
    endrule

    // Retrieve indices of scheduled transacions.
    rule getScheduled if (pendingTrFlags == 0);
        let scheduled <- scheduler.response.get();
        // Lowest bit corresponds to the currently running transactions, so remove it.
        pendingTrFlags <= scheduled[maxRounds - 1 : 1];
`ifdef DEBUG
        $display("[%8d] Puppetmaster: scheduler returned %b", cycle, scheduled);
`endif
    endrule

    // Send first (lowest-index) pending transaction to first idle puppet.
    rule sendTransaction if (findIndex(getIsDone, puppets) matches tagged Valid .puppetIndex
            &&& pendingTrFlags != 0);
        // Find first scheduled transaction and remove from pending set.
        PendingTransactionCount trIndex = truncate(pack(countZerosLSB(pendingTrFlags)));
        pendingTrFlags <= pendingTrFlags & ~(1 << trIndex);
        // Start transaction on idle puppet.
        let started = pendingTrs[trIndex];
        sentToPuppet[puppetIndex] <= started;
        puppets[puppetIndex].start(started.renamedTr);
        // Move last transaction in pending transaction buffer to replace it.
        if (0 < pendingTrCount[0]) begin
            let newPendingTrCount = pendingTrCount[0] - 1;
            pendingTrs[trIndex] <= pendingTrs[newPendingTrCount];
            pendingTrCount[0] <= newPendingTrCount;
        end
`ifdef DEBUG
        $display("[%8d] Puppetmaster: starting T#%h on puppet %0d", cycle,
                 started.renamedTr.tid, puppetIndex);
`endif
    endrule

    rule sendMessages;
        let puppetsDone = map(getIsDone, puppets);
        prevPuppetsDone <= puppetsDone;
        for (Integer i = 0; i < numPuppets; i = i + 1) begin
            case (tuple2(prevPuppetsDone[i], puppetsDone[i])) matches
                {True, False} : begin
                    msgArbiter.users[i].request.put(PuppetmasterResponse {
                        id: getTid(sentToPuppet[i]),
                        status: Started,
                        timestamp: cycle
                    });
                end
                {False, True} : begin
                    msgArbiter.users[i].request.put(PuppetmasterResponse {
                        id: getTid(sentToPuppet[i]),
                        status: Finished,
                        timestamp: cycle
                    });
                    reqArbiter.users[i].request.put(DeleteRequest {
                        renamedTr: sentToPuppet[i].renamedTr
                    });
                end
            endcase
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    // Incoming transactions get forwarded to the renamer.
    interface Put request;
        method Action put(InputTransaction inputTr);
            partialMode <= False;
            renamer.rename.request.put(RenameRequest { inputTr: inputTr });
            msgArbiter.users[numPuppets].request.put(PuppetmasterResponse {
                id: inputTr.tid,
                status: Received,
                timestamp: cycle
            });
        endmethod
    endinterface

    interface Get response = msgArbiter.master.request;

    method pollPuppets = zipWith(ifPuppetBusy, map(getTid, sentToPuppet), puppets);

    method Action setPuppetClockMultiplier(ClockMultiplier multiplier);
        for (Integer i = 0; i < numPuppets; i = i + 1) begin
            puppets[i].setClockMultiplier(multiplier);
        end
    endmethod

    method Action clearState();
        partialMode <= True;
`ifdef DEBUG
        $display("[%8d] Puppetmaster: clearing state", cycle);
`endif
    endmethod
endmodule

////////////////////////////////////////////////////////////////////////////////
// End-to-end puppetmaster tests.
////////////////////////////////////////////////////////////////////////////////
typedef 4 NumberE2ETestRounds;
typedef TMul#(NumberE2ETestRounds, SizeSchedulingPool) NumberE2ETests;

Integer numE2ETests = valueOf(NumberE2ETests);

function Vector#(NumberE2ETests, PuppetmasterRequest) makeE2ETests();
    Vector#(NumberE2ETests, PuppetmasterRequest) testInputs = newVector;
    for (Integer i = 0; i < numE2ETests; i = i + 1) begin
        testInputs[i].tid = fromInteger(i);
        testInputs[i].trType = case (i % 4) matches
            0 : DatabaseRead;
            1 : DatabaseWrite;
            2 : DatabaseIncrement;
            3 : DatabaseSwap;
        endcase;
        testInputs[i].readObjectCount = fromInteger(objSetSize);
        testInputs[i].writtenObjectCount = fromInteger(objSetSize);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i].readObjects[j] = fromInteger(objSetSize * i * 2 + j * 2);
            testInputs[i].writtenObjects[j] = fromInteger(
                case (i / maxScheduledObjects) matches
                    0 : (objSetSize * i           * 2 + j * 2 + 1);  // conflict with none
                    1 : (objSetSize * (i - i % 2) * 2 + j * 2 + 1);  // conflict with one
                    2 : (objSetSize * (i % 2)     * 2 + j * 2 + 1);  // conflict with half
                    3 : (objSetSize               * 2 + j * 2 + 1);  // conflict with all
                endcase
            );
        end
    end
    return testInputs;
endfunction

////////////////////////////////////////////////////////////////////////////////
// Test runner.
////////////////////////////////////////////////////////////////////////////////
typedef struct {
    Maybe#(TransactionId) maybeTid;
} PuppetStatus;

instance FShow#(PuppetStatus);
    function Fmt fshow(PuppetStatus status);
        case (status.maybeTid) matches
            { tagged Invalid } : return $format("--");
            { tagged Valid .tid } : return $format("%2h", tid);
        endcase
    endfunction
endinstance

function PuppetStatus toStatus(Maybe#(TransactionId) maybeTid);
    return PuppetStatus { maybeTid : maybeTid };
endfunction

module mkPuppetmasterTestbench();
    Puppetmaster myPuppetmaster <- mkPuppetmaster();

    Reg#(UInt#(TLog#(TAdd#(NumberE2ETests, 1)))) counter <- mkReg(0);
    Reg#(UInt#(TLog#(TAdd#(NumberE2ETests, 2)))) renamedCounter[2] <- mkCReg(2, 0);
    Reg#(UInt#(32)) cycle <- mkReg(0);
    Reg#(Vector#(NumberPuppets, Maybe#(TransactionId))) prevResult <- mkReg(?);

    let testInputs = makeE2ETests();

    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule

    rule feed if (counter < fromInteger(numE2ETests));
        counter <= counter + 1;
        myPuppetmaster.request.put(testInputs[counter]);
    endrule

    rule drain;
        let msg <- myPuppetmaster.response.get();
        case (msg.status) matches
            Renamed : renamedCounter[0] <= renamedCounter[0] + 1;
        endcase
    endrule

    rule clear if (renamedCounter[1] == fromInteger(numE2ETests));
        renamedCounter[1] <= renamedCounter[1] + 1;
        myPuppetmaster.clearState();
    endrule

    rule stream;
        let result = myPuppetmaster.pollPuppets();
        prevResult <= result;
        if (prevResult != result) begin
            $display("%5d: ", cycle, fshow(map(toStatus, result)));
        end
    endrule
endmodule
