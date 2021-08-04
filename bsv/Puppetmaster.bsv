////////////////////////////////////////////////////////////////////////////////
//  Filename      : Puppetmaster.bsv
//  Description   : Top-level module. Accepts a stream of trancactions and sends
//                  them to puppets for execution in parallel, avoiding
//                  conflicts between concurrently executing transactions.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import ClientServer::*;
import Connectable::*;
import FIFO::*;
import GetPut::*;
import Vector::*;

import PmConfig::*;
import PmCore::*;
import PmIfc::*;
import Puppets::*;
import Renamer::*;
import Scheduler::*;
import Shard::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef TSub#(SizeSchedulingPool, 1) NumberPendingTransactions;

typedef Bit#(TLog#(TAdd#(NumberPendingTransactions, 1))) PendingTransactionCount;

typedef InputTransaction PuppetmasterRequest;

interface Puppetmaster;
    interface Put#(PuppetmasterRequest) request;
    interface Get#(TransactionId) renamed;
    interface Get#(TransactionId) freed;
    interface Get#(TransactionId) failed;
    method Vector#(NumberPuppets, Maybe#(TransactionId)) pollPuppets();
    method Action transactionFinished(PuppetId pid);
    method Action clearState();
endinterface

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
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
module mkPuppetmaster#(PuppetToHostIndication puppetIndication)(Puppetmaster);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    // Renamed transactions while they are waiting to be sent to the scheduler or puppets.
    Vector#(NumberPendingTransactions, Reg#(RenameResponse)) pendingTrs <-
        replicateM(mkReg(defaultValue()));
    // Number of (valid) transactions in the above vector.
    Reg#(PendingTransactionCount) pendingTrCount[2] <- mkCReg(2, 0);
    // For each transaction in pendingTrs, 1 if scheduled to run.
    Reg#(Bit#(NumberPendingTransactions)) pendingTrFlags <- mkReg(0);
    // Transactions running on each puppet.
    Vector#(NumberPuppets, Reg#(RenameResponse)) runningTrs <- replicateM(mkReg(?));
    // For each transaction in runningTrs, True if running.
    Reg#(Vector#(NumberPuppets, Bool)) runningTrFlags <- mkReg(replicate(False));
    // Clock.
    Reg#(Timestamp) cycle <- mkReg(0);
    // Whether we should execute all remaining pending transactions.
    Reg#(Bool) partialMode <- mkReg(?);

    // Submodules.
    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();

    // Fifos to serialize status messages.
    FIFO#(TransactionId) renamedMsgFifo <- mkFIFO();
    FIFO#(TransactionId) freedMsgFifo <- mkFIFO();
    FIFO#(TransactionId) failedMsgFifo <- mkFIFO();

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function SchedulerTransaction getSchedTr(RenameResponse resp) = resp.schedulerTr;
    function RenamedTransaction getRenamedTr(RenameResponse resp) = resp.renamedTr;
    function TransactionId getTid(RenameResponse resp) = resp.renamedTr.tid;

    function Maybe#(a) boolToMaybe(a value, Bool valid) =
        valid ? tagged Valid value : tagged Invalid;

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
    /// Variables.
    ////////////////////////////////////////////////////////////////////////////////
    let pendingSchedTrs = map(getSchedTr, readVReg(pendingTrs));

    let runningSchedTrs = map(getSchedTr, readVReg(runningTrs));
    let maybeRunningSchedTrs = zipWith(boolToMaybe, runningSchedTrs, runningTrFlags);
    let runningSchedTr = maybeTrUnion(maybeRunningSchedTrs);
    let runningTids = map(getTid, readVReg(runningTrs));

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
        renamedMsgFifo.enq(result.renamedTr.tid);
        $fdisplay(stderr, "[%8d] Puppetmaster: renamed T#%h", cycle, result.renamedTr.tid);
    endrule

    // Notify caller about failed transactions.
    rule getFailed;
        let result <- renamer.fail.get();
        failedMsgFifo.enq(result.tid);
        $fdisplay(stderr, "[%8d] Puppetmaster: failed T#%h", cycle, result.tid);
    endrule

    // Notify caller about freed transactions.
    rule getFreed;
        let result <- renamer.delete.response.get();
        freedMsgFifo.enq(result.tid);
        $fdisplay(stderr, "[%8d] Puppetmaster: freed T#%h", cycle, result.tid);
    endrule

    // When pending buffer is full or partial mode is active, send scheduling request.
    rule doSchedule if (
        pendingTrFlags == 0 && (
            pendingTrCount[1] == fromInteger(maxPendingTransactions) || (
                0 < pendingTrCount[1]
                && pendingTrCount[1] < fromInteger(maxPendingTransactions)
                && partialMode
            )
        )
    );
        scheduler.request.put(cons(runningSchedTr, pendingSchedTrs));
`ifdef DEBUG
        $fdisplay(stderr, "[%8d] Puppetmaster: starting scheduler with ", cycle,
                  fshow(map(getRenamedTr, readVReg(pendingTrs))));
`endif
    endrule

    // Retrieve indices of scheduled transacions.
    rule getScheduled if (pendingTrFlags == 0);
        let scheduled <- scheduler.response.get();
        // Lowest bit corresponds to the currently running transactions, so remove it.
        match {.realScheduled, .*} = split(scheduled);
        Bit#(NumberPendingTransactions) newPendingTrFlags = realScheduled;
        // In partial mode, some of the "transactions" are just empty placeholders.
        let mask = (1 << pendingTrCount[1]) - 1;
        newPendingTrFlags = newPendingTrFlags & mask;
        pendingTrFlags <= newPendingTrFlags;
`ifdef DEBUG
        $fdisplay(stderr, "[%8d] Puppetmaster: scheduler returned %b, new flags %b",
                  cycle, scheduled, newPendingTrFlags);
`endif
    endrule

    // Send first (lowest-index) pending transaction to first idle puppet.
    rule sendTransaction if (findElem(False, runningTrFlags) matches tagged Valid .puppetIndex
            &&& pendingTrFlags != 0);
        // Find first scheduled transaction and remove from pending set.
        PendingTransactionCount trIndex = truncate(pack(countZerosLSB(pendingTrFlags)));
        let newPendingTrFlags = pendingTrFlags & ~(1 << trIndex);
        // Start transaction on idle puppet.
        let started = pendingTrs[trIndex];
        runningTrs[puppetIndex] <= started;
        runningTrFlags[puppetIndex] <= True;
        puppetIndication.startTransaction(
            puppetIndex, started.renamedTr.tid, started.renamedTr.trData
        );
        // If transaction is not the last in the buffer, move the last to this position.
        let newPendingTrCount = pendingTrCount[0] - 1;
        if (trIndex != newPendingTrCount) begin
            newPendingTrFlags[trIndex] = newPendingTrFlags[newPendingTrCount];
            pendingTrs[trIndex] <= pendingTrs[newPendingTrCount];
        end
        // Remove last transaction from buffer.
        pendingTrs[newPendingTrCount] <= defaultValue();
        pendingTrCount[0] <= newPendingTrCount;
        newPendingTrFlags = newPendingTrFlags & ~(1 << newPendingTrCount);
        pendingTrFlags <= newPendingTrFlags;
        $fdisplay(stderr, "[%8d] Puppetmaster: starting T#%h on puppet %0d", cycle,
                  started.renamedTr.tid, puppetIndex);
`ifdef DEBUG
        $fdisplay(stderr, "[%8d] Puppetmaster: new flags are %b", newPendingTrFlags);
`endif
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    // Incoming transactions get forwarded to the renamer.
    interface Put request;
        method Action put(InputTransaction inputTr);
            partialMode <= False;
            renamer.rename.request.put(RenameRequest { inputTr: inputTr });
            $fdisplay(stderr, "[%8d] Puppetmaster: enqueued T#%h", cycle, inputTr.tid);
        endmethod
    endinterface

    interface renamed = toGet(renamedMsgFifo);
    interface freed = toGet(freedMsgFifo);
    interface failed = toGet(failedMsgFifo);

    method pollPuppets = zipWith(boolToMaybe, runningTids, runningTrFlags);

    method Action transactionFinished(PuppetId pid);
        runningTrFlags[pid] <= False;
        renamer.delete.request.put(DeleteRequest {
            renamedTr: runningTrs[pid].renamedTr
        });
        $fdisplay(stderr, "[%8d] Puppetmaster: finished T#%h on puppet %0d", cycle,
                  runningTrs[pid].renamedTr.tid, pid);
    endmethod

    method Action clearState();
        partialMode <= True;
`ifdef DEBUG
        $fdisplay(stderr, "[%8d] Puppetmaster: clearing state", cycle);
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
        testInputs[i].trData = extend(pack(case (i % 4) matches
            0 : DatabaseRead;
            1 : DatabaseWrite;
            2 : DatabaseIncrement;
            3 : DatabaseSwap;
        endcase));
        testInputs[i].readObjectCount = fromInteger(objSetSize);
        testInputs[i].writtenObjectCount = fromInteger(objSetSize);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i].readObjects[j] =
                fromInteger(objSetSize * i * 2 + j * 2) << addrOffset;
            testInputs[i].writtenObjects[j] = fromInteger(
                case (i / maxScheduledObjects) matches
                    0 : (objSetSize * i           * 2 + j * 2 + 1);  // conflict with none
                    1 : (objSetSize * (i - i % 2) * 2 + j * 2 + 1);  // conflict with one
                    2 : (objSetSize * (i % 2)     * 2 + j * 2 + 1);  // conflict with half
                    3 : (objSetSize               * 2 + j * 2 + 1);  // conflict with all
                endcase
            ) << addrOffset;
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

(* mutually_exclusive = "feed, clear" *)
(* descending_urgency = "mkConnectionGetPut, myPuppetmaster_sendTransaction" *)
(* descending_urgency = "drainFailed, drainFreed" *)
module mkPuppetmasterTestbench();
    Puppets myPuppets <- mkPuppets();
    Puppetmaster myPuppetmaster <- mkPuppetmaster(myPuppets.indication);
    mkConnection(myPuppets.finish, toPut(myPuppetmaster.transactionFinished));

    Reg#(UInt#(TLog#(TAdd#(NumberE2ETests, 1)))) counter <- mkReg(0);
    Reg#(UInt#(TLog#(TAdd#(NumberE2ETests, 2)))) renamedCounter[2] <- mkCReg(2, 0);
    Reg#(UInt#(TLog#(TAdd#(NumberE2ETests, 2)))) finishedCounter[2] <- mkCReg(2, 0);
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

    rule drainRenamed;
        let _ <- myPuppetmaster.renamed.get();
        renamedCounter[0] <= renamedCounter[0] + 1;
    endrule

    rule drainFreed;
        let _ <- myPuppetmaster.freed.get();
        finishedCounter[0] <= finishedCounter[0] + 1;
    endrule

    rule drainFailed;
        let _ <- myPuppetmaster.failed.get();
        finishedCounter[0] <= finishedCounter[0] + 1;
    endrule

    rule clear if (renamedCounter[1] == fromInteger(numE2ETests));
        renamedCounter[1] <= renamedCounter[1] + 1;
        myPuppetmaster.clearState();
    endrule

    rule finish if (finishedCounter[1] == fromInteger(numE2ETests));
        $finish();
    endrule

    rule stream;
        let result = myPuppetmaster.pollPuppets();
        prevResult <= result;
        if (prevResult != result) begin
            $fdisplay(stderr, "[%8d] Puppetmaster: running ", cycle,
                      fshow(map(toStatus, result)));
            $display("%5d: ", cycle, fshow(map(toStatus, result)));
        end
    endrule
endmodule
