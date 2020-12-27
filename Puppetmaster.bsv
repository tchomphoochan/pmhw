////////////////////////////////////////////////////////////////////////////////
//  Filename      : Puppetmaster.bsv
//  Description   : Top-level module. Accepts a stream of trancactions and sends
//                  them to puppets for execution in parallel, avoiding
//                  conflicts between concurrently executing transactions.
////////////////////////////////////////////////////////////////////////////////
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;
import Puppets::*;
import Renamer::*;
import Scheduler::*;
import Shard::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef 4 LogNumberPuppets;

typedef TExp#(LogNumberPuppets) NumberPuppets;

typedef InputTransaction PuppetmasterRequest;
typedef Vector#(NumberPuppets, Maybe#(TransactionId)) PuppetmasterResponse;
typedef Server#(PuppetmasterRequest, PuppetmasterResponse) Puppetmaster;

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer transactionTime = 100;
Integer maxPendingTransactions = 16;

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
module mkPuppetmaster(Puppetmaster);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    // Stores renamed transactions while they are waiting to be sent to the scheduler.
    Vector#(TSub#(SizeSchedulingPool, 1), Reg#(RenamedTransaction)) buffer <-
        replicateM(mkReg(?));
    // Points to the first empty slot in the buffer.
    Reg#(SchedulingPoolIndex) bufferIndex <- mkReg(0);
    // Intermediate storage for scheduling result.
    Reg#(Bit#(TSub#(SizeSchedulingPool, 1))) pendingTrFlags <- mkReg(0);
    // Status of each puppet: executing a transaction or idle.
    Reg#(Vector#(NumberPuppets, Maybe#(RenamedTransaction))) runningTransactions <-
        mkReg(replicate(tagged Invalid));

    // Submodules.
    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();
    Vector#(NumberPuppets, Puppet) puppets <- replicateM(mkTimedPuppet(transactionTime));

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function SchedulerTransaction convertTransaction(RenamedTransaction tr);
        return SchedulerTransaction {
            readSet : tr.readSet,
            writeSet : tr.writeSet
        };
    endfunction

    function Maybe#(TransactionId) maybeGetTid(Maybe#(RenamedTransaction) maybeTr);
        case (maybeTr) matches
            tagged Valid .tr : return tagged Valid tr.tid;
            tagged Invalid : return tagged Invalid;
        endcase
    endfunction

    function Maybe#(TransactionId) extractTid(TransactionId tid, bit flag);
        case (flag) matches
            1'b0 : return tagged Invalid;
            1'b1 : return tagged Valid tid;
        endcase
    endfunction

    function SchedulerTransaction maybeTrUnion(
            Vector#(vsize, Maybe#(RenamedTransaction)) vec);
        SchedulerTransaction result;
        result.readSet = 0;
        result.writeSet =  0;
        for (Integer i = 0; i < valueOf(vsize); i = i + 1) begin
            if (vec[i] matches tagged Valid .tr) begin
                result.readSet = result.readSet | tr.readSet;
                result.writeSet = result.writeSet | tr.writeSet;
            end
        end
        return result;
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    // Put renamed transactions into a buffer.
    rule getRenamed if (bufferIndex < fromInteger(maxScheduledObjects - 1));
        bufferIndex <= bufferIndex + 1;
        let result <- renamer.response.get();
        buffer[bufferIndex] <= result;
    endrule

    // When buffer is full, send scheduling request.
    rule doSchedule if (bufferIndex == fromInteger(maxScheduledObjects - 1)
            && pendingTrFlags == 0);
        let runningTrSet = maybeTrUnion(runningTransactions);
        let transactions = readVReg(buffer);
        let converted = map(convertTransaction, transactions);
        let toSchedule = cons(runningTrSet, converted);
        scheduler.request.put(toSchedule);
    endrule

    // Retrieve indices of scheduled transacions.
    rule getScheduled if (pendingTrFlags == 0);
        let scheduled <- scheduler.response.get();
        // Lowest bit corresponds to the currently running transactions, so remove it.
        pendingTrFlags <= scheduled[maxRounds - 1 : 1];
    endrule

    // Send first (lowest-index) transaction to first idle puppet.
    // For the rest of the puppets, detect if running transaction has finished.
    rule updatePuppets if (
            findElem(tagged Invalid, runningTransactions)
            matches tagged Valid .puppetIndex
            &&& pendingTrFlags != 0);
        // Find first scheduled transaction and remove from pending set.
        SchedulingPoolIndex trIndex = truncate(pack(countZerosLSB(pendingTrFlags)));
        let startedTransaction = buffer[trIndex];
        pendingTrFlags <= pendingTrFlags & ~(1 << trIndex);
        // Move last transaction in buffer to replace transaction being started.
        if (0 < bufferIndex) begin
            buffer[trIndex] <= buffer[bufferIndex - 1];
            bufferIndex <= bufferIndex - 1;
        end
        // Start transaction on idle puppet and query running puppets.
        Vector#(NumberPuppets, Maybe#(RenamedTransaction)) newTrs;
        for (Integer i = 0; i < valueOf(NumberPuppets); i = i + 1) begin
            if (fromInteger(i) == puppetIndex) begin
                puppets[i].start(startedTransaction.tid);
                newTrs[i] = tagged Valid startedTransaction;
            end else if (puppets[i].isDone())
                    newTrs[i] = tagged Invalid;
                else
                    newTrs[i] = runningTransactions[i];
        end
        runningTransactions <= newTrs;
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    // Incoming transactions get forwarded to the renamer.
    interface Put request = renamer.request;

    interface Get response = toGet(map(maybeGetTid, runningTransactions));

endmodule

////////////////////////////////////////////////////////////////////////////////
// End-to-end puppetmaster tests.
////////////////////////////////////////////////////////////////////////////////
typedef 4 NumberPuppetmasterTests;

Integer numTests = valueOf(NumberPuppetmasterTests);

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

    Vector#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool), PuppetmasterRequest)
        testInputs = newVector;
    for (Integer i = 0; i < numTests * maxScheduledObjects; i = i + 1) begin
        testInputs[i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i].readObjects[j] = fromInteger(i * objSetSize * 2 + j * 2);
            testInputs[i].writeObjects[j] = fromInteger(case (i % numTests) matches
                0 : (i * objSetSize * 2 + j * 2 + 1);
                1 : ((i - i % 2) * objSetSize * 2 + j * 2 + 1);
                2 : ((i * objSetSize * 2 + j * 2 + 1)
                     % (maxScheduledObjects * objSetSize * 2 - 2));
                3 : ((i % 2) * objSetSize * 2 + j * 2 + 1);
            endcase);
        end
    end

    Reg#(UInt#(TAdd#(TLog#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool)), 1)))
        counter <- mkReg(0);
    Reg#(UInt#(32)) cycle <- mkReg(0);
    Reg#(PuppetmasterResponse) prevResult <- mkReg(?);

    rule feed if (counter < fromInteger(numTests * maxScheduledObjects));
        counter <= counter + 1;
        myPuppetmaster.request.put(testInputs[counter]);
    endrule

    rule stream;
        cycle <= cycle + 1;
        let result <- myPuppetmaster.response.get();
        prevResult <= result;
        if (prevResult != result)
            $display("%5d: ", cycle, fshow(map(toStatus, result)));
    endrule
endmodule
