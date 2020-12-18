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
    Vector#(SizeSchedulingPool, Reg#(RenamedTransaction)) buffer <- replicateM(mkReg(?));
    // Points to the first empty slot in the buffer.
    Reg#(Bit#(TAdd#(LogSizeSchedulingPool, 1))) bufferIndex <- mkReg(0);
    // Transaction ids for converting the indices returned the scheduling step.
    Reg#(Vector#(SizeSchedulingPool, TransactionId)) trIds <- mkReg(?);
    // Intermediate storage for scheduling result.
    Reg#(ContainedTransactions) pendingTransactions <- mkReg(0);
    // Status of each puppet: executing a transaction or idle.
    Reg#(Vector#(NumberPuppets, Maybe#(TransactionId))) runningTransactions <- mkReg(replicate(tagged Invalid));

    // Submodules.
    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();
    Vector#(NumberPuppets, Puppet) puppets <- replicateM(mkTimedPuppet(transactionTime));

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function TransactionId getTid(RenamedTransaction tr);
        return tr.tid;
    endfunction

    function SchedulerTransaction convertTransaction(RenamedTransaction tr);
        return SchedulerTransaction {
            readSet : tr.readSet,
            writeSet : tr.writeSet
        };
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    // Put renamed transactions into a buffer.
    rule getRenamed if (bufferIndex < fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= bufferIndex + 1;
        let result <- renamer.response.get();
        buffer[bufferIndex] <= result;
    endrule

    // When buffer is full, send scheduling request.
    rule doSchedule if (bufferIndex == fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= 0;
        let transactions = readVReg(buffer);
        trIds <= map(getTid, transactions);
        scheduler.request.put(map(convertTransaction, transactions));
    endrule

    // Retrieve indices of scheduled transacions.
    rule getScheduled if (pendingTransactions == 0);
        let scheduled <- scheduler.response.get();
        pendingTransactions <= scheduled;
    endrule

    // Send first (lowest-index) transaction to first idle puppet.
    // For the rest of the puppets, detect if running transaction has finished.
    rule updatePuppets if (
            findElem(tagged Invalid, runningTransactions) matches tagged Valid .puppetIndex
            &&& pendingTransactions != 0);
        SchedulingPoolIndex trIndex = truncate(pack(countZerosLSB(pendingTransactions)));
        let startedTransactionId = trIds[trIndex];
        pendingTransactions <= pendingTransactions & ~(1 << trIndex);
        Vector#(NumberPuppets, Maybe#(TransactionId)) newTrs;
        for (Integer i = 0; i < valueOf(NumberPuppets); i = i + 1) begin
            if (fromInteger(i) == puppetIndex) begin
                puppets[i].start(startedTransactionId);
                newTrs[i] = tagged Valid startedTransactionId;
            end else begin
                newTrs[i] = (puppets[i].isDone() ? tagged Invalid : runningTransactions[i]);
            end
        end
        runningTransactions <= newTrs;
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    // Incoming transactions get forwarded to the renamer.
    interface Put request = renamer.request;

    interface Get response = toGet(runningTransactions);

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

    Vector#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool), PuppetmasterRequest) testInputs = newVector;
    for (Integer i = 0; i < numTests * maxScheduledObjects; i = i + 1) begin
        testInputs[i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i].readObjects[j] = fromInteger(i * objSetSize * 2 + j * 2);
            testInputs[i].writeObjects[j] = fromInteger(case (i % numTests) matches
                0 : (i * objSetSize * 2 + j * 2 + 1);
                1 : ((i - i % 2) * objSetSize * 2 + j * 2 + 1);
                2 : ((i * objSetSize * 2 + j * 2 + 1) % (maxScheduledObjects * objSetSize * 2 - 2));
                3 : ((i % 2) * objSetSize * 2 + j * 2 + 1);
            endcase);
        end
    end

    Reg#(UInt#(TAdd#(TLog#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool)), 1))) counter <- mkReg(0);
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
