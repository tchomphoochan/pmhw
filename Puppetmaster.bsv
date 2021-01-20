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

typedef TExp#(LogNumberPuppets) NumberPuppets;

typedef InputTransaction PuppetmasterRequest;

typedef enum {
    Received,
    Started,
    Finished
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
endinterface

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer numPuppets = valueOf(NumberPuppets);

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
    // Stores renamed transactions while they are waiting to be sent to the scheduler.
    Vector#(TSub#(SizeSchedulingPool, 1), Reg#(RenamerResponse)) buffer <-
        replicateM(mkReg(?));
    // Points to the first empty slot in the buffer.
    Reg#(SchedulingPoolIndex) bufferIndex[2] <- mkCReg(2, 0);
    // Intermediate storage for scheduling result.
    Reg#(Bit#(TSub#(SizeSchedulingPool, 1))) pendingTrFlags <- mkReg(0);
    // Last transaction sent to each puppet.
    Reg#(Vector#(NumberPuppets, RenamerResponse)) sentToPuppet <- mkReg(?);
    // Clock.
    Reg#(Timestamp) cycle <- mkReg(0);
    // Store previous puppet state to detect when transactions finish running.
    Reg#(Vector#(NumberPuppets, Bool)) prevPuppetFlags <- mkReg(replicate(False));

    // Submodules.
    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();
    Vector#(NumberPuppets, Puppet) puppets <- replicateM(mkPuppet());
    // Arbiter to serialize status messages.
    let arb1 <- mkRoundRobin;
    Arbiter#(TAdd#(NumberPuppets, 1), PuppetmasterResponse, void) msgArbiter <-
        mkArbiter(arb1, 1);
    // Arbiter to serialize transaction deletion requests.
    let arb2 <- mkRoundRobin;
    Arbiter#(NumberPuppets, RenamerDeleteRequest, void) reqArbiter <- mkArbiter(arb2, 1);

    // Connect deletion request arbiter to renamer.
    mkConnection(reqArbiter.master.request, renamer.deleteRequest);

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function Maybe#(a) maybeFromBool(a value, Bool flag) =
        flag ? tagged Valid value : tagged Invalid;

    function Maybe#(a) maybeFromBit(a value, bit flag) =
        flag == 1'b1 ? tagged Valid value : tagged Invalid;

    function SchedulerTransaction getSchedTr(RenamerResponse resp) = resp.schedulerTr;

    function TransactionId getTid(RenamerResponse resp) = resp.renamedTr.tid;

    function Bool getPuppetFlag(Puppet puppet) = !puppet.isDone();

    let puppetFlags = map(getPuppetFlag, puppets);

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

    // Put renamed transactions into a buffer.
    rule getRenamed if (bufferIndex[1] < fromInteger(maxScheduledObjects - 1));
        bufferIndex[1] <= bufferIndex[1] + 1;
        let result <- renamer.response.get();
        buffer[bufferIndex[1]] <= result;
`ifdef DEBUG
        $display("[%6d] Puppetmaster: renamed %2h", cycle, result.renamedTr.tid);
`endif
    endrule

    // When buffer is full, send scheduling request.
    rule doSchedule if (bufferIndex[1] == fromInteger(maxScheduledObjects - 1)
            && pendingTrFlags == 0);
        let sentTransactions = map(getSchedTr, sentToPuppet);
        let runningTransactions = zipWith(maybeFromBool, sentTransactions, puppetFlags);
        let runningTrSet = maybeTrUnion(runningTransactions);
        let transactions = readVReg(buffer);
        let converted = map(getSchedTr, transactions);
        let toSchedule = cons(runningTrSet, converted);
        scheduler.request.put(toSchedule);
`ifdef DEBUG
        $display("[%6d] Puppetmaster: scheduler starting", cycle);
`endif
    endrule

    // Retrieve indices of scheduled transacions.
    rule getScheduled if (pendingTrFlags == 0);
        let scheduled <- scheduler.response.get();
        // Lowest bit corresponds to the currently running transactions, so remove it.
        pendingTrFlags <= scheduled[maxRounds - 1 : 1];
`ifdef DEBUG
        $display("[%6d] Puppetmaster: scheduler finished", cycle);
`endif
    endrule

    // Send first (lowest-index) pending transaction to first idle puppet.
    rule sendTransaction if (findElem(False, puppetFlags) matches tagged Valid .puppetIndex
            &&& pendingTrFlags != 0);
        // Find first scheduled transaction and remove from pending set.
        SchedulingPoolIndex trIndex = truncate(pack(countZerosLSB(pendingTrFlags)));
        pendingTrFlags <= pendingTrFlags & ~(1 << trIndex);
        // Move last transaction in buffer to replace transaction being started.
        if (0 < bufferIndex[0]) begin
            buffer[trIndex] <= buffer[bufferIndex[0] - 1];
            bufferIndex[0] <= bufferIndex[0] - 1;
        end
        // Start transaction on idle puppet.
        let started = buffer[trIndex];
        sentToPuppet[puppetIndex] <= started;
        puppets[puppetIndex].start(started.renamedTr);
`ifdef DEBUG
        $display("[%6d] Puppetmaster: starting %2h on puppet #%0d", cycle,
                 started.renamedTr.tid, puppetIndex);
`endif
    endrule

    rule sendMessages;
        prevPuppetFlags <= puppetFlags;
        for (Integer i = 0; i < numPuppets; i = i + 1) begin
            case (tuple2(prevPuppetFlags[i], puppetFlags[i])) matches
                {False, True} : begin
                    msgArbiter.users[i].request.put(PuppetmasterResponse {
                        id: getTid(sentToPuppet[i]),
                        status: Started,
                        timestamp: cycle
                    });
                end
                {True, False} : begin
                    msgArbiter.users[i].request.put(PuppetmasterResponse {
                        id: getTid(sentToPuppet[i]),
                        status: Finished,
                        timestamp: cycle
                    });
                    reqArbiter.users[i].request.put(RenamerDeleteRequest {
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
            renamer.renameRequest.put(RenamerRenameRequest { inputTr: inputTr });
            msgArbiter.users[numPuppets].request.put(PuppetmasterResponse {
                id: inputTr.tid,
                status: Received,
                timestamp: cycle
            });
        endmethod
    endinterface

    interface Get response = msgArbiter.master.request;
    
    method pollPuppets = zipWith(maybeFromBool, map(getTid, sentToPuppet), puppetFlags);

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
        testInputs[i].readObjectCount = fromInteger(objSetSize);
        testInputs[i].writtenObjectCount = fromInteger(objSetSize);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i].readObjects[j] = fromInteger(objSetSize * i * 2 + j * 2);
            testInputs[i].writtenObjects[j] = fromInteger(case (i % 4) matches
                0 : (objSetSize * i           * 2 + j * 2 + 1);  // conflict with none
                1 : (objSetSize * (i - i % 2) * 2 + j * 2 + 1);  // conflict with 1 each
                2 : (objSetSize * (i % 2)     * 2 + j * 2 + 1);  // conflict with half
                3 : (objSetSize               * 2 + j * 2 + 1);  // conflict with all
            endcase);
        end
    end

    Reg#(UInt#(TAdd#(TLog#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool)), 1)))
        counter <- mkReg(0);
    Reg#(UInt#(32)) cycle <- mkReg(0);
    Reg#(Vector#(NumberPuppets, Maybe#(TransactionId))) prevResult <- mkReg(?);

    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule

    rule feed if (counter < fromInteger(numTests * maxScheduledObjects));
        counter <= counter + 1;
        myPuppetmaster.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result = myPuppetmaster.pollPuppets();
        prevResult <= result;
        if (prevResult != result)
            $display("%5d: ", cycle, fshow(map(toStatus, result)));
    endrule
endmodule
