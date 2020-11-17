// Module to do 2 to 1 transaction set merger
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmTypes::*;

// A transaction set is composed of read set, a write set, and bit vector indices.
// The indices are specific to a given round and indicate which transactions are in the set.
typedef Bit#(SizeSchedulingPool) ContainedTransactions;
typedef struct {
    ObjectSet readSet;
    ObjectSet writeSet;
    ContainedTransactions indices;
} TransactionSet deriving(Bits, Eq, FShow);

// Interface.
typedef Vector#(SizeSchedulingPool, TransactionSet) SchedulingRequest;
typedef TransactionSet SchedulingResponse;
typedef Server#(SchedulingRequest, SchedulingResponse) Scheduler;

(* noinline *)
function TransactionSet addSets(TransactionSet ts1, TransactionSet ts2);
    return TransactionSet{
        readSet: ts1.readSet | ts2.readSet,
        writeSet: ts1.writeSet | ts2.writeSet,
        indices: ts1.indices | ts2.indices
    };
endfunction

(* noinline *)
function TransactionSet tryMergeSets(TransactionSet ts1, TransactionSet ts2);
    let r1w2_set = ts1.readSet & ts2.writeSet;
    let w1r2_set = ts1.writeSet & ts2.readSet;
    let w1w2_set = ts1.writeSet & ts2.writeSet;
    let conflicts = r1w2_set | w1r2_set | w1w2_set;
    let has_conflicts = conflicts != 0;
    if (has_conflicts) begin
        return ts1;
    end else begin
        return addSets(ts1, ts2);
    end
endfunction

(* noinline *)
function Vector#(SizeSchedulingPool, TransactionSet) doCompare(
        Bit#(TAdd#(1,LogSizeSchedulingPool)) step,
        Bit#(TAdd#(1,LogSizeSchedulingPool)) offset,
        Bit#(TAdd#(1,LogSizeSchedulingPool)) stride,
        Vector#(SizeSchedulingPool, TransactionSet) transactions);

    Integer numComparators = valueOf(NumberComparators);
    Integer maxIndices = valueOf(SizeSchedulingPool);

    let firstIndex = (step * fromInteger(numComparators) + offset) * stride;
    let secondIndex = firstIndex + (stride >> 1);
    if (secondIndex < fromInteger(maxIndices)) begin
        transactions[firstIndex] = tryMergeSets(transactions[firstIndex], transactions[secondIndex]);
    end
    return transactions;
endfunction

// Main module.
module mkScheduler(Scheduler);
    Integer numComparators = valueOf(NumberComparators);
    Integer maxRounds = valueOf(LogSizeSchedulingPool) + 1;
    Integer maxIndices = valueOf(SizeSchedulingPool);

    Reg#(Vector#(SizeSchedulingPool, TransactionSet)) workingTransactions <- mkReg(?);
    // round is never equal to 0. -1 means the module does not have any computation running.
    // A value between 1 and maxRounds means that the tournament is running and
    //     the number of transactions has been halved that many times (minus one).
    // maxRounds means that we are done.
    Reg#(Bit#(TAdd#(1,LogSizeSchedulingPool))) round <- mkReg(-1);
    // step is the number of times we have worked on this round. 
    Reg#(Bit#(TAdd#(1,LogSizeSchedulingPool))) step <- mkReg(0);

    rule doTournament if (round != -1 && round != fromInteger(maxRounds));
        let stride = 1 << round;
        Vector#(SizeSchedulingPool, TransactionSet) newTransactions = workingTransactions;
        for (Integer i = 0; i < numComparators; i = i + 1) begin
            newTransactions = doCompare(step, fromInteger(i), stride, newTransactions);
        end
        workingTransactions <= newTransactions;
        if (((step + 1) * fromInteger(numComparators) - 1) * stride >= fromInteger(numComparators)) begin
            round <= round + 1;
            step <= 0;
        end else begin
            step <= step + 1;
        end
    endrule

    interface Put request;
        method Action put(SchedulingRequest inputTransactions) if (round == -1);
            workingTransactions <= inputTransactions;
            round <= 1;
            step <= 0;
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(SchedulingResponse) get() if (round == fromInteger(maxRounds));
            round <= -1;
            return workingTransactions[0];
        endmethod
    endinterface
endmodule

module mkSchedulerTestbench();
    Scheduler myScheduler <- mkScheduler();

    Vector#(1, SchedulingRequest) testInputs = newVector;
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[0][i] = TransactionSet{readSet: 'h43, writeSet: 'h12, indices: (1<<i)};
    end

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < 1);
        counter <= counter + 1;
        myScheduler.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result <- myScheduler.response.get();
        $display(fshow(result));
    endrule
endmodule
