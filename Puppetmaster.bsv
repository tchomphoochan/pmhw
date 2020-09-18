// Module to do 2 to 1 transaction set merger
import Vector::*;
import PmTypes::*;

// A transaction set is composed of read set, a write set, and bit vector transaction_ids
typedef struct {
    ObjectSet readSet;
    ObjectSet writeSet;
    TransactionIds transactionIds; // TODO change that name to make it clear it is internal data
} TransactionSet deriving(Bits, Eq, FShow);

Integer numberOfComparators = 2;

function TransactionSet transactionSetMerger(TransactionSet ts1, TransactionSet ts2);
    let r1w2_set = ts1.readSet & ts2.writeSet;
    let w1r2_set = ts1.writeSet & ts2.readSet;
    let w1w2_set = ts1.writeSet & ts2.writeSet;
    let conflicts = r1w2_set | w1r2_set | w1w2_set;
    let has_conflicts = conflicts != 0;
    if(has_conflicts) begin 
        return ts1;
    end else begin
        return unionTransaction(ts1,ts2); 
    end
endfunction


function TransactionSet unionTransaction(TransactionSet ts1, TransactionSet ts2);
    let readSet = ts1.readSet | ts2.readSet;
    let writeSet = ts1.writeSet | ts2.writeSet;
    let transactionIds = ts1.transactionIds | ts2.transactionIds;
    return TransactionSet{
        readSet: readSet,
        writeSet: writeSet,
        transactionIds: transactionIds
    };
endfunction

interface Scheduler;
    method Action req_Schedule(Vector#(SizeSchedulingPool, TransactionSet) inputTransactions);
    method ActionValue#(TransactionSet) resp_Schedule();
endinterface

module mkScheduler(Scheduler);
    // Declare the state (the registers of the module)
    Reg#(Vector#(SizeSchedulingPool, TransactionSet)) workingTransactions <- mkReg(?);
    Reg#(Bit#(TAdd#(1,TLog#(SizeSchedulingPool)))) round <- mkReg(-1);
    Reg#(Bit#(TAdd#(1,TLog#(SizeSchedulingPool)))) step <- mkReg(0);

    Bit#(TAdd#(1,TLog#(SizeSchedulingPool))) tournamentFinished = fromInteger(valueOf(TAdd#(1, TLog#(SizeSchedulingPool))));

    // round is never equal to 0. -1 means the module does not have any computation going
    // a value between 1 and tournamentFinished means that the tournament is running
    // tournamentFinished means that we are ready to consume the response.

    // Define the rules of the state machine
    rule doTournament if(round != -1 && round != tournamentFinished );
        let stride = 1 << round;
        Vector#(SizeSchedulingPool,TransactionSet) newTransactions = workingTransactions;
        for (Integer i = 0; i < numberOfComparators; i = i + 1) begin
            let firstIndex = (step * fromInteger(numberOfComparators) + fromInteger(i)) * stride;
            let secondIndex = firstIndex + (stride >> 1);
            if (secondIndex < fromInteger(valueOf(SizeSchedulingPool))) begin
                newTransactions[firstIndex] = transactionSetMerger(workingTransactions[firstIndex], workingTransactions[secondIndex]);
            end
        end
        workingTransactions <= newTransactions;
        if ((step * fromInteger(numberOfComparators) + fromInteger(numberOfComparators) - 1) * stride >= fromInteger(numberOfComparators)) begin
            round <= round + 1;
            step <= 0;
        end else begin
            step <= step + 1;
        end
    endrule

    // Define the methods of the module
    method Action req_Schedule(Vector#(SizeSchedulingPool, TransactionSet) inputTransactions) if (round == -1);
        workingTransactions <= inputTransactions;
        round <= 1;
        step <= 0;
    endmethod

    method ActionValue#(TransactionSet) resp_Schedule() if (round == tournamentFinished);
        round <= -1;
        return workingTransactions[0];
    endmethod
endmodule

module mkTestbench();
   Scheduler my_test_scheduler <- mkScheduler();
   Vector#(SizeSchedulingPool, TransactionSet) test_input;
   for (Integer i=0; i<valueOf(SizeSchedulingPool); i=i+1) begin
       test_input[i] = TransactionSet{
                                    readSet: 43,
                                    writeSet: 12,
                                    transactionIds: (1<<i)};

   end


   rule feed;
        my_test_scheduler.req_Schedule(test_input);
   endrule

   rule stream;
        let result <- my_test_scheduler.resp_Schedule;
        $display("result:", fshow(result));
   endrule

endmodule