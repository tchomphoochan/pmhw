////////////////////////////////////////////////////////////////////////////////
//  Filename      : Scheduler.bsv
//  Description   : Computes a vector of compatible transactions from among the
//                  ones in the input vector based on their read and write sets.
////////////////////////////////////////////////////////////////////////////////
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmTypes::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
// A transaction set is composed of read set, a write set, and the bit vector
// indices. The indices are specific to a given round and indicate which
// transactions are in the set.
typedef Bit#(SizeSchedulingPool) ContainedTransactions;
typedef struct {
    ObjectSet readSet;
    ObjectSet writeSet;
    ContainedTransactions indices;
} TransactionSet deriving(Bits, Eq, FShow);

typedef Vector#(SizeSchedulingPool, TransactionSet) SchedulingRequest;
typedef TransactionSet SchedulingResponse;
typedef Server#(SchedulingRequest, SchedulingResponse) Scheduler;

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer numComparators = valueOf(NumberComparators);
Integer maxRounds = valueOf(LogSizeSchedulingPool) + 1;
Integer maxIndices = valueOf(SizeSchedulingPool);

////////////////////////////////////////////////////////////////////////////////
/// Helper functions.
////////////////////////////////////////////////////////////////////////////////
// Merges two transactions sets, returning their union if they don't conflict,
// and returning the first set if they do.
function TransactionSet mergeTransactionSets(TransactionSet ts1, TransactionSet ts2);
    let r1w2_set = ts1.readSet & ts2.writeSet;
    let w1r2_set = ts1.writeSet & ts2.readSet;
    let w1w2_set = ts1.writeSet & ts2.writeSet;
    let conflicts = r1w2_set | w1r2_set | w1w2_set;
    let has_conflicts = conflicts != 0;
    if (has_conflicts) begin
        return ts1;
    end else begin
        return TransactionSet{
            readSet: ts1.readSet | ts2.readSet,
            writeSet: ts1.writeSet | ts2.writeSet,
            indices: ts1.indices | ts2.indices
        };
    end
endfunction

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Tournament scheduler implementation.
///
/// We start with a vector of transaction sets, and merge them into half as many
/// transactions, each of which is either a combination of two transactions, if
/// those don't conflict, or only one of the two, if they do conlict.
///
/// Two transactions conflict if one of the reads an object that the other one
/// writes or if both of them write the same object. If two transactions
/// conflict, the first one (according to the order in the vector) progresses to
/// the next round.
///
/// Since the number of comparators might not be large enough to
/// allow comparing all pairs of transactions in the pool, some rounds can take
/// multiple cycles.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkScheduler(Scheduler);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    // Transaction sets. The number of "active" transaction sets is halved in
    // each round. These are stored in this vector contiguously, flushed to the
    // left (starting at index 0).
    Reg#(Vector#(SizeSchedulingPool, TransactionSet)) workingTransactions <- mkReg(?);
    // Tournament round.
    // - -1 means the module does not have any computation running
    // - it is never equal to 0
    // - value between 1 and maxRounds means that the tournament is running
    // - maxRounds means that we are done
    Reg#(Bit#(TAdd#(1,LogSizeSchedulingPool))) round <- mkReg(-1);
    // Number of transactions that have already been merged in this round.
    Reg#(Bit#(LogSizeSchedulingPool)) offset <- mkReg(0);

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    rule doTournament if (round != -1 && round != fromInteger(maxRounds));
        // Extract transactions to be merged, starting at offset.
        let revOffset = fromInteger(maxIndices - 1) - offset + 1;
        let rotatedTrVec = rotateBy(workingTransactions, unpack(revOffset)); // rotates to right
        Vector#(SizeComparisonPool, TransactionSet) currentTransactions = take(rotatedTrVec);
        // Merge transactions pairwise.
        let mergedTransactions = mapPairs(mergeTransactionSets, id, currentTransactions);
        // Split original vector into chunks and replace chunk corresponding
        // to these transactions in the next round.
        Vector#(TDiv#(SizeSchedulingPool, NumberComparators), Vector#(NumberComparators, TransactionSet)) chunks = toChunks(workingTransactions);
        let chunkIndex = (offset >> 1) >> valueOf(LogNumberComparators);
        chunks[chunkIndex] = mergedTransactions;
        // Concatenate chunks and update state.
        workingTransactions <= concat(chunks);
        // Compute next offset and round.
        Bit#(LogSizeSchedulingPool) nextOffset = offset + fromInteger((numComparators * 2) % maxIndices);
        let startBit = fromInteger(valueOf(LogSizeSchedulingPool) - 1) - (round - 1);
        nextOffset = nextOffset[startBit:0];  // = nextOffset % SizeSchedulingPool
        offset <= nextOffset;
        if (nextOffset == 0) begin
            round <= round + 1;
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Put request;
        method Action put(SchedulingRequest inputTransactions) if (round == -1);
            workingTransactions <= inputTransactions;
            round <= 1;
            offset <= 0;
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(SchedulingResponse) get() if (round == fromInteger(maxRounds));
            round <= -1;
            return workingTransactions[0];
        endmethod
    endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
// Scheduler tests.
////////////////////////////////////////////////////////////////////////////////
typedef 8 NumberSchedulerTests;

module mkSchedulerTestbench();
    Scheduler myScheduler <- mkScheduler();

    Vector#(NumberSchedulerTests, SchedulingRequest) testInputs = newVector;
    // Empty transactions.
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[0][i] = TransactionSet{
            readSet: 'b0,
            writeSet: 'b0,
            indices: 'b1 << i
        };
    end
    // Non-conflicting read-only transactions.
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[1][i] = TransactionSet{
            readSet: 'b1 << i,
            writeSet: 'b0,
            indices: 'b1 << i
        };
    end
    // Overlapping read-only transactions.
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[2][i] = TransactionSet{
            readSet: 'b1111 << i,
            writeSet: 'b0,
            indices: 'b1 << i
        };
    end
    // Non-conflicting read-write transactions.
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[3][i] = TransactionSet{
            readSet: 'b11 << (2 * i),
            writeSet: 'b1 << (valueOf(NumberLiveObjects) - i - 1),
            indices: 'b1 << i
        };
    end
    // Non-conflicting read-write transactions with overlapping reads.
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[4][i] = TransactionSet{
            readSet: 'b1111 << i,
            writeSet: 'b1 << (valueOf(NumberLiveObjects) - i - 1),
            indices: 'b1 << i
        };
    end
    // Transactions with read-write conflicts.
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[5][i] = TransactionSet{
            readSet: 'b11 << (2 * i),
            writeSet: 'b100 << (2 * i),
            indices: 'b1 << i
        };
    end
    // Transacions with write-write conflicts.
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[6][i] = TransactionSet{
            readSet: 'b1000 << i,
            writeSet: 'b1 << (i % 3),
            indices: 'b1 << i
        };
    end
    // Transactions with both conflicts.
    for (Integer i=0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[7][i] = TransactionSet{
            readSet: 'b1010 << i,
            writeSet: 'b101 << i,
            indices: 'b1 << i
        };
    end

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < fromInteger(valueOf(NumberSchedulerTests)));
        counter <= counter + 1;
        myScheduler.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result <- myScheduler.response.get();
        $display(fshow(result));
    endrule
endmodule
