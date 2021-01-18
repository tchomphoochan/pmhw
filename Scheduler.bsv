////////////////////////////////////////////////////////////////////////////////
//  Filename      : Scheduler.bsv
//  Description   : Computes a vector of compatible transactions from among the
//                  ones in the input vector based on their read and write sets.
////////////////////////////////////////////////////////////////////////////////
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef 3 LogSizeSchedulingPool;
typedef 1 LogNumberComparators;

typedef TMul#(2, NumberComparators) SizeComparisonPool;
typedef TDiv#(SizeSchedulingPool, NumberComparators) NumberComparisonChunks;

typedef TExp#(LogSizeSchedulingPool) SizeSchedulingPool;
typedef TExp#(LogNumberComparators) NumberComparators;

typedef Bit#(LogSizeSchedulingPool) SchedulingPoolIndex;
typedef Bit#(SizeSchedulingPool) ContainedTransactions;

typedef Vector#(SizeSchedulingPool, TransactionSet) SchedulingPool;
typedef Vector#(SizeComparisonPool, TransactionSet) ComparisonPool;
typedef Vector#(NumberComparators, TransactionSet) MergedComparisonPool;

// A transaction set is composed of read set, a write set, and the bit vector
// indices. The indices are specific to a given round and indicate which
// transactions are in the set.
typedef struct {
    ObjectSet readSet;
    ObjectSet writeSet;
    ContainedTransactions indices;
} TransactionSet deriving(Bits, Eq, FShow);

typedef Vector#(SizeSchedulingPool, SchedulerTransaction) SchedulingRequest;
typedef ContainedTransactions SchedulingResponse;
typedef Server#(SchedulingRequest, SchedulingResponse) Scheduler;

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer logNumComparators = valueOf(LogNumberComparators);
Integer numComparators = valueOf(NumberComparators);
Integer maxRounds = valueOf(LogSizeSchedulingPool);
Integer maxScheduledObjects = valueOf(SizeSchedulingPool);

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
(* synthesize *)
module mkScheduler(Scheduler);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    // Transaction sets. It is Valid only when there is a request being processed.
    // The number of "active" transaction sets is halved in each round. These are
    // stored contiguously, flushed to the left (starting at index 0).
    Reg#(Maybe#(SchedulingPool)) maybeTrSets <- mkReg(tagged Invalid);
    // Tournament round.
    Reg#(SchedulingPoolIndex) round <- mkReg(0);
    // Number of transactions that have already been merged in this round.
    Reg#(SchedulingPoolIndex) offset <- mkReg(0);

    ////////////////////////////////////////////////////////////////////////////////
    /// Functions.
    ////////////////////////////////////////////////////////////////////////////////
    function TransactionSet transactionToSet(SchedulerTransaction tr, Integer i);
        return TransactionSet {
            readSet: tr.readSet,
            writeSet: tr.writeSet,
            indices: 1 << i
        };
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    rule doTournament if (maybeTrSets matches tagged Valid .trSets
                          &&& round < fromInteger(maxRounds));
        // Extract transactions to be merged, starting at offset. rotateBy rotates
        // to the right, so we compute the offset from the other end of the vector.
        SchedulingPoolIndex revOffset = fromInteger(maxScheduledObjects - 1) - offset + 1;
        SchedulingPool rotatedTrSets = rotateBy(trSets, unpack(revOffset));
        ComparisonPool activeTrSets = take(rotatedTrSets);
        // Merge transactions pairwise.
        MergedComparisonPool mergedTrSets = mapPairs(mergeTransactionSets, id, activeTrSets);
        // Split original vector into chunks and replace chunk corresponding
        // to these transactions in the next round.
        Vector#(NumberComparisonChunks, MergedComparisonPool) chunks = toChunks(trSets);
        SchedulingPoolIndex chunkIndex = (offset >> logNumComparators) >> 1;
        chunks[chunkIndex] = mergedTrSets;
        // Concatenate chunks and update state.
        SchedulingPool newTrSets = concat(chunks);
        maybeTrSets <= tagged Valid newTrSets;
        // Compute next offset and round.
        // newOffset = (offset + 2*numComparators) % (SizeSchedulingPool / 2^round)
        SchedulingPoolIndex numMergedTr = fromInteger((numComparators * 2) % maxScheduledObjects);
        SchedulingPoolIndex mask = (1 << (fromInteger(maxRounds) - round)) - 1;
        SchedulingPoolIndex newOffset = (offset + numMergedTr) & mask;
        offset <= newOffset;
        if (newOffset == 0) begin
            round <= round + 1;
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Put request;
        method Action put(SchedulingRequest req) if (!isValid(maybeTrSets));
            maybeTrSets <= tagged Valid zipWith(transactionToSet, req, genVector);
            round <= 0;
            offset <= 0;
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(SchedulingResponse) get() if (
                maybeTrSets matches tagged Valid .trSets
                &&& round == fromInteger(maxRounds));
            maybeTrSets <= tagged Invalid;
            return trSets[0].indices;
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
    for (Integer i=0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[0][i] = SchedulerTransaction {
            readSet: 'b0,
            writeSet: 'b0
        };
    end
    // Non-conflicting read-only transactions.
    for (Integer i=0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[1][i] = SchedulerTransaction {
            readSet: 'b1 << i,
            writeSet: 'b0
        };
    end
    // Overlapping read-only transactions.
    for (Integer i=0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[2][i] = SchedulerTransaction {
            readSet: 'b1111 << i,
            writeSet: 'b0
        };
    end
    // Non-conflicting read-write transactions.
    for (Integer i=0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[3][i] = SchedulerTransaction {
            readSet: 'b11 << (2 * i),
            writeSet: 'b1 << (maxLiveObjects - i - 1)
        };
    end
    // Non-conflicting read-write transactions with overlapping reads.
    for (Integer i=0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[4][i] = SchedulerTransaction {
            readSet: 'b1111 << i,
            writeSet: 'b1 << (maxLiveObjects - i - 1)
        };
    end
    // Transactions with read-write conflicts.
    for (Integer i=0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[5][i] = SchedulerTransaction {
            readSet: 'b11 << (2 * i),
            writeSet: 'b100 << (2 * i)
        };
    end
    // Transacions with write-write conflicts.
    for (Integer i=0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[6][i] = SchedulerTransaction {
            readSet: 'b1000 << i,
            writeSet: 'b1 << (i % 3)
        };
    end
    // Transactions with both conflicts.
    for (Integer i=0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[7][i] = SchedulerTransaction {
            readSet: 'b1010 << i,
            writeSet: 'b101 << i
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
