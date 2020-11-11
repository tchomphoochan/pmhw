import Arbitrate::*;
import Vector::*;

// Numeric constant types.
typedef 64 LogMaxNumberTransactions;
typedef 32 LogSizeMemory;
typedef 3 LogNumberTransactionObjects;
typedef 10 LogNumberLiveObjects;
typedef 2 LogNumberShards;
typedef 3 LogNumberHashes;
typedef TSub#(LogNumberLiveObjects, LogNumberShards) LogSizeShard;
typedef 2 LogSizeRenamerBuffer;
typedef 4 LogSizeSchedulingPool;
typedef 1 LogNumberComparators;

// Numeric types.
typedef TExp#(LogNumberTransactionObjects) NumberTransactionObjects;
typedef TExp#(LogNumberLiveObjects) NumberLiveObjects;
typedef TExp#(LogNumberShards) NumberShards;
typedef TExp#(LogNumberHashes) NumberHashes;
typedef TExp#(LogSizeRenamerBuffer) SizeRenamerBuffer;
typedef TExp#(LogSizeSchedulingPool) SizeSchedulingPool;
typedef TExp#(LogNumberComparators) NumberComparators;

// Bit vector types.
typedef Bit#(LogMaxNumberTransactions) TransactionId;
typedef Bit#(LogSizeMemory) ObjectAddress;
typedef Bit#(LogNumberLiveObjects) ObjectName;
typedef Bit#(LogNumberShards) ShardIndex;
typedef Bit#(LogSizeShard) ShardKey;
typedef Bit#(LogSizeRenamerBuffer) RenamerEntryIndex;
typedef Bit#(NumberLiveObjects) ObjectSet;

// Record types.
typedef struct {
   TransactionId tid;
   Vector#(NumberTransactionObjects, ObjectAddress) readObjects;
   Vector#(NumberTransactionObjects, ObjectAddress) writeObjects;
} InputTransaction deriving(Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    ObjectSet readSet;
    ObjectSet writeSet;
} RenamedTransaction deriving(Bits, Eq, FShow);

// Type class instances.
instance ArbRequestTC#(RenamedTransaction);
   function Bool isReadRequest(a x) = False;
   function Bool isWriteRequest(a x) = True;
endinstance