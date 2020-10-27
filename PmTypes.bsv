import Vector::*;

// Numeric types.
typedef 64 LogMaxNumberTransactions;
typedef 32 LogSizeMemory;
typedef 3 LogNumberTransactionObjects;
typedef TExp#(LogNumberTransactionObjects) NumberTransactionObjects;
typedef 10 LogNumberLiveObjects;
typedef TExp#(LogNumberLiveObjects) NumberLiveObjects;
typedef 4 LogSizeSchedulingPool;
typedef TExp#(LogSizeSchedulingPool) SizeSchedulingPool;
typedef 2 LogSizeRenamerBuffer;
typedef TExp#(LogSizeRenamerBuffer) SizeRenamerBuffer;
typedef 2 LogNumberShards;
typedef TExp#(LogNumberShards) NumberShards;
typedef TSub#(LogNumberLiveObjects, LogNumberShards) LogSizeShard;
typedef TExp#(LogSizeShard) SizeShard;

// Primitive types.
typedef Bit#(LogSizeMemory) ObjectAddress;
typedef Bit#(LogNumberLiveObjects) ObjectName;
typedef Bit#(LogSizeShard) ShardedObjectName;
typedef Bit#(NumberLiveObjects) ObjectSet;
typedef Bit#(SizeSchedulingPool) TransactionIds;

// Record types.
typedef struct {
    Bit#(TLog#(tagCount)) tag;
    a value;
} TaggedValue#(numeric type tagCount, type a) deriving(Bits, Eq, FShow);
typedef struct {
   Bit#(LogMaxNumberTransactions) uniqueIds;
   Vector#(NumberTransactionObjects, Maybe#(Bit#(LogSizeMemory))) readObjects;
   Vector#(NumberTransactionObjects, Maybe#(Bit#(LogSizeMemory))) writeObjects;
} InputTransaction deriving(Bits, Eq, FShow);
typedef struct {
    Bit#(LogMaxNumberTransactions) uniqueIds;
    ObjectSet readSet;
    ObjectSet writeSet;
} RenamedTransaction deriving(Bits, Eq, FShow);
