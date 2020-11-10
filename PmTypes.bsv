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
typedef Bit#(LogMaxNumberTransactions) TransactionId;
typedef Bit#(LogSizeMemory) ObjectAddress;
typedef Bit#(LogNumberLiveObjects) ObjectName;
typedef Bit#(LogNumberShards) ShardIndex;
typedef Bit#(LogSizeShard) ShardedObjectName;
typedef Bit#(LogSizeRenamerBuffer) RenamerEntryIndex;
typedef Bit#(NumberLiveObjects) ObjectSet;
typedef Bit#(SizeSchedulingPool) TransactionIds;

// Record types.
typedef struct {
    TransactionId tid;
    ObjectAddress address;
    Bool isWrittenObject;
 } ShardRenameRequest deriving(Bits, Eq, FShow);
typedef struct {
    TransactionId tid;
    ObjectName name;
    Bool isWrittenObject;
 } ShardRenameResponse deriving(Bits, Eq, FShow);
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
