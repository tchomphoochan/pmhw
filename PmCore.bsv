////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmCore.bsv
//  Description   : Common types and constants used by multiple modules.
////////////////////////////////////////////////////////////////////////////////
import Vector::*;

typedef 64 LogMaxNumberTransactions;
typedef 32 LogSizeMemory;
typedef 10 LogNumberLiveObjects;
typedef 3 LogNumberTransactionObjects;

typedef TExp#(LogNumberLiveObjects) NumberLiveObjects;
typedef TExp#(LogNumberTransactionObjects) NumberTransactionObjects;

typedef Bit#(LogMaxNumberTransactions) TransactionId;
typedef Bit#(LogSizeMemory) ObjectAddress;
typedef Bit#(NumberLiveObjects) ObjectSet;

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

Integer logMaxLiveObjects = valueOf(LogNumberLiveObjects);
Integer maxLiveObjects = valueOf(NumberLiveObjects);