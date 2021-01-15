////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmCore.bsv
//  Description   : Common types and constants used by multiple modules.
////////////////////////////////////////////////////////////////////////////////
import Vector::*;

import PmIfc::*;

typedef 10 LogNumberLiveObjects;
typedef 3 LogNumberTransactionObjects;

typedef TAdd#(LogNumberTransactionObjects, 2) LogTransactionObjectCount;

typedef TExp#(LogNumberLiveObjects) NumberLiveObjects;
typedef TExp#(LogNumberTransactionObjects) NumberTransactionObjects;

typedef Bit#(LogNumberLiveObjects) ObjectName;
typedef Bit#(LogTransactionObjectCount) TransactionObjectCounter;
typedef Bit#(NumberLiveObjects) ObjectSet;

typedef Vector#(NumberTransactionObjects, ObjectAddress) InputObjects;
typedef Vector#(NumberTransactionObjects, ObjectName) RenamedObjects;

typedef enum { ReadObject, WrittenObject } ObjectType deriving (Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    InputObjects readObjects;
    InputObjects writtenObjects;
    TransactionObjectCounter readObjectCount;
    TransactionObjectCounter writtenObjectCount;
} InputTransaction deriving(Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    RenamedObjects readObjects;
    RenamedObjects writtenObjects;
    TransactionObjectCounter readObjectCount;
    TransactionObjectCounter writtenObjectCount;
} RenamedTransaction deriving(Bits, Eq, FShow);

typedef struct {
    ObjectSet readSet;
    ObjectSet writeSet;
} SchedulerTransaction deriving(Bits, Eq, FShow);

Integer logMaxLiveObjects = valueOf(LogNumberLiveObjects);
Integer maxLiveObjects = valueOf(NumberLiveObjects);
Integer objSetSize = valueOf(NumberTransactionObjects);
