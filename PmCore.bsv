////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmCore.bsv
//  Description   : Common types and constants used by multiple modules.
////////////////////////////////////////////////////////////////////////////////
import Vector::*;

import PmIfc::*;

typedef 10 LogNumberLiveObjects;

typedef TExp#(LogNumberLiveObjects) NumberLiveObjects;

typedef Bit#(LogNumberLiveObjects) ObjectName;
typedef Bit#(NumberLiveObjects) ObjectSet;

typedef Vector#(MaxNumberTransactionObjects, ObjectAddress) InputObjects;
typedef Vector#(MaxNumberTransactionObjects, ObjectName) RenamedObjects;

typedef enum { ReadObject, WrittenObject } ObjectType deriving (Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    TransactionType trType;
    InputObjects readObjects;
    InputObjects writtenObjects;
    TransactionObjectCounter readObjectCount;
    TransactionObjectCounter writtenObjectCount;
} InputTransaction deriving(Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    TransactionType trType;
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
Integer objSetSize = valueOf(MaxNumberTransactionObjects);
