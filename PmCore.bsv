////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmCore.bsv
//  Description   : Common types and constants used by multiple modules.
////////////////////////////////////////////////////////////////////////////////
import Vector::*;

import PmIfc::*;

typedef 10 LogNumberLiveObjects;
typedef 3 LogNumberTransactionObjects;

typedef TExp#(LogNumberLiveObjects) NumberLiveObjects;
typedef TExp#(LogNumberTransactionObjects) NumberTransactionObjects;

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

typedef struct {
    ObjectSet readSet;
    ObjectSet writeSet;
} SchedulerTransaction deriving(Bits, Eq, FShow);

Integer logMaxLiveObjects = valueOf(LogNumberLiveObjects);
Integer maxLiveObjects = valueOf(NumberLiveObjects);