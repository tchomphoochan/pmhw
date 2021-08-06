////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmIfc.bsv
//  Description   : External interface for Puppetmaster and associated types.
////////////////////////////////////////////////////////////////////////////////
import PmConfig::*;

// Sizes determined by C++ interface.
typedef Bit#(64) TransactionId;
typedef Bit#(64) TransactionData;
typedef Bit#(64) ObjectAddress;
typedef Bit#(32) Timestamp;
typedef Bit#(16) ClockMultiplier;

// Sizes determined by design.
typedef 3 LogMaxNumberTransactionObjects;
typedef TAdd#(LogMaxNumberTransactionObjects, 1) LogMaxTransactionObjectCount;
typedef TExp#(LogMaxNumberTransactionObjects) MaxNumberTransactionObjects;
typedef Bit#(LogMaxTransactionObjectCount) TransactionObjectCounter;

// Sizes determined by config.
typedef UInt#(LogNumberPuppets) PuppetId;

typedef enum {
    DatabaseRead,
    DatabaseWrite,
    DatabaseIncrement,
    DatabaseSwap,
    MessageFetch,
    MessagePost
} TransactionType deriving (Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    Timestamp cycle;
} Message deriving(Bits, Eq, FShow);

interface PuppetmasterToHostIndication;
    method Action transactionRenamed(Message m);
    method Action transactionFreed(Message m);
    method Action transactionFailed(Message m);
endinterface

interface HostToPuppetmasterRequest;
    method Action enqueueTransaction(
        TransactionId tid,
        TransactionData trData,
        TransactionObjectCounter readObjectCount,
        ObjectAddress readObj1,
        ObjectAddress readObj2,
        ObjectAddress readObj3,
        ObjectAddress readObj4,
        ObjectAddress readObj5,
        ObjectAddress readObj6,
        ObjectAddress readObj7,
        ObjectAddress readObj8,
        TransactionObjectCounter writtenObjectCount,
        ObjectAddress writtenObj1,
        ObjectAddress writtenObj2,
        ObjectAddress writtenObj3,
        ObjectAddress writtenObj4,
        ObjectAddress writtenObj5,
        ObjectAddress writtenObj6,
        ObjectAddress writtenObj7,
        ObjectAddress writtenObj8
    );
    method Action setPuppetClockMultiplier(ClockMultiplier multiplier);
    method Action clearState();
endinterface

interface PuppetToHostIndication;
    method Action startTransaction(
        PuppetId pid,
        TransactionId tid,
        TransactionData trData,
        Timestamp cycle
    );
endinterface

interface HostToPuppetRequest;
    method Action transactionFinished(PuppetId pid);
endinterface
