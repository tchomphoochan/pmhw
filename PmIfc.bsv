////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmIfc.bsv
//  Description   : External interface for Puppetmaster and associated types.
////////////////////////////////////////////////////////////////////////////////
typedef 64 LogMaxNumberTransactions;
typedef 32 LogSizeMemory;
typedef 3 LogMaxNumberTransactionObjects;

typedef TAdd#(LogMaxNumberTransactionObjects, 1) LogMaxTransactionObjectCount;
typedef TExp#(LogMaxNumberTransactionObjects) MaxNumberTransactionObjects;

typedef Bit#(64) Timestamp;
typedef Bit#(60) ClockMultiplier;
typedef Bit#(LogMaxNumberTransactions) TransactionId;
typedef Bit#(LogSizeMemory) ObjectAddress;
typedef Bit#(LogMaxTransactionObjectCount) TransactionObjectCounter;

typedef enum {
    DatabaseRead,
    DatabaseWrite,
    DatabaseIncrement,
    DatabaseSwap,
    MessageFetch,
    MessagePost
} TransactionType deriving (Bits, Eq, FShow);

interface PuppetmasterToHostIndication;
    method Action transactionReceived(TransactionId tid, Timestamp timestamp);
    method Action transactionStarted(TransactionId tid, Timestamp timestamp);
    method Action transactionFinished(TransactionId tid, Timestamp timestamp);
endinterface

interface HostToPuppetmaster;
    method Action enqueueTransaction(
        TransactionId tid,
        TransactionType trType,
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

interface PmTop; 
    interface HostToPuppetmaster request;
endinterface
