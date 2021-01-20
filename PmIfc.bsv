////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmIfc.bsv
//  Description   : External interface for Puppetmaster and associated types.
////////////////////////////////////////////////////////////////////////////////
typedef 64 LogMaxNumberTransactions;
typedef 32 LogSizeMemory;
typedef 3 LogMaxNumberTransactionObjects;

typedef TAdd#(LogMaxNumberTransactionObjects, 2) LogMaxTransactionObjectCount;
typedef TExp#(LogMaxNumberTransactionObjects) MaxNumberTransactionObjects;

typedef Bit#(LogMaxNumberTransactions) TransactionId;
typedef Bit#(LogSizeMemory) ObjectAddress;
typedef Bit#(LogMaxTransactionObjectCount) TransactionObjectCounter;

interface PuppetmasterToHostIndication;
    method Action transactionReceived(TransactionId tid, Bit#(64) timestamp);
    method Action transactionStarted(TransactionId tid, Bit#(64) timestamp);
    method Action transactionFinished(TransactionId tid, Bit#(64) timestamp);
endinterface

interface HostToPuppetmaster;
    method Action enqueueTransaction(
        TransactionId tid,
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
endinterface

interface PmTop; 
    interface HostToPuppetmaster request;
endinterface
