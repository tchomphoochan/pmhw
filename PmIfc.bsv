////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmIfc.bsv
//  Description   : External interface for Puppetmaster and associated types.
////////////////////////////////////////////////////////////////////////////////
typedef 64 LogMaxNumberTransactions;
typedef 32 LogSizeMemory;

typedef Bit#(LogMaxNumberTransactions) TransactionId;
typedef Bit#(LogSizeMemory) ObjectAddress;

typedef struct {
    Bit#(1) valid;
    Bit#(1) write;
    ObjectAddress object;
} Object deriving(Bits, Eq);

interface PuppetmasterToHostIndication;
    method Action transactionStarted(TransactionId tid, Bit#(64) timestamp);
    method Action transactionFinished(TransactionId tid, Bit#(64) timestamp);
endinterface

interface HostToPuppetmaster;
    method Action enqueueTransaction(
        TransactionId tid,
        Object obj1,
        Object obj2,
        Object obj3,
        Object obj4,
        Object obj5,
        Object obj6,
        Object obj7,
        Object obj8,
        Object obj9,
        Object obj10,
        Object obj11,
        Object obj12,
        Object obj13,
        Object obj14,
        Object obj15,
        Object obj16
    );
endinterface

interface PmTop; 
    interface HostToPuppetmaster request;
endinterface

