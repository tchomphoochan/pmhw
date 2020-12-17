////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmIfc.bsv
//  Description   : External interface for Puppetmaster and associated types.
////////////////////////////////////////////////////////////////////////////////
typedef 64 LogMaxNumberTransactions;
typedef 32 LogSizeMemory;

typedef Bit#(LogMaxNumberTransactions) TransactionId;
typedef Bit#(LogSizeMemory) ObjectAddress;

typedef union tagged {
    ObjectAddress ReadObject;
    ObjectAddress WriteObject;
} Object;

interface PuppetmasterToHost;
    method Action transactionStarted(TransactionId tid);
    method Action transactionFinished(TransactionId tid);
endinterface

interface Host2Puppetmaster;
    method Action enqueueTransaction(
        TransactionId tid,
        Maybe#(Object) obj1,
        Maybe#(Object) obj2,
        Maybe#(Object) obj3,
        Maybe#(Object) obj4,
        Maybe#(Object) obj5,
        Maybe#(Object) obj6,
        Maybe#(Object) obj7,
        Maybe#(Object) obj8,
        Maybe#(Object) obj9,
        Maybe#(Object) obj10,
        Maybe#(Object) obj11,
        Maybe#(Object) obj12,
        Maybe#(Object) obj13,
        Maybe#(Object) obj14,
        Maybe#(Object) obj15,
        Maybe#(Object) obj16
    );
endinterface
