////////////////////////////////////////////////////////////////////////////////
//  Filename      : HwTypes.bsv
//  Description   : External interface for Puppetmaster and associated types.
////////////////////////////////////////////////////////////////////////////////
import PmConfig::*;

// Sizes determined by C++ interface.
typedef Bit#(64) TransactionId;
typedef Bit#(64) TransactionData;
typedef Bit#(64) ObjectAddress;
typedef Bit#(32) Timestamp;
typedef Bit#(16) ClockPeriod;

// Sizes determined by design.
typedef 3 LogMaxNumberTransactionObjects;
typedef TAdd#(LogMaxNumberTransactionObjects, 1) LogMaxTransactionObjectCount;
typedef TExp#(LogMaxNumberTransactionObjects) MaxNumberTransactionObjects;
typedef Bit#(LogMaxTransactionObjectCount) TransactionObjectCounter;

// Sizes determined by config.
typedef UInt#(LogNumberPuppets) PuppetId;

/*
Hardware-to-software interface for Puppetmaster's progress.
*/
// typedef enum {
//   Renamed,
//   Freed,
//   Rejected
// } DebugType deriving (Bits, Eq, FShow);
typedef struct {
    // DebugType msgType;
    TransactionId tid;
    Timestamp startTime;
    Timestamp endTime;
} DebugMessage deriving(Bits, Eq, FShow);
interface DebugIndication;
    method Action transactionRenamed(DebugMessage m);
    method Action transactionFreed(DebugMessage m);
    method Action transactionFailed(DebugMessage m);
endinterface

/*
Software-to-hardware interface for configuring the overall Puppetmaster testing setup.
For example, software can configure whether to use real puppets on the host or use simulated puppets in FPGA.
Software can configure whether inputs should be fed from the host or generated on the fly.
*/
interface HostSetupRequest;
    /*
    Set whether to use a fake transaction streamer.
    */
    method Action setTxnDriver(Bool useSimulated);
    /*
    Set whether to use simulated puppets. If so, simulated puppets have a certain simulated clock period.
    Sadly Connectal doesn't support Maybe types, so Bool and ClockPeriod are given separately here.
    */
    method Action setSimulatedPuppets(Bool useSimulated, ClockPeriod clockPeriod);
endinterface

/*
Software-to-hardware interface for submitting transactions through the real transaction driver.
*/
interface HostTxnRequest;
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
    method Action clearState();
endinterface

/*
Puppetmaster's message to the executor.
*/
typedef struct {
    PuppetId pid;
    TransactionId tid;
    TransactionData trData;
    Timestamp cycle;
} ExecutorRequest deriving (Bits, Eq, FShow);

/*
Hardware-to-software interface for the real executor to relay the work back to host CPU.
*/
typedef ExecutorRequest WorkMessage;
interface WorkIndication;
    method Action startWork(WorkMessage msg);
endinterface

/*
Software-to-hardware interface for host worker to notify executor it's done.
*/
interface HostWorkDone;
    method Action workDone(PuppetId pid);
endinterface

/*
Transaction data parsed into tranasction type for fake executors
*/
typedef enum {
    DatabaseRead,
    DatabaseWrite,
    DatabaseTransfer,
    MessageFetch,
    MessagePost
} TransactionType deriving (Bits, Eq, FShow);

function Timestamp getDuration(TransactionType trType);
    return case (trType) matches
        DatabaseRead : 75;
        DatabaseWrite : 75;
        DatabaseTransfer : 300;
        MessageFetch : 550;
        MessagePost : 700;
    endcase;
endfunction
