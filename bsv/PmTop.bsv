////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmTop.bsv
//  Description   : Top-level connections for Puppetmaster.
////////////////////////////////////////////////////////////////////////////////
import ClientServer::*;
import Connectable::*;
import GetPut::*;
import Vector::*;

import PmIfc::*;
import PmCore::*;
import Puppetmaster::*;
import TxnDriver::*;
import Executor::*;

/*
Connectal automatically sets up software-to-host communication based on this interface.
We simply need to implement those interfaces so the hardware knows what to do with these messages.
*/
interface PmTop;
    // Host can configure whether to use fake or real transaction driver and executor.
    interface HostSetupRequest hostSetupRequest;
    // Host can schedule a transaction (through the real transaction driver).
    interface HostTxnRequest hostTxnRequest;
    // Host-based worker can notify Puppetmaster if it has finished some transactions.
    interface HostWorkDone hostWorkDone;
endinterface

module mkPmTop#(
    /*
    Connectal automatically sets up host-to-software communication based on this interface.
    Hardware can call these "indication" methods to send messages.
    */
    // TODO: ensure this is lowest priority
    DebugIndication debugInd,
    // Real executor can tell the host workers there's work to do.
    WorkIndication workInd
)(PmTop);

    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////

    // Input side
    RealTxnDriver realTxnDriver <- mkRealTxnDriver;
    // TODO: add FakeTxnDriver
    TxnDriver allTxnDrivers[1] = {
        realTxnDriver.txnDriver
    };
    TxnDriverMux#(1) txnDriverMux <- mkTxnDriverMux(arrayToVector(allTxnDrivers));

    // Processing component
    // Create the actual Puppetmaster instance.
    Puppetmaster pm <- mkPuppetmaster;

    // Output side
    RealExecutor realExecutor <- mkRealExecutor;
    FakeExecutor fakeExecutor <- mkFakeExecutor;
    Executor allExecutors[2] = {
        realExecutor.executor,
        fakeExecutor.executor
    };
    ExecutorMux#(2) executorMux <- mkExecutorMux(arrayToVector(allExecutors));


    ////////////////////////////////////////////////////////////////////////////////
    /// Internal connections.
    ////////////////////////////////////////////////////////////////////////////////

    // Input side to processing component
    mkConnection(txnDriverMux.txnDriver.transactions, pm.requests);

    // Processing component to output side
    mkConnection(pm.responses, executorMux.executor.requests);

    // Output side back to processing component (free the completed transactions)
    mkConnection(executorMux.executor.responses, toPut(pm.transactionFinished));

    // Real executor controller pipes result back to host
    // so host can do the work
    mkConnection(realExecutor.toHost, toPut(workInd.startWork));

    // Puppetmaster also provides debug messages in a queue.
    // Pipe those debug messages out to host.
    mkConnection(pm.renamed, toPut(debugInd.transactionRenamed));
    mkConnection(pm.freed, toPut(debugInd.transactionFreed));
    mkConnection(pm.failed, toPut(debugInd.transactionFailed));


    ////////////////////////////////////////////////////////////////////////////////
    /// External interfaces.
    ////////////////////////////////////////////////////////////////////////////////

    // Overall test setup
    // - Configure the fake transaction driver
    // - Configure the fake executor
    interface HostSetupRequest hostSetupRequest;
        method Action setSimulatedPuppets(Maybe#(ClockPeriod) clockPeriod);
            case (clockPeriod) matches
                tagged Valid .p: begin
                    fakeExecutor.setClockPeriod(p);
                    executorMux.select(1);
                end
                tagged Invalid: begin
                    executorMux.select(0);
                end
            endcase
        endmethod
    endinterface

    // Real transaction driver takes data from host
    interface HostTxnRequest hostTxnRequest;
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
            ObjectAddress readObjects[8] = {
                readObj1,
                readObj2,
                readObj3,
                readObj4,
                readObj5,
                readObj6,
                readObj7,
                readObj8
            };
            ObjectAddress writtenObjects[8] = {
                writtenObj1,
                writtenObj2,
                writtenObj3,
                writtenObj4,
                writtenObj5,
                writtenObj6,
                writtenObj7,
                writtenObj8
            };
            realTxnDriver.fromHost.put(InputTransaction {
                tid: tid,
                trData: trData,
                readObjects: arrayToVector(readObjects),
                writtenObjects: arrayToVector(writtenObjects),
                readObjectCount: readObjectCount,
                writtenObjectCount: writtenObjectCount
            });
	    endmethod
        method clearState = pm.clearState;
	endinterface

    // Real executor controller gets updates from host
    // so it can tell Puppetmaster to free a transaction
    interface HostWorkDone hostWorkDone;
        method Action workDone(PuppetId pid);
            realExecutor.fromHost.put(pid);
        endmethod
    endinterface

endmodule
