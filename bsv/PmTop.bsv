////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmTop.bsv
//  Description   : Connectal-friendly wrapper for Puppetmaster.
////////////////////////////////////////////////////////////////////////////////
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;
import Puppetmaster::*;

interface PmTop;
    interface HostToPuppetmaster request;
endinterface

module mkPmTop#(PuppetmasterToHostIndication indication)(PmTop);
    Puppetmaster pm <- mkPuppetmaster();

    rule sendRenamedMessages;
        let tid <- pm.renamed.get();
        indication.transactionRenamed(tid);
    endrule

    rule sendFreedMessages;
        let tid <- pm.freed.get();
        indication.transactionFreed(tid);
    endrule

    rule sendFailedMessages;
        let tid <- pm.failed.get();
        indication.transactionFailed(tid);
    endrule

    interface HostToPuppetmaster request;
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
            pm.request.put(InputTransaction {
                tid: tid,
                trType: trType,
                readObjects: arrayToVector(readObjects),
                writtenObjects: arrayToVector(writtenObjects),
                readObjectCount: readObjectCount,
                writtenObjectCount: writtenObjectCount
            });
	    endmethod

        method setPuppetClockMultiplier = pm.setPuppetClockMultiplier;

        method clearState = pm.clearState;
	endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
// PmTop tests (these reuse the Puppetmaster end-to-end test data).
////////////////////////////////////////////////////////////////////////////////
module mkTestIndication(PuppetmasterToHostIndication);
    method Action transactionRenamed(TransactionId tid);
        $display("renamed T#%h", tid);
    endmethod

    method Action transactionFreed(TransactionId tid);
        $display("freed T#%h", tid);
    endmethod

    method Action transactionFailed(TransactionId tid);
        $display("failed T#%h", tid);
    endmethod
endmodule

module mkPmTopTestbench();
    let myIndication <- mkTestIndication();
    PmTop myPmTop <- mkPmTop(myIndication);

    Reg#(Bit#(32)) testIndex <- mkReg(0);
    Reg#(Timestamp) cycle <- mkReg(0);

    let testInputs = makeE2ETests();

    (* fire_when_enabled, no_implicit_conditions *)
    rule tick;
        cycle <= cycle + 1;
    endrule

    rule feed if (testIndex < fromInteger(numE2ETests));
        testIndex <= testIndex + 1;
        let testInput = testInputs[testIndex];
        let readObjs = testInput.readObjects;
        let writtenObjs = testInput.writtenObjects;
        myPmTop.request.enqueueTransaction(
            testInput.tid,
            testInput.trType,
            testInput.readObjectCount,
            readObjs[0],
            readObjs[1],
            readObjs[2],
            readObjs[3],
            readObjs[4],
            readObjs[5],
            readObjs[6],
            readObjs[7],
            testInput.writtenObjectCount,
            writtenObjs[0],
            writtenObjs[1],
            writtenObjs[2],
            writtenObjs[3],
            writtenObjs[4],
            writtenObjs[5],
            writtenObjs[6],
            writtenObjs[7]
        );
    endrule
endmodule
