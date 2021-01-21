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

    rule receiveResponseFromPm;
        let result <- pm.response.get();
        case (result.status) matches
            Received : indication.transactionReceived(result.id, result.timestamp);
            Started : indication.transactionStarted(result.id, result.timestamp);
            Finished : indication.transactionFinished(result.id, result.timestamp);
        endcase
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
    method Action transactionReceived(TransactionId tid, Timestamp timestamp);
        $display("[%6d] PmTop: received %4h", timestamp, tid);
    endmethod

    method Action transactionStarted(TransactionId tid, Timestamp timestamp);
        $display("[%6d] PmTop: started %4h", timestamp, tid);
    endmethod

    method Action transactionFinished(TransactionId tid, Timestamp timestamp);
        $display("[%6d] PmTop: finished %4h", timestamp, tid);
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
