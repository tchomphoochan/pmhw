////////////////////////////////////////////////////////////////////////////////
//  Filename      : PmTop.bsv
//  Description   : Connectal-friendly wrapper for Puppetmaster.
////////////////////////////////////////////////////////////////////////////////
import ClientServer::*;
import Connectable::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;
import Puppetmaster::*;
import Puppets::*;

interface PmTop;
    interface HostToPuppetmasterRequest request;
`ifdef EXTERNAL_PUPPETS
    interface HostToPuppetRequest puppetRequest;
`endif
endinterface

`ifdef EXTERNAL_PUPPETS
module mkPmTop#(
    PuppetmasterToHostIndication indication,
    PuppetToHostIndication puppetIndication
)(PmTop);
    Puppetmaster pm <- mkPuppetmaster(puppetIndication);
`else
(* descending_urgency = "mkConnectionGetPut, pm_sendTransaction" *)
module mkPmTop#(PuppetmasterToHostIndication indication)(PmTop);
    Puppets puppets <- mkPuppets();
    Puppetmaster pm <- mkPuppetmaster(puppets.indication);
    mkConnection(puppets.finish, toPut(pm.transactionFinished));
`endif

    mkConnection(pm.renamed, toPut(indication.transactionRenamed));
    mkConnection(pm.freed, toPut(indication.transactionFreed));
    mkConnection(pm.failed, toPut(indication.transactionFailed));

    interface HostToPuppetmasterRequest request;
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
            pm.request.put(InputTransaction {
                tid: tid,
                trData: trData,
                readObjects: arrayToVector(readObjects),
                writtenObjects: arrayToVector(writtenObjects),
                readObjectCount: readObjectCount,
                writtenObjectCount: writtenObjectCount
            });
	    endmethod

`ifdef EXTERNAL_PUPPETS
        method Action setPuppetClockPeriod(ClockPeriod period);
            // Do nothing, puppets are external.
        endmethod
`else
        method setPuppetClockPeriod = puppets.setClockPeriod;
`endif

        method clearState = pm.clearState;
	endinterface

`ifdef EXTERNAL_PUPPETS
    interface HostToPuppetRequest puppetRequest;
        method Action transactionFinished(PuppetId pid);
            pm.transactionFinished(pid);
        endmethod
    endinterface
`endif
endmodule

////////////////////////////////////////////////////////////////////////////////
// PmTop tests (these reuse the Puppetmaster end-to-end test data).
////////////////////////////////////////////////////////////////////////////////
module mkTestIndication(PuppetmasterToHostIndication);
    method Action transactionRenamed(Message m);
        $display("renamed T#%h", m.tid);
    endmethod

    method Action transactionFreed(Message m);
        $display("freed T#%h", m.tid);
    endmethod

    method Action transactionFailed(Message m);
        $display("failed T#%h", m.tid);
    endmethod
endmodule

`ifdef EXTERNAL_PUPPETS
(* descending_urgency = "mkConnectionGetPut, myPmTop_pm_sendTransaction" *)
`endif
module mkPmTopTestbench();
    let myIndication <- mkTestIndication();
`ifdef EXTERNAL_PUPPETS
    Puppets puppets <- mkPuppets();
    PmTop myPmTop <- mkPmTop(myIndication, puppets.indication);
    mkConnection(puppets.finish, toPut(myPmTop.puppetRequest.transactionFinished));
`else
    PmTop myPmTop <- mkPmTop(myIndication);
`endif

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
            testInput.trData,
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
