// Toplevel connectal PM
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
                readObjects: arrayToVector(readObjects),
                writtenObjects: arrayToVector(writtenObjects),
                readObjectCount: readObjectCount,
                writtenObjectCount: writtenObjectCount
            });
	    endmethod
	endinterface
endmodule

module mkTestIndication(PuppetmasterToHostIndication);
    method Action transactionReceived(TransactionId tid, Bit#(64) timestamp);
        $display("[%6d] PmTop: received %4h", timestamp, tid);
    endmethod

    method Action transactionStarted(TransactionId tid, Bit#(64) timestamp);
        $display("[%6d] PmTop: started %4h", timestamp, tid);
    endmethod

    method Action transactionFinished(TransactionId tid, Bit#(64) timestamp);
        $display("[%6d] PmTop: finished %4h", timestamp, tid);
    endmethod
endmodule

typedef 64 NumberPmTopTests;

Integer numPmTopTests = valueOf(NumberPmTopTests);

module mkPmTopTestbench();
    let myIndication <- mkTestIndication();
    PmTop myPmTop <- mkPmTop(myIndication);

    Vector#(NumberPmTopTests, Vector#(2, Vector#(8, ObjectAddress))) testInputs = newVector;
    for (Integer i = 0; i < numPmTopTests; i = i + 1) begin
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i][0][j] = fromInteger(objSetSize * i * 2 + j * 2);
            testInputs[i][1][j] = fromInteger(case (i % 4) matches
                    0 : (objSetSize * i           * 2 + j * 2 + 1);  // conflict with none
                    1 : (objSetSize * (i - i % 2) * 2 + j * 2 + 1);  // conflict with one
                    2 : (objSetSize * (i % 2)     * 2 + j * 2 + 1);  // conflict with half
                    3 : (objSetSize               * 2 + j * 2 + 1);  // conflict with all
                endcase);
        end
    end

    Reg#(Bit#(32)) testIndex <- mkReg(0);
    Reg#(Bit#(64)) cycle <- mkReg(0);

    (* fire_when_enabled, no_implicit_conditions *)
    rule tick;
        cycle <= cycle + 1;
    endrule

    rule feed if (testIndex < fromInteger(numPmTopTests));
        testIndex <= testIndex + 1;
        let readObjs = testInputs[testIndex][0];
        let writtenObjs = testInputs[testIndex][1];
        myPmTop.request.enqueueTransaction(
            extend(testIndex),
            fromInteger(objSetSize),
            readObjs[0],
            readObjs[1],
            readObjs[2],
            readObjs[3],
            readObjs[4],
            readObjs[5],
            readObjs[6],
            readObjs[7],
            fromInteger(objSetSize),
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
