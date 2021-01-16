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
            Started : indication.transactionStarted(result.id, result.timestamp);
            Finished : indication.transactionFinished(result.id, result.timestamp);
        endcase
    endrule

    interface HostToPuppetmaster request;
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
            Object allObjects[16] = {
                obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9, obj10, obj11, obj12,
                obj13, obj14, obj15, obj16
            };
            Vector#(8, ObjectAddress) readObjects = newVector;
            Vector#(8, ObjectAddress) writtenObjects = newVector;
            Integer readIndex = 0;
            Integer writeIndex = 0;
            for (Integer i = 0; i < 16; i = i + 1) begin
                let obj = allObjects[i];
                if (obj.valid == 1 && obj.write == 1 && writeIndex < 8) begin
                    writtenObjects[writeIndex] = obj.object;
                    writeIndex = writeIndex + 1;
                end else if (obj.valid == 1 && obj.write == 0 && readIndex < 8) begin
                    readObjects[readIndex] = obj.object;
                    readIndex = readIndex + 1;
                end
            end
            pm.request.put(InputTransaction {
                tid: tid,
                readObjects: readObjects,
                writtenObjects: writtenObjects,
                readObjectCount: fromInteger(readIndex),
                writtenObjectCount: fromInteger(writeIndex)
            });
	    endmethod
	endinterface
endmodule

module mkTestIndication(PuppetmasterToHostIndication);
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

    Vector#(NumberPmTopTests, Vector#(16, Object)) testInputs = newVector;
    for (Integer i = 0; i < numPmTopTests; i = i + 1) begin
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i][2 * j] = Object {
                valid: 1,
                write: 0,
                object: fromInteger(objSetSize * i * 2 + j * 2)
            };
            testInputs[i][2 * j + 1] = Object {
                valid: 1,
                write: 1,
                object: fromInteger(case (i % 4) matches
                    0 : (objSetSize * i           * 2 + j * 2 + 1);  // conflict with none
                    1 : (objSetSize * (i - i % 2) * 2 + j * 2 + 1);  // conflict with 1 each
                    2 : (objSetSize * (i % 2)     * 2 + j * 2 + 1);  // conflict with half
                    3 : (objSetSize               * 2 + j * 2 + 1);  // conflict with all
                endcase)
            };
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
        let objs = testInputs[testIndex];
        myPmTop.request.enqueueTransaction(
            extend(testIndex), objs[0], objs[1], objs[2], objs[3], objs[4], objs[5],
            objs[6], objs[7], objs[8], objs[9], objs[10], objs[11], objs[12], objs[13],
            objs[14], objs[15]);
        $display("[%6d] PmTop: enqueued %2h", cycle, testIndex);
    endrule
endmodule
