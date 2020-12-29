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
    Reg#(Bit#(64)) count <- mkReg(0);
    Reg#(PuppetmasterResponse) prevResult <- mkReg(?);

    rule tick;
        count <= count + 1;
    endrule

    rule receiveResponseFromPm;
        let result <- pm.response.get();
        for (Integer i = 0; i < valueOf(NumberPuppets); i = i + 1) begin
            case (tuple2(prevResult[i], result[i])) matches
                {tagged Invalid, tagged Valid .tid} : indication.transactionStarted(tid, count);
                {tagged Valid .tid, tagged Invalid} : indication.transactionFinished(tid, count);
            endcase
        end
        prevResult <= result;
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
            Vector#(8, ObjectAddress) writeObjects = newVector;
            Integer readIndex = 0;
            Integer writeIndex = 0;
            for (Integer i = 0; i < 16; i = i + 1) begin
                let obj = allObjects[i];
                if (obj.write == 1) begin
                    writeObjects[writeIndex] = obj.object;
                    writeIndex = writeIndex + 1;
                end else begin
                    readObjects[readIndex] = obj.object;
                    readIndex = readIndex + 1;
                end
            end
            pm.request.put(InputTransaction {
                tid: tid,
                readObjects: readObjects,
                writeObjects: writeObjects
            });
	    endmethod
	endinterface
endmodule
