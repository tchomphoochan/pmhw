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
