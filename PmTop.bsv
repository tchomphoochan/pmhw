// Toplevel connectal PM
import PmIfc::*;
import Puppetmaster::*;

module mkPmTop#(HostToPuppetmaster indication)(PmTop);
        Puppetmaster pm <- mkPuppetmaster();
        Reg#(Bit#(64)) count <- mkReg(0);

        rule tick;
                count <= count + 1;
        endrule

        rule receiveResponseFromPm;
		noAction;
                // method Action transactionStarted(TransactionId tid);
                // method Action transactionFinished(TransactionId tid);
                // indication.start                
        endrule

        interface HostToPuppetmaster request;
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
                        Maybe#(Object) obj16);
			noAction;
                	// pm.request.put(testInputs[counter][index]);
		endmethod
        endinterface
endmodule
