// Toplevel connectal PM
import PmIfc::*;
import Puppetmaster::*;

interface PmTop; 
        interface HostToPuppetmaster request;
endinterface

module mkPmTop#(PuppetmasterToHostIndication indication)(PmTop);
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
	noAction;
endmethod

        endinterface
endmodule
