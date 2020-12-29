import ClientServer::*;
import FIFOF::*;
import GetPut::*;
import SpecialFIFOs::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;

interface Puppet;
    method Action start(TransactionId tid);
    method Bool isDone();
endinterface

module mkTimedPuppet#(Integer delay)(Puppet);
    Array#(Reg#(UInt#(32))) timeLeft <- mkCReg(2, 0);

    (* no_implicit_conditions, fire_when_enabled *)
    rule incTime if (0 < timeLeft[0]);
        timeLeft[0] <= timeLeft[0] - 1;
    endrule

    method Action start(TransactionId tid);
        timeLeft[1] <= fromInteger(delay);
    endmethod

    method Bool isDone();
        return timeLeft[1] == 0;
    endmethod
endmodule
