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
    Array#(Reg#(UInt#(32))) elapsed <- mkCReg(2, 0);

    rule incTime if (elapsed[1] < fromInteger(delay));
        elapsed[1] <= elapsed[1] + 1;
    endrule

    method Action start(TransactionId tid);
        elapsed[0] <= 0;
    endmethod

    method Bool isDone();
        return elapsed[1] == fromInteger(delay);
    endmethod
endmodule