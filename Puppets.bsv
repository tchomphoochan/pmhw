import PmCore::*;
import PmIfc::*;

interface Puppet;
    method Action start(RenamedTransaction tr);
    method Bool isDone();
endinterface

module mkTimedPuppet#(Integer delayBase)(Puppet);
    Array#(Reg#(Bit#(32))) timeLeft <- mkCReg(2, 0);

    (* no_implicit_conditions, fire_when_enabled *)
    rule incTime if (0 < timeLeft[0]);
        timeLeft[0] <= timeLeft[0] - 1;
    endrule

    method Action start(RenamedTransaction tr);
        let objCount = extend(tr.readObjectCount) + extend(tr.writtenObjectCount);
        timeLeft[1] <= objCount * fromInteger(delayBase);
    endmethod

    method Bool isDone();
        return timeLeft[1] == 0;
    endmethod
endmodule
