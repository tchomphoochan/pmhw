import PmCore::*;
import PmIfc::*;

typedef 5 LogTransactionDelayBase;

typedef TExp#(LogTransactionDelayBase) TransactionDelayBase;

typedef Bit#(TAdd#(LogTransactionObjectCount, LogTransactionDelayBase)) TransactionTimer;

interface Puppet;
    method Action start(RenamedTransaction tr);
    method Bool isDone();
endinterface

Integer delayBase = valueOf(TransactionDelayBase);

module mkTimedPuppet(Puppet);
    Reg#(TransactionTimer) timeLeft[2] <- mkCReg(2, 0);

    (* no_implicit_conditions, fire_when_enabled *)
    rule incTime if (0 < timeLeft[0]);
        timeLeft[0] <= timeLeft[0] - 1;
    endrule

    method Action start(RenamedTransaction tr);
        let objCount = tr.readObjectCount + tr.writtenObjectCount;
        timeLeft[1] <= extend(objCount) * fromInteger(delayBase);
    endmethod

    method Bool isDone();
        return timeLeft[1] == 0;
    endmethod
endmodule
