////////////////////////////////////////////////////////////////////////////////
//  Filename      : Puppet.bsv
//  Description   : Execution unit for Puppetmaster.
////////////////////////////////////////////////////////////////////////////////
import PmCore::*;
import PmIfc::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
interface Puppet;
    method Action start(RenamedTransaction tr);
    method Bool isDone();
    method Action setClockMultiplier(ClockMultiplier multiplier);
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Puppet implementation.
///
/// Simulates executing a transaction by waiting for a number of cycles. This
/// number is calculated by multiplying TransactionDelayBase by the number of
/// objects (total, not distinct) in the transaction.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
(* synthesize *)
module mkPuppet(Puppet);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Reg#(Timestamp) timeLeft[2] <- mkCReg(2, 0);
    Reg#(ClockMultiplier) multiplier <- mkReg(2000);
    Reg#(TransactionId) tid <- mkReg(?);
    Reg#(Timestamp) cycle <- mkReg(0);

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule incTime if (0 < timeLeft[0]);
        timeLeft[0] <= timeLeft[0] - 1;
`ifdef DISPLAY_LOG
        if (timeLeft[0] - 1 == 0) begin
            $display("[%8d] Puppet: finishing T#%h", cycle, tid);
        end
`endif
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    method Action start(RenamedTransaction tr);
        Timestamp trDuration = case (tr.trType) matches
            DatabaseRead : 1;
            DatabaseWrite : 1;
            DatabaseIncrement : 2;
            DatabaseSwap : 4;
            MessageFetch : 3;
            MessagePost : 2;
        endcase;
        timeLeft[1] <= trDuration * extend(multiplier);
        tid <= tr.tid;
`ifdef DISPLAY_LOG
        $display("[%8d] Puppet: starting T#%h", cycle, tr.tid);
`endif
    endmethod

    method Bool isDone();
        return timeLeft[1] == 0;
    endmethod

    method Action setClockMultiplier(ClockMultiplier m);
        multiplier <= m;
    endmethod
endmodule
