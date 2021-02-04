////////////////////////////////////////////////////////////////////////////////
//  Filename      : Puppet.bsv
//  Description   : Execution unit for Puppetmaster.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import GetPut::*;

import PmCore::*;
import PmIfc::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef struct {
    RenamedTransaction renamedTr;
} PuppetRequest deriving (Bits, Eq, FShow);

typedef struct {
    RenamedTransaction renamedTr;
} PuppetResponse deriving (Bits, Eq, FShow);

interface Puppet;
    interface Put#(PuppetRequest) start;
    interface Get#(PuppetResponse) finish;
    method Bool isDone();
    method TransactionId currentTid();
    method Action setClockMultiplier(ClockMultiplier multiplier);
endinterface

////////////////////////////////////////////////////////////////////////////////
/// Typeclasses.
////////////////////////////////////////////////////////////////////////////////
// Tells the arbiter whether we need to route responses back.
instance ArbRequestTC#(PuppetResponse);
    function Bool isReadRequest(a x) = False;
    function Bool isWriteRequest(a x) = True;
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Public functions.
////////////////////////////////////////////////////////////////////////////////
function Timestamp getDuration(TransactionType trType);
    return case (trType) matches
        DatabaseRead : 1;
        DatabaseWrite : 1;
        DatabaseIncrement : 2;
        DatabaseSwap : 4;
        MessageFetch : 3;
        MessagePost : 2;
    endcase;
endfunction

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
    Reg#(RenamedTransaction) tr <- mkReg(?);
    Reg#(Timestamp) cycle <- mkReg(0);

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule incTime if (1 < timeLeft[0]);
        timeLeft[0] <= timeLeft[0] - 1;
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Get finish;
        method ActionValue#(PuppetResponse) get() if (timeLeft[0] == 1);
            timeLeft[0] <= 0;
`ifdef DISPLAY_LOG
            $display("[%8d] Puppet: finishing T#%h", cycle, tr.tid);
`endif
            return PuppetResponse { renamedTr: tr };
        endmethod
    endinterface

    method Bool isDone();
        return timeLeft[1] == 0;
    endmethod

    method TransactionId currentTid();
        return tr.tid;
    endmethod

    interface Put start;
        method Action put(PuppetRequest req) if (timeLeft[1] == 0);
            timeLeft[1] <= getDuration(req.renamedTr.trType) * extend(multiplier);
            tr <= req.renamedTr;
`ifdef DISPLAY_LOG
            $display("[%8d] Puppet: starting T#%h", cycle, req.renamedTr.tid);
`endif
        endmethod
    endinterface

    method Action setClockMultiplier(ClockMultiplier m);
        multiplier <= m;
    endmethod
endmodule
