////////////////////////////////////////////////////////////////////////////////
//  Filename      : Puppets.bsv
//  Description   : Hardware-based execution units for Puppetmaster.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import GetPut::*;
import Vector::*;

import PmConfig::*;
import PmCore::*;
import PmIfc::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef TExp#(LogNumberPuppets) NumberPuppets;

interface Puppets;
    interface PuppetToHostIndication indication;
    interface Get#(PuppetId) finish;
    method Action setClockMultiplier(ClockMultiplier multiplier);
endinterface

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer numPuppets = valueOf(NumberPuppets);

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
/// number is calculated by multiplying a fixed number depending on the
/// transaction type by the clock multiplier.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
(* synthesize *)
module mkPuppets(Puppets);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Vector#(NumberPuppets, Array#(Reg#(Timestamp))) timeLeft <-
        replicateM(mkCReg(2, 0));
    Reg#(ClockMultiplier) multiplier <- mkReg(2000);
    Reg#(Timestamp) cycle <- mkReg(0);

    Vector#(NumberPuppets, Vector#(2, Reg#(Timestamp))) timeLeftV =
        map(arrayToVector, timeLeft);

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule incTime;
        for (Integer i = 0; i < numPuppets; i = i + 1) begin
            if (1 < timeLeft[i][0]) begin
                timeLeft[i][0] <= timeLeft[i][0] - 1;
            end
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Get finish;
        method ActionValue#(PuppetId) get() if (
            findElem(1, readVReg(map(last, timeLeftV))) matches tagged Valid .pid
        );
            timeLeft[pid][1] <= 0;
            $fdisplay(stderr, "[%8d] Puppet %d finished", cycle, pid);
            return pid;
        endmethod
    endinterface

    interface PuppetToHostIndication indication;
        method Action startTransaction(
                PuppetId pid, TransactionId tid, TransactionData trData, Timestamp cycle
            );
            TransactionType trType = unpack(truncate(trData));
            timeLeft[pid][1] <= getDuration(trType) * extend(multiplier);
            $fdisplay(stderr, "[%8d] Puppet: starting T#%h", cycle, tid);
        endmethod
    endinterface

    method Action setClockMultiplier(ClockMultiplier m);
        multiplier <= m;
    endmethod
endmodule
