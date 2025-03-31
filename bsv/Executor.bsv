import PmConfig::*;
import InternalTypes::*;
import HwTypes::*;
import GetPut::*;
import FIFO::*;
import SpecialFIFOs::*;
import Vector::*;

/*
Output side of the Puppetmaster scheduler.
*/
interface Executor;
    /*
    Puppetmaster tells the executor what transaction to run
    */
    interface Put#(ExecutorRequest) requests;
    /*
    The executor lets Puppetmaster know when a transaction is completed.
    */
    interface Get#(PuppetId) responses;
endinterface


/*
Output MUX
*/
interface ExecutorMux#(numeric type n);
    method Action select(Bit#(TLog#(n)) idx);
    interface Executor executor;
endinterface

module mkExecutorMux(Vector#(n, Executor) execs, ExecutorMux#(n) ifc);
    Reg#(Bit#(TLog#(n))) index <- mkRegU;

    method Action select(Bit#(TLog#(n)) idx);
        index <= idx;
    endmethod

    interface executor = execs[index];
endmodule


/*
Real executor that relays work message to host CPU to perform actual work.
*/
interface RealExecutor;
    interface Executor executor;
    interface Get#(WorkMessage) toHost;
    interface Put#(PuppetId) fromHost;
endinterface

module mkRealExecutor(RealExecutor);
    FIFO#(ExecutorRequest) reqFF <- mkBypassFIFO;
    FIFO#(PuppetId) doneFF <- mkBypassFIFO;

    interface toHost = toGet(reqFF);
    interface fromHost = toPut(doneFF);

    interface Executor executor;
        interface requests = toPut(reqFF);
        interface responses = toGet(doneFF);
    endinterface
endmodule


/*
Fake executor that just busy-waits.
*/
interface FakeExecutor;
    method Action setClockPeriod(ClockPeriod period);
    interface Executor executor;
endinterface

typedef TExp#(LogNumberPuppets) NumberPuppets;
Integer numPuppets = valueOf(NumberPuppets);

module mkFakeExecutor(FakeExecutor);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    Vector#(NumberPuppets, Array#(Reg#(Timestamp))) timeLeft <-
        replicateM(mkCReg(2, 0));
    Reg#(ClockPeriod) period <- mkReg(20);
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
    method Action setClockPeriod(ClockPeriod p);
        period <= p;
    endmethod

    interface Executor executor;
        interface Put requests;
            method Action put(ExecutorRequest req);
                TransactionType trType = unpack(truncate(req.trData));
                timeLeft[req.pid][1] <= getDuration(trType) / extend(period);
                $fdisplay(stderr, "[%8d] Puppet: starting T#%h", cycle, req.tid);
            endmethod
        endinterface

        interface Get responses;
            method ActionValue#(PuppetId) get() if (
                findElem(1, readVReg(map(last, timeLeftV))) matches tagged Valid .pid
            );
                timeLeft[pid][1] <= 0;
                $fdisplay(stderr, "[%8d] Puppet %d finished", cycle, pid);
                return pid;
            endmethod
        endinterface

    endinterface
endmodule