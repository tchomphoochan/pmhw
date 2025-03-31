import HwTypes::*;
import InternalTypes::*;
import GetPut::*;
import FIFO::*;
import SpecialFIFOs::*;
import Vector::*;

/*
Input side of the Puppetmaster scheduler.
*/
interface TxnDriver;
    /*
    Transactions to be made available to Puppetmaster.
    */
    interface Get#(InputTransaction) transactions;
endinterface


/*
Input MUX
*/
interface TxnDriverMux#(numeric type n);
    method Action select(Bit#(TLog#(n)) idx);
    interface TxnDriver txnDriver;
endinterface

module mkTxnDriverMux(Vector#(n, TxnDriver) txnDrivers, TxnDriverMux#(n) ifc);
    Reg#(Bit#(TLog#(n))) index <- mkRegU;

    method Action select(Bit#(TLog#(n)) idx);
        index <= idx;
    endmethod

    interface txnDriver = txnDrivers[index];
endmodule


/*
Real transaction driver that takes data from the host CPU.
*/
interface RealTxnDriver;
    interface Put#(InputTransaction) fromHost;
    interface TxnDriver txnDriver;
endinterface

module mkRealTxnDriver(RealTxnDriver);
    FIFO#(InputTransaction) hostFF <- mkBypassFIFO;

    interface fromHost = toPut(hostFF);
    interface TxnDriver txnDriver;
        interface transactions = toGet(hostFF);
    endinterface
endmodule
