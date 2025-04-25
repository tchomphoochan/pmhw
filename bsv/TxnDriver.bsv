import PmConfig::*;
import HwTypes::*;
import InternalTypes::*;
import GetPut::*;
import FIFO::*;
import SpecialFIFOs::*;
import Vector::*;
import BRAMFIFO::*;

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
    method Bit#(TLog#(n)) selected;
    interface TxnDriver txnDriver;
endinterface

module mkTxnDriverMux(Vector#(n, TxnDriver) txnDrivers, TxnDriverMux#(n) ifc);
    Reg#(Bit#(TLog#(n))) index <- mkRegU;

    method Action select(Bit#(TLog#(n)) idx);
        index <= idx;
    endmethod

    method Bit#(TLog#(n)) selected = index;

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

typedef 26 MaxExpectedShortObjectAddressSize;
typedef Bit#(MaxExpectedShortObjectAddressSize) ShortObjectAddress;
typedef Vector#(MaxNumberTransactionObjects, ShortObjectAddress) ShortInputObjects;

// 26*16 + 8*2 = 432 bits/txn
typedef struct {
    ShortInputObjects readObjects;
    ShortInputObjects writtenObjects;
    TransactionObjectCounter readObjectCount;
    TransactionObjectCounter writtenObjectCount;
} TransactionStub deriving (Bits, Eq, FShow);

function ObjectAddress shortToLongObjAddr(ShortObjectAddress obj);
  return extend(obj) << valueOf(NumberAddressOffsetBits);
endfunction

function ShortObjectAddress longToShortObjAddr(ObjectAddress obj);
  return truncate(obj >> valueOf(NumberAddressOffsetBits));
endfunction

function InputTransaction toRealTxn(TransactionStub stub, Bit#(64) tid);
    return InputTransaction {
        tid: tid,
        trData: 0,
        readObjects: map(shortToLongObjAddr, stub.readObjects),
        writtenObjects: map(shortToLongObjAddr, stub.writtenObjects),
        readObjectCount: stub.readObjectCount,
        writtenObjectCount: stub.writtenObjectCount
    };
endfunction

function TransactionStub toTxnStub(InputTransaction txn);
    return TransactionStub {
        readObjects: map(longToShortObjAddr, txn.readObjects),
        writtenObjects: map(longToShortObjAddr, txn.writtenObjects),
        readObjectCount: txn.readObjectCount,
        writtenObjectCount: txn.writtenObjectCount
    };
endfunction

/*
Fake transaction driver
*/
interface FakeTxnDriver;
    method Action resetState;
    interface Put#(InputTransaction) fromHost;
    method Action setStreamOpen(Bool ok);
    interface TxnDriver txnDriver;
endinterface

// 432 * 2^15 = 14.1 Mb
typedef 15 LogFakeTxnBRAMSize;
typedef TExp#(LogFakeTxnBRAMSize) FakeTxnBRAMSize;
typedef Bit#(LogFakeTxnBRAMSize) FakeTxnBRAMAddr;

module mkFakeTxnDriver(FakeTxnDriver);
  FIFO#(TransactionStub) txns <- mkSizedBRAMFIFO(valueOf(FakeTxnBRAMSize));
  Reg#(Bool) started <- mkReg(False);
  Reg#(FakeTxnBRAMAddr) outCount <- mkReg(0);

  method Action resetState;
      txns.clear();
      started <= False;
      outCount <= 0;
  endmethod

  interface Put fromHost;
      method Action put(InputTransaction txn);
          txns.enq(toTxnStub(txn));
      endmethod
  endinterface

  method Action setStreamOpen(Bool ok);
      started <= ok;
  endmethod

  interface TxnDriver txnDriver;
      interface Get transactions;
          method ActionValue#(InputTransaction) get if (started);
              let stub <- toGet(txns).get;
              let txn = toRealTxn(stub, extend(outCount));
              outCount <= outCount+1;
              return txn;
          endmethod
      endinterface
  endinterface
endmodule
