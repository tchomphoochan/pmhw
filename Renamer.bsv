import Arbiter::*;
import Arbitrate::*;
import ClientServer::*;
import Connectable::*;
import GetPut::*;
import Vector::*;

import PmTypes::*;
import Shard::*;
interface Renamer;
    method Action putInputTransaction(InputTransaction it);
    method ActionValue#(RenamedTransaction) getRenamedTransaction();
endinterface

typedef struct {
    InputTransaction inputTr;
    Bit#(TAdd#(LogNumberTransactionObjects, 1)) readSetIndex;
    Bit#(TAdd#(LogNumberTransactionObjects, 1)) writeSetIndex;
} InputBufferEntry deriving(Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    ObjectSet readSet;
    ObjectSet writeSet;
    Bit#(TAdd#(LogNumberTransactionObjects, 1)) readObjectCount;
    Bit#(TAdd#(LogNumberTransactionObjects, 1)) writtenObjectCount;
} SetBufferEntry deriving(Bits, Eq, FShow);

instance ArbRequestTC#(ShardRenameRequest);
   function Bool isReadRequest(a x) = True;
   function Bool isWriteRequest(a x) = False;
endinstance

instance ArbRequestTC#(ShardRenameResponse);
   function Bool isReadRequest(a x) = False;
   function Bool isWriteRequest(a x) = True;
endinstance

instance ArbRequestTC#(RenamedTransaction);
   function Bool isReadRequest(a x) = False;
   function Bool isWriteRequest(a x) = True;
endinstance

module mkRenamer(Renamer);
    ////////////////////////////////////////////////////////////////////////////
    /// Constants
    ////////////////////////////////////////////////////////////////////////////
    //Integer maxTransactions = valueOf(SizeRenamerBuffer);
    //Integer numShards = valueOf(NumberShards);
    //Integer setSize = valueOf(NumberTransactionObjects);

    ////////////////////////////////////////////////////////////////////////////
    /// State
    ////////////////////////////////////////////////////////////////////////////
    // Shards.
    Vector#(NumberShards, Shard) shards <- replicateM(mkShard);
    // Input buffers.
    Vector#(SizeRenamerBuffer, Reg#(Maybe#(InputBufferEntry))) inputBuffer <- replicateM(mkReg(tagged Invalid));
    Reg#(Bit#(LogSizeRenamerBuffer)) inputBufferStart <- mkReg(0);
    Reg#(Bit#(LogSizeRenamerBuffer)) inputBufferEnd <- mkReg(0);
    // Arbiter.
    Vector#(NumberShards, Arbiter#(SizeRenamerBuffer, ShardRenameRequest, ShardRenameResponse)) shardArbiters;
    for (Integer i = 0; i < valueOf(NumberShards); i = i + 1) begin
        let arb <- mkRoundRobin;
        shardArbiters[i] <- mkArbiter(arb, 1);
        mkConnection(shardArbiters[i].master, shards[i]);
    end
    Vector#(SizeRenamerBuffer, Arbiter#(NumberShards, ShardRenameResponse, void)) transactionArbiters;
    for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
        let arb <- mkRoundRobin;
        transactionArbiters[i] <- mkArbiter(arb, 1);
        for (Integer j = 0; j < valueOf(NumberShards); j = j + 1) begin
            mkConnection(shardArbiters[j].users[i].response, transactionArbiters[i].users[j].request);
        end
    end
    Vector#(SizeRenamerBuffer, Client#(RenamedTransaction, ShardRenameResponse)) aggregators;
    for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
        aggregators[i] <- mkShardRenameResponseAggregator;
        mkConnection(transactionArbiters[i].master.request, aggregators[i].response);
    end
    let arbi <- mkRoundRobin;
    Arbiter#(SizeRenamerBuffer, RenamedTransaction, void) outputArbiter <- mkArbiter(arbi, 1);
    for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
        mkConnection(aggregators[i].request, outputArbiter.users[i].request);
    end


    ////////////////////////////////////////////////////////////////////////////
    /// Functions
    ////////////////////////////////////////////////////////////////////////////
    function ObjectAddress getShard(ObjectAddress objectId);
        return objectId[valueOf(LogNumberShards) - 1:0];  // FIXME: inconsistent shard index.
    endfunction

    ////////////////////////////////////////////////////////////////////////////
    /// Rules
    ////////////////////////////////////////////////////////////////////////////
    rule scatter;
        for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
            let entry = inputBuffer[i];
            if (isValid(entry)) begin
                let newEntry = fromMaybe(?, entry);
                if (newEntry.readSetIndex < fromInteger(valueOf(NumberTransactionObjects))) begin
                    let readObject = newEntry.inputTr.readObjects[newEntry.readSetIndex];
                    newEntry.readSetIndex = newEntry.readSetIndex + 1;
                    let request = ShardRenameRequest{tid: newEntry.inputTr.tid, address: readObject, isWrittenObject: False};
                    shardArbiters[getShard(readObject)].users[i].request.put(request);
                    inputBuffer[i] <= tagged Valid newEntry;
                end else if (newEntry.writeSetIndex < fromInteger(valueOf(NumberTransactionObjects))) begin
                    let writtenObject = newEntry.inputTr.writeObjects[newEntry.writeSetIndex];
                    newEntry.writeSetIndex = newEntry.writeSetIndex + 1;
                    let request = ShardRenameRequest{tid: newEntry.inputTr.tid, address: writtenObject, isWrittenObject: True};
                    shardArbiters[getShard(writtenObject)].users[i].request.put(request);
                    inputBuffer[i] <= tagged Valid newEntry;
                end else begin
                    inputBuffer[i] <= tagged Invalid;
                end
            end
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////
    /// Methods
    ////////////////////////////////////////////////////////////////////////////
    method Action putInputTransaction(InputTransaction it) if (!isValid(inputBuffer[inputBufferEnd + 1]));
        let entry = InputBufferEntry{inputTr: it, readSetIndex: 0, writeSetIndex: 0};
        inputBuffer[inputBufferEnd + 1] <= tagged Valid entry;
        inputBufferEnd <= inputBufferEnd + 1;
    endmethod

    method ActionValue#(RenamedTransaction) getRenamedTransaction = outputArbiter.master.request.get;
endmodule

module mkRenamerTestbench();
    Renamer myRenamer <- mkRenamer();

    Vector#(1, InputTransaction) test_inputs;
    test_inputs[0].tid = 0;
    for (Integer i = 0; i < valueOf(NumberTransactionObjects); i = i + 1) begin
        test_inputs[0].readObjects[i] = fromInteger(i) * 8;
        test_inputs[0].writeObjects[i] = (fromInteger(i) + 1) * 4;
    end

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < 1);
        counter <= counter + 1;
        myRenamer.putInputTransaction(it);
    endrule

    rule stream;
        let result <- myRenamer.getRenamedTransaction();
        $display(fshow(result));
    endrule

endmodule

module mkShardRenameResponseAggregator(Client#(RenamedTransaction, ShardRenameResponse));
    let defaultValue = SetBufferEntry{tid: ?, readSet: 0, writeSet: 0, readObjectCount: 0, writtenObjectCount: 0};
    Reg#(SetBufferEntry) entry <- mkReg(defaultValue);
    Reg#(Bool) isDone <- mkReg(False);

    interface Get request;
        method ActionValue#(RenamedTransaction) get() if (isDone);
            entry <= defaultValue;
            isDone <= False;
            return RenamedTransaction{
                tid: entry.tid,
                readSet: entry.readSet,
                writeSet: entry.writeSet
            };
        endmethod
    endinterface

    interface Put response;
        method Action put(ShardRenameResponse response) if (!isDone);
            SetBufferEntry newEntry = entry;
            newEntry.tid = response.tid;
            if (response.isWrittenObject) begin
                newEntry.writeSet = entry.writeSet | (1 << response.name);
                newEntry.writtenObjectCount = entry.writtenObjectCount + 1;
            end else begin
                newEntry.readSet = entry.readSet | (1 << response.name);
                newEntry.readObjectCount = entry.readObjectCount + 1;
            end
            entry <= newEntry;
            if (newEntry.readObjectCount == fromInteger(valueOf(NumberTransactionObjects))
                    && newEntry.writtenObjectCount == fromInteger(valueOf(NumberTransactionObjects))) begin
                isDone <= True;
            end
        endmethod
    endinterface
endmodule