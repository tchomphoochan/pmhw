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
    ObjectSet readSet;
    ObjectSet writeSet;
} SetBufferEntry deriving(Bits, Eq, FShow);

instance ArbRequestTC#(ShardRenameRequest);
   function Bool isReadRequest(a x) = True;
   function Bool isWriteRequest(a x) = False;
endinstance

instance ArbRequestTC#(ShardRenameResponse);
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
    // Output buffer.
    Vector#(SizeRenamerBuffer, Reg#(Maybe#(RenamedTransaction))) outputBuffer <- replicateM(mkReg(tagged Invalid));
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
    Vector#(SizeRenamerBuffer, Client#(SetBufferEntry, ShardRenameResponse)) aggregators;
    for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
        aggregators[i] <- mkShardRenameResponseAggregator;
        mkConnection(transactionArbiters[i].master.request, aggregators[i].response);
    end

    ////////////////////////////////////////////////////////////////////////////
    /// Functions
    ////////////////////////////////////////////////////////////////////////////
    function ObjectAddress getShard(ObjectAddress objectId);
        return objectId[valueOf(LogNumberShards) - 1:0];
    endfunction

    ////////////////////////////////////////////////////////////////////////////
    /// Rules
    ////////////////////////////////////////////////////////////////////////////
    rule scatter;
        for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
            let entry = inputBuffer[i];
            if (isValid(entry)) begin
                let newEntry = fromMaybe(?, entry);
                ObjectAddress currentObject = 0;
                Bool found = False;
                let maybeReadObject = newEntry.inputTr.readObjects[newEntry.readSetIndex];
                let maybeWriteObject = newEntry.inputTr.writeObjects[newEntry.writeSetIndex];
                if (newEntry.readSetIndex <= fromInteger(valueOf(NumberTransactionObjects)) && isValid(maybeReadObject)) begin
                    currentObject = fromMaybe(?, maybeReadObject);
                    newEntry.readSetIndex = newEntry.readSetIndex + 1;
                    let request = ShardRenameRequest{address: currentObject, isWrittenObject: False};
                    shardArbiters[getShard(currentObject)].users[i].request.put(request);
                    inputBuffer[i] <= tagged Valid newEntry;
                end else if (newEntry.writeSetIndex <= fromInteger(valueOf(NumberTransactionObjects)) && isValid(maybeWriteObject)) begin
                    currentObject = fromMaybe(?, maybeWriteObject);
                    newEntry.writeSetIndex = newEntry.writeSetIndex + 1;
                    let request = ShardRenameRequest{address: currentObject, isWrittenObject: True};
                    shardArbiters[getShard(currentObject)].users[i].request.put(request);
                    inputBuffer[i] <= tagged Valid newEntry;
                end else begin
                    // ???
                end
            end
        end
    endrule

    rule push;
        for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
            let inputEntry = inputBuffer[i];
            let outputEntry = outputBuffer[i];
            if (isValid(inputEntry) && !isValid(outputEntry)) begin
                let renamedSet <- aggregators[i].request.get();
                let newEntry = fromMaybe(?, inputEntry);
                ObjectAddress currentObject = 0;
                let maybeReadObject = newEntry.inputTr.readObjects[newEntry.readSetIndex];
                let maybeWriteObject = newEntry.inputTr.writeObjects[newEntry.writeSetIndex];
                if ((newEntry.readSetIndex > fromInteger(valueOf(NumberTransactionObjects)) || !isValid(maybeReadObject))
                        && (newEntry.writeSetIndex > fromInteger(valueOf(NumberTransactionObjects)) || !isValid(maybeWriteObject))) begin
                    let result = RenamedTransaction{
                        uniqueIds: newEntry.inputTr.uniqueIds,
                        readSet: renamedSet.readSet,
                        writeSet: renamedSet.writeSet
                    };
                    outputBuffer[i] <= tagged Valid result;
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

    method ActionValue#(RenamedTransaction) getRenamedTransaction() if (any(isValid, readVReg(outputBuffer)));
        Bool found = False;
        RenamedTransaction result = ?;
        for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
            let entry = outputBuffer[i];
            if (!found && isValid(entry)) begin
                result = fromMaybe(?, entry);
            end
        end
        return result;
    endmethod
endmodule

module mkRenamerTestbench();
    Renamer myRenamer <- mkRenamer();

    rule feed;
        InputTransaction it;
        it.uniqueIds = 0;
        for (Integer i = 0; i < valueOf(NumberTransactionObjects); i = i + 1) begin
            it.readObjects[i] = tagged Valid (fromInteger(i) * 8);
            it.writeObjects[i] = tagged Valid ((fromInteger(i) + 1) * 4);
        end
        myRenamer.putInputTransaction(it);
    endrule

    rule stream;
        let result <- myRenamer.getRenamedTransaction();
        $display("result:", fshow(result));
    endrule

endmodule

module mkShardRenameResponseAggregator(Client#(SetBufferEntry, ShardRenameResponse));
    let defaultValue = SetBufferEntry{readSet: 0, writeSet: 0};
    Reg#(SetBufferEntry) entry <- mkReg(defaultValue);
    Reg#(Bool) isDone <- mkReg(False);

    interface Get request;
        method ActionValue#(SetBufferEntry) get() if (isDone);
            entry <= defaultValue;
            isDone <= False;
            return entry;
        endmethod
    endinterface

    interface Put response;
        method Action put(ShardRenameResponse response) if (!isDone);
            SetBufferEntry newEntry = entry;
            if (response.isWrittenObject) begin
                newEntry.writeSet = entry.writeSet | (1 << response.name);
            end else begin
                newEntry.readSet = entry.readSet | (1 << response.name);
            end
            entry <= newEntry;
        endmethod
    endinterface
endmodule