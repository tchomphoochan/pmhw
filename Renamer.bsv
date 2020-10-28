import Arbiter::*;
import FIFO::*;
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
    Vector#(SizeRenamerBuffer, Reg#(SetBufferEntry)) renamedSetBuffer <- replicateM(mkReg(?));
    Reg#(Bit#(LogSizeRenamerBuffer)) inputBufferStart <- mkReg(0);
    Reg#(Bit#(LogSizeRenamerBuffer)) inputBufferEnd <- mkReg(0);
    // Output buffer.
    Vector#(SizeRenamerBuffer, Reg#(Maybe#(RenamedTransaction))) outputBuffer <- replicateM(mkReg(tagged Invalid));
    // Arbiter.
    Vector#(NumberShards, Arbiter_IFC#(SizeRenamerBuffer)) forwardArbiters <- replicateM(mkArbiter(False));
    Vector#(NumberShards, Vector#(SizeRenamerBuffer, FIFO#(ObjectAddress))) forwardQueues <- replicateM(replicateM(mkFIFO1));
    Vector#(SizeRenamerBuffer, Arbiter_IFC#(NumberShards)) backwardArbiters <- replicateM(mkArbiter(False));
    Vector#(SizeRenamerBuffer, Vector#(NumberShards, FIFO#(ShardedObjectName))) backwardQueues <- replicateM(replicateM(mkFIFO1));

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
                    forwardArbiters[getShard(currentObject)].clients[i].request();
                    forwardQueues[getShard(currentObject)][i].enq(currentObject);
                    inputBuffer[i] <= tagged Valid newEntry;
                end else if (newEntry.writeSetIndex <= fromInteger(valueOf(NumberTransactionObjects)) && isValid(maybeWriteObject)) begin
                    currentObject = fromMaybe(?, maybeWriteObject);
                    newEntry.writeSetIndex = newEntry.writeSetIndex + 1;
                    forwardArbiters[getShard(currentObject)].clients[i].request();
                    forwardQueues[getShard(currentObject)][i].enq(currentObject);
                    inputBuffer[i] <= tagged Valid newEntry;
                end else begin
                    // ???
                end
            end
        end
    endrule

    rule forwardArbitrate;
        for (Integer i = 0; i < valueOf(NumberShards); i = i + 1) begin
            let queueIndex = forwardArbiters[i].grant_id;
            let address = forwardQueues[i][queueIndex].first();
            forwardQueues[i][queueIndex].deq();
            shards[i].putRenameRequest(TaggedValue{tag: fromInteger(i), value: address});
        end
    endrule

    rule backwardArbitrate;
        for (Integer i = 0; i < valueOf(NumberShards); i = i + 1) begin
            let result <- shards[i].getRenameResponse();
            backwardArbiters[i].clients[result.tag].request();
            backwardQueues[i][result.tag].enq(result.value);
        end
    endrule

    rule gather;
        for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
            let entry = renamedSetBuffer[i];
            let queueIndex = backwardArbiters[i].grant_id;
            ShardedObjectName shardObjName = backwardQueues[i][queueIndex].first();
            backwardQueues[i][queueIndex].deq();
            ObjectName objName = {fromInteger(i), shardObjName};
            entry.readSet = entry.readSet | (1 << objName);  // FIXME: differentiate between read and write sets.
            renamedSetBuffer[i] <= entry;
        end
    endrule

    rule push;
        for (Integer i = 0; i < valueOf(SizeRenamerBuffer); i = i + 1) begin
            let inputEntry = inputBuffer[i];
            let outputEntry = outputBuffer[i];
            if (isValid(inputEntry) && !isValid(outputEntry)) begin
                let newEntry = fromMaybe(?, inputEntry);
                ObjectAddress currentObject = 0;
                let maybeReadObject = newEntry.inputTr.readObjects[newEntry.readSetIndex];
                let maybeWriteObject = newEntry.inputTr.writeObjects[newEntry.writeSetIndex];
                if ((newEntry.readSetIndex > fromInteger(valueOf(NumberTransactionObjects)) || !isValid(maybeReadObject))
                        && (newEntry.writeSetIndex > fromInteger(valueOf(NumberTransactionObjects)) || !isValid(maybeWriteObject))) begin
                    let result = RenamedTransaction{
                        uniqueIds: newEntry.inputTr.uniqueIds,
                        readSet: renamedSetBuffer[i].readSet,
                        writeSet: renamedSetBuffer[i].writeSet
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
        let sets = SetBufferEntry{readSet: 0, writeSet: 0};
        renamedSetBuffer[inputBufferEnd + 1] <= sets;
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
