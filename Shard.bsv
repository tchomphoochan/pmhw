import BRAM::*;
import Vector::*;

import PmTypes::*;

interface Shard;
    method Action putRenameRequest(TaggedValue#(NumberParallelTransactions, ObjectAddress) addr);
    method ActionValue#(TaggedValue#(NumberParallelTransactions, ShardedObjectName)) getRenameResponse();
endinterface

typedef 3 LogNumberHashes;
typedef TExp#(LogNumberHashes) NumberHashes;

typedef struct {
    Bit#(TAdd#(LogNumberLiveObjects, 1)) counter;
    ObjectAddress objectId;
} RenameTableEntry deriving(Bits, Eq, FShow);

module mkShard(Shard);
    BRAM_Configure cfg = defaultValue();
    cfg.loadFormat = tagged Hex "mem.vmh";
    BRAM2Port#(ShardedObjectName, RenameTableEntry) bram <- mkBRAM2Server(cfg);

    Reg#(TaggedValue#(NumberParallelTransactions, ObjectAddress)) inputAddress <- mkReg(?);
    Reg#(TaggedValue#(NumberParallelTransactions, ShardedObjectName)) outputName <- mkReg(?);
    Reg#(Bit#(LogNumberHashes)) tries <- mkReg(0);
    Reg#(Bool) isAddressValid <- mkReg(False);
    Reg#(Bool) isReadInProgress <- mkReg(False);
    Reg#(Bool) isRenameDone <- mkReg(False);

    function BRAMRequest#(ShardedObjectName, RenameTableEntry) makeReadRequest(ShardedObjectName addr);
        return BRAMRequest{
            write: False,
            responseOnWrite: False,
            address: addr,
            datain: ?
        };
    endfunction

    rule startRename if (isAddressValid && !isRenameDone && !isReadInProgress);
        $display("startRename");
        let startBit = valueOf(LogSizeShard) - 1;
        Bit#(LogSizeShard) objectName = (inputAddress.value + {0,tries})[startBit:0];
        outputName <= TaggedValue{tag: inputAddress.tag, value: objectName};
        tries <= tries + 1;
        isReadInProgress <= True;
        bram.portA.request.put(makeReadRequest(objectName));
    endrule

    rule doRename if (isAddressValid && !isRenameDone && isReadInProgress);
        $display("doRename %d", tries);
        RenameTableEntry entry <- bram.portA.response.get();
        $display("entry: ", fshow(entry));
        if (entry.counter == 0 || entry.objectId == inputAddress.value && entry.counter < fromInteger(valueOf(NumberLiveObjects))) begin
            isAddressValid <= False;
            isReadInProgress <= False;
            isRenameDone <= True;
            bram.portA.request.put(BRAMRequest{
                write: True,
                responseOnWrite: False,
                address: outputName.value,
                datain: RenameTableEntry{
                    counter: entry.counter + 1,
                    objectId: inputAddress.value
                }
            });
        end else if (entry.objectId == inputAddress.value || tries == fromInteger(valueOf(NumberHashes) - 1)) begin
            // Fail.
            $display("fail");
        end else begin
            $display("next");
            let startBit = valueOf(LogSizeShard) - 1;
            Bit#(LogSizeShard) objectName = (inputAddress.value + {0,tries})[startBit:0];
            outputName <= TaggedValue{tag: inputAddress.tag, value: objectName};
            tries <= tries + 1;
            bram.portA.request.put(makeReadRequest(objectName));
        end
    endrule

    method Action putRenameRequest(TaggedValue#(NumberParallelTransactions, ObjectAddress) addr) if (!isAddressValid);
        $display("putRenameRequest");
        inputAddress <= addr;
        tries <= 0;
        isAddressValid <= True;
    endmethod

    method ActionValue#(TaggedValue#(NumberParallelTransactions, ShardedObjectName)) getRenameResponse() if (isRenameDone);
        $display("getRenameResponse");
        isRenameDone <= False;
        return outputName;
    endmethod
endmodule

module mkShardTestbench();
    Shard myShard <- mkShard();

    Vector#(5, TaggedValue#(NumberParallelTransactions, ObjectAddress)) test_input;
    test_input[0] = TaggedValue{tag: 0, value: 32'h00000000};
    test_input[1] = TaggedValue{tag: 1, value: 32'h10000005};
    test_input[2] = TaggedValue{tag: 0, value: 32'h20000006};
    test_input[3] = TaggedValue{tag: 1, value: 32'h70000100};
    test_input[4] = TaggedValue{tag: 1, value: 32'h20000006};

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < 5);
        $display("feed");
        counter <= counter + 1;
        myShard.putRenameRequest(test_input[counter]);
    endrule

    rule stream;
        let res <- myShard.getRenameResponse();
        $display(fshow(res));
    endrule
endmodule
