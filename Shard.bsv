import BRAM::*;
import ClientServer::*;
import Vector::*;

import PmTypes::*;

typedef Server#(ShardRenameRequest, ShardRenameResponse) Shard;

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

    Reg#(ShardRenameRequest) req <- mkReg(?);
    Reg#(ShardRenameResponse) resp <- mkReg(?);
    Reg#(Bit#(LogNumberHashes)) tries <- mkReg(0);
    Reg#(Bool) isAddressValid <- mkReg(False);
    Reg#(Bool) isReadInProgress <- mkReg(False);
    Reg#(Bool) isRenameDone <- mkReg(False);

    function ShardIndex getIndex();
        Integer startBit = valueOf(LogSizeMemory) - 1;
        Integer endBit = valueOf(LogSizeMemory) - valueOf(LogNumberShards);
        return req.address[startBit:endBit];
    endfunction

    function ShardedObjectName getName();
        Integer startBit = valueOf(LogSizeShard) - 1;
        return resp.name[startBit:0];
    endfunction

    function ShardedObjectName getNextName();
        // Computes hash function h_i(x) = (x + i) % b.
        // x: address, i: offset (tries), b: base (SizeShard)
        ObjectAddress key = req.address + {0,tries};
        Integer startBit = valueOf(LogSizeShard) - 1;
        return key[startBit:0];
    endfunction

    function BRAMRequest#(ShardedObjectName, RenameTableEntry) makeReadRequest(ShardedObjectName addr);
        return BRAMRequest{
            write: False,
            responseOnWrite: False,
            address: addr,
            datain: ?
        };
    endfunction

    rule startRename if (isAddressValid && !isRenameDone && !isReadInProgress);
        resp <= ShardRenameResponse{name: {getIndex(), getNextName()}, isWrittenObject: req.isWrittenObject};
        tries <= tries + 1;
        isReadInProgress <= True;
        bram.portA.request.put(makeReadRequest(getNextName()));
    endrule

    rule doRename if (isAddressValid && !isRenameDone && isReadInProgress);
        RenameTableEntry entry <- bram.portA.response.get();
        if (entry.counter == 0 || entry.objectId == req.address && entry.counter < fromInteger(valueOf(NumberLiveObjects))) begin
            isAddressValid <= False;
            isReadInProgress <= False;
            isRenameDone <= True;
            bram.portA.request.put(BRAMRequest{
                write: True,
                responseOnWrite: False,
                address: getName(),
                datain: RenameTableEntry{
                    counter: entry.counter + 1,
                    objectId: req.address
                }
            });
        end else if (entry.objectId == req.address || tries == fromInteger(valueOf(NumberHashes) - 1)) begin
            // Fail.
            $display("fail");
        end else begin
            resp <= ShardRenameResponse{name: {getIndex(), getNextName()}, isWrittenObject: req.isWrittenObject};
            tries <= tries + 1;
            bram.portA.request.put(makeReadRequest(getNextName()));
        end
    endrule

    interface Put request;
        method Action put(ShardRenameRequest request) if (!isAddressValid);
            req <= request;
            tries <= 0;
            isAddressValid <= True;
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(ShardRenameResponse) get() if (isRenameDone);
            isRenameDone <= False;
            return resp;
        endmethod
    endinterface
endmodule

module mkShardTestbench();
    Shard myShard <- mkShard();

    Vector#(5, ShardRenameRequest) test_input;
    test_input[0] = ShardRenameRequest{address: 32'h00000000, isWrittenObject: False};
    test_input[1] = ShardRenameRequest{address: 32'h80000005, isWrittenObject: True};
    test_input[2] = ShardRenameRequest{address: 32'h20000006, isWrittenObject: False};
    test_input[3] = ShardRenameRequest{address: 32'hC0000100, isWrittenObject: False};
    test_input[4] = ShardRenameRequest{address: 32'h20000006, isWrittenObject: True};

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < 5);
        counter <= counter + 1;
        myShard.request.put(test_input[counter]);
    endrule

    rule stream;
        let res <- myShard.response.get();
        $display(fshow(res));
    endrule
endmodule
