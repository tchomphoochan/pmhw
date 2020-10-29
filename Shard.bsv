import BRAM::*;
import Vector::*;

import PmTypes::*;

interface Shard;
    method Action putRenameRequest(ShardRenameRequest req);
    method ActionValue#(ShardRenameResponse) getRenameResponse();
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

    Reg#(ShardRenameRequest) request <- mkReg(?);
    Reg#(ShardRenameResponse) response <- mkReg(?);
    Reg#(Bit#(LogNumberHashes)) tries <- mkReg(0);
    Reg#(Bool) isAddressValid <- mkReg(False);
    Reg#(Bool) isReadInProgress <- mkReg(False);
    Reg#(Bool) isRenameDone <- mkReg(False);

    function Bit#(LogSizeShard) makeNewName();
        // Computes hash function h_i(x) = (x + i) % b.
        // x: address, i: offset (tries), b: base (SizeShard)
        ObjectAddress key = request.address + {0,tries};
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
        let objectName = makeNewName();
        response <= ShardRenameResponse{index: request.index, name: objectName, isWrite: request.isWrite};
        tries <= tries + 1;
        isReadInProgress <= True;
        bram.portA.request.put(makeReadRequest(objectName));
    endrule

    rule doRename if (isAddressValid && !isRenameDone && isReadInProgress);
        RenameTableEntry entry <- bram.portA.response.get();
        if (entry.counter == 0 || entry.objectId == request.address && entry.counter < fromInteger(valueOf(NumberLiveObjects))) begin
            isAddressValid <= False;
            isReadInProgress <= False;
            isRenameDone <= True;
            bram.portA.request.put(BRAMRequest{
                write: True,
                responseOnWrite: False,
                address: response.name,
                datain: RenameTableEntry{
                    counter: entry.counter + 1,
                    objectId: request.address
                }
            });
        end else if (entry.objectId == request.address || tries == fromInteger(valueOf(NumberHashes) - 1)) begin
            // Fail.
            $display("fail");
        end else begin
            let objectName = makeNewName();
            response <= ShardRenameResponse{index: request.index, name: objectName, isWrite: request.isWrite};
            tries <= tries + 1;
            bram.portA.request.put(makeReadRequest(objectName));
        end
    endrule

    method Action putRenameRequest(ShardRenameRequest req) if (!isAddressValid);
        request <= req;
        tries <= 0;
        isAddressValid <= True;
    endmethod

    method ActionValue#(ShardRenameResponse) getRenameResponse() if (isRenameDone);
        isRenameDone <= False;
        return response;
    endmethod
endmodule

module mkShardTestbench();
    Shard myShard <- mkShard();

    Vector#(5, ShardRenameRequest) test_input;
    test_input[0] = ShardRenameRequest{index: 0, address: 32'h00000000, isWrite: False};
    test_input[1] = ShardRenameRequest{index: 1, address: 32'h10000005, isWrite: True};
    test_input[2] = ShardRenameRequest{index: 0, address: 32'h20000006, isWrite: False};
    test_input[3] = ShardRenameRequest{index: 1, address: 32'h70000100, isWrite: False};
    test_input[4] = ShardRenameRequest{index: 1, address: 32'h20000006, isWrite: True};

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < 5);
        counter <= counter + 1;
        myShard.putRenameRequest(test_input[counter]);
    endrule

    rule stream;
        let res <- myShard.getRenameResponse();
        $display(fshow(res));
    endrule
endmodule
