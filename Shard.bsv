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

    function Bit#(LogSizeShard) makeNewName();
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
        let objectName = makeNewName();
        resp <= ShardRenameResponse{index: req.index, name: objectName, isWrite: req.isWrite};
        tries <= tries + 1;
        isReadInProgress <= True;
        bram.portA.request.put(makeReadRequest(objectName));
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
                address: resp.name,
                datain: RenameTableEntry{
                    counter: entry.counter + 1,
                    objectId: req.address
                }
            });
        end else if (entry.objectId == req.address || tries == fromInteger(valueOf(NumberHashes) - 1)) begin
            // Fail.
            $display("fail");
        end else begin
            let objectName = makeNewName();
            resp <= ShardRenameResponse{index: req.index, name: objectName, isWrite: req.isWrite};
            tries <= tries + 1;
            bram.portA.request.put(makeReadRequest(objectName));
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
    test_input[0] = ShardRenameRequest{index: 0, address: 32'h00000000, isWrite: False};
    test_input[1] = ShardRenameRequest{index: 1, address: 32'h10000005, isWrite: True};
    test_input[2] = ShardRenameRequest{index: 0, address: 32'h20000006, isWrite: False};
    test_input[3] = ShardRenameRequest{index: 1, address: 32'h70000100, isWrite: False};
    test_input[4] = ShardRenameRequest{index: 1, address: 32'h20000006, isWrite: True};

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
