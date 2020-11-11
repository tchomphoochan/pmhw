import Arbitrate::*;
import BRAM::*;
import ClientServer::*;
import Vector::*;

import PmTypes::*;

// Interface.
typedef struct {
    TransactionId tid;
    ObjectAddress address;
    Bool isWrittenObject;
 } ShardRenameRequest deriving(Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    ObjectName name;
    Bool isWrittenObject;
} ShardRenameResponse deriving(Bits, Eq, FShow);

typedef Server#(ShardRenameRequest, ShardRenameResponse) Shard;

// Type class instances.
instance ArbRequestTC#(ShardRenameRequest);
   function Bool isReadRequest(a x) = True;
   function Bool isWriteRequest(a x) = False;
endinstance

instance ArbRequestTC#(ShardRenameResponse);
   function Bool isReadRequest(a x) = False;
   function Bool isWriteRequest(a x) = True;
endinstance

// Internal structures.
typedef Bit#(TAdd#(LogNumberLiveObjects, 1)) ReferenceCounter;
typedef struct {
    ReferenceCounter counter;
    ObjectAddress objectId;
} RenameTableEntry deriving(Bits, Eq, FShow);

// Helper functions.
function ShardIndex getShard(ObjectAddress address);
    Integer startBit = valueOf(LogNumberLiveObjects)- 1;
    Integer endBit = valueOf(LogSizeShard);
    return address[startBit:endBit];
endfunction

// Main module.
module mkShard(Shard);
    BRAM_Configure cfg = defaultValue();
    cfg.loadFormat = tagged Hex "mem.vmh";
    BRAM2Port#(ShardKey, RenameTableEntry) bram <- mkBRAM2Server(cfg);

    Reg#(ShardRenameRequest) req <- mkReg(?);
    Reg#(ShardRenameResponse) resp <- mkReg(?);
    Reg#(Bit#(LogNumberHashes)) tries <- mkReg(0);
    Reg#(Bool) isAddressValid <- mkReg(False);
    Reg#(Bool) isReadInProgress <- mkReg(False);
    Reg#(Bool) isRenameDone <- mkReg(False);

    // Computes hash function h_i(x) = (x + i) % b.
    // x: address, i: offset (tries), b: base (SizeShard)
    function ShardKey getNextName();
        ObjectAddress key = req.address + {0,tries};
        Integer startBit = valueOf(LogSizeShard) - 1;
        return key[startBit:0];
    endfunction

    function makeShardResponse();
        return ShardRenameResponse{
            tid: req.tid,
            name: {getShard(req.address), getNextName()},
            isWrittenObject: req.isWrittenObject
        };
    endfunction

    function makeNextReadRequest();
        return BRAMRequest{
            write: False,
            responseOnWrite: False,
            address: getNextName(),
            datain: ?
        };
    endfunction

    function BRAMRequest#(ShardKey, RenameTableEntry) makeWriteRequest(ReferenceCounter counter);
        Integer startBit = valueOf(LogSizeShard) - 1;
        ShardKey currentName = resp.name[startBit:0];
        let entry = RenameTableEntry{
            counter: counter + 1,
            objectId: req.address
        };
        return BRAMRequest{
            write: True,
            responseOnWrite: False,
            address: currentName,
            datain: entry
        };
    endfunction

    rule startRename if (isAddressValid && !isRenameDone && !isReadInProgress);
        resp <= makeShardResponse();
        tries <= tries + 1;
        isReadInProgress <= True;
        bram.portA.request.put(makeNextReadRequest());
    endrule

    rule doRename if (isAddressValid && !isRenameDone && isReadInProgress);
        RenameTableEntry entry <- bram.portA.response.get();
        if (entry.counter == 0 || entry.objectId == req.address && entry.counter < fromInteger(valueOf(NumberLiveObjects))) begin
            isAddressValid <= False;
            isReadInProgress <= False;
            isRenameDone <= True;
            bram.portA.request.put(makeWriteRequest(entry.counter));
        end else if (entry.objectId == req.address || tries == fromInteger(valueOf(NumberHashes) - 1)) begin
            // TODO: actually fail and clean up.
            $display("fail");
        end else begin
            resp <= makeShardResponse();
            tries <= tries + 1;
            bram.portA.request.put(makeNextReadRequest());
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

    Vector#(5, ShardRenameRequest) testInputs;
    testInputs[0] = ShardRenameRequest{tid: 64'h1, address: 32'h00000000, isWrittenObject: False};
    testInputs[1] = ShardRenameRequest{tid: 64'h1, address: 32'h00000205, isWrittenObject: True};
    testInputs[2] = ShardRenameRequest{tid: 64'h1, address: 32'hA0000406, isWrittenObject: False};
    testInputs[3] = ShardRenameRequest{tid: 64'h1, address: 32'h00000300, isWrittenObject: False};
    testInputs[4] = ShardRenameRequest{tid: 64'h2, address: 32'hA0000406, isWrittenObject: True};

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < 5);
        counter <= counter + 1;
        myShard.request.put(testInputs[counter]);
    endrule

    rule stream;
        let res <- myShard.response.get();
        $display(fshow(res));
    endrule
endmodule
