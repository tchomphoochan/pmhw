////////////////////////////////////////////////////////////////////////////////
//  Filename      : Shard.bsv
//  Description   : Maps an object address into a smaller namespace so that it
//                  can be stored in a bit vector.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import BRAM::*;
import ClientServer::*;
import Vector::*;

import Scheduler::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef 64 LogMaxNumberTransactions;
typedef 32 LogSizeMemory;
typedef 2 LogNumberShards;
typedef 3 LogNumberHashes;

typedef TSub#(LogNumberLiveObjects, LogNumberShards) LogSizeShard;

typedef TExp#(LogNumberShards) NumberShards;
typedef TExp#(LogNumberHashes) NumberHashes;

typedef Bit#(LogMaxNumberTransactions) TransactionId;
typedef Bit#(LogSizeMemory) ObjectAddress;
typedef Bit#(LogNumberShards) ShardIndex;
typedef Bit#(LogNumberHashes) HashIndex;
typedef Bit#(LogSizeShard) ShardKey;

typedef struct {
    ReferenceCounter counter;
    ObjectAddress objectId;
} RenameTableEntry deriving(Bits, Eq, FShow);

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

// Type class instances telling the arbiter in the renamer module which messages
// need responses routed back.
instance ArbRequestTC#(ShardRenameRequest);
   function Bool isReadRequest(a x) = True;
   function Bool isWriteRequest(a x) = False;
endinstance

instance ArbRequestTC#(ShardRenameResponse);
   function Bool isReadRequest(a x) = False;
   function Bool isWriteRequest(a x) = True;
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Numeric constants.
////////////////////////////////////////////////////////////////////////////////
Integer logMaxShardObjects = valueOf(LogSizeShard);
Integer maxHashes = valueOf(NumberHashes);

////////////////////////////////////////////////////////////////////////////////
/// Helper functions.
////////////////////////////////////////////////////////////////////////////////
// Return the shard index for a given address, which are the low order bits
// preceding the key used by the shards.
function ShardIndex getShard(ObjectAddress address);
    return address[logMaxLiveObjects - 1 : logMaxShardObjects];
endfunction

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Shard implementation.
///
/// The object address space is partitioned into shards. Each shard computes a
/// "name" (an address with less bits) using primitive hashing (modulus) with
/// linear probing. The probing step only checks at most NumberHashes slots.
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
module mkShard(Shard);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    BRAM_Configure cfg = defaultValue();
    cfg.loadFormat = tagged Hex "mem.vmh";
    BRAM2Port#(ShardKey, RenameTableEntry) bram <- mkBRAM2Server(cfg);

    Reg#(ShardRenameRequest) req <- mkReg(?);
    Reg#(ShardRenameResponse) resp <- mkReg(?);
    Reg#(HashIndex) tries <- mkReg(0);
    Reg#(Bool) isAddressValid <- mkReg(False);
    Reg#(Bool) isReadInProgress <- mkReg(False);
    Reg#(Bool) isRenameDone <- mkReg(False);

    ////////////////////////////////////////////////////////////////////////////////
    /// Helper functions.
    ////////////////////////////////////////////////////////////////////////////////
    // Computes hash function h_i(x) = (x + i) % b.
    // x: address, i: offset (tries), b: base (SizeShard)
    function ShardKey getNextName();
        ObjectAddress key = req.address + zeroExtend(tries);
        return key[logMaxShardObjects - 1 : 0];
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
        ShardKey currentName = resp.name[logMaxShardObjects - 1 : 0];
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

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    // This rule is needed because doRename is blocked before the first read
    // request gets sent to the BRAM.
    rule startRename if (isAddressValid && !isRenameDone && !isReadInProgress);
        resp <= makeShardResponse();
        tries <= tries + 1;
        isReadInProgress <= True;
        bram.portA.request.put(makeNextReadRequest());
    endrule

    rule doRename if (isAddressValid && !isRenameDone && isReadInProgress);
        RenameTableEntry entry <- bram.portA.response.get();
        if (entry.counter == 0 || entry.objectId == req.address && entry.counter < fromInteger(maxLiveObjects)) begin
            isAddressValid <= False;
            isReadInProgress <= False;
            isRenameDone <= True;
            bram.portA.request.put(makeWriteRequest(entry.counter));
        end else if (entry.objectId == req.address || tries == fromInteger(maxHashes - 1)) begin
            // TODO: actually fail and clean up.
            $display("fail");
        end else begin
            resp <= makeShardResponse();
            tries <= tries + 1;
            bram.portA.request.put(makeNextReadRequest());
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
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

////////////////////////////////////////////////////////////////////////////////
// Shard tests.
////////////////////////////////////////////////////////////////////////////////
typedef 5 NumberShardTests;

module mkShardTestbench();
    Shard myShard <- mkShard();

    Vector#(NumberShardTests, ShardRenameRequest) testInputs;
    testInputs[0] = ShardRenameRequest{tid: 64'h1, address: 32'h00000000, isWrittenObject: False};
    testInputs[1] = ShardRenameRequest{tid: 64'h1, address: 32'h00000205, isWrittenObject: True};
    testInputs[2] = ShardRenameRequest{tid: 64'h1, address: 32'hA0000406, isWrittenObject: False};
    testInputs[3] = ShardRenameRequest{tid: 64'h1, address: 32'h00000300, isWrittenObject: False};
    testInputs[4] = ShardRenameRequest{tid: 64'h2, address: 32'hA0000406, isWrittenObject: True};

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < fromInteger(valueOf(NumberShardTests)));
        counter <= counter + 1;
        myShard.request.put(testInputs[counter]);
    endrule

    rule stream;
        let res <- myShard.response.get();
        $display(fshow(res));
    endrule
endmodule
