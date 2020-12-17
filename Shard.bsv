////////////////////////////////////////////////////////////////////////////////
//  Filename      : Shard.bsv
//  Description   : Maps an object address into a smaller namespace so that it
//                  can be stored in a bit vector.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import BRAM::*;
import ClientServer::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef 2 LogNumberShards;
typedef 3 LogNumberHashes;

typedef TSub#(LogNumberLiveObjects, LogNumberShards) LogSizeShard;
typedef TAdd#(1, LogNumberLiveObjects) ObjectCount;

typedef TExp#(LogNumberShards) NumberShards;
typedef TExp#(LogNumberHashes) NumberHashes;

typedef Bit#(LogNumberShards) ShardIndex;
typedef Bit#(LogNumberHashes) HashIndex;
typedef Bit#(LogSizeShard) ShardKey;
typedef Bit#(LogNumberLiveObjects) ObjectName;
typedef Bit#(ObjectCount) ReferenceCounter;

typedef struct {
    ReferenceCounter counter;
    ObjectAddress objectId;
} RenameTableEntry deriving(Bits, Eq, FShow);

typedef union tagged {
    struct {
        TransactionId tid;
        ObjectAddress address;
        Bool isWrittenObject;
    } Rename;
    struct {
        ObjectName name;
    } Delete;
} ShardRequest deriving(Bits, Eq, FShow);

typedef struct {
    Bool success;
    TransactionId tid;
    ObjectName name;
    Bool isWrittenObject;
} ShardRenameResponse deriving(Bits, Eq, FShow);

typedef Server#(ShardRequest, ShardRenameResponse) Shard;

// Type class instances telling the arbiter in the renamer module which messages
// need responses routed back.
instance ArbRequestTC#(ShardRequest);
   function Bool isReadRequest(ShardRequest r) = r matches tagged Rename .* ? True : False;
   function Bool isWriteRequest(ShardRequest r) = r matches tagged Delete .* ? True : False;
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
function ShardIndex getShard(Bit#(n) address) provisos (Min#(LogNumberLiveObjects, n, LogNumberLiveObjects));
    return address[logMaxLiveObjects - 1 : logMaxShardObjects];
endfunction

// Return the key for a given address used by the shards.
function ShardKey getKey(Bit#(n) address) provisos (Min#(LogSizeShard, n, LogSizeShard));
    return address[logMaxShardObjects - 1 : 0];
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

    Reg#(ShardRequest) req <- mkReg(?);
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
        return getKey(req.Rename.address + zeroExtend(tries));
    endfunction

    function ShardRenameResponse makeShardResponse();
        return ShardRenameResponse{
            success: True,
            tid: req.Rename.tid,
            name: {getShard(req.Rename.address), getNextName()},
            isWrittenObject: req.Rename.isWrittenObject
        };
    endfunction

    function BRAMRequest#(ShardKey, RenameTableEntry) makeNextReadRequest();
        return BRAMRequest{
            write: False,
            responseOnWrite: False,
            address: req matches tagged Delete .r ? getKey(r.name) : getNextName(),
            datain: ?
        };
    endfunction

    function BRAMRequest#(ShardKey, RenameTableEntry) makeWriteRequest(RenameTableEntry entry);
        let newEntry = RenameTableEntry{
            counter: req matches tagged Rename .* ? entry.counter + 1 : entry.counter - 1,
            objectId: req matches tagged Rename .r ? r.address : entry.objectId
        };
        return BRAMRequest{
            write: True,
            responseOnWrite: False,
            address: getKey(req matches tagged Delete .r ? r.name : resp.name),
            datain: newEntry
        };
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
    // This rule is needed because doRename is blocked before the first read
    // request gets sent to the BRAM.
    rule startRename if (isAddressValid && !isRenameDone && !isReadInProgress &&& req matches tagged Rename .*);
        resp <= makeShardResponse();
        tries <= tries + 1;
        isReadInProgress <= True;
        bram.portA.request.put(makeNextReadRequest());
    endrule

    rule doRename if (isAddressValid && !isRenameDone && isReadInProgress &&& req matches tagged Rename .request);
        RenameTableEntry entry <- bram.portA.response.get();
        if (entry.counter == 0 || entry.counter < fromInteger(maxLiveObjects) && entry.objectId == request.address) begin
            // Sucessful rename request: found empty slot or non-full existing entry for the object.
            isAddressValid <= False;
            isReadInProgress <= False;
            isRenameDone <= True;
            bram.portA.request.put(makeWriteRequest(entry));
        end else if (entry.counter == fromInteger(maxLiveObjects) && entry.objectId == request.address || tries == fromInteger(maxHashes - 1)) begin
            // Rename request failed: slot is full or hash functions exhausted.
            let newResp = resp;
            newResp.success = False;
            resp <= newResp;
            isAddressValid <= False;
            isReadInProgress <= False;
            isRenameDone <= True;
        end else begin
            // Try next hash function (next offset).
            resp <= makeShardResponse();
            tries <= tries + 1;
            bram.portA.request.put(makeNextReadRequest());
        end
    endrule

    rule startDelete (isAddressValid && !isRenameDone && !isReadInProgress &&& req matches tagged Delete .*);
        isReadInProgress <= True;
        bram.portA.request.put(makeNextReadRequest());
    endrule

    rule endDelete (isAddressValid && !isRenameDone && isReadInProgress &&& req matches tagged Delete .request);
        RenameTableEntry entry <- bram.portA.response.get();
        isAddressValid <= False;
        isReadInProgress <= False;
        bram.portA.request.put(makeWriteRequest(entry));
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Put request;
        method Action put(ShardRequest request) if (!isAddressValid);
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
typedef 17 NumberShardTests;

module mkShardTestbench();
    Shard myShard <- mkShard();

    Vector#(NumberShardTests, ShardRequest) testInputs;
    testInputs[0] = tagged Rename {tid: 64'h1, address: 32'h00000000, isWrittenObject: False};
    testInputs[1] = tagged Rename {tid: 64'h1, address: 32'h00000205, isWrittenObject: True};
    testInputs[2] = tagged Rename {tid: 64'h1, address: 32'hA0000406, isWrittenObject: False};
    testInputs[3] = tagged Rename {tid: 64'h1, address: 32'h00000300, isWrittenObject: False};
    testInputs[4] = tagged Rename {tid: 64'h2, address: 32'hA0000406, isWrittenObject: True};
    testInputs[5] = tagged Rename {tid: 64'h1, address: 32'hB0000406, isWrittenObject: False};
    testInputs[6] = tagged Rename {tid: 64'h3, address: 32'hC0000406, isWrittenObject: False};
    testInputs[7] = tagged Rename {tid: 64'h4, address: 32'hD0000406, isWrittenObject: False};
    testInputs[8] = tagged Rename {tid: 64'h5, address: 32'hE0000406, isWrittenObject: False};
    testInputs[9] = tagged Rename {tid: 64'h6, address: 32'hF0000406, isWrittenObject: True};
    testInputs[10] = tagged Rename {tid: 64'h7, address: 32'hF0000806, isWrittenObject: False};
    testInputs[11] = tagged Rename {tid: 64'h8, address: 32'hF0000C06, isWrittenObject: False};
    testInputs[12] = tagged Delete {name: 10'h00B};
    testInputs[13] = tagged Rename {tid: 64'h8, address: 32'hF0000C06, isWrittenObject: False};
    testInputs[14] = tagged Delete {name: 10'h006};
    testInputs[15] = tagged Delete {name: 10'h006};
    testInputs[16] = tagged Rename {tid: 64'h9, address: 32'hA0000006, isWrittenObject: False};

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
