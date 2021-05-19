////////////////////////////////////////////////////////////////////////////////
//  Filename      : Shard.bsv
//  Description   : Maps an object address into a smaller namespace so that it
//                  can be stored in a bit vector.
////////////////////////////////////////////////////////////////////////////////
import Arbitrate::*;
import BRAM::*;
import Vector::*;

import PmConfig::*;
import PmCore::*;
import PmIfc::*;

////////////////////////////////////////////////////////////////////////////////
/// Module interface.
////////////////////////////////////////////////////////////////////////////////
typedef TAdd#(1, LogNumberLiveObjects) ObjectCount;

typedef TExp#(LogNumberShards) NumberShards;
typedef TExp#(LogNumberHashes) NumberHashes;
typedef TExp#(LogSizeShard) SizeShard;

typedef Bit#(LogNumberShards) ShardIndex;
typedef Bit#(LogNumberHashes) HashIndex;
typedef Bit#(LogSizeShard) ShardKey;
typedef Bit#(ObjectCount) ReferenceCounter;

typedef struct {
    ReferenceCounter counter;
    ObjectAddress objectId;
} RenameTableEntry deriving(Bits, Eq, FShow);

typedef struct {
    ObjectAddress address;
    ObjectType objType;
} ShardRenameRequest deriving(Bits, Eq, FShow);

typedef struct {
    ObjectName name;
} ShardDeleteRequest deriving(Bits, Eq, FShow);

typedef union tagged {
    ShardRenameRequest Rename;
    ShardDeleteRequest Delete;
    void Reset_;
} ShardRequest deriving(Bits, Eq, FShow);

typedef struct {
    ShardRenameRequest request;
    Maybe#(ObjectName) name;
} ShardRenameResponse deriving(Bits, Eq, FShow);

interface Shard;
    interface Put#(ShardRequest) request;
    interface Get#(ShardRenameResponse) response;
    method Bool isReady();
endinterface

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
Integer numShards = valueOf(NumberShards);
Integer logMaxShardObjects = valueOf(LogSizeShard);
Integer maxHashes = valueOf(NumberHashes);
Integer numKeys = valueOf(SizeShard);

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
(* descending_urgency = "doRename, startRename, endDelete, startDelete", synthesize *)
module mkShard(Shard);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    BRAM_Configure cfg = defaultValue();
    BRAM2Port#(ShardKey, RenameTableEntry) bram <- mkBRAM2Server(cfg);

    // Currently processed request.
    Reg#(Maybe#(ShardRequest)) maybeReq <- mkReg(tagged Valid tagged Reset_);
    // Last name tried or last key reset.
    Reg#(ShardKey) lastKey <- mkReg(0);
    // Number of hash functions used.
    Reg#(HashIndex) tries <- mkReg(0);
    // True if response is ready.
    Reg#(Bool) isDone <- mkReg(False);
    // True if rename was successful.
    Reg#(Bool) isSuccess <- mkReg(?);
`ifdef DEBUG_SH
    Reg#(Timestamp) cycle <- mkReg(0);
`endif

    ////////////////////////////////////////////////////////////////////////////////
    /// Helper functions.
    ////////////////////////////////////////////////////////////////////////////////
    // Computes hash function h_i(x) = (x + i) % b.
    // x: address, i: offset (tries), b: base (SizeShard)
    function ShardKey getNextKey(ShardRenameRequest req);
        return getKey(req.address + zeroExtend(tries));
    endfunction

    function ObjectName keyToName(ShardRenameRequest req, ShardKey key);
        return {getShard(req.address), key};
    endfunction

    function BRAMRequest#(ShardKey, RenameTableEntry) makeReadRequest(ShardKey bramAddr);
        return BRAMRequest{
            write: False,
            responseOnWrite: False,
            address: bramAddr,
            datain: ?
        };
    endfunction

    function BRAMRequest#(ShardKey, RenameTableEntry) makeWriteRequest(
            ShardKey bramAddr, RenameTableEntry entry);
        return BRAMRequest{
            write: True,
            responseOnWrite: False,
            address: bramAddr,
            datain: entry
        };
    endfunction

    ////////////////////////////////////////////////////////////////////////////////
    /// Rules.
    ////////////////////////////////////////////////////////////////////////////////
`ifdef DEBUG_SH
    (* no_implicit_conditions, fire_when_enabled *)
    rule tick;
        cycle <= cycle + 1;
    endrule
`endif

    // This rule is needed because doRename is blocked before the first read
    // request gets sent to the BRAM.
    rule startRename if (maybeReq matches tagged Valid .sreq
                         &&& sreq matches tagged Rename .req &&& !isDone);
        lastKey <= getNextKey(req);
        tries <= tries + 1;
        bram.portA.request.put(makeReadRequest(getNextKey(req)));
`ifdef DEBUG_SH
        $display("[%8d] Shard: start renaming O#%h", cycle, req.address);
`endif
    endrule

    rule doRename if (maybeReq matches tagged Valid .sreq
                      &&& sreq matches tagged Rename .req &&& !isDone);
        RenameTableEntry entry <- bram.portA.response.get();
        if (entry.counter == 0 || entry.counter < fromInteger(maxLiveObjects)
                && entry.objectId == req.address) begin
            // Sucessful rename request: found empty slot or non-full existing entry.
            isDone <= True;
            isSuccess <= True;
            let newEntry = RenameTableEntry {
                counter: entry.counter + 1,
                objectId: req.address
            };
            bram.portA.request.put(makeWriteRequest(lastKey, newEntry));
`ifdef DEBUG_SH
            $display("[%8d] Shard: done renaming O#%h to O'#%h", cycle, req.address,
                     keyToName(req, lastKey));
`endif
        end else if (entry.counter == fromInteger(maxLiveObjects)
                     && entry.objectId == req.address
                     || tries == fromInteger(maxHashes - 1)) begin
            // Rename request failed: slot is full or hash functions exhausted.
            isDone <= True;
            isSuccess <= False;
`ifdef DEBUG_SH
            $display("[%8d] Shard: failed to rename O#%h", cycle, req.address);
`endif
        end else begin
            // Try next hash function (next offset).
            lastKey <= getNextKey(req);
            tries <= tries + 1;
            bram.portA.request.put(makeReadRequest(getNextKey(req)));
`ifdef DEBUG_SH
            $display("[%8d] Shard: try %0d of renaming O#%h", cycle, tries, req.address);
`endif
        end
    endrule

    rule startDelete (maybeReq matches tagged Valid .sreq
                      &&& sreq matches tagged Delete .req &&& !isDone);
        bram.portA.request.put(makeReadRequest(getKey(req.name)));
`ifdef DEBUG_SH
        $display("[%8d] Shard: start deleting O'#%h", cycle, req.name);
`endif
    endrule

    rule endDelete (maybeReq matches tagged Valid .sreq
                    &&& sreq matches tagged Delete .req &&& !isDone);
        RenameTableEntry entry <- bram.portA.response.get();
        maybeReq <= tagged Invalid;
        let newEntry = RenameTableEntry {
            counter: entry.counter - 1,
            objectId: entry.objectId
        };
        bram.portA.request.put(makeWriteRequest(getKey(req.name), newEntry));
`ifdef DEBUG_SH
        $display("[%8d] Shard: done deleting O'#%h", cycle, req.name);
`endif
    endrule

    // Write zeros into every line of the renaming table (BRAM).
    rule doReset if (maybeReq matches tagged Valid .sreq
                    &&& sreq matches tagged Reset_);
        let zeroEntry = RenameTableEntry { counter: 0, objectId: ? };
        bram.portA.request.put(makeWriteRequest(lastKey, zeroEntry));
        if (lastKey == fromInteger(numKeys - 1)) begin
            maybeReq <= tagged Invalid;
            lastKey <= 0;
        end else begin
            lastKey <= lastKey + 1;
        end
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Put request;
        method Action put(ShardRequest request) if (!isValid(maybeReq));
            maybeReq <= tagged Valid request;
            lastKey <= 0;
            tries <= 0;
`ifdef DEBUG_SH
        $display("[%8d] Shard: received request", cycle);
`endif
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(ShardRenameResponse) get() if (
                maybeReq matches tagged Valid .sreq
                &&& sreq matches tagged Rename .req &&& isDone);
            maybeReq <= tagged Invalid;
            isDone <= False;
            let name = keyToName(req, lastKey);
`ifdef DEBUG_SH
        $display("[%8d] Shard: finished renaming O#%h", cycle, req.address);
`endif
            if (isSuccess) begin
                return ShardRenameResponse { request: req, name: tagged Valid name };
            end else begin
                return ShardRenameResponse { request: req, name: tagged Invalid };
            end
        endmethod
    endinterface

    method Bool isReady = maybeReq != tagged Valid tagged Reset_;
endmodule

////////////////////////////////////////////////////////////////////////////////
// Shard tests.
////////////////////////////////////////////////////////////////////////////////
typedef 17 NumberShardTests;

function ShardRequest makeRenameReq(ObjectAddress addr, ObjectType t);
    return tagged Rename ShardRenameRequest { address: addr, objType: t };
endfunction

function ShardRequest makeDeleteReq(ObjectName name);
    return tagged Delete ShardDeleteRequest {name: name};
endfunction

module mkShardTestbench();
    Shard myShard <- mkShard();

    Vector#(NumberShardTests, ShardRequest) testInputs;
    testInputs[0] = makeRenameReq(64'h00000000, ReadObject);
    testInputs[1] = makeRenameReq(64'h00000205, WrittenObject);
    testInputs[2] = makeRenameReq(64'hA0000406, ReadObject);
    testInputs[3] = makeRenameReq(64'h00000300, ReadObject);
    testInputs[4] = makeRenameReq(64'hA0000406, WrittenObject);
    testInputs[5] = makeRenameReq(64'hB0000406, ReadObject);
    testInputs[6] = makeRenameReq(64'hC0000406, ReadObject);
    testInputs[7] = makeRenameReq(64'hD0000406, ReadObject);
    testInputs[8] = makeRenameReq(64'hE0000406, ReadObject);
    testInputs[9] = makeRenameReq(64'hF0000406, WrittenObject);
    testInputs[10] = makeRenameReq(64'hF0000806, ReadObject);
    testInputs[11] = makeRenameReq(64'hF0000C06, ReadObject);
    testInputs[12] = makeDeleteReq(10'h00B);
    testInputs[13] = makeRenameReq(64'hF0000C06, ReadObject);
    testInputs[14] = makeDeleteReq(10'h006);
    testInputs[15] = makeDeleteReq(10'h006);
    testInputs[16] = makeRenameReq(64'hA0000006, ReadObject);

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
