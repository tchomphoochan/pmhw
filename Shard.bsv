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
typedef 3 LogNumberShards;
typedef 3 LogNumberHashes;

typedef TSub#(LogNumberLiveObjects, LogNumberShards) LogSizeShard;
typedef TAdd#(1, LogNumberLiveObjects) ObjectCount;

typedef TExp#(LogNumberShards) NumberShards;
typedef TExp#(LogNumberHashes) NumberHashes;

typedef Bit#(LogNumberShards) ShardIndex;
typedef Bit#(LogNumberHashes) HashIndex;
typedef Bit#(LogSizeShard) ShardKey;
typedef Bit#(ObjectCount) ReferenceCounter;

typedef struct {
    ReferenceCounter counter;
    ObjectAddress objectId;
} RenameTableEntry deriving(Bits, Eq, FShow);

typedef struct {
    TransactionId tid;
    ObjectAddress address;
    ObjectType type_;
    TransactionObjectCounter readObjectCount;
    TransactionObjectCounter writtenObjectCount;
} ShardRenameRequest deriving(Bits, Eq, FShow);

typedef struct {
    ObjectName name;
} ShardDeleteRequest deriving(Bits, Eq, FShow);

typedef union tagged {
    ShardRenameRequest Rename;
    ShardDeleteRequest Delete;
} ShardRequest deriving(Bits, Eq, FShow);

typedef struct {
    ShardRenameRequest request;
    Maybe#(ObjectName) name;
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
Integer numShards = valueOf(NumberShards);
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
(* descending_urgency = "doRename, startRename, endDelete, startDelete", synthesize *)
module mkShard(Shard);
    ////////////////////////////////////////////////////////////////////////////////
    /// Design elements.
    ////////////////////////////////////////////////////////////////////////////////
    BRAM_Configure cfg = defaultValue();
    cfg.loadFormat = tagged Hex "mem.vmh";
    BRAM2Port#(ShardKey, RenameTableEntry) bram <- mkBRAM2Server(cfg);

    // Currently processed request.
    Reg#(Maybe#(ShardRequest)) maybeReq <- mkReg(tagged Invalid);
    // Last name tried.
    Reg#(ShardKey) lastKey <- mkReg(?);
    // Number of hash functions used.
    Reg#(HashIndex) tries <- mkReg(0);
    // True if response is ready.
    Reg#(Bool) isDone <- mkReg(False);
    // True if rename was successful.
    Reg#(Bool) isSuccess <- mkReg(?);
`ifdef DEBUG
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
`ifdef DEBUG
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
`ifdef DEBUG
        $display("[%6d] Shard: start rename of %4h: %8h", cycle, req.tid, req.address);
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
`ifdef DEBUG
            $display("[%6d] Shard: done rename of %4h: %8h to %3h", cycle, req.tid,
                     req.address, keyToName(req, lastKey));
`endif
        end else if (entry.counter == fromInteger(maxLiveObjects)
                     && entry.objectId == req.address
                     || tries == fromInteger(maxHashes - 1)) begin
            // Rename request failed: slot is full or hash functions exhausted.
            isDone <= True;
            isSuccess <= False;
`ifdef DEBUG
            $display("[%6d] Shard: failed rename of %4h: %8h", cycle, req.tid, req.address);
`endif
        end else begin
            // Try next hash function (next offset).
            lastKey <= getNextKey(req);
            tries <= tries + 1;
            bram.portA.request.put(makeReadRequest(getNextKey(req)));
`ifdef DEBUG
            $display("[%6d] Shard: try #%0d rename of %4h: %8h", cycle, tries, req.tid,
                     req.address);
`endif
        end
    endrule

    rule startDelete (maybeReq matches tagged Valid .sreq
                      &&& sreq matches tagged Delete .req &&& !isDone);
        bram.portA.request.put(makeReadRequest(getKey(req.name)));
`ifdef DEBUG
        $display("[%6d] Shard: start delete: %3h", cycle, req.name);
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
`ifdef DEBUG
        $display("[%6d] Shard: done delete: %3h", cycle, req.name);
`endif
    endrule

    ////////////////////////////////////////////////////////////////////////////////
    /// Interface connections and methods.
    ////////////////////////////////////////////////////////////////////////////////
    interface Put request;
        method Action put(ShardRequest request) if (!isValid(maybeReq));
            maybeReq <= tagged Valid request;
            tries <= 0;
`ifdef DEBUG
        $display("[%6d] Shard: received request", cycle);
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
`ifdef DEBUG
        $display("[%6d] Shard: finished %4h: %8h", cycle, req.tid, req.address);
`endif
            if (isSuccess) begin
                return ShardRenameResponse { request: req, name: tagged Valid name };
            end else begin
                return ShardRenameResponse { request: req, name: tagged Invalid };
            end
        endmethod
    endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
// Shard tests.
////////////////////////////////////////////////////////////////////////////////
typedef 17 NumberShardTests;

function ShardRequest makeRenameReq(TransactionId id, ObjectAddress addr, ObjectType t);
    return tagged Rename ShardRenameRequest {tid: id, address: addr, type_: t};
endfunction

function ShardRequest makeDeleteReq(ObjectName name);
    return tagged Delete ShardDeleteRequest {name: name};
endfunction

module mkShardTestbench();
    Shard myShard <- mkShard();

    Vector#(NumberShardTests, ShardRequest) testInputs;
    testInputs[0] = makeRenameReq(64'h1, 32'h00000000, ReadObject);
    testInputs[1] = makeRenameReq(64'h1, 32'h00000205, WrittenObject);
    testInputs[2] = makeRenameReq(64'h1, 32'hA0000406, ReadObject);
    testInputs[3] = makeRenameReq(64'h1, 32'h00000300, ReadObject);
    testInputs[4] = makeRenameReq(64'h2, 32'hA0000406, WrittenObject);
    testInputs[5] = makeRenameReq(64'h1, 32'hB0000406, ReadObject);
    testInputs[6] = makeRenameReq(64'h3, 32'hC0000406, ReadObject);
    testInputs[7] = makeRenameReq(64'h4, 32'hD0000406, ReadObject);
    testInputs[8] = makeRenameReq(64'h5, 32'hE0000406, ReadObject);
    testInputs[9] = makeRenameReq(64'h6, 32'hF0000406, WrittenObject);
    testInputs[10] = makeRenameReq(64'h7, 32'hF0000806, ReadObject);
    testInputs[11] = makeRenameReq(64'h8, 32'hF0000C06, ReadObject);
    testInputs[12] = makeDeleteReq(10'h00B);
    testInputs[13] = makeRenameReq(64'h8, 32'hF0000C06, ReadObject);
    testInputs[14] = makeDeleteReq(10'h006);
    testInputs[15] = makeDeleteReq(10'h006);
    testInputs[16] = makeRenameReq(64'h9, 32'hA0000006, ReadObject);

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
