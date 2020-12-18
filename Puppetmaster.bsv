// Top-level Puppetmaster module.
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import PmIfc::*;
import Renamer::*;
import Scheduler::*;
import Shard::*;

typedef InputTransaction PuppetmasterRequest;
typedef Vector#(SizeSchedulingPool, Maybe#(TransactionId)) PuppetmasterResponse;
typedef Server#(PuppetmasterRequest, PuppetmasterResponse) Puppetmaster;

module mkPuppetmaster(Puppetmaster);
    Vector#(SizeSchedulingPool, Reg#(RenamedTransaction)) buffer <- replicateM(mkReg(?));
    Reg#(Bit#(TAdd#(LogSizeSchedulingPool, 1))) bufferIndex <- mkReg(0);
    // Transaction ids for converting the indices returned the scheduling step.
    Reg#(Vector#(SizeSchedulingPool, TransactionId)) trIds <- mkReg(?);

    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();

    function TransactionId getTid(RenamedTransaction tr);
        return tr.tid;
    endfunction

    function TransactionSet transactionToSet(RenamedTransaction tr, Integer i);
        return TransactionSet {
            readSet: tr.readSet,
            writeSet: tr.writeSet,
            indices: 1 << i
        };
    endfunction

    function Maybe#(TransactionId) recoverTid(TransactionId tid, bit flag);
        return flag == 1'b1 ? tagged Valid tid : tagged Invalid;
    endfunction

    // Put renamed transactions into a buffer.
    rule receive if (bufferIndex < fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= bufferIndex + 1;
        let result <- renamer.response.get();
        buffer[bufferIndex] <= result;
    endrule

    // When buffer is full, send scheduling request.
    rule process if (bufferIndex == fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= 0;
        let transactions = readVReg(buffer);
        trIds <= map(getTid, transactions);
        scheduler.request.put(zipWith(transactionToSet, transactions, genVector));
    endrule

    // Incoming transactions get forwarded to the renamer.
    interface Put request = renamer.request;

    // Recover scheduled transaction ids from scheduler response.
    interface Get response;
        method ActionValue#(PuppetmasterResponse) get();
            let result <- scheduler.response.get();
            return zipWith(recoverTid, trIds, unpack(result.indices));
        endmethod
    endinterface

endmodule

typedef 4 NumberPuppetmasterTests;

module mkPuppetmasterTestbench();
    Puppetmaster myPuppetmaster <- mkPuppetmaster();

    Vector#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool), PuppetmasterRequest) testInputs = newVector;
    for (Integer i = 0; i < valueOf(NumberPuppetmasterTests) * maxScheduledObjects; i = i + 1) begin
        testInputs[i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i].readObjects[j] = case (i % valueOf(NumberPuppetmasterTests)) matches
                0 : (fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2);
                1 : (fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2);
                2 : (fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2);
                3 : (fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2);
            endcase;
            testInputs[i].writeObjects[j] = case (i % valueOf(NumberPuppetmasterTests)) matches
                0 : (fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1);
                1 : (fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1);
                2 : ((fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1) % (fromInteger(maxScheduledObjects) * fromInteger(objSetSize) * 2 - 2));
                3 : (fromInteger(i % 2) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1);
            endcase;
        end
    end

    Reg#(UInt#(TAdd#(TLog#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool)), 1))) counter <- mkReg(0);

    rule feed if (counter < fromInteger(valueOf(NumberPuppetmasterTests) * maxScheduledObjects));
        counter <= counter + 1;
        myPuppetmaster.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result <- myPuppetmaster.response.get();
        $display(fshow(result));
    endrule
endmodule
