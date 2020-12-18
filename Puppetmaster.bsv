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

    function Maybe#(TransactionId) recoverTid(TransactionId tid, bit flag);
        return flag == 1'b1 ? tagged Valid tid : tagged Invalid;
    endfunction

    function SchedulerTransaction convertTransaction(RenamedTransaction tr);
        return SchedulerTransaction {
            readSet : tr.readSet,
            writeSet : tr.writeSet
        };
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
        scheduler.request.put(map(convertTransaction, transactions));
    endrule

    // Incoming transactions get forwarded to the renamer.
    interface Put request = renamer.request;

    // Recover scheduled transaction ids from scheduler response.
    interface Get response;
        method ActionValue#(PuppetmasterResponse) get();
            let indices <- scheduler.response.get();
            return zipWith(recoverTid, trIds, unpack(indices));
        endmethod
    endinterface

endmodule

typedef 4 NumberPuppetmasterTests;

Integer numTests = valueOf(NumberPuppetmasterTests);

module mkPuppetmasterTestbench();
    Puppetmaster myPuppetmaster <- mkPuppetmaster();

    Vector#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool), PuppetmasterRequest) testInputs = newVector;
    for (Integer i = 0; i < numTests * maxScheduledObjects; i = i + 1) begin
        testInputs[i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[i].readObjects[j] = fromInteger(i * objSetSize * 2 + j * 2);
            testInputs[i].writeObjects[j] = fromInteger(case (i % numTests) matches
                0 : (i * objSetSize * 2 + j * 2 + 1);
                1 : ((i - i % 2) * objSetSize * 2 + j * 2 + 1);
                2 : ((i * objSetSize * 2 + j * 2 + 1) % (maxScheduledObjects * objSetSize * 2 - 2));
                3 : ((i % 2) * objSetSize * 2 + j * 2 + 1);
            endcase);
        end
    end

    Reg#(UInt#(TAdd#(TLog#(TMul#(NumberPuppetmasterTests, SizeSchedulingPool)), 1))) counter <- mkReg(0);

    rule feed if (counter < fromInteger(numTests * maxScheduledObjects));
        counter <= counter + 1;
        myPuppetmaster.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result <- myPuppetmaster.response.get();
        $display(fshow(result));
    endrule
endmodule
