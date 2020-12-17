// Top-level Puppetmaster module.
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import Renamer::*;
import Scheduler::*;
import Shard::*;

typedef InputTransaction PuppetmasterRequest;
typedef Vector#(SizeSchedulingPool, Maybe#(TransactionId)) PuppetmasterResponse;
typedef Server#(PuppetmasterRequest, PuppetmasterResponse) Puppetmaster;

module mkPuppetmaster(Puppetmaster);
    Vector#(SizeSchedulingPool, Reg#(RenamedTransaction)) buffer <- replicateM(mkReg(?));
    Reg#(Bit#(TAdd#(LogSizeSchedulingPool, 1))) bufferIndex <- mkReg(0);

    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();

    // Put renamed transactions into a buffer.
    rule receive if (bufferIndex < fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= bufferIndex + 1;
        let result <- renamer.response.get();
        buffer[bufferIndex] <= result;
    endrule

    // When buffer is full, send scheduling request.
    rule process if (bufferIndex == fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= 0;
        scheduler.request.put(readVReg(buffer));
    endrule

    // Incoming transactions get forwarded to the renamer.
    interface Put request = renamer.request;

    interface Get response = scheduler.response;

endmodule

typedef 4 NumberPuppetmasterTests;

module mkPuppetmasterTestbench();
    Puppetmaster myPuppetmaster <- mkPuppetmaster();

    Vector#(NumberPuppetmasterTests, Vector#(SizeSchedulingPool, PuppetmasterRequest)) testInputs = newVector;
    // Conflict-free transactions.
    for (Integer i = 0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[0][i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[0][i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInputs[0][i].writeObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1;
        end
    end
    // Pairwise conflicts.
    for (Integer i = 0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[1][i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[1][i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInputs[1][i].writeObjects[j] = fromInteger(i - i % 2) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1;
        end
    end
    // Conflict between first and last.
    for (Integer i = 0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[2][i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[2][i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInputs[2][i].writeObjects[j] = (fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1) % (fromInteger(valueOf(SizeSchedulingPool)) * fromInteger(objSetSize) * 2 - 2);
        end
    end
    // Conflict between each pair.
    for (Integer i = 0; i < maxScheduledObjects; i = i + 1) begin
        testInputs[3][i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[3][i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInputs[3][i].writeObjects[j] = fromInteger(i % 2) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1;
        end
    end

    Reg#(UInt#(TAdd#(TLog#(NumberPuppetmasterTests), 1))) counter <- mkReg(0);
    Reg#(UInt#(TAdd#(LogSizeSchedulingPool, 1))) index <- mkReg(0);

    rule feed if (counter < fromInteger(valueOf(NumberPuppetmasterTests)));
        if (index == fromInteger(maxScheduledObjects - 1)) begin
            index <= 0;
            counter <= counter + 1;
        end else begin
            index <= index + 1;
        end
        myPuppetmaster.request.put(testInputs[counter][index]);
    endrule

    rule stream;
        let result <- myPuppetmaster.response.get();
        $display(fshow(result));
    endrule
endmodule
