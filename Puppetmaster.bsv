// Top-level Puppetmaster module.
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmCore::*;
import Renamer::*;
import Scheduler::*;
import Shard::*;

typedef Vector#(SizeSchedulingPool, InputTransaction) PuppetmasterRequest;
typedef Vector#(SizeSchedulingPool, Maybe#(TransactionId)) PuppetmasterResponse;
typedef Server#(PuppetmasterRequest, PuppetmasterResponse) Puppetmaster;

module mkPuppetmaster(Puppetmaster);
    Reg#(Maybe#(PuppetmasterRequest)) req <- mkReg(tagged Invalid);
    Reg#(Bit#(TAdd#(LogSizeSchedulingPool, 1))) inputIndex <- mkReg(0);
    Vector#(SizeSchedulingPool, Reg#(RenamedTransaction)) buffer <- replicateM(mkReg(?));
    Reg#(Bit#(TAdd#(LogSizeSchedulingPool, 1))) bufferIndex <- mkReg(0);

    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();

    // Send input transactions to renamer.
    rule send if (isValid(req) && inputIndex < fromInteger(valueOf(SizeSchedulingPool)));
        inputIndex <= inputIndex + 1;
        let inputs = fromMaybe(?, req);
        renamer.request.put(inputs[inputIndex]);
    endrule

    // Put renamed transactions into a buffer.
    rule receive if (isValid(req) && bufferIndex < fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= bufferIndex + 1;
        let result <- renamer.response.get();
        buffer[bufferIndex] <= result;
    endrule

    // When buffer is full, send scheduling request.
    rule process if (isValid(req) && bufferIndex == fromInteger(valueOf(SizeSchedulingPool)));
        req <= tagged Invalid;
        let inputs = fromMaybe(?, req);
        Vector#(SizeSchedulingPool, RenamedTransaction) scheduled;
        // Reorder transactions to follow original order.
        for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
            function Bool matchesTid(RenamedTransaction tr);
                return tr.tid == inputs[i].tid;
            endfunction
            scheduled[i] = fromMaybe(?, find(matchesTid, readVReg(buffer)));
        end
        scheduler.request.put(scheduled);
    endrule

    interface Put request;
        method Action put(PuppetmasterRequest request) if (!isValid(req));
            req <= tagged Valid request;
            inputIndex <= 0;
            bufferIndex <= 0;
        endmethod
    endinterface

    interface Get response = scheduler.response;

endmodule

typedef 4 NumberPuppetmasterTests;

module mkPuppetmasterTestbench();
    Puppetmaster myPuppetmaster <- mkPuppetmaster();

    Vector#(NumberPuppetmasterTests, PuppetmasterRequest) testInputs = newVector;
    // Conflict-free transactions.
    for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[0][i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[0][i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInputs[0][i].writeObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1;
        end
    end
    // Pairwise conflicts.
    for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[1][i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[1][i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInputs[1][i].writeObjects[j] = fromInteger(i - i % 2) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1;
        end
    end
    // Conflict between first and last.
    for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[2][i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[2][i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInputs[2][i].writeObjects[j] = (fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1) % (fromInteger(valueOf(SizeSchedulingPool)) * fromInteger(objSetSize) * 2 - 2);
        end
    end
    // Conflict between each pair.
    for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInputs[3][i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInputs[3][i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInputs[3][i].writeObjects[j] = fromInteger(i % 2) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1;
        end
    end

    Reg#(UInt#(TAdd#(TLog#(NumberPuppetmasterTests), 1))) counter <- mkReg(0);

    rule feed if (counter < fromInteger(valueOf(NumberPuppetmasterTests)));
        counter <= counter + 1;
        myPuppetmaster.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result <- myPuppetmaster.response.get();
        $display(fshow(result));
    endrule
endmodule