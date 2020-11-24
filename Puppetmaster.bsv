// Top-level Puppetmaster module.
import ClientServer::*;
import GetPut::*;
import Vector::*;

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
    Reg#(Maybe#(PuppetmasterResponse)) resp <- mkReg(tagged Invalid);

    let renamer <- mkRenamer();
    let scheduler <- mkScheduler();

    rule send if (isValid(req) && !isValid(resp) && inputIndex < fromInteger(valueOf(SizeSchedulingPool)));
        inputIndex <= inputIndex + 1;
        let inputs = fromMaybe(?, req);
        renamer.request.put(inputs[inputIndex]);
    endrule

    rule receive if (isValid(req) && !isValid(resp) && bufferIndex < fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= bufferIndex + 1;
        let result <- renamer.response.get();
        buffer[bufferIndex] <= result;
    endrule

    rule process if (isValid(req) && !isValid(resp) && bufferIndex == fromInteger(valueOf(SizeSchedulingPool)));
        let inputs = fromMaybe(?, req);
        Vector#(SizeSchedulingPool, TransactionSet) scheduled;
        for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
            function Bool matchesTid(RenamedTransaction tr);
                return tr.tid == inputs[i].tid;
            endfunction
            let entry = fromMaybe(?, find(matchesTid, readVReg(buffer)));
            scheduled[i] = TransactionSet{
                readSet: entry.readSet,
                writeSet: entry.writeSet,
                indices: 1 << i
            };
        end
        scheduler.request.put(scheduled);
    endrule

    rule convert if (isValid(req) && !isValid(resp) && bufferIndex == fromInteger(valueOf(SizeSchedulingPool)));
        let result <- scheduler.response.get();
        PuppetmasterResponse response = replicate(tagged Invalid);
        let inputs = fromMaybe(?, req);
        Integer index = 0;
        for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
            if (result.indices[i] == 1'b1) begin
                response[index] = tagged Valid inputs[i].tid;
                index = index + 1;
            end
        end
        resp <= tagged Valid response;
    endrule

    interface Put request;
        method Action put(PuppetmasterRequest request) if (!isValid(req));
            req <= tagged Valid request;
            inputIndex <= 0;
            bufferIndex <= 0;
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(PuppetmasterResponse) get() if (isValid(req) && isValid(resp));
            req <= tagged Invalid;
            resp <= tagged Invalid;
            return fromMaybe(?, resp);
        endmethod
    endinterface

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

    Reg#(UInt#(32)) counter <- mkReg(0);

    rule feed if (counter < fromInteger(valueOf(NumberPuppetmasterTests)));
        counter <= counter + 1;
        myPuppetmaster.request.put(testInputs[counter]);
    endrule

    rule stream;
        let result <- myPuppetmaster.response.get();
        $display(fshow(result));
    endrule
endmodule