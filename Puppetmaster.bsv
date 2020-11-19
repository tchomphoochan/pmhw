// Top-level Puppetmaster module.
import ClientServer::*;
import GetPut::*;
import Vector::*;

import PmTypes::*;
import Renamer::*;
import Scheduler::*;

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
        renamer.putInputTransaction(inputs[inputIndex]);
    endrule

    rule receive if (isValid(req) && !isValid(resp) && bufferIndex < fromInteger(valueOf(SizeSchedulingPool)));
        bufferIndex <= bufferIndex + 1;
        let result <- renamer.getRenamedTransaction();
        buffer[bufferIndex] <= result;
    endrule

    rule process if (isValid(req) && !isValid(resp) && bufferIndex == fromInteger(valueOf(SizeSchedulingPool)));
        Vector#(SizeSchedulingPool, TransactionSet) scheduled;
        for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
            scheduled[i] = TransactionSet{
                readSet: buffer[i].readSet,
                writeSet: buffer[i].writeSet,
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
                response[index] = tagged Valid inputs[index].tid;
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

module mkPuppetmasterTestbench();
    Puppetmaster myPuppetmaster <- mkPuppetmaster();
    Reg#(Bool) done <- mkReg(False);

    Vector#(SizeSchedulingPool, InputTransaction) testInput = newVector;
    for (Integer i = 0; i < valueOf(SizeSchedulingPool); i = i + 1) begin
        testInput[i].tid = fromInteger(i);
        for (Integer j = 0; j < objSetSize; j = j + 1) begin
            testInput[i].readObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2;
            testInput[i].writeObjects[j] = fromInteger(i) * fromInteger(objSetSize) * 2 + fromInteger(j) * 2 + 1;
        end
    end

    rule feed if (!done);
        done <= True;
        myPuppetmaster.request.put(testInput);
    endrule

    rule stream;
        let result <- myPuppetmaster.response.get();
        $display(fshow(result));
    endrule
endmodule