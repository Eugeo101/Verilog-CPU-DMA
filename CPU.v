// `timescale 1ns/1ns //4GHZ Processor
module Clock_Gen (clk);
    // i/o
    output reg clk;
    //assign
    initial begin
        clk = 0;
    end
    always #5 clk = ~clk;
endmodule

module CPU(clock);
    input clock;
    //1) MIPS HAVE Instruction memory that have addresses of instructions in 32bits fetched in IR
    //2) we will decode this IR in rs,rt,rd then we will put formats inside Register file and make Alu Operation Then Store in Reg File!!
    //3) No Branching or DataMemory involved
    //4)  I Assumed Clock is 1Ghz and 4Kbyte Imemory (cache)
    
    parameter wordSize = 32; //32 bit
    parameter registerNum = 32; // 32 registers 
    parameter addressSize = 32; //32 bit bus
    parameter instructionLength = 32; //32 bits in Mips

    //1st PC
    reg [addressSize - 1 : 0] PC;
    initial begin
        PC = 0; //initialy begin with 0 address
    end
    
    //2nd Imemory
    reg [instructionLength - 1 : 0] Imemory [0 : 1023]; //4kb cache can have 1024 instruction

    //initially put instructions in Imemory
    integer i;
    initial begin  // i assumed that s0, s1, s2 placed at my RF at 0, 1, 2 address of RF (Register File)
        Imemory[0] = 32'b00000000001000000001000000100000; // 000000 00001 00000 00010 00000 100000   add $s2, $s1, $s0  1
        Imemory[1] = 32'b00000000001000000001000000100010; // 000000 00001 00000 00010 00000 100010   sub $s2, $s1, $s0  1
        Imemory[2] = 32'b00000000001000000001000000100100; // 000000 00001 00000 00010 00000 100100   and $s2, $s1, $s0  0
        Imemory[3] = 32'b00000000001000000001000000100101; // 000000 00001 00000 00010 00000 100101   or $s2, $s1, $s0   1
        Imemory[4] = 32'b00000000111000110001000000100000; // 000000 00111 00011 00010 00000 100000   add $s2, $s7, $s3   a
        Imemory[5] = 32'b00000000111001100001000000100010 ; // 000000 00111 00110 00010 00000 100010   sub $s2, $s7, $s6  1
        Imemory[6] = 32'b00000001010010110001000000100100 ; // 000000 01010 01011 00010 00000 100100   and $s2, $s10, $s11  10(0x0000A)
        //rest of instruction memmory filled with 0 only
        for ( i = 7; i < 1023 ; i = i + 1) begin
            Imemory[i] = 0;
        end
    end  

    //3rd Register File
    reg [(wordSize - 1) : 0] RF [0 : (registerNum - 1)]; //register file
    //intitialize values of RF such that s0 has 0 value, s1 has 1 value and so on and result will always be seen at $s2 ISA
    initial begin
        for (i = 0; i < registerNum - 1; i = i + 1) begin
            RF[i] = i;
        end
    end

    //4th FULL-Path
    reg [wordSize - 1 : 0] IR, result;
    wire [4 : 0] rs, rt, rd, shamt;
    wire [5 : 0] op, func;
    wire [wordSize - 1 : 0] Rs, Rt;

    //parse in op,rs,rt,rd,shamt,function
    assign op = IR[31 : 26];
    assign rs = IR[25 : 21];
    assign rt = IR[20 : 16];
    assign rd = IR[15 : 11];
    assign shamt = IR[10 : 6];
    assign func = IR[5 : 0];

    //load values of rs, rt from RF
    assign Rs = RF[rs];
    assign Rt = RF[rt];
    

    //assign 
    
    // integer sum = 0;
    always @(posedge clock) begin //Our Path
        IR <= Imemory[PC >> 2]; //put instruction insider IR (Instruction Register)
        PC <= PC + 4;

        //now call the Alu with AluCtl which is opcode and select which one by shamt and func
        if (op == 6'b000000) begin //Rformat
            if (shamt == 5'b00000) begin //so not Shift R instruction its just (and / or)>>logic or (+ / -)>>Arithmatic
               case (func)
                   32 : result <= (Rs + Rt);  //add 32 is from function of add at MIPS Proecesor
                   34 : result <= (Rs - Rt);  //sub
                   36 : result <= (Rs & Rt);  //and
                   37 : result <= (Rs | Rt);  //or
                   default:  result <= 0;
               endcase 
            end
        end
        
        //result is ready now store in RF
        RF[rd] <= result;

    end

    

endmodule


module tb_CPU;
    // i/o
    wire clock;
    Clock_Gen c1(clock);
    CPU MIPS1(clock);

endmodule









// module toDecimal (in, decimal);
//     input [4:0] in;
//     output decimal;
//     integer i, sum = 0;
//     initial begin
//         for (i = 0; i < 5; i = i + 1) begin
//             sum = sum + (in[i] * 1<<i);
//         end
//     end
//     assign decimal = sum;
// endmodule