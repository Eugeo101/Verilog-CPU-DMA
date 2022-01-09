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

module CPU(input clock, input HRQ, input reg [31 : 0] ADDRESS, input reg [31 : 0] COUNT, output reg HLDA, output reg read_write, output reg [31 : 0] address, output reg [31 : 0] count);
    //1) MIPS HAVE Instruction memory that have addresses of instructions in 32bits fetched in IR
    //2) we will decode this IR in rs,rt,rd then we will put formats inside Register file and make Alu Operation Then Store in Reg File!!
    //3) No Branching or DataMemory involved
    //4)  I Assumed Clock is 1Ghz and 4Kbyte Imemory (cache)

    reg [31 : 0]Increasing_signal = 0;

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

    //cpu will read so  read_write = 0, write so read_write = 1
    always @(posedge clock) begin //Our Path

        //HRQ = 0 so do no nothing
        if (HRQ === 0) begin

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
            // No request and No Transfer
            HLDA = 0;
        end

        else if (HRQ === 1) begin
            //display when DMA have bus so it's at DMA CODE


            //already finished the cycle
            address = ADDRESS;
            count = COUNT;
            HLDA <= 1; //double check that he finished the cycle then HLDA = 1
            read_write <= 1; //so cpu write on main_memory from Disk(I/O)
            // read_write <= 0; //so cpu read from main_memory and write in Disk(I/O)
            Increasing_signal = Increasing_signal + 1;

        end

    end

    

endmodule


module DMA (clock, DREQ, HLDA, address, count, read_write, mem_disk, mem_ram, DACK, HRQ); //ram and disk is input so wont change
    input clock;
    input DREQ;
    input HLDA;
    input read_write;
    input [32 * 32 - 1 : 0] mem_disk;
    input [32 * 32 - 1 : 0] mem_ram;
    output reg DACK;
    output reg HRQ;
    reg [7 : 0] DATA_REGISTER; // has 8 bits
    inout reg [31 : 0] address, count;
    reg [31 : 0] ADDRESS_REM;
    reg my_first_time;
    reg first_time;
    reg first_loob;
    reg [31 : 0] sum;
    reg [31 : 0] ADDRESS;
    reg [31 : 0] COUNT;
    reg [31 : 0] data; // has 8 bits

    //array unflaterned
    reg [31 : 0] RAM [0 : 31];
    reg [31 : 0] DISK [0 : 31];
    reg [31 : 0] RAM_OUT [0 : 31];
    reg [31 : 0] DISK_OUT [0 : 31];
    reg [2 : 0] counter;
    integer r = 0;
    integer c = 0;
    integer start_index = 0;
    // integer end_index = 31;
    integer start_index_byte = 0;
    // integer end_index_byte = 7;
    parameter registerNum = 32;
    parameter wordSize = 32;

    
    initial begin
        //at first cpu have the bus
        //remainder first of 4
        COUNT = count;
        my_first_time = 0;
        first_time = 1;
        first_loob = 1;
        counter = 0;
        HRQ = 0;
        r = 0;
        c = 0;
        start_index = 0;
        start_index_byte = 0;
    end
    
    always @(*) begin
        if (DREQ == 1) begin
            HRQ = 1; // make HRQ = high and request bus when DISK make DMA Request
        end

        if(HLDA == 1'b1) begin
            DACK = 1;
            // $display("at time %0t, DACK is %b", $time, DACK);
        end
        if (my_first_time == 0 && (read_write == 1 || read_write == 0) ) begin
            if (read_write == 1) begin
                $display("The CPU Give The Bus to DMA At Time %0t, to Transfer %d bytes from byte number %d at DISK, Transfer from DISK to Main Memory", $time, count, address);
            end
            else if(read_write == 0) begin
                $display("The CPU Give The Bus to DMA At Time %0t, to Transfer %d bytes from byte number %d at Main Memory, Transfer from Main Memory to DISK", $time, count, address);
            end
            my_first_time = 1;
        end

        //when bus is ready so DMA USE IT
        if (DACK == 1) begin 
            start_index = 0;
            start_index_byte = 0;

            //remainder first of 4
            sum = address;
            while(sum >= 4)
                sum = sum - 4;
            ADDRESS_REM = sum;

            COUNT = count;
            first_time = 1;
            ADDRESS = address;
            // $display("at time %0t, COUNT is %b", $time, COUNT);

            //first make the vector be array then change it's values
            for ( r = 0; r < registerNum; r = r + 1) begin
                RAM[r] = mem_ram[start_index +: 32];
                start_index = 32 * (r + 1);
            end

            for ( r = 0; r < registerNum; r = r + 1) begin
                DISK[r] = mem_disk[start_index +: 32];
                start_index = 32 * (r + 1);
            end
            RAM[0] = mem_ram[31 : 0];
            DISK[0] = mem_disk[31 : 0];
            //so read from main memory and write in desk
            if (read_write === 0) begin 
                for ( r = 0; COUNT != 0; r = r + 1) begin       
                    if (COUNT != 0) begin
                        data = RAM[r];
                        // start_index_byte = r * 32; as r will move to next row
                        first_loob = 1;
                        counter = 0;
                            for ( c = 0; c < 4; c = c + 1) begin
                                // #10 // i couldnt make a clock im sorry i cant make nested loops
                                if (COUNT != 0) begin
                                    //first_time
                                    if (first_time == 1) begin
                                        first_time = 0;
                                    end
                                    //first_loob
                                    if (first_loob == 1) begin
                                        first_loob = 0;
                                        case (ADDRESS_REM)
                                            32'd 0 : DATA_REGISTER = data[7 : 0];  
                                            32'd 1 : DATA_REGISTER = data[15 : 8];  
                                            32'd 2 : DATA_REGISTER = data[23 : 16];  
                                            32'd 3 : DATA_REGISTER = data[31 : 24];  
                                            default: DATA_REGISTER = 32'bZ;
                                        endcase
                                        c = ADDRESS_REM;
                                        DISK_OUT[r][7 : 0] = DATA_REGISTER;
                                    end
                                    else begin
                                        DATA_REGISTER = data[start_index_byte +: 8];
                                        DISK_OUT[r][start_index_byte +: 8] = DATA_REGISTER;
                                    end
                                    // $display("at time %0t, start_index_byte is %d", $time, start_index_byte);
                                    // $display("at time %0t, counter is %d", $time, counter);
                                    start_index_byte = 8 * (counter + 1);
                                    COUNT = COUNT - 1;
                                    $display("The DMA Sent The byte number %d whose value is %b from Main Memory to the DISK At Time %0t", count - COUNT, DATA_REGISTER, $time);
                                    #10 // i couldnt make a clock im sorry i cant make nested loops
                                    counter = counter + 1;
                                end
                            end
                    end
                    ADDRESS_REM = 0;
                    // $display("at time %0t, DISK_OUT[r] is %d", $time, DISK_OUT[r]);
                end 
                // continue the last disk
                for ( r = r; r < registerNum; r = r + 1) begin
                    DISK_OUT[r] = DISK[r];
                end
                //re make new array
                for (r = 0; r < registerNum; r = r + 1) begin
                    RAM_OUT[r] = RAM[r];
                end
                //transfer finisihed
                HRQ = 0;
                DACK = 0;
                $display("The Transfer Completed At Time %0t", $time);
                $display("The CPU Have The Bus At Time %0t", $time);
            end

            //read_write === 1, so wrtie in main memory and from desk
            else begin 
                for ( r = 0; COUNT != 0; r = r + 1) begin
                    
                    if (COUNT != 0) begin
                        data = DISK[r];
                        first_loob = 1;
                        counter = 0;
                        for ( c = 0; c < 4; c = c + 1) begin
                            // #10 // i couldnt make a clock im sorry i cant make nested loops
                            if (COUNT != 0) begin
                                //first_time
                                if (first_time == 1) begin
                                    first_time = 0;
                                end
                                //first_loob
                                if (first_loob == 1) begin
                                    first_loob = 0;
                                    case (ADDRESS_REM)
                                        32'd 0 : DATA_REGISTER = data[7 : 0];  
                                        32'd 1 : DATA_REGISTER = data[15 : 8];  
                                        32'd 2 : DATA_REGISTER = data[23 : 16];  
                                        32'd 3 : DATA_REGISTER = data[31 : 24];  
                                        default: DATA_REGISTER = 32'bZ;
                                    endcase
                                    c = ADDRESS_REM;
                                    // $display("at time %0t, DATA_REGISTER is %d at c = ", $time, DATA_REGISTER, c);
                                    RAM_OUT[r][7 : 0] = DATA_REGISTER;
                                end
                                else begin
                                    DATA_REGISTER = data[start_index_byte +: 8];
                                    RAM_OUT[r][start_index_byte +: 8] = DATA_REGISTER;
                                end


                                // $display("at time %0t, start_index_byte is %d", $time, start_index_byte);
                                // $display("at time %0t, counter is %d", $time, counter);
                                start_index_byte =  8 * (counter + 1);
                                COUNT = COUNT - 1;
                                $display("The DMA Sent The byte number %d whose value is %b from the DISK to the Main Memory At Time %0t", count - COUNT, DATA_REGISTER, $time);
                                #10 // i couldnt make a clock im sorry i cant make nested loops
                                counter = counter + 1;
                            end
                            
                        end
                        ADDRESS_REM = 0;
                        // $display("at time %0t, RAM_OUT[r] is %d", $time, RAM_OUT[r]);
                    end
                end
                // continue the last disk
                for ( r = r; r < registerNum; r = r + 1) begin
                    RAM_OUT[r] = RAM[r];
                end
                // //re make new array
                for (r = 0; r < registerNum; r = r + 1) begin
                    DISK_OUT[r] = DISK[r];
                end
                //transfer finisihed
                HRQ = 0;
                DACK = 0;
                $display("The CPU Have The Bus At Time %0t", $time);
            end
            
        end
    end
        
    
endmodule

module RAM (mem);

    //32 register
    reg [31 : 0] main_memory [0 : 31]; //memory to be flatern
    parameter registerNum = 32;
    parameter wordSize = 32;
    integer j = 0;
    integer r = 0;
    integer c = 0;
    reg [31 : 0] data;
    output reg [32 * 32 - 1 : 0] mem;
    
    initial begin //flattern
        //make the array
        for ( j = 0; j < registerNum; j = j + 1) begin
            main_memory[j] = j;
        end

        //flattern array        
        j = 0;
        for ( r = 0; r < registerNum; r = r + 1) begin
            data = main_memory[r];
            for ( c = 0; c < wordSize; c = c + 1) begin
                mem[j] = data[c];
                j = j + 1;
            end
        end
    end
    
endmodule

// I/O Device
module DISK (mem, DREQ, DACK, ADDRESS, COUNT);
    input DACK;
    output reg DREQ;
    output reg[31 : 0] ADDRESS, COUNT;
    //32 register
    reg [31 : 0] disk_memory [0 : 31]; //memory to be flatern
    parameter registerNum = 32;
    parameter wordSize = 32;
    integer j = 0;
    integer r = 0;
    integer c = 0;
    reg [31 : 0] data;
    // reg [32 * 32 - 1 : 0] my_mem;
    output reg [32 * 32 - 1 : 0] mem;
    
    
    initial begin //after 20 seconds Disk Wants to transfer (cpu made 2 cycles)
        #20 DREQ = 0;
        DREQ = 1;
        $display("The DISK Requested Transfer from DMA At Time %0t", $time);
        ADDRESS = 2; //number in bytes
        COUNT = 16; // 16 byte

        //flattern
        //make the array
        for ( j = 0; j < registerNum; j = j + 1) begin
            disk_memory[j] = 31 - j;
        end

        //flattern array  
        j = 0;
        for ( r = 0; r < registerNum; r = r + 1) begin
            data = disk_memory[r];
            for ( c = 0; c < wordSize; c = c + 1) begin
                mem[j] = data[c];
                j = j + 1;
            end
        end
    end
    always @(*) begin
        // DMA FINISHED TRANSFER
        if (DACK == 0) begin
            DREQ = 0;
        end
    end

endmodule


module tb_CPU;
    // i/o

    wire clock;
    wire DACK;
    wire DREQ, HLDA, read_write;

    //intializing Disk getting memory
    wire [32 * 32 - 1 : 0] mem_disk;
    wire [32 * 32 - 1 : 0] mem_ram;
    wire HRQ;
    wire [31 : 0] ADDRESS; //outputs of DISK
    wire [31 : 0] COUNT; // outputs of DISK
    wire [31 : 0] address; // outputs of CPU
    wire [31 : 0] count; // outputs of CPU


    //1- clock
    Clock_Gen c1(clock);

    //2- Disk(I/O) + RAM
    DISK D1(mem_disk, DREQ, DACK, ADDRESS, COUNT);
    RAM R1(mem_ram);

    //3- DMA
    DMA DMA_1(clock, DREQ, HLDA, address, count, read_write, mem_disk, mem_ram, DACK, HRQ);

    //4- CPU    

    CPU MIPS2(clock, HRQ, ADDRESS, COUNT, HLDA, read_write, address, count);
    
    

endmodule
