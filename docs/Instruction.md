# Instruction Encoding
All instructions are 4 bytes long.  
The first 7 bits are always the opcode.
The Destination register, if present, is always at bits 7 thru 11.  
The Source0 register, if present, is always at bits 12 thru 16.  
The Source1 register, if present, is always at bits 17 thru 21.  
When the ALU is used and the opcode does not specify anything else,  
the ALU input A is always equal to Source0, and B to Source1.
All instructions fall into one of { TBD } formats, which specify  
which of the above registers is present, if there is an immediate  
and how the immediate is encoded if present.  
Any bits not specified by the format may be further specified  
by individual opcodes.  
Any bits specified by neither the format nor the opcode should be zero.


# Instructions Formats
- A: Reg-Reg Instructions  
    Instructions of this format have a Source0, Source1 and Destination register,  
    and no immediate.  
    Bits 22 thru 31 are free.

- B: Reg-Imm Instructions  
    Instructions of this format have a Source0 and Destination register,  
    as well as a 12-bit immediate at bits 20 thru 31.  
    Bits 17 thru 19 are free.

- C: Immediate Instructions  
    Instructions of this format have a Destination register  
    and a 20-bit immediate at bits 12 thru 31.  
    No bits are free.

- D: Branch and Store Instructions  
    Instructions of this format have both Source registers  
    and a 12-bit immediate at bits 7 thru 11, 25 thru 31.  
    Bits 22 thru 24 are free.

# Instruction Listing
- 0x0: ALU Op A (Format A)  
    Perform an ALU op.  
    The ALU op to perform is given by bits 22 thru 26.  
    A Branch condition, where relevant, is given by bits 27 thru 30.

- 0x10: ALU Op B0 (Format B)  
    Perform an ALU op.  
    The ALU op to perform is given by bits 17 thru 19.

- 0x11: ALU Op B1 (Format B)  
    Perform an ALU op.  
    The ALU op to perform is given by bits 17 thru 19 with an offset of 8.

- 0x12: ALU Op B2 (Format B)  
    Perform an ALU op.  
    The ALU op to perform is given by bits 17 thru 19 with an offset of 16.

- 0x13: ALU Op B3 (Format B)  
    Perform an ALU op.  
    The ALU op to perform is given by bits 17 thru 19 with an offset of 24.

- 0x14: Load Relative (Format B)  
    Compute an address as (RIP + Immediate + Source0)
    and load a value into the destination register.  
    The amount of bytes to load is given by bits 17 thru 18.

- 0x17: Load Effective Relative Address (Format B)  
    Perform an address calculation as in 0x14,  
    storing the address itself into the destination register.

- 0x18: Load Offset (Format B)  
    Compute an address as Immediate + Source0,  
    then load a value from the resulting address and store it into the destination.  
    The amount of bytes to load is given by bits 17 thru 18.

- 0x1A: Delayed Jump Absolute (Format B)  
    Store the address of the next instruction in the destination register.  
    Then jump to (Source0 + Immediate).  
    Two delay slots after this instruction are always executed in spite of the branch.

- 0x1B: Jump Absolute (Format B)  
    Store the address of the next instruction in the destination register.  
    Then jump to (Source0 + Immediate).

- 0x1C: ALU Op B4 (Format B)  
    Perform an ALU op, swapping the inputs.  
    The ALU op to perform is given by bits 17 thru 19.

- 0x1D: ALU Op B5 (Format B)  
    Perform an ALU op, swapping the inputs.  
    The ALU op to perform is given by bits 17 thru 19 with an offset of 8.

- 0x1E: ALU Op B6 (Format B)  
    Perform an ALU op, swapping the inputs.  
    The ALU op to perform is given by bits 17 thru 19 with an offset of 16.

- 0x1F: ALU Op B7 (Format B)  
    Perform an ALU op, swapping the inputs.  
    The ALU op to perform is given by bits 17 thru 19 with an offset of 24.

- 0x20: Load Immediate (Format C)  
    Load the immediate into the destination register.

- 0x21: Load Upper Immediate(Format C)  
    Load the immediate shifted to the left by 12 bits into the destination register.

- 0x22: Jump Relative (Format C)  
    Store the address of the next instruction into the Destination register.  
    Then jump to (RIP + Immediate).

- 0x23: Delayed Jump Relative (Format C)  
    Store the address of the next instruction into the Destination register.  
    Then jump to (RIP + Immediate).  
    Two delay slots after this instruction are always executed in spite of the branch.

- 0x30: Store Relative (Format D)  
    Compute an offset, add it onto the address of this instruction
    and store the value of Source1 into the resulting address.  
    The offset is computed as in 0x14.  
    How many bytes to write is given by bits 22 thru 23

- 0x33: Store Offset (Format D)  
    Compute an address and store the lowest byte of Source1 into the resulting address.  
    The address is computed as in 0x18.
    How many bytes to write is given by bits 22 thru 23

- 0x36: Branch Relational (Format D)  
    Compute a condition on Source0 and Source1, and jump to (RIP + Immediate) if it is true.  
    The condition is given by bits 22 thru 24.

- 0x37: Branch Equality (Format D)  
    Compute a condition on Source0 and Source1, and jump to (RIP + Immediate) if it is true.
    The condition is given by bit 22 thru 24 with an offset of 8.

- 0x38: Delayed Branch Relational (Format D)  
    Compute a condition on Source0 and Source1, and jump to (RIP + Immediate) if it is true.  
    The condition is given by bits 22 thru 24.  
    Two delay slots after this instruction are always executed in spite of the branch.

- 0x39: Delayed Branch Equality (Format D)  
    Compute a condition on Source0 and Source1, and jump to (RIP + Immediate) if it is true.
    The condition is given by bit 22 thru 24 with an offset of 8.  
    Two delay slots after this instruction are always executed in spite of the branch.

