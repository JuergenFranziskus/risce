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

- 0x14: Load Byte Relative (Format B)  
    Compute an address as (RIP + Immediate + Source0)
    and load a byte into the destination register.  

- 0x15: Load Short Relative (Format B)  
    Perform the same operation as in 0x14, loading two bytes instead of one.

- 0x16: Load Word Relative (Format B)  
    Perform the same operation as in 0x14, loading four bytes instead of one.

- 0x17: Load Effective Relative Address (Format B)  
    Perform an address calculation as in 0x14,  
    storing the address itself into the destination register.

- 0x18: Load Byte Offset (Format B)  
    Compute an address as Immediate + Source0,  
    then load a byte from the resulting address and store it into the destination.

- 0x19: Load Short Offset (Format B)  
    Same operation as in 0x18, loading two bytes instead of one.

- 0x1A: Load Word Offset (Format B)  
    Same operation as in 0x18, loading four bytes instead of one.

- 0x1B: Jump Absolute Offset (Format B)  
    Store the address of the next instruction into the Destination register.  
    Then jump to (Immediate + Source0).


- 0x20: Load Immediate (Format C)  
    Load the immediate into the destination register.

- 0x21: Load Upper Immediate(Format C)  
    Load the immediate shifted to the left by 12 bits into the destination register.

- 0x22: Jump Relative ( Format C)  
    Store the address of the next instruction into the Destination register.  
    Then jump to (RIP + Immediate).

- 0x30: Store Byte Relative (Format D)  
    Compute an offset, add it onto the address of this instruction
    and store the lowest byte of Source1 into the resulting address.  
    The offset is computed as in 0x14.

- 0x31: Store Short Relative (Format D)  
    Perform the same operation as in 0x30, storing two bytes instead of one.

- 0x32: Store Word Relative (Format D)  
    Perform the same operation as in 0x30, storing four bytes instead of one.


- 0x33: Store Byte Offset (Format D)  
    Compute an address and store the lowest byte of Source1 into the resulting address.  
    The address is computed as in 0x18.

- 0x34: Store Short Offset (Format D)  
    Perform the same operation as in 0x33, storing two bytes instead of one.

- 0x35: Store Word Offset (Format D)  
    Perform the same operation as in 0x33, storing four bytes instead of one.


- 0x36: Branch Relational (Format D)  
    Compute a condition on Source0 and Source1, and jump to (RIP + Immediate) if it is true.  
    The condition is given by bits 22 thru 24 thusly:
    * 0: Greater (signed)
    * 1: Less (signed)
    * 2: Above (unsigned)
    * 3: Below (unsigned)
    * 4: Not Greater
    * 5: Not Less
    * 6: Not Above
    * 7: Not Below

- 0x37: Branch Equality (Format D)  
    Compute a condition on Source0 and Source1, and jump to (RIP + Immediate) if it is true.
    The condition is given by bit 22 thusly:
    * 0: Equal
    * 1: Not Equal
