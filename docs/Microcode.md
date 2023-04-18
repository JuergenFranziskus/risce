# Microcode
The following control lines flowing from the decoding stage need to be properly set by a microcode pla.

## ALU Op (2 bits, 0-1)
- 00: Use ALU Op as decoded for instruction  0x00
- 01: Use ALU op as decoded for instructions 0x10 thru 0x13, 0x1C thru 0x1F
- 10: Force ALU op to the Compose operation (0x1)
- 11: Force ALU op to the B-Passthrough operation (0x0)

## ALU Swap (1 bit, 2)
When set, swap the A and B inputs to the ALU *after* taking into account the Immediate Mode.

## ExecMode (1 bit, 3)
When set, perform a branching operation.  
Otherwise, perform a regular ALU operation

## MemMode (2 bits, 4-5)
- 00: No Memory operation, pass value through to Writeback Stage
- 01: LEA
- 10: Store
- 11: Load

## ImmediateMode (1 bit, 6)
When set, substitute the immediate for the B input on the ALU.

## MemSize (1 bit, 7)
When set, use the MemSize as decoded from Format B load instructions (bits 17 thru 18).  
Otherwise, as decoded from Format D store instructions (22 thru 23)

## Condition (2 bits, 8-9)
- 00: Use condition as decoded for instruction 0x00
- 01: Use condition as decoded for instruction 0x36
- 10: Use condition as decoded for instruction 0x37

## AddressMode (1 bit, 10)
When set, compute memory address as RIP + Source0 + Immediate.  
Otherwise, compute as Source0 + Immediate.

## BranchMode (1 bit, 11)
When set, perform an unconditional branch and store the return address into a register.  
Otherwise, perform an unconditional branch and don't store anything.

## BranchDestMode (1 bit, 12)
When set, calculate the branch destination as RIP + Immediate.  
Otherwise, calculate as Immediate + Source0
