# ALU Opcodes
The ALU has inputs for the Source0 register, called A, Source1 register, called B, and an immediate,
which may be substituted for B by a flag input.  
The boolean output from the branch unit can also be read, called I.  
The ALU output is called O.  
It also has a 5-bit operation selector.


# Opcode Listing
* 0x0: B Passthrough  
    O = B
* 0x1: B Compose  
    O = B << 0xC
* 0x2: Add  
    O = A + B
* 0x3: Sub  
    O = A - B
* 0x4: Negate  
    O = -A
* 0x5: And  
    O = A & B
* 0x6: Or  
    O = A | B
* 0x7: Nand  
    O = ~(A & B)
* 0x8: Xor  
    O = A ^ B
* 0x9: Shift Left  
    O = A << B
* 0xA: Logical Shift Right  
    O = A >> B
* 0xB: Arithmetic Shift Right  
    O = A >> B
* 0xC: Rotate Left  
    O = A rol B
* 0xD: Rotate Right  
    O = A ror B
* 0xE: Mul  
    O = low(A * B)
* 0xF: UMul  
    O = high(A * B)
* 0x10: IMul  
    O = high(A * B)
* 0x11: UDiv  
    O = A / B
* 0x12: URem  
    O = A % B
* 0x13: IDiv  
    O = A / B
* 0x14: IRem  
    O = A % B
* 0x15: Set  
    O = I ? 1 : 0
