# Assembler Pseudo Instructions
The assembler recognizes some instruction mnemonics which may not correspond to exactly one machine instruction.



## Enter
The 'enter' instruction is supposed to aid in preserving registers upon function entry and
setting up a stack frame.  
It takes a variable number of parameters:  
First, it may take zero or more register names.  
Finally, it may take a single immediate value.  

A stack frame is first set up by pushing the current value of rbp, then copying rsp into rbp.  
Afterwards all mentioned registers are saved on the stack in the order listed.  
Finally, if an immediate operand is given, that many further bytes are allocated on the stack by decrementing rsp.

If no registers are mentioned, all callee-saved registers except for rbp and rsp are saved.  
If no immediate operand is given, is defaults to zero.


## Leave
The 'leave' instruction is the reverse of the 'enter' instruction.  
It takes as parameters only a list of register names.

The registers that were saved by the 'enter' instruction are first restored from the stack in reverse of the written order.  
The base pointer (rbp) is then copied back into the stack pointer (rsp), deallocating all memory allocated by the function.  
Finally, a value is popped into rbp.
