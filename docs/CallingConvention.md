# Calling Convention
The RiscE processor does not assume any implicit calling convention.  
The assembler however does, and as such has certain convenience instructions
such as a call and ret instruction that assume a fixed link register.  

# Register Usage Table
| Register | Usage | Preserved |
| -------- | ----- | --------- |
| r0 | Fixed to zero | n/a |
| r1 | Parameter #1 and return value | No |
| r2 | Parameter #2 and return value | No |
| r3 - r6 | Parameters 3 thru 6 and scratch | No |
| r7 - r15 | Scratch registers | No |
| r16 - r28 | Saved registers | Yes |
| r29 | Base Pointer | Yes |
| r30 | Stack pointer | Yes |
| r31 | Link register | No |


# Stack
The stack pointer should be initialized by the binary entry point such that is can freely grow downward.
Upon entry to a function, the stack needs to be aligned to four bytes.


# Parameter Passing
A parameter can only be directly passed to a function if it is smaller than the word size of four bytes.
If a larger parameter is required, a pointer to it is passed instead.  
The first 6 parameters are passed in the registers r1 thru r6.  
All other parameters are pushed onto the stack in order from right to left, respecting alignment.

# Return Value
Return values up to 8 bytes in size are passed in registers r0:r1.  
If a larger return value is needed, the calling function must allocate space for it on it's stack frame
and pass a pointer to that as an implicit first parameter.
