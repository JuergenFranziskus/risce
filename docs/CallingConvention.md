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
| r3 - r6 | Parameters 2 thru 6 and scratch | No |
| r7 - r15 | Scratch registers | No |
| r16 - r29 | Saved registers | Yes |
| r30 | Stack pointer | Yes |
| r31 | Link register | No |



