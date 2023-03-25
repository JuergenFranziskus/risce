# Stages
The RiscE has a classic 5-stage risc pipeline.

# Stalling and Bubbling
While being largely independent, for good performance and correctness,
it is sometimes necessary for one stage to communicate with another directly.  
The broadest case of this is stalling and bubbling:  
Each stage has a one-bit 'bubble' output, as well as a corresponding input.  
The bubble line tells the very next stage
that its inputs for this cycle are invalid and that it should not perform any work.  
Naturally, if a stage receives a bubble signal, it should always propagate it to its own successor.  


# Register Bypass
To deal with data dependence between adjacent instructions,
there are two bypass channels flowing backwards into the Execution stage.  
Register reads are always performed in the Decoding stage.
In some cases, it may be possible that the current data saved in the register file
does not accurately represent the logical state of a register at this point
in the instruction stream, as another instruction that is already
farther ahead in the pipeline has computed a new value that is yet to be written.  
The values read by the Decoding stage are therefor wrong and need to be corrected.  
To accomplish this, both the Memory and WriteBack stages send a bypass signal
back to the Execute stage, informing it about register values that are yet to be written,  
but are already known.  
The Execute stage can then compare these bypassed registers to its own inputs
and choose the newer values as necessary.

# Memory Load Stalling
There is one case where a newer value cannot simply be bypassed backward into the Execute stage
because its correct value isn't known yet,  
namely when a load operation is being performed.  
In that case, the Memory stage knows that it is about to overwrite a register,
but it can't know the new value until the load is complete.
Another inter-stage channel is used to communicate this fact back to the Execute stage,  
which may then, if it deems itself to be dependent on the register's new value,  
stall the pipeline.
