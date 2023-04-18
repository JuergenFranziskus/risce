# Branch Unit
The Branch Unit computes a condition over the Source0 and Source1 inputs, henceforth called A and B,   
and instructs the Fetch Stage to transfer control if the condition is true.


# Operation Codes
* 0x0: Greater Than (Signed)  
    I = A > B
* 0x1: Less Than (Signed)  
    I = A < B
* 0x2: Above (Unsigned)  
    I = A > B
* 0x3: Below (Unsigned)  
    I = A < B
* 0x4: Not Greater  
    I = A <= B
* 0x5: Not Less  
    I = A >= B
* 0x6: Not Above  
    I = A <= B
* 0x7: Not Below  
    I = A >= B
* 0x8: Equal  
    I = A == B
* 0x9: Not Equal  
    I = A != B

