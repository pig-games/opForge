; Small rendering and sample-mixing operations with nested/default arguments.
.module ops
.cpu 68020
.pub
P .macro x, y=3
    .word .x,.y
.endmacro
R .macro x, y=4
    MOVEQ #.x,D1
    MOVE.W 4(A0),D2
    ADD.W D2,D0
    .P .x,.y
.endmacro
M .macro x, y=2
    MOVE.W .x(A0,D1.W),D3
    EOR.W D3,D0
    MOVE.W D0,.y(A1)
    .P .x
.endmacro
T .macro x, y=6
    .R .x,.y
    ADDQ.L #2,A0
    .byte .x,.y
.endmacro
.endmodule
