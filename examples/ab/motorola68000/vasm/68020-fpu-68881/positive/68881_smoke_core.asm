; Motorola 68020 + 68881 curated smoke fixture derived from the allmodes example

start:
    FMOVE FP0,FP1
    FMOVE.B D0,FP2
    FMOVE.W FP2,D1
    FMOVE.S (A0),FP3
    FMOVE.D (A0)+,FP4
    FMOVE.X -(A1),FP5
    FMOVE.P 8(A2),FP6
    FADD FP1,FP2
    FADD.S (A0),FP0
    FCOS.W (A0),FP3
    RTS
