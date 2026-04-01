; Motorola 68020 external FPU register and control-register surface
.cpu 68020
.fpu 68881
.org $1000

start:
    ; Touch all FP data registers directly.
    FMOVE FP0,FP1
    FMOVE FP2,FP3
    FMOVE FP4,FP5
    FMOVE FP6,FP7

    ; Control-register transfers.
    FMOVE.L D0,FPCR
    FMOVE.L FPCR,D1
    FMOVE.L D2,FPSR
    FMOVE.L FPSR,D3
    FMOVE.L D4,FPIAR
    FMOVE.L FPIAR,D5

    ; Register-list transfers and paired destinations.
    FMOVEM FP0/FP2,(A0)
    FMOVEM FP4/FP6,-(A1)
    FMOVEM (A0)+,FP1/FP3
    FMOVEM (A1)+,FP5/FP7
    FSINCOS FP0,.pair(FP6,FP7)

    ; Register-aware unary and binary forms.
    FADD FP1,FP2
    FMOD FP3,FP4
    FGETMAN FP5,FP6
    FTRAPGT.W #1
    RTS

.end
