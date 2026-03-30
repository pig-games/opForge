; Motorola 68040 integrated FPU surface fixture
.cpu 68040
.fpu 68040
.org $1000

start:
    ; Integrated move, register, and control-register surface.
    FMOVE FP0,FP1
    FMOVEM FP0/FP2,(A0)
    FMOVEM (A0)+,FP1/FP3
    FMOVE.L D0,FPCR
    FMOVE.L FPSR,D1
    FMOVE.L FPIAR,D2

    ; Core arithmetic and tests.
    FADD FP0,FP1
    FSUB FP1,FP2
    FMUL FP2,FP3
    FDIV FP3,FP4
    FSQRT FP4
    FABS FP5
    FNEG FP6
    FINT FP7
    FINTRZ FP0
    FCMP FP0,FP1
    FTST FP1

    ; Conditional and state-management surface.
    FBEQ after_fb
after_fb:
    FDBNE D0,after_fdb
after_fdb:
    FSNE D3
    FTRAPGT.W #1
    FSAVE (A1)
    FRESTORE (A1)+

    RTS

.end
