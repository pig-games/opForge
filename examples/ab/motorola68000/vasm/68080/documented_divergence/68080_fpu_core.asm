; AB documented-divergence fixture: bounded 68080 FPU core and extensions

    .fpu 68080

start:
    FMOVE FP0,FP1
    FMOVECR #11,FP0
    FMOVEM FP0/FP2,(A0)
    FMOVEM (A0)+,FP1/FP3
    FMOVE.D D0,FP0
    FMOVE.D FP1,D1
    FSIN FP0,FP1
    FETOX FP0,FP1
    FLOADI.D D0,FP0
    FSTOREI.X FP1,D1
    FMOVERZ.L FP0,D0
    FMOVEURZ.W FP1,D1
    FBEQ after_fb
after_fb:
    FDBNE.L D0,after_fdb_long
after_fdb_long:
    FDBNE D0,after_fdb
after_fdb:
    RTS
