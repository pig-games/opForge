; Motorola 68080 FPU surface fixture

        .cpu 68080
        .fpu 68080
        .org $1000

start:
        FNOP
        FMOVE FP0,FP1
        FMOVECR #11,FP0
        FMOVEM FP0/FP2,(A0)
        FMOVEM (A0)+,FP1/FP3
        FMOVE.L D0,FPCR
        FMOVE.L FPSR,D1
        FMOVE.D D0,FP0
        FMOVE.D FP1,D1

        FADD FP1,FP2
        FMUL.W E4,FP3,E5
        FADD.W D0,E1,E2
        FCMP.W E4,FP3,E5
        FSCALE E4,FP3,E5
        FREM E4,FP3,E5
        FSIN FP0,FP1
        FSINCOS FP0,.pair(FP1,FP2)
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
        FSNE D3

        RTS

        .end
