; Motorola 68020 external FPU instruction catalog
.cpu 68020
.fpu 68881
.org $1000

start:
    ; Core move and arithmetic surface.
    FNOP
    FMOVE FP0,FP1
    FMOVECR #11,FP0
    FMOVE.S (A0),FP2
    FMOVE.D (A0)+,FP3
    FMOVE.X -(A1),FP4
    FMOVE.P 8(A2),FP5
    FADD FP1,FP2
    FADD.S (A0),FP3
    FADD.D (A0)+,FP4
    FADD.X -(A1),FP5
    FADD.P 8(A2),FP6
    FSUB FP2,FP3
    FMUL FP3,FP4
    FDIV FP4,FP5
    FSGLDIV FP1,FP2
    FSGLMUL FP3,FP4
    FSQRT FP5
    FABS FP6
    FNEG FP7
    FINT FP0
    FINTRZ FP1
    FCMP FP1,FP2
    FTST FP3

    ; Conditional, trap, and state-control surface.
    FBEQ after_fb
after_fb:
    FDBNE D0,after_fdb
after_fdb:
    FSNE D1
    FTRAPGT.W #1
    FSAVE (A0)
    FRESTORE (A0)+

    ; Trigonometric and hyperbolic surface.
    FSIN FP0,FP1
    FCOS.W (A1),FP2
    FSINCOS FP3,.pair(FP4,FP5)
    FTAN FP5,FP6
    FASIN FP6,FP7
    FACOS FP7,FP0
    FATAN FP0,FP1
    FSINH FP1,FP2
    FCOSH FP2,FP3
    FTANH FP3,FP4
    FATANH FP4,FP5

    ; Extended math surface.
    FETOX FP5,FP6
    FETOXM1 FP6,FP7
    FTENTOX FP7,FP0
    FTWOTOX FP0,FP1
    FLOGN FP1,FP2
    FLOGNP1 FP2,FP3
    FLOG10 FP3,FP4
    FLOG2 FP4,FP5
    FGETEXP FP5,FP6
    FGETMAN FP6,FP7
    FSCALE FP7,FP0
    FMOD FP0,FP1
    FREM FP1,FP2
    RTS

.end
