; Motorola 68030 narrow MMU and external FPU smoke fixture
.cpu 68030
.fpu 68881
.org $1000

start:
    PFLUSH #0,#0
    FSIN FP0,FP1
    FBEQ after_fb
after_fb:
    FETOX FP0,FP1
    RTS

.end
