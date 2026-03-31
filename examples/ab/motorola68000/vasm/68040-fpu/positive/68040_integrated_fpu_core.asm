; Motorola 68040 integrated FPU surface fixture

start:
    FMOVE FP0,FP1
    FADD FP0,FP1
    FSUB FP1,FP2
    FCMP FP0,FP1
    RTS
