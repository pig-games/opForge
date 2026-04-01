; Motorola 68020 external FPU operand and addressing-mode surface
.cpu 68020
.fpu 68881
.org $1000

start:
    ; Register-direct and sized integer / FP transfers.
    FMOVE FP0,FP1
    FMOVE.B D0,FP2
    FMOVE.W FP2,D1
    FMOVE.L D2,FPCR
    FMOVE.L FPSR,D3
    FMOVE.L FPIAR,D4

    ; Native floating-point format transfers.
    FMOVE.S (A0),FP3
    FMOVE.D (A0)+,FP4
    FMOVE.X -(A1),FP5
    FMOVE.P 8(A2),FP6
    FMOVE.S FP0,(A0)
    FMOVE.D FP1,8(A0)
    FMOVE.X FP2,-(A1)
    FMOVE.P FP3,($123456).L

    ; Memory-source addressing families for sized FPU operations.
    FMOVE.W (A0),FP3
    FMOVE.L (A0)+,FP4
    FMOVE.W -(A1),FP5
    FMOVE.L 8(A2),FP6
    FMOVE.W 0(A3,D5.W),FP7
    FMOVE.W ($1234).W,FP0
    FMOVE.L ($123456).L,FP1
    FMOVE.W 8(PC),FP2

    ; Representative arithmetic and transcendental EA forms.
    FADD FP1,FP2
    FADD.S (A0),FP0
    FADD.D (A0)+,FP1
    FADD.X -(A1),FP2
    FADD.P 8(A2),FP3
    FCOS.W (A0),FP3
    FSINCOS FP3,.pair(FP4,FP5)

    ; State save / restore and register-list transfers.
    FMOVEM FP0/FP2,(A4)
    FMOVEM (A4)+,FP1/FP3
    FMOVEM FP0/FP2,-(A5)
    FSAVE (A6)
    FRESTORE (A6)+
    RTS

.end
