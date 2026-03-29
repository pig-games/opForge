; Motorola 68000 compare and operand-state smoke fixture
.cpu 68000
.org $1000

start:
    CMPA.W ($1234).W,A0
    CMPA.L ($123456).L,A1
    NEGX.B D0
    NEGX.W (A0)
    NBCD D1
    NBCD -(A2)
    TAS D2
    TAS (A3)
    RTS

.end
