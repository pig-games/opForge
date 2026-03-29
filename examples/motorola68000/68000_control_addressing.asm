; Motorola 68000 control-transfer and indexed-addressing smoke fixture
.cpu 68000
.org $1000

start:
    LEA 4(A0,D1.W),A1
    PEA 4(A0)
    BSR.W helper
    JMP 4(PC)
helper:
    JSR (A0)
    ADDQ.W #1,D0
    RTS

.end
