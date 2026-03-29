; Motorola 68000 register exchange smoke fixture
.cpu 68000
.org $1000

start:
    EXG D0,D1
    EXG A2,A3
    EXG D2,A3
    RTS

.end
