; Motorola 68000 effective-address smoke fixture
.cpu 68000
.org $1000

start:
    MOVE.W #$1234,D0
    MOVE.L (A0)+,(A1)
    MOVE.W -(A2),D0
    MOVE.W 4(A3),D2
    MOVEA.L ($001234).L,A0
    PEA ($001234).L
    JSR (A0)
    JMP ($001234).L

.end
