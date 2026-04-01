; Motorola 68010 delta smoke fixture
.cpu 68010
.org $1000

start:
    BKPT #3
    MOVE CCR,D0
    MOVE CCR,($1234).W
    MOVEC SFC,D0
    MOVEC D1,DFC
    MOVEC VBR,A2
    MOVES.W D0,(A0)
    MOVES.L (A1),A2
    RTD #4

.end
