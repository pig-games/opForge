; AB fixture: 68080 MOVS/MOVZ sign- and zero-extend moves

    .apollo on

start:
    MOVS.B D0,D1
    MOVZ.W (A0),D2
    RTS
