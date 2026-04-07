; AB fixture: 68080 MOVE2/MOVZ2 register pair moves

    .apollo on

start:
    MOVE2.W (A0),.pair(D2,D3)
    MOVZ2.B (A0),.pair(D4,D5)
    RTS
