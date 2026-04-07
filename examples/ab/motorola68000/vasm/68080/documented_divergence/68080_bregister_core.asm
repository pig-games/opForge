; AB documented-divergence fixture: representative 68080 B-register core

start:
    ADDQ.L #1,B0
    SUBQ.L #8,B7
    CMP.L B2,D3
    LEA 1(A0),B1
    LEA (B2),A3
    MOVE.L B0,D1
    MOVE.L D0,B4
    MOVEA.L D0,B5
    RTS