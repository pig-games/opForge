
start:
    ROXL.B #1,D0
    ROXR.W D1,D2
    ASL (A0)
    LSR.W ($1234).W
    ROXL 4(A1)
    ROR.W -(A2)
    RTS
