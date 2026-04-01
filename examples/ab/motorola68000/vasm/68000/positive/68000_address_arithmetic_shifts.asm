; Motorola 68000 address arithmetic and shift/rotate smoke fixture

start:
    MOVEQ #1,D0
    MOVEQ #2,D1
    ADDA.L (A0),A1
    SUBA.W ($1234).W,A2
    ASR.B #1,D0
    LSL.W #1,D1
    LSR.W #1,D2
    ROL.W #1,D3
    RTS

