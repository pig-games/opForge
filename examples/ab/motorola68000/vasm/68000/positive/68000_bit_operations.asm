; Motorola 68000 bit-operation smoke fixture

start:
    BTST #3,D0
    BTST D1,(A0)
    BTST #1,4(A1)
    BTST #1,4(PC)
    BCHG #5,D2
    BCHG D3,($1234).W
    BCLR #1,(A1)
    BCLR D4,D5
    BSET #7,($123456).L
    BSET D6,4(A2)
    RTS

