; Motorola 68000 arithmetic and size smoke fixture
.cpu 68000
.org $1000

start:
    MOVEQ #1,D0
    MOVEQ #2,D1
    ADD.W #1,D0
    SUB.L D1,D0
    CMP.W (A0),D1
    AND.B (A0),D0
    OR.L #$12345678,D2
    EOR.W D0,(A1)
    ADDQ.W #8,D0
    SUBQ.L #1,(A0)
    ASL.B #1,D0
    ROR.W #1,D3
    RTS

.end
