; Motorola 68000 immediate and unary-operation smoke fixture
.cpu 68000
.org $1000

start:
    ORI.B #$12,D0
    ANDI.W #$1234,(A0)
    ADDI.W #1,4(A0)
    SUBI.B #1,(A1)+
    EORI.L #$12345678,D1
    CMPI.W #$1234,($1234).W
    CMPI.W #1,4(PC)
    CLR.W D2
    NEG.B (A0)
    NOT.L D3
    TST.W 4(A3)
    RTS

.end
