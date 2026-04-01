; Motorola 68020 later integer-family smoke fixture
.cpu 68020
.org $1000

start:
    MOVEC CACR,D0
    MOVEC D1,MSP
    LINK.L A6,#-8
    EXTB.L D2
    MULU.L (A0),D1
    MULS.L ($1234).W,D2
    DIVS.L (A0),D1
    DIVU.L (A0),D1
    DIVS.L (A0),D2:D3
    DIVU.L (A0),D2:D3
    DIVSL.L (A0),D2:D3
    DIVUL.L (A0),D2:D3
    BRA.L far_target
    BSR.L helper
    BNE.L far_target
    CAS.W D0,D1,(A0)
    CAS2.L D0:D1,D2:D3,(A0):(A1)
    CHK2.L ($1234).W,A1
    CMP2.B ($1234).W,D0
    PACK D0,D1,#-1
    UNPK -(A0),-(A1),#1
    TRAPNE
    TRAPGT.W #$1234
    CALLM #5,($1234).W
    RTM A3
helper:
    RTS
far_target:
    NOP
    RTS

.end
