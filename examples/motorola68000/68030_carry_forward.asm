; Motorola 68030 carry-forward smoke fixture
.cpu 68030
.org $1000

start:
    BRA.L far_target
    LINK.L A6,#-8
    EXTB.L D0
    MULU.L (A0),D1
    MOVEC CACR,D0
    CAS.W D0,D1,(A0)
    CAS2.W D0:D1,D2:D3,(A0):(A1)
    PACK D0,D1,#1
    TRAPNE
    CALLM #5,($1234).W
far_target:
    RTS

.end
