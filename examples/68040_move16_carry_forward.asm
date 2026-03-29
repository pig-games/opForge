; Motorola 68040 carry-forward and MOVE16 smoke fixture
.cpu 68040
.org $1000

start:
    BRA.L far_target
    LINK.L A6,#-8
    EXTB.L D0
    MULU.L (A0),D1
    MOVEC CACR,D0
    CAS.W D0,D1,(A0)
    CAS2.W D0:D1,D2:D3,(A0):(A1)
    CHK2.W ($1234).W,D0
    PACK D0,D1,#1
    TRAPNE
    MOVE16 (A0)+,(A1)+
    MOVE16 ($1234).L,(A1)
far_target:
    RTS

.end
