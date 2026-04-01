; Motorola 68030 carry-forward integer smoke fixture

start:
    BRA.L far_target
    EXTB.L D0
    MULU.L (A0),D1
    MOVEC CACR,D0
    CAS.W D0,D1,(A0)
    CAS2.W D0:D1,D2:D3,(A0):(A1)
    PACK D0,D1,#1
    TRAPNE
far_target:
    RTS
