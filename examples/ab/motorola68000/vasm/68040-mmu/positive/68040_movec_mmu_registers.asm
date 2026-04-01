; Motorola 68040 MMU MOVEC/control-register smoke fixture

start:
    MOVEC TC,D0
    MOVEC MMUSR,D1
    MOVEC URP,A0
    MOVEC DTT1,A1
    MOVEC A2,SRP
    RTS
