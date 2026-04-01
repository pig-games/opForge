; Motorola 68040 MOVEC MMU/control-register smoke fixture
.cpu 68040
.org $1000

start:
    MOVEC TC,D0
    MOVEC MMUSR,D1
    MOVEC URP,A0
    MOVEC DTT1,A1
    MOVEC A2,SRP
    RTS

.end
