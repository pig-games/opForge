; AB documented-divergence fixture: bounded 68080 AMMX core

    .apollo on

start:
    LOAD (A0),E0
    PADD.B E0,E1,E2
    PSUB.W E8,E9,E10
    PACK3216 D0,D1,E2
    VPERM #$3210AB78,D0,E1,E6
    RTS
