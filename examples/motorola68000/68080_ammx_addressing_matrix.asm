; Motorola 68080 full supported AMMX matrix for the current opForge slice

        .cpu 68080
        .org $1000

        .apollo on

start:
        LOAD (A0),E0
        LOAD (A7),E2
        LOAD.W #$1234,D1
        LOAD 6(A0),E3
        LOAD 4(B1),E4
        LOAD 0(A2,D3.W),E5
        LOAD 6(B3,D4.W),E6
        LOAD ammxTarget(PC),E7
        LOAD 0(PC,D5.W),E8
        LOAD $1234.W,E9
        LOAD $00123456.L,E10
        STORE E11,4(B2)
        STORE E12,0(A0,D1.W)
        STORE E13,$4321.W

        PADD.B E0,E1,E2
        PADD.W E4,E5,E6
        PSUB.B E8,E9,E10
        PSUB.W E12,E13,E14
        PMINSB D0,D1,D2
        PMINSW D0,D1,D2
        PMINUB D0,D1,D2
        PMINUW D0,D1,D2
        PMAXSB D0,D1,D2
        PMAXSW D0,D1,D2
        PMAXUB D0,D1,D2
        PMAXUW D0,D1,D2
        LSLQ D0,D1,D2
        LSRQ D0,D1,D2
        STOREM D0,D1,(A0)
        STOREM3 D0,#3,(A0)
        TEX8.512 (A0,(A1,A2)),D0
        TEX16.256 (A0,(A1,A2)),D1
        TEX24.64 (A0,(A1,A2))*D0,D2
        TEX.B (A0,A1*D3,A2),D4

        PACK3216 D0,D1,E2
        PACK3216 D2,D3,E10

        VPERM #$3210AB78,D0,E1,E6
        VPERM #$76543210,E8,E9,E10

ammxTarget:
        .byte 0

        .end
