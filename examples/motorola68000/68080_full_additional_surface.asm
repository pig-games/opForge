; Motorola 68080 bounded additional surface (integer + AMMX + FPU)

        .cpu 68080
        .apollo on
        .org $1000

start:
        MOVEQ #1,D0
        CLR.Q D2
        ADDIW.L #$1020,D0
        CMPIW.L #$1122,(A0)
        EXTUB.L E8
        EXTUW.L D1
        PERM #$0ABC,D0,E1
        MOVEC PCR,D0
        MOVEC D1,MWR
        MOVE SR,E0
        MOVE16 ($1234).L,(A1)
        DBRA.L D1,branchExtLoop
        BRA.S+ branchExtBra
        BSR.S+ branchExtBsr
        BNE.S+ branchExtBcc

        .org $10B0
branchExtLoop:
        NOP
branchExtBra:
        NOP
branchExtBsr:
        RTS
branchExtBcc:
        NOP

        MOVIW.L #$5566,D1
        MOV3Q #7,D2
        LOAD (A0),E0
        PADD.B E0,E1,E2
        PADDB D0,D1,D2
        PADDUSB D0,D1,D2
        PSUB.W E8,E9,E10
        PSUBUSB D0,D1,D2
        PAVGB D0,D1,D2
        PACK3216 D0,D1,E2
        VPERM #$3210AB78,D0,E1,E6

        .fpu 68080
        FMOVE FP0,FP1
        FMOVECR #11,FP0
        FSIN FP0,FP1
        FETOX FP0,FP1

        .end
