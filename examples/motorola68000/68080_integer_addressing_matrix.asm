; Motorola 68080 integer additional forms addressing matrix

        .cpu 68080
        .org $1000

start:
        MOVEQ #1,D0

        ADDIW.L #$8001,D0
        ADDIW.L #$2222,(A0)
        ADDIW.L #$3333,(A1)+
        ADDIW.L #$4444,-(A2)
        ADDIW.L #$5555,16(A3)
        ADDIW.L #$6666,0(A4,D1.W)
        ADDIW.L #$7777,($1234).W
        ADDIW.L #$0102,($123456).L

        CMPIW.L #$8001,D2
        CMPIW.L #$1020,(A5)
        CMPIW.L #$1122,12(A6)
        CMPIW.L #$5566,($2100).W
        CMPIW.L #$7F00,($00123456).L
        ; Generated BANK prefix coverage for all currently supported cases.
        MOVE SR,E0
        EXTUB.L E8
        EXTUW.L E16
        PERM #$0ABC,E0,D1
        PERM #$0ABC,D0,E1
        PERM #$0ABC,E8,E16

        MOVIW.L #$8123,D3
        MOVIW.L #$0101,(A0)
        MOVIW.L #$0202,($2200).W
        MOVIW.L #$0303,($00102200).L

        .apollo on
        MOV3Q #5,D4
        MOV3Q #-1,D5
        ADDQ.L #1,B0
        SUBQ.L #8,B7
        CMP.L B2,D3
        LEA 1(A0),B1
        LEA (B2),A3
        MOVE.L B0,D1
        MOVE.L D0,B4
        MOVEA.L D0,B5

        .end