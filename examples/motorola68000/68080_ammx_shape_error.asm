; Motorola 68080 AMMX shape/alignment error case

        .cpu 68080
        .org $1000

        .apollo on

start:
        PADD.B A0,E1,E2
        BFLYB D0,D1,.pair(D1,D2)
        MINTERM D1-D4,D4
        TRANSLO D0-D2,.pair(D4,D5)

        .end
