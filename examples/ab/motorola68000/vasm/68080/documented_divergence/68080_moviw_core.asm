; AB fixture: 68080 MOVIW in-range immediates

    .apollo on

start:
    MOVIW.L #$8123,D0
    MOVIW.L #-1,(A0)
    RTS
