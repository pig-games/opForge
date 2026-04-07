; Motorola 68080 positive coverage: vasm ignores the opForge-only .apollo directive

    .apollo on

start:
    MOVEQ #1,D0
    ADD.L D0,D1
    LEA ($1234).W,A0
    RTS
