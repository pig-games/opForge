; Basic Motorola 68000 move/address smoke fixture

start:
    MOVEQ #1,D0
    MOVE.W #$1234,D1
    MOVE.L D1,(A0)
    MOVEA.L ($001234).L,A1
    RTS

