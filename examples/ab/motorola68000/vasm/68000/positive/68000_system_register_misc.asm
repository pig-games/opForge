; Motorola 68000 system and register-utility smoke fixture

start:
    LINK A6,#-8
    MOVE SR,D0
    MOVE ($1234).W,CCR
    MOVE #$2700,SR
    MOVE USP,A1
    MOVE A2,USP
    ANDI #$1F,CCR
    ORI #$2700,SR
    EORI #$0F,CCR
    SWAP D0
    EXT.W D1
    EXT.L D2
    TRAP #15
    NOP
    STOP #$2700
    UNLK A6
    RESET
    RTE
    RTR
    TRAPV
    ILLEGAL

