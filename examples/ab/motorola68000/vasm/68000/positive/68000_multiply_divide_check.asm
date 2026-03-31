; Motorola 68000 multiply, divide, and bounds-check smoke fixture

start:
    CHK ($1234).W,D0
    MULU.W (A0),D1
    MULS #$00FF,D2
    DIVU ($123456).L,D3
    DIVS.W 4(PC),D4
    RTS

