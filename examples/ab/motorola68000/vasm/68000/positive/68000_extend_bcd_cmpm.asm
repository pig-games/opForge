
start:
    ADDX.B D0,D1
    ADDX.W -(A0),-(A1)
    SUBX.L D2,D3
    ABCD D4,D5
    SBCD -(A2),-(A3)
    CMPM.W (A4)+,(A5)+
    RTS
