; Motorola 68020 positive coverage: DIVSL/DIVUL aliases match Motorola long-divide encoding

start:
    DIVSL.L (A0),D2:D3
    DIVUL.L (A0),D2:D3
    RTS
