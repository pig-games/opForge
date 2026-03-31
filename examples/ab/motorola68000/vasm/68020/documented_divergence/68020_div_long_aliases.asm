; Motorola 68020 documented divergence: DIVSL/DIVUL encodings currently differ from vasm

start:
    DIVSL.L (A0),D2:D3
    DIVUL.L (A0),D2:D3
    RTS
