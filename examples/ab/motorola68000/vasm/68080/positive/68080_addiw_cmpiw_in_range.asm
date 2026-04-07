; Motorola 68080 documented divergence: in-range ADDIW/CMPIW bytes differ from vasm

start:
    ADDIW.L #$8001,D0
    CMPIW.L #$8001,(A0)
    RTS
