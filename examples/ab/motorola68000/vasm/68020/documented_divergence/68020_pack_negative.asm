; Motorola 68020 documented divergence: vasm rejects negative PACK immediates

start:
    PACK D0,D1,#-1
    RTS
