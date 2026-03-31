; Motorola 68030 documented divergence: vasm rejects CALLM/RTM on its selected 68030 architecture mode

start:
    CALLM #5,($1234).W
    RTM A0
    RTS
