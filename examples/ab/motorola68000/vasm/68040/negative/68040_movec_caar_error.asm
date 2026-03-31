; Motorola 68040 rejects MOVEC CAAR even though earlier later-family CPUs allow it.

start:
    MOVEC CAAR,D0
    RTS
