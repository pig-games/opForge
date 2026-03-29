; Motorola 68040 rejects MOVEC CAAR even though earlier later-family CPUs allow it.
.cpu 68040
.org $1000

    MOVEC CAAR,D0

.end
