; Baseline 68000 bit-modify instructions do not permit PC-relative destinations.
.cpu 68000
.org $1000

    BCHG #1,4(PC)

.end
