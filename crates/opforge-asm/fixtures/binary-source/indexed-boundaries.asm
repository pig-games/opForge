; Package-owned index-register predicates and narrow/wide address selection.
        .cpu m6502
        .org $1000
        lda 0,x
        lda 255,x
        lda 256,x
        lda 65535,x
        ldx 0,y
        ldx 255,y
        ldx 256,y
        ldx 65535,y
        lda $20,y
        sta $2000,x
        sta $2100,y
        .end
