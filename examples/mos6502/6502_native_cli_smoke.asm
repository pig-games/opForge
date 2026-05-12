; Native AmigaOS CLI 6502 small-assembly contract.
; This is the first flat-binary target for the package-backed native path.

        .cpu 6502
        .org $0800
start   lda #$42
        sta $20
        lda $20,x
        sta $0200
        lda $0200,x
        lda $0200,y
done    jmp done
