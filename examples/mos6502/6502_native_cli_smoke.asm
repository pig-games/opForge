; Native AmigaOS CLI 6502 small-assembly contract.
; This is the first flat-binary target for the package-backed native path.

        .cpu 6502
        .org $0800
start   lda #$42
        sta $0200
done    jmp done
