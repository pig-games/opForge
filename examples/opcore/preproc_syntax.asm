; 65C02 preprocessor branch-selection coverage (no source .define).
; Preprocessor symbols are provided via -D/--define.

.cpu 65c02

.ifdef VAL
        lda #$41
.else
        lda #$42
.endif

.ifndef UNKNOWN
        sta $0200
.else
        stz $0200
.endif

.include "preproc_syntax.inc"
