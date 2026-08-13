; grouping.asm - Test generic grouping functionality
; This ensures that (1+2) is evaluated as math, not indirect addressing
    .cpu 65c02
    .org $100
    lda #(1+2)
    adc #(5*2)
