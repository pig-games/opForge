; Copy one 256-byte page, with backward branch and absolute indexed operands.
; Purpose-written migration case; assembles the routine, does not execute it.
        .cpu m6502
SourcePage = $2000
PageBytes = 16*16
DestinationPage = SourcePage+PageBytes
        .org $1000
copyPage:
        ldx #0
copyByte:
        lda SourcePage,x
        sta DestinationPage,x
        inx
        bne copyByte
        rts
copyEnd:
CodeBytes = copyEnd-copyPage
        .word SourcePage,DestinationPage,PageBytes,CodeBytes
        .end
