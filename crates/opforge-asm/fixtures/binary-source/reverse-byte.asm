; Reverse the eight bits from SourceByte into DestinationByte.
; SourceByte and X are scratch; eight rotations overwrite the old destination.
; Purpose-written migration case; assembles the routine, does not execute it.
        .cpu m6502
SourceByte = $20
DestinationByte = SourceByte+1
BitCount = 2*4
        .org $1000
reverseByte:
        ldx #BitCount
nextBit:
        lsr SourceByte
        rol DestinationByte
        dex
        bne nextBit
        rts
reverseEnd:
CodeBytes = reverseEnd-reverseByte
        .word SourceByte,DestinationByte,CodeBytes
        .end
