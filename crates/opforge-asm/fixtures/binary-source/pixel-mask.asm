; Update a two-bit pixel inside a packed byte using symbolic configuration.
        .cpu m6502
FieldMask = ((1 << BitsPerPixel)-1) << PixelShift
KeepMask = (~FieldMask) & $ff
EncodedColor = (ColorValue & ((1 << BitsPerPixel)-1)) << PixelShift
PixelShift = PixelSlot * BitsPerPixel
BitsPerPixel = 2
PixelSlot = 2
ColorValue = 3
BufferAddress = BaseAddress+Offset
Offset = 4
BaseAddress = $20
        .org $1000
updatePixel:
        lda BufferAddress
        and #KeepMask
        ora #EncodedColor
        sta BufferAddress
        rts
routineEnd:
        .word routineEnd-updatePixel
        .byte FieldMask,KeepMask,EncodedColor
        .end
