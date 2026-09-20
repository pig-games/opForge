.cpu m6502
.org $1000
source = $20
destination = $30
count = 4
entry:
 jsr copy
 jsr fold
 rts
copy:
copySize = count
 ldx #0
copyLoop:
 lda source,x
 sta destination,x
 inx
 cpx #copySize
 bne copyLoop
 rts
fold:
foldValue = foldMask & $ff
foldMask = (1 << foldBits)-1
foldBits = 4
 ldx #0
 lda #0
foldLoop:
 eor destination,x
 inx
 cpx #count
 bne foldLoop
 and #foldValue
 rts
.word copyLoop,foldLoop,foldValue
.end
