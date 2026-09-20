.cpu m6502
.org $1000
source = $20
destination = $30
count = 4
entry:
 jsr copy
 jsr fold
 rts
copy .block
size = count
 ldx #0
loop:
 lda source,x
 sta destination,x
 inx
 cpx #size
 bne loop
 rts
.endblock
fold .block
value = mask & $ff
mask = (1 << bits)-1
bits = 4
 ldx #0
 lda #0
loop:
 eor destination,x
 inx
 cpx #count
 bne loop
 and #value
 rts
.bend
.word copy.loop,fold.loop,fold.value
.end
