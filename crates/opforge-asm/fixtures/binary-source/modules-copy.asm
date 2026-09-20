.module demo.main
.cpu m6502
.org $1000
entry
 jsr demo.copy.run
 jsr demo.fold.run
 rts
.endmodule
.module demo.copy
source = $20
destination = $30
count = 4
.pub
run .block
.priv
size = count
 ldx #0
loop lda source,x
 sta destination,x
 inx
 cpx #size
 bne loop
 rts
.endblock
loopAddress = demo.copy.run.loop
.endmodule
.module demo.fold
destination = $30
count = 4
.pub
run .block
.priv
value = mask & $ff
mask = (1 << bits)-1
bits = 4
 ldx #0
 lda #0
loop eor destination,x
 inx
 cpx #count
 bne loop
 and #value
 rts
.bend
loopAddress = demo.fold.run.loop
result = demo.fold.run.value
.endmodule
.module demo.addresses
.word demo.copy.loopAddress,demo.fold.loopAddress,demo.fold.result
.endmodule
.end
