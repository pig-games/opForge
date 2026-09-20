.module files.library
.cpu m6502
.org $1000
source = $20
destination = $21
.pub
mask = 15
run .block
 ldx #8
loop lsr source
 rol destination
 dex
 bne loop
 rts
.bend
.endmodule
.end
