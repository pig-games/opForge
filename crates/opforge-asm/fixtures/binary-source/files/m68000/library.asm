.module files.library
.cpu m68000
.org $1000
.pub
value = 7
run .block
 andi.l #255,d0
 ori.l #$3a00,d0
 rts
.bend
.endmodule
.end
