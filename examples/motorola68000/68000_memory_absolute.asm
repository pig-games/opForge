; Baseline memory sources, absolute destinations and pointer immediates.
.module memory_absolute
    .cpu 68000
    .org $1000

    move.l (a0),$1000
    move.w (a1),$7fff
    move.b (a2)+,$8000
    move.l -(a3),$123456
    move.l (a0),cell
    cmpa.l #cell,a0
    adda.l #cell,a1
    suba.l #cell,a2
    rts
cell .long 0

.endmodule
.end
