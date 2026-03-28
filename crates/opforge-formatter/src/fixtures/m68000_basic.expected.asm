.cpu 68000
start   lea 4(a0, d1.w), a1
        move.l d0, (a1)
        addq.w #1, d0
done    rts
