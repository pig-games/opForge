; Return 0 if unsigned D0 is in the inclusive range, otherwise -1.
; D0 is overwritten. Purpose-written migration case; no routine-execution claim.
        .cpu m68000
Minimum = 32
Maximum = Minimum+95-1
Invalid = -1
        .org $1000
checkRange:
        sub.l #Minimum,d0
        cmp.l #Maximum-Minimum,d0
        bhi.s outside
        moveq #0,d0
        rts
outside:
        moveq #Invalid,d0
        rts
checkEnd:
CodeBytes = checkEnd-checkRange
        .word Minimum,Maximum,CodeBytes
        .long Invalid
        .end
