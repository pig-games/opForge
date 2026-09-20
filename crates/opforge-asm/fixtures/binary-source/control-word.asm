; Keep the payload byte in D0 and add the configured version/type fields.
        .cpu m68000
HeaderPrefix = (Version << 12) | (Kind << 8)
PayloadMask = (1 << PayloadBits)-1
Version = 3
Kind = $a
PayloadBits = 8
        .org $1000
packControl:
        andi.l #PayloadMask,d0
        ori.l #HeaderPrefix,d0
        rts
routineEnd:
        .word HeaderPrefix,PayloadMask,routineEnd-packControl
        .end
