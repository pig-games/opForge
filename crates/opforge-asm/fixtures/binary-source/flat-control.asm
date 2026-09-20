.cpu m68000
.org $1000
prefix = $3a00
encode:
encodeMask = (1 << encodeBits)-1
encodeBits = 8
 andi.l #encodeMask,d0
 ori.l #prefix,d0
 rts
encodeDone:
clear:
clearMask = $ffff-encodeMask
 andi.l #clearMask,d0
 eori.l #prefix,d0
 rts
clearDone:
.word encode,clear,encodeDone-encode,clearDone-clear,encodeMask,clearMask
.end
