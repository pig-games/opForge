.cpu m68000
.org $1000
prefix = $3a00
encode .block
mask = (1 << bits)-1
bits = 8
 andi.l #mask,d0
 ori.l #prefix,d0
 rts
done:
.endblock
clear .block
mask = $ffff-encode.mask
 andi.l #mask,d0
 eori.l #prefix,d0
 rts
done:
.bend
.word encode,clear,encode.done-encode,clear.done-clear,encode.mask,clear.mask
.end
