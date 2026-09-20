.cpu m68000
.org $1000
prefix = $3a00
words .namespace
encode .block
mask = (1 << bits)-1
bits = 8
 andi.l #mask,d0
 ori.l #prefix,d0
 rts
done
.endblock
clear .block
mask = $ffff-words.encode.mask
 andi.l #mask,d0
 eori.l #prefix,d0
 rts
done
.bend
.endnamespace
.namespace words
.word encode,clear,words.encode.done-encode,words.clear.done-clear,words.encode.mask,words.clear.mask
.endn
.end
