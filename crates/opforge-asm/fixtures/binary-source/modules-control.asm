.module masks
.cpu m68000
.org $1000
.pub
prefix = $3a00
.endmodule
.module masks.encode
bits = 8
.pub
run .block
.priv
mask = (1 << bits)-1
 andi.l #mask,d0
 ori.l #prefix,d0
 rts
done
.endblock
size = masks.encode.run.done-run
maskValue = masks.encode.run.mask
.endmodule
.module masks.clear
.pub
run .block
.priv
mask = $ffff-masks.encode.maskValue
 andi.l #mask,d0
 eori.l #prefix,d0
 rts
done
.bend
size = masks.clear.run.done-run
maskValue = masks.clear.run.mask
.endmodule
.module output
.word masks.encode.run,masks.clear.run,masks.encode.size,masks.clear.size,masks.encode.maskValue,masks.clear.maskValue
.endmodule
.end
