.module files.consumer
.use files.library as work
.pub
entry
 jsr work.run
 lda #work.mask
 rts
.endmodule
.end
