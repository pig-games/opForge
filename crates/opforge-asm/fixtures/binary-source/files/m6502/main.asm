.module files.main
.word work.run,consumer.entry
.use files.consumer
.use files.library
.use files.library as work
 lda #library.mask
 rts
.endmodule
.end
