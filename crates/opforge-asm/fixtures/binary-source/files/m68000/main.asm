.module files.main
.word work.run,consumer.entry
.use files.consumer
.use files.library
.use files.library as work
 moveq #library.value,d1
 rts
.endmodule
.end
