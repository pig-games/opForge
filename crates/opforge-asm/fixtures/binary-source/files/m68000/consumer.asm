.module files.consumer
.use files.library as work
.pub
entry
 moveq #work.value,d0
 rts
.endmodule
.end
