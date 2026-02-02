; Module import error (private symbol)

.module alpha
    .priv
SECRET  .const 9
.endmodule

.module beta
    .use alpha (SECRET)
    .byte 0
.endmodule

.end
