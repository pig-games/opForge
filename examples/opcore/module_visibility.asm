; Module visibility

.module alpha
    .pub
PUBLIC  .const 1
    .priv
PRIVATE .const 2
    .byte PUBLIC
    .byte PRIVATE
.endmodule

.module beta
    .pub
VALUE   .const 3
    .byte VALUE
.endmodule

.end
