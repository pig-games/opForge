; Module imports

.module alpha
    .pub
VALUE   .const 1
OTHER   .const 2
    .byte VALUE
    .byte OTHER
.endmodule

.module beta
    .use alpha as A
    .use alpha (VALUE as V)
    .byte V
    .byte A.OTHER
.endmodule

.end
