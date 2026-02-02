; Module import without .include

.module example.autoload.app
    .use example.autoload.lib (LIBVAL)
    .byte LIBVAL
.endmodule

.end
