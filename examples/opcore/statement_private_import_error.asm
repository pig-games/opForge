; Test: importing a private .statement across modules should fail.

.module statement.private.app
    .cpu 65c02
    .use statement.private.export.lib (LIBVAL, PUSHB)
    .org $0000
    lda #LIBVAL
    PUSHB $AA
    brk
.endmodule

.end
