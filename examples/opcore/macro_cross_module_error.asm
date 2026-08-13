; Test: cross-module macro visibility (expected error)
;
; .macro definitions follow the same import rules as other symbols.
; A macro defined in a library module is only visible to the
; importing module when explicitly listed in the .use import list.
;
; Here, only LIBVAL is imported — EMIT_PAIR is NOT imported, so
; invoking it produces an error.

.module macro.cross.app
    .cpu 65c02

    .use macro.export.lib (LIBVAL)

    .org $0000

    lda #LIBVAL         ; Works: LIBVAL is explicitly imported
    .EMIT_PAIR $AA, $BB ; ERROR: EMIT_PAIR not imported
    brk
.endmodule

.end
