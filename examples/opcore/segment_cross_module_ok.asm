; Test: successful cross-module segment import on 65C02.
;
; When EMIT_BYTE is explicitly listed in the .use import list, the segment
; defined in the library module becomes available beside an imported const.

.module segment.cross.ok.app
    .cpu 65c02

    .use segment.export.lib (LIBVAL, EMIT_BYTE)

    .org $0800

    lda #LIBVAL         ; Use the exported const ($42)
    .EMIT_BYTE $AA      ; Invoke the exported segment
    .EMIT_BYTE $BB
    brk
.endmodule

.end
