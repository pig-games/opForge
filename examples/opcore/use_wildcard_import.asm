; Wildcard selective import for all public symbols.

.module wildcard.import.lib
    .pub
VAL .const $11

EMITB .segment v
    .byte .v
.endsegment

PAIR .macro a, b
    .byte .a
    .byte .b
.endmacro

.statement PUSHB byte:v
    .byte .v
.endstatement
.endmodule

.module wildcard.import.app
    .cpu 65c02
    .use wildcard.import.lib (*)
    .org $0000

    lda #VAL
    .EMITB $22
    .PAIR $33, $44
    PUSHB $55
    brk
.endmodule

.end
