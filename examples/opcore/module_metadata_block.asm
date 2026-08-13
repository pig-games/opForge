; Demonstrates root-module metadata block directives.

.module main
    .meta
        .name "Metadata Block Demo"
        .version "2.0.0"
        .output
            .name "meta-block-demo"
            .m6502
                .name "meta-block-demo-6502"
            .endm6502
        .endoutput
    .endmeta

    .org 0000h
START
    .byte 01h, 02h, 03h, 04h
.endmodule
