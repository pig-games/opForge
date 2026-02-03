; Demonstrates sections.

.module main
    .section data
    .byte 1, 2
    .endsection
    .org 1000h
    .dsection data
.endmodule
