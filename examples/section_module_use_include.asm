; Section data across modules with include

.include "section_module_use_lib.inc"

.module example.section.app
    .use example.section.include.lib (LIBVAL)
    .section data
    .byte 1, LIBVAL
    .endsection
    .org 1000h
    .dsection data
.endmodule

.end
