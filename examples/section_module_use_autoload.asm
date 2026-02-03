; Section data across modules with autoload

.module example.section.app
    .use example.section.lib (LIBVAL)
    .section data
    .byte 1, LIBVAL
    .endsection
    .org 1000h
    .dsection data
.endmodule

.end
