.module tables
.cpu 68020
.include "layout.inc"
.pub
.section data,kind=data
values .block
.for item in {0,1,2,3,4,5,6,7}
    .word item*STRIDE,LIMIT-item
.endfor
    .long $12345678,$89abcdef
.bend
.endsection
.endmodule
