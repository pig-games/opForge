.module engine
.cpu 68020
.use tables
.include "layout.inc"
.pub
.section code,kind=code
update .block
    MOVEQ #STRIDE,D1
    MOVE.W 4(A0),D2
    ADD.W D0,D2
    EOR.W D2,D3
    MOVE.W D3,8(A0)
    RTS
.bend
unused .block
    MOVEQ #$7f,D7
    RTS
.bend
.endsection
.endmodule
