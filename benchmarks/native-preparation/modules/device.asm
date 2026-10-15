.module device
.cpu 68020
.include "layout.inc"
.pub
.section code,kind=code
flush .block
    MOVEQ #LIMIT,D4
    MOVE.L D4,(A1)+
    ADDQ.L #4,A1
    MOVE.W 2(A0,D1.W),D5
    OR.W D5,D4
    RTS
.bend
.endsection
.endmodule
