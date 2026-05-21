; Motorola 68000 qualified imported label call smoke fixture
.module demo.routines
.cpu 68000
.org $1010
.pub
drawSprite:
    RTS
.endmodule

.module demo.main
.cpu 68000
.use demo.routines
.org $1000

start:
    JSR demo.routines.drawSprite
    RTS

.endmodule
.end
