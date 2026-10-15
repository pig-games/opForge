.module main
.cpu 68020
.use engine
.use device
.use tables
.include "layout.inc"
.region ram, $1000, $ffff
.section code,kind=code
entry .block
    MOVEQ #LIMIT,D0
    LEA tables.values,A0
    JSR engine.update
    JSR device.flush
    RTS
.bend
.long engine.update,device.flush,tables.values
.endsection
.place code in ram
.place data in ram
.output "build/out.bin",format=bin,sections=code,data
.output "build/out.hunk",format=hunk,sections=code,data
.endmodule
