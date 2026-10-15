.module main
.cpu 68020
.use draw
.use mix
.region ram, $1000, $7ffff
.section code,kind=code
entry .block
    LEA mix.samples,A0
    MOVEQ #0,D0
    JSR draw.render
    JSR mix.process
    RTS
.bend
.long draw.render,mix.process,mix.samples
.endsection
.place code in ram
.place data in ram
.output "build/out.bin",format=bin,sections=code,data
.output "build/out.hunk",format=hunk,sections=code,data
.endmodule
