; Qualified module import with logical section map and reference-driven reachability.

.module opasm.amigaos.engine
    .cpu 68000
    .pub
    .section code, kind=code, logical
sessionPass:
    rts
unusedPass:
    rts
    .endsection
.endmodule

.module app.main
    .cpu 68000
    .region rom, $1000, $10ff
    .use opasm.amigaos.engine as engine map { code -> app_code }
    .section app_code, kind=code
start:
    jsr engine.sessionPass
    rts
    .endsection
    .place app_code in rom
.endmodule

.end
