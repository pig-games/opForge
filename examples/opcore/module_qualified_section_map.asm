; Qualified module import with selected root, logical section map, and reachability.

.module opasm.amigaos.engine
    .pub
    .section code, kind=code, logical
sessionPass: .byte $11
unusedPass:  .byte $22
    .endsection
.endmodule

.module app.main
    .region rom, $1000, $10ff
    .section app_code, kind=code
start: .byte $aa
       .byte engine.sessionPass
    .endsection
    .place app_code in rom
    .use opasm.amigaos.engine (sessionPass) as engine map { code -> app_code }
.endmodule

.end
