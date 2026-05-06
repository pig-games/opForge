; Native AmigaOS 68020-baseline package-backed tokenizer runtime scaffold.
;
; This first slice owns only the executable glue and the tokenizer-only native
; ABI boundary. Package loading, pipeline resolution, and tokenizer execution
; are deferred to later modules.

        .module main
        .cpu 68020
        .pub
        .use tkpkg.amigaos.abi (abiMarker, wireContractMarker, ENTRY_ORD_INIT)
        .use tkpkg.amigaos.buffers (controlBlockV1)
        .use tkpkg.amigaos.service (tkpkg_service_dispatch_v1)

        .section entry, kind=code

start:
        LEA abiMarker, A0
        TST.B (A0)
        LEA wireContractMarker, A0
        TST.B (A0)
        CLR.L D0
        RTS

tkpkg_entry_dispatch_v1:
        JSR tkpkg_service_dispatch_v1
        RTS

tkpkg_entry_bootstrap_v1:
        LEA controlBlockV1, A0
        MOVEQ #ENTRY_ORD_INIT, D0
        JSR tkpkg_service_dispatch_v1
        RTS

        .endsection
        .output "build/tkpkg_entry", format=hunk, sections=entry, code, data, bss
        .endmodule