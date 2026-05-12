; Native AmigaOS 68020-baseline package-backed tokenizer runtime scaffold.
;
; This first slice owns only the executable glue and the tokenizer-only native
; ABI boundary. Package loading, pipeline resolution, and tokenizer execution
; are deferred to later modules.

	.module main
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi (AbiMarker, WireContractMarker, ENTRY_ORD_INIT)
	.use tkpkg.amigaos.buffers (ControlBlockV1)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)

	.section entry, kind=code
	.pub

; Minimal executable entry used to keep the tkpkg runtime linkable as a hunk.
start	.block
	lea AbiMarker, a0
	tst.b (a0)
	lea WireContractMarker, a0
	tst.b (a0)
	clr.l d0
	rts
	.bend  ; start

; Forward an already prepared request to the shared tkpkg service dispatcher.
tkpkgEntryDispatchV1	.block
	jsr tkpkgServiceDispatchV1
	rts
	.bend  ; tkpkgEntryDispatchV1

; Initialize the shared control block through the public service ordinal.
tkpkgEntryBootstrapV1	.block
	lea ControlBlockV1, a0
	moveq #ENTRY_ORD_INIT, d0
	jsr tkpkgServiceDispatchV1
	rts
	.bend  ; tkpkgEntryBootstrapV1

	.endsection
	.output "build/tkpkg_entry", format=hunk, sections=entry, code, data, bss
	.endmodule
