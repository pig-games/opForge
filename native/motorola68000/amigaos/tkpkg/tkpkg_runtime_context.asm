; Versioned, read-only runtime context for tkpkg consumers.
;
; Item 5.7 introduces this ABI only. Consumer migration is deferred to Items
; 5.7.1 and 5.7.2.

	.module tkpkg.amigaos.runtime_context
	.cpu 68020
	.pub
	.use tkpkg.amigaos.engine_context_adapter as adapter

RUNTIME_CONTEXT_ABI_VERSION = 1
RUNTIME_CONTEXT_SYMBOL_FOUND = 0
RUNTIME_CONTEXT_SYMBOL_UNRESOLVED = 1
RUNTIME_CONTEXT_SYMBOL_ABSENT = 2
RUNTIME_CONTEXT_DIAGNOSTIC_CAPACITY = 96

	.section code, kind=code
	.pub

getAbiVersionV1	.block
	moveq #RUNTIME_CONTEXT_ABI_VERSION, d0
	rts
	.bend  ; getAbiVersionV1

getPassV1	.block
	jmp adapter.getPassV1
	.bend  ; getPassV1

getAddressV1	.block
	jmp adapter.getAddressV1
	.bend  ; getAddressV1

lookupSymbolV1	.block
	jmp adapter.lookupSymbolV1
	.bend  ; lookupSymbolV1

; Record one neutral diagnostic without exposing engine or service buffers.
; Inputs: D0 = code, A0 = message, D1 = message length, D2 = source span.
; Outputs: D0 = 0.
reportDiagnosticV1	.block
	move.l d0, LastDiagnosticCode
	move.l a0, LastDiagnosticMessage
	move.w d1, LastDiagnosticLength
	move.l d2, LastDiagnosticSpan
	moveq #0, d0
	rts
	.bend  ; reportDiagnosticV1

; Read the last neutral diagnostic record.
; Outputs: D0 = code, A0 = message, D1 = length, D2 = source span.
getLastDiagnosticV1	.block
	move.l LastDiagnosticCode, d0
	movea.l LastDiagnosticMessage, a0
	moveq #0, d1
	move.w LastDiagnosticLength, d1
	move.l LastDiagnosticSpan, d2
	rts
	.bend  ; getLastDiagnosticV1

	.endsection

	.section bss, kind=bss
	.priv

LastDiagnosticCode
	.res long, 1
LastDiagnosticMessage
	.res long, 1
LastDiagnosticLength
	.res word, 1
	.align 4
LastDiagnosticSpan
	.res long, 1

	.endsection
	.endmodule
