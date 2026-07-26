; Versioned, read-only runtime context for tkpkg consumers.
;
; Item 5.7.1 migrates the expression consumer to this ABI. Selection and
; encoding migration remains deferred to Item 5.7.2.

	.module tkpkg.amigaos.runtime_context
	.cpu 68020
	.pub
	.use tkpkg.amigaos.engine_context_adapter as adapter

RUNTIME_CONTEXT_ABI_VERSION = 1
RUNTIME_CONTEXT_SYMBOL_FOUND = 0
RUNTIME_CONTEXT_SYMBOL_UNRESOLVED = 1
RUNTIME_CONTEXT_SYMBOL_ABSENT = 2
RUNTIME_CONTEXT_DIAGNOSTIC_CAPACITY = 96
RUNTIME_CONTEXT_STABILITY_CAPACITY = 16

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

; Materialize a bounded, read-only stability snapshot for one consumer call.
; Inputs: D0 = symbol count.
; Outputs: D0 = 0/A0 = snapshot on success; D0 = 1/A0 = 0 when too large.
getSymbolStabilityTableV1	.block
	movem.l d1-d3/a1, -(sp)
	cmpi.w #RUNTIME_CONTEXT_STABILITY_CAPACITY, d0
	bhi.s tooLarge
	move.w d0, d2
	lea RuntimeContextStabilityTable, a1
	beq.s ready
	moveq #0, d3

copyLoop
	move.l d3, d0
	jsr adapter.isSymbolFinalV1
	move.b d0, (a1)+
	addq.w #1, d3
	cmp.w d2, d3
	blo.s copyLoop

ready
	lea RuntimeContextStabilityTable, a0
	moveq #0, d0
	bra.s return

tooLarge
	suba.l a0, a0
	moveq #1, d0

return
	movem.l (sp)+, d1-d3/a1
	rts
	.bend  ; getSymbolStabilityTableV1

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
RuntimeContextStabilityTable
	.res byte, RUNTIME_CONTEXT_STABILITY_CAPACITY

	.endsection
	.endmodule
