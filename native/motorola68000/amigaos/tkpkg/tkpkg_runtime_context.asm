; Versioned, read-only runtime context for tkpkg consumers.
;
; Item 5.7.1 migrates the expression consumer to this ABI. Selection and
; encoding migration remains deferred to Item 5.7.2.

	.module tkpkg.amigaos.runtime_context
	.cpu 68020
	.pub
	.use tkpkg.amigaos.engine_context_adapter as adapter
	.use tkpkg.amigaos.state_service as state_service

RUNTIME_CONTEXT_ABI_VERSION = 1
RUNTIME_CONTEXT_SYMBOL_FOUND = 0
RUNTIME_CONTEXT_SYMBOL_UNRESOLVED = 1
RUNTIME_CONTEXT_SYMBOL_ABSENT = 2
RUNTIME_CONTEXT_DIAGNOSTIC_CAPACITY = 96
RUNTIME_CONTEXT_STABILITY_CAPACITY = 512
RUNTIME_CONTEXT_SYMBOL_NAME_BYTES = 64

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

; Inputs: A0/D0 = exact symbol text. Outputs: D0 = 1 only for a PC-backed
; assembler label, matching Rust's target-reference classification.
isSymbolTargetReferenceV1	.block
	jmp adapter.isSymbolTargetReferenceV1
	.bend  ; isSymbolTargetReferenceV1

; Read one opaque package-owned runtime-state key.
; Inputs: A0/D0 = key text. Outputs: D0 = 0 found/D1 value, 1 absent.
getCpuStateFlagV1	.block
	jmp state_service.getFlagV1
	.bend  ; getCpuStateFlagV1

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

; Materialize a bounded, read-only symbol-table snapshot for legacy bridge
; consumers.  The returned tables are context-owned copies, never engine
; storage.
; Outputs: D0 = 0/A0 = names/A1 = values/D1 = count on success;
;          D0 = 1/A0/A1 = 0 on capacity failure.
getSymbolTableSnapshotV1	.block
	jsr adapter.getSymbolCountV1
	cmpi.w #RUNTIME_CONTEXT_STABILITY_CAPACITY, d0
	bhi.s tableTooLarge
	move.w d0, d4
	beq.s tableReady
	moveq #0, d3

tableLoop
	move.l d3, d0
	jsr adapter.getSymbolNameV1
	movea.l a0, a2
	move.l d3, d2
	lsl.l #6, d2
	lea RuntimeContextSymbolNames, a1
	adda.l d2, a1
	moveq #RUNTIME_CONTEXT_SYMBOL_NAME_BYTES - 1, d2

copyNameLoop
	move.b (a2)+, (a1)+
	dbf d2, copyNameLoop
	move.l d3, d0
	jsr adapter.getSymbolValueV1
	move.l d3, d2
	lsl.l #2, d2
	lea RuntimeContextSymbolValues, a1
	move.l d0, 0(a1, d2.l)
	addq.w #1, d3
	cmp.w d4, d3
	blo.s tableLoop

tableReady
	lea RuntimeContextSymbolNames, a0
	lea RuntimeContextSymbolValues, a1
	move.l d4, d1
	moveq #0, d0
	rts

tableTooLarge
	suba.l a0, a0
	suba.l a1, a1
	moveq #0, d1
	moveq #1, d0
	rts
	.bend  ; getSymbolTableSnapshotV1

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
RuntimeContextSymbolNames
	.res byte, RUNTIME_CONTEXT_STABILITY_CAPACITY * RUNTIME_CONTEXT_SYMBOL_NAME_BYTES
RuntimeContextSymbolValues
	.res long, RUNTIME_CONTEXT_STABILITY_CAPACITY

	.endsection
	.endmodule
