; Sole transitional owner of tkpkg reads from opasm engine state.

	.module tkpkg.amigaos.engine_context_adapter
	.cpu 68020
	.pub
	.use opasm.amigaos.engine

RUNTIME_CONTEXT_SYMBOL_FOUND = 0
RUNTIME_CONTEXT_SYMBOL_UNRESOLVED = 1
RUNTIME_CONTEXT_SYMBOL_ABSENT = 2

	.section code, kind=code
	.pub

getPassV1	.block
	jsr engine.opasmEngineGetSessionPassV1
	rts
	.bend  ; getPassV1

getAddressV1	.block
	jsr engine.opasmEngineGetSessionCurrentPcV1
	rts
	.bend  ; getAddressV1

; Inputs: A0 = symbol text, D0 = symbol text length.
; Outputs: D0 = RUNTIME_CONTEXT_SYMBOL_*, D1 = value when found.
lookupSymbolV1	.block
	movem.l d2-d7/a1-a4, -(sp)
	movea.l a0, a4
	move.w d0, d5
	jsr engine.opasmEngineGetLabelCountV1
	move.w d0, d7
	beq.s absent
	subq.w #1, d7
	moveq #0, d6

loop
	move.l d6, d0
	jsr engine.opasmEngineGetLabelNameV1
	movea.l a0, a1
	movea.l a4, a0
	move.w d5, d0
	bsr.w stringEqualsCasefoldV1
	beq.s next
	move.l d6, d0
	jsr engine.opasmEngineGetLabelValueV1
	move.l d0, d1
	move.l d6, d0
	jsr engine.opasmEngineIsLabelFinalV1
	tst.b d0
	beq.s unresolved
	moveq #RUNTIME_CONTEXT_SYMBOL_FOUND, d0
	bra.s return

unresolved
	moveq #RUNTIME_CONTEXT_SYMBOL_UNRESOLVED, d0
	bra.s return

next
	addq.w #1, d6
	dbf d7, loop

absent
	moveq #RUNTIME_CONTEXT_SYMBOL_ABSENT, d0
	moveq #0, d1

return
	movem.l (sp)+, d2-d7/a1-a4
	rts
	.bend  ; lookupSymbolV1

; Inputs: A0/D0 = exact symbol text. Outputs: D0 = 1 when the resolved engine
; label is PC-backed, otherwise 0.  The engine owns label classification.
isSymbolTargetReferenceV1	.block
	movem.l d1-d3/a0-a1, -(sp)
	jsr engine.opasmEngineResolveLabelValueV1
	tst.l d0
	bne.s symbolNotTarget
	jsr engine.opasmEngineLastResolvedLabelIsTargetReferenceV1
	bra.s symbolTargetReturn
symbolNotTarget
	moveq #0, d0
symbolTargetReturn
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; isSymbolTargetReferenceV1

; Return the symbol index retained by the last successful neutral symbol
; classification/evaluation.  The index is opaque to tkpkg; opasm translates
; it to section ownership when materializing an output-fixup record.
; Outputs: D0.W = symbol index, or $ffff when no symbol was resolved.
getLastResolvedSymbolIndexV1	.block
	jmp engine.opasmEngineGetLastResolvedLabelV1
	.bend  ; getLastResolvedSymbolIndexV1

; Inputs: D0 = symbol index.
; Outputs: D0 = 1 when the symbol is finalized, 0 otherwise.
isSymbolFinalV1	.block
	jmp engine.opasmEngineIsLabelFinalV1
	.bend  ; isSymbolFinalV1

getSymbolCountV1	.block
	jmp engine.opasmEngineGetLabelCountV1
	.bend  ; getSymbolCountV1

; Inputs: D0 = symbol index.
; Outputs: A0 = NUL-terminated symbol name.
getSymbolNameV1	.block
	jmp engine.opasmEngineGetLabelNameV1
	.bend  ; getSymbolNameV1

; Inputs: D0 = symbol index.
; Outputs: D0 = symbol value.
getSymbolValueV1	.block
	jmp engine.opasmEngineGetLabelValueV1
	.bend  ; getSymbolValueV1

; Inputs: A0 = first text, A1 = NUL-terminated second text, D0 = first length.
; Outputs: D0 = 1 when equal case-insensitively, 0 otherwise.
stringEqualsCasefoldV1	.block
	move.w d0, d4
	beq.s checkEnd
	subq.w #1, d4

loop
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d3
	move.b (a1)+, d3
	bsr.s foldAsciiLowerV1
	move.b d0, d2
	move.b d3, d0
	bsr.s foldAsciiLowerV1
	cmp.b d0, d2
	bne.s no
	dbf d4, loop

checkEnd
	tst.b (a1)
	bne.s no
	moveq #1, d0
	rts

no
	moveq #0, d0
	rts
	.bend  ; stringEqualsCasefoldV1

foldAsciiLowerV1	.block
	cmpi.b #'A', d0
	blo.s done
	cmpi.b #'Z', d0
	bhi.s done
	ori.b #$20, d0

done
	rts
	.bend  ; foldAsciiLowerV1

	.endsection
	.endmodule
