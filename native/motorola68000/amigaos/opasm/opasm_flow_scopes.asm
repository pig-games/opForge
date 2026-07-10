OPASM_SCOPE_DEPTH_CAPACITY = 8
OPASM_SCOPE_NAME_CAPACITY = 32
OPASM_SCOPE_TEXT_CAPACITY = 64

; Reset bounded block/namespace state for one assembly pass.
; Outputs: D0 = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resetStateV1	.block
	clr.w ScopeDepth
	moveq #0, d0
	rts
	.bend  ; resetStateV1

; Apply the current `.block` scope directive and skip it.
; Inputs: D7.W = current statement index.
; Outputs: D0 = status; D1 = 1; D2.W = next statement index.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
beginBlockScopeV1	.block
	bsr.w pushFromStatementLabel
	bne.s fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; beginBlockScopeV1

; Apply the current `.namespace` scope directive and skip it.
; Inputs: D7.W = current statement index.
; Outputs: D0 = status; D1 = 1; D2.W = next statement index.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
beginNamespaceScopeV1	.block
	bsr.w pushFromStatementOperand
	bne.s fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; beginNamespaceScopeV1

; Apply a scope close directive and skip it.
; Inputs: D7.W = current statement index.
; Outputs: D0 = status; D1 = 1; D2.W = next statement index.
; Clobbers: D0-D1/CCR.
; CCR: reflects D0 on return.
endScopeDirectiveV1	.block
	bsr.w popScope
	bne.s fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; endScopeDirectiveV1

; Rewrite the current symbol directive's label with active scope prefixes.
; Inputs: D7.W = statement index.
; Outputs: D0 = 0 on success, 1 on qualification failure.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
qualifyStatementLabelIfScopedV1	.block
	tst.w ScopeDepth
	beq.s ok
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	move.l d0, d5
	movea.l a0, a2
	moveq #0, d2
	move.w ScopeDepth, d2
	movea.l a2, a0
	move.l d5, d0
	bsr.w buildTextAtDepth
	bne.s fail
	move.l d1, d5
	movea.l a0, a1
	moveq #0, d0
	move.w d7, d0
	movea.l a1, a0
	move.l d5, d1
	jsr eng.opasmEngineSetStatementLabelTextV1
	rts
ok
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; qualifyStatementLabelIfScopedV1

; Resolve an unqualified token through active scope prefixes before globals.
; Inputs: A0/D0 = trimmed token text/length.
; Outputs: D0 = 0 on success, 1 when no scoped value matches; D3 on success.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
resolveLabelValueV1	.block
	movem.l d1-d2/d4-d6/a0-a2, -(sp)
	tst.w ScopeDepth
	beq.s fail
	movea.l a0, a2
	move.l d0, d6
	movea.l a0, a1
	move.l d0, d4
dotScan
	tst.l d4
	beq.s noDot
	cmpi.b #'.', (a1)+
	beq.s fail
	subq.l #1, d4
	bra.s dotScan
noDot
	moveq #0, d2
	move.w ScopeDepth, d2
scan
	tst.w d2
	beq.s fail
	movea.l a2, a0
	move.l d6, d0
	bsr.w buildTextAtDepth
	bne.s next
	move.l d1, d0
	jsr eng.opasmEngineResolveLabelValueV1
	beq.s ok
next
	subq.w #1, d2
	bra.s scan
ok
	movem.l (sp)+, d1-d2/d4-d6/a0-a2
	moveq #0, d0
	rts
fail
	movem.l (sp)+, d1-d2/d4-d6/a0-a2
	moveq #1, d0
	rts
	.bend  ; resolveLabelValueV1

	.priv

; Push the label attached to the current `.block` as one scope component.
; Inputs: D7.W = statement index.
; Outputs: D0 = 0 on success, 1 on missing/over-capacity name.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushFromStatementLabel	.block
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	bsr.w pushText
	rts
	.bend  ; pushFromStatementLabel

; Push the first operand token of the current `.namespace` as one scope component.
; Inputs: D7.W = statement index.
; Outputs: D0 = 0 on success, 1 on missing/over-capacity name.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushFromStatementOperand	.block
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	bsr.w pushText
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	rts
fail
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #1, d0
	rts
	.bend  ; pushFromStatementOperand

; Push one identifier token onto the bounded scope stack.
; Inputs: A0/D0 = name text/length.
; Outputs: D0 = 0 on success, 1 on malformed/capacity failure.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushText	.block
	bsr.w skipWhitespace
	tst.l d0
	beq.s fail
	moveq #0, d2
	move.w ScopeDepth, d2
	cmpi.w #OPASM_SCOPE_DEPTH_CAPACITY, d2
	bhs.s fail
	move.l d2, d3
	lsl.l #5, d3
	lea ScopeNames, a1
	adda.l d3, a1
	moveq #OPASM_SCOPE_NAME_CAPACITY - 1, d4
copy
	tst.l d0
	beq.s finish
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s finish
	cmpi.b #9, d1
	beq.s finish
	cmpi.b #';', d1
	beq.s finish
	tst.w d4
	beq.s fail
	move.b (a0)+, (a1)+
	subq.l #1, d0
	subq.w #1, d4
	bra.s copy
finish
	cmpi.w #OPASM_SCOPE_NAME_CAPACITY - 1, d4
	beq.w fail
	clr.b (a1)
	move.w ScopeDepth, d2
	addq.w #1, d2
	move.w d2, ScopeDepth
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; pushText

; Pop one active scope component.
; Outputs: D0 = 0 on success, 1 when no scope is active.
; Clobbers: D0-D1/CCR.
; CCR: reflects D0 on return.
popScope	.block
	tst.w ScopeDepth
	beq.s fail
	move.w ScopeDepth, d1
	subq.w #1, d1
	move.w d1, ScopeDepth
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; popScope

; Build `scope[0]....scope[depth-1].name` into the fixed scratch buffer.
; Inputs: A0/D0 = raw name; D2.W = requested scope depth.
; Outputs: D0 = 0 on success; A0/D1 = scratch text/length.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
buildTextAtDepth	.block
	movea.l a0, a2
	move.l d0, d6
	lea ScopeScratch, a1
	movea.l a1, a0
	moveq #OPASM_SCOPE_TEXT_CAPACITY - 1, d4
	clr.l d1
	clr.w d3
scopeLoop
	cmp.w d2, d3
	bhs.s rawName
	move.l d3, d5
	lsl.l #5, d5
	lea ScopeNames, a1
	adda.l d5, a1
scopeChar
	move.b (a1)+, d5
	beq.s scopeEnd
	tst.w d4
	beq.s fail
	move.b d5, (a0)+
	addq.l #1, d1
	subq.w #1, d4
	bra.s scopeChar
scopeEnd
	tst.w d4
	beq.s fail
	move.b #'.', (a0)+
	addq.l #1, d1
	subq.w #1, d4
	addq.w #1, d3
	bra.s scopeLoop
rawName
	tst.l d6
	beq.s fail
rawLoop
	tst.l d6
	beq.s done
	tst.w d4
	beq.s fail
	move.b (a2)+, (a0)+
	addq.l #1, d1
	subq.w #1, d4
	subq.l #1, d6
	bra.s rawLoop
done
	clr.b (a0)
	lea ScopeScratch, a0
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; buildTextAtDepth

; Skip ASCII spaces and tabs in A0/D0.
; Inputs: A0/D0 = text/length.
; Outputs: A0/D0 = first non-whitespace byte/remaining length.
; Clobbers: CCR.
; CCR: reflects D0 on return.
skipWhitespace	.block
	tst.l d0
	beq.s done
	cmpi.b #' ', (a0)
	beq.s skip
	cmpi.b #9, (a0)
	bne.s done
skip
	addq.l #1, a0
	subq.l #1, d0
	bra.s skipWhitespace
done
	rts
	.bend  ; skipWhitespace

	.section bss, kind=bss

ScopeDepth
	.res word, 1

ScopeNames
	.res byte, OPASM_SCOPE_DEPTH_CAPACITY * OPASM_SCOPE_NAME_CAPACITY

ScopeScratch
	.res byte, OPASM_SCOPE_TEXT_CAPACITY
