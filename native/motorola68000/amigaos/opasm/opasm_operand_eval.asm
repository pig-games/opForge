; Native opasm operand/evaluation request construction.

	.module opasm.amigaos.operand_eval
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.engine as eng
	.use opasm.amigaos.flow_scopes as scopes

SCOPED_SNAPSHOT_SOURCE_CAPACITY = 512
SCOPED_SNAPSHOT_CAPACITY = 1024
SCOPED_SNAPSHOT_NAME_BYTES = 64

	.section code, kind=code
	.pub

; Build one selected-instruction evaluation request.
; Inputs: D0.W = statement index; A0 = OPASM_SERVICE_* frame.
; Outputs: D0 = status; D1.W = request bytes on success.
prepareSelectedRequestV1	.block
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a1
	jsr eng.prepareSelectedEvaluateRequestV1
	rts
	.bend  ; prepareSelectedRequestV1

; Build one textual expression evaluation request.
; Inputs: A0 = expression text; D0.L = text bytes; D1.W = statement index;
;         A1 = OPASM_SERVICE_* frame.
; Outputs: D0 = status; D1.W = request bytes on success.
prepareExpressionRequestV1	.block
	move.l d1, -(sp)
	move.l a1, -(sp)
	move.l a0, -(sp)
	move.l d0, -(sp)
	movea.l abi.OPASM_SERVICE_IMPORT_NAME_RESOLVER_PTR(a1), a2
	move.l a2, d1
	beq.s expressionImportReady
	move.l 12(sp), d0
	jsr eng.opasmEngineGetStatementOwnerTextV1
	tst.l d0
	beq.s expressionImportReady
	movea.l a0, a1
	move.l d0, d1
	movea.l 4(sp), a0
	move.l (sp), d0
	jsr (a2)
	tst.l d1
	beq.s expressionImportMapped
expressionImportReady
	movea.l 4(sp), a0
	move.l (sp), d0
expressionImportMapped
	movea.l 8(sp), a1
	move.l 12(sp), d1
	adda.l #16, sp
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a1), a2
	movea.l a2, a1
	jsr eng.prepareEvaluateExpressionRequestV1
	rts
	.bend  ; prepareExpressionRequestV1

; Append the evaluation extension to a prepared request.
; Inputs: A0 = OPASM_SERVICE_* frame; D0.W = request bytes.
; Outputs: D0 = engine status.
prepareExpressionExtensionV1	.block
	moveq #0, d1
	bra.w prepareExtensionCommon
	.bend  ; prepareExpressionExtensionV1

; Append the evaluation extension and imported aliases for a selected CPU
; instruction request. The original operand text remains authoritative.
; Inputs: A0 = OPASM_SERVICE_* frame; D0.W = request bytes;
;         D1.W = stored statement index.
; Outputs: D0 = engine status.
prepareSelectedExtensionV1	.block
	move.w d1, SelectedStatementIndex.l
	moveq #1, d1
	bra.w prepareExtensionCommon
	.bend  ; prepareSelectedExtensionV1

; Prepare the directive-specific evaluation extension.
; Inputs: A0 = OPASM_SERVICE_* frame.
; Outputs: D0 = engine status.
prepareDirectiveExpressionExtensionV1	.block
	movem.l d2/a0-a1, -(sp)
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a1
	movea.l a1, a0
	jsr eng.prepareDirectiveEvaluateExpressionExtensionV1
	move.l d0, d2
	movea.l a1, a0
	suba.l a1, a1
	moveq #0, d0
	moveq #0, d1
	bsr.w materializeScopedSnapshot
	move.l d2, d0
	movem.l (sp)+, d2/a0-a1
	tst.l d0
	rts
	.bend  ; prepareDirectiveExpressionExtensionV1

	.priv

prepareExtensionCommon	.block
	movem.l d3-d5/a0-a4, -(sp)
	move.l d0, d4
	move.l d1, d5
	movea.l a0, a4
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a2
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a3
	movea.l a3, a1
	movea.l a2, a0
	jsr eng.prepareEvaluateExpressionExtensionV1
	move.l d0, d3
	movea.l a3, a0
	movea.l a4, a1
	move.l d4, d0
	move.l d5, d1
	bsr.w materializeScopedSnapshot
	move.l d3, d0
	movem.l (sp)+, d3-d5/a0-a4
	tst.l d0
	rts
	.bend  ; prepareExtensionCommon

; Add imported ordinary names referenced by one selected instruction operand
; to the package evaluation snapshot. The selected request text is never
; rewritten, so CPU-family addressing-mode selection sees the actual source.
; Inputs: D0.W = selected request bytes; A6 = service frame.
materializeSelectedImportAliases	.block
	movem.l d0-d7/a0-a6, -(sp)
	moveq #0, d7
	move.w d0, d7
	cmpi.w #9, d7
	blo.w selectedAliasReturn
	movea.l abi.OPASM_SERVICE_IMPORT_NAME_RESOLVER_PTR(a6), a2
	move.l a2, d0
	beq.w selectedAliasReturn
	suba.l a4, a4
	moveq #0, d5
	moveq #0, d0
	move.w SelectedStatementIndex.l, d0
	jsr eng.opasmEngineGetStatementOwnerTextV1
	tst.l d0
	beq.s selectedAliasOwnerReady
	movea.l a0, a4
	move.l d0, d5
selectedAliasOwnerReady
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a6), a3
	moveq #0, d4
	move.b 8(a3), d4
	addi.l #9, d4
	cmp.l d7, d4
	bhi.w selectedAliasReturn
	adda.l d4, a3
	sub.l d4, d7
	moveq #0, d2

selectedAliasScan
	tst.l d7
	beq.w selectedAliasReturn
	moveq #0, d0
	move.b (a3), d0
	tst.b d2
	beq.s selectedAliasUnquoted
	cmp.b d2, d0
	beq.w selectedAliasQuoteEnd
	cmpi.b #'\\', d0
	bne.w selectedAliasAdvanceOne
	cmpi.l #1, d7
	bls.w selectedAliasAdvanceOne
	addq.l #2, a3
	subq.l #2, d7
	bra.s selectedAliasScan

selectedAliasQuoteEnd
	moveq #0, d2
	bra.w selectedAliasAdvanceOne

selectedAliasUnquoted
	cmpi.b #'"', d0
	beq.w selectedAliasQuoteBegin
	cmpi.b #39, d0
	beq.w selectedAliasQuoteBegin
	bsr.w selectedImportTokenStart
	tst.l d0
	beq.w selectedAliasAdvanceOne
	moveq #1, d4

selectedAliasTokenScan
	cmp.l d7, d4
	bhs.s selectedAliasTokenReady
	moveq #0, d0
	move.b 0(a3, d4.l), d0
	bsr.w selectedImportTokenContinue
	tst.l d0
	beq.s selectedAliasTokenReady
	addq.l #1, d4
	bra.s selectedAliasTokenScan

selectedAliasTokenReady
	tst.l d5
	beq.s selectedAliasTrySuffix
	movem.l d2-d7/a2-a5, -(sp)
	movea.l a3, a0
	move.l d4, d0
	movea.l a4, a1
	move.l d5, d1
	jsr (a2)
	move.l d1, -(sp)
	move.l d0, -(sp)
	move.l a0, -(sp)
	movem.l 12(sp), d2-d7/a2-a5
	movea.l (sp), a0
	move.l 4(sp), d0
	move.l 8(sp), d1
	adda.l #52, sp
	tst.l d1
	bne.s selectedAliasTrySuffix
	move.l d0, -(sp)
	move.l a0, -(sp)
	jsr eng.opasmEngineResolveLabelValueV1
	tst.l d0
	beq.s selectedAliasMappedExact
	movea.l (sp), a0
	move.l 4(sp), d0
	jsr eng.opasmEngineResolveUniqueLabelFinalComponentV1
	tst.l d0
	beq.s selectedAliasMappedExact
	addq.l #8, sp
	bra.s selectedAliasTrySuffix
selectedAliasMappedExact
	addq.l #8, sp
	bra.s selectedAliasMappedResolved
selectedAliasTrySuffix
	moveq #0, d2
	movea.l a3, a0
	move.l d4, d0
	bsr.w resolveSelectedLastComponentV1
	tst.l d0
	bne.w selectedAliasAdvanceToken
	bra.s selectedAliasResolved
selectedAliasMappedResolved
	moveq #1, d2
selectedAliasResolved
	movea.l a3, a0
	move.l d4, d0
	bsr.w appendSelectedSnapshotAliasV1
	tst.l d0
	bne.w selectedAliasAdvanceToken
	; A successful qualified import is authoritative for this request. Retain
	; its final component beside the source spelling so the package's direct
	; label fast path consumes the same request-local visibility decision.
	tst.l d2
	beq.s selectedAliasAdvanceToken
	movea.l a3, a0
	move.l d4, d0
	bsr.w finalIdentifierComponentV1
	tst.l d0
	beq.s selectedAliasMappedDone
	bsr.w appendSelectedSnapshotAliasV1
selectedAliasMappedDone
	moveq #0, d2

selectedAliasAdvanceToken
	adda.l d4, a3
	sub.l d4, d7
	bra.w selectedAliasScan

selectedAliasQuoteBegin
	move.b d0, d2

selectedAliasAdvanceOne
	addq.l #1, a3
	subq.l #1, d7
	bra.w selectedAliasScan

selectedAliasReturn
	movem.l (sp)+, d0-d7/a0-a6
	rts
	.bend  ; materializeSelectedImportAliases

; Append one already-authorized alias/value to the bounded selected snapshot.
; Inputs: A0/D0 = alias text/length; D3 = value. Outputs: D0 = status.
appendSelectedSnapshotAliasV1	.block
	cmpi.l #SCOPED_SNAPSHOT_NAME_BYTES, d0
	bhs.s appendAliasFail
	move.l d0, -(sp)
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	cmpi.w #SCOPED_SNAPSHOT_CAPACITY, d1
	bhs.s appendAliasStackFail
	move.l d3, d6
	lsl.l #6, d1
	lea ScopedSnapshotNames.l, a1
	adda.l d1, a1
	move.l (sp), d0
	bsr.w copySnapshotName
	moveq #0, d0
	move.w ScopedSnapshotCount.l, d0
	lsl.l #2, d0
	lea ScopedSnapshotValues.l, a0
	move.l d6, 0(a0, d0.l)
	lea ScopedSnapshotCount.l, a0
	addq.w #1, (a0)
	addq.l #4, sp
	moveq #0, d0
	rts
appendAliasStackFail
	addq.l #4, sp
appendAliasFail
	moveq #1, d0
	rts
	.bend  ; appendSelectedSnapshotAliasV1

; Return the final component of a dotted architecture-neutral identifier.
; Inputs/outputs: A0/D0 = token slice; D0 = 0 when no suffix exists.
finalIdentifierComponentV1	.block
	movea.l a0, a1
	move.l d0, d1
	moveq #0, d6
finalComponentScan
	tst.l d1
	beq.s finalComponentReady
	cmpi.b #'.', (a1)+
	bne.s finalComponentNext
	movea.l a1, a0
	move.l d1, d6
	subq.l #1, d6
finalComponentNext
	subq.l #1, d1
	bra.s finalComponentScan
finalComponentReady
	move.l d6, d0
	rts
	.bend  ; finalIdentifierComponentV1

; Resolve the final component of a dotted architecture-neutral identifier.
; This supports engines whose structural module pass retained an unqualified
; exported label while the import token itself remains fully qualified.
; Inputs: A0/D0 = token text/length.
; Outputs: D0 = engine resolve status; D3 = value on success.
resolveSelectedLastComponentV1	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	move.l d0, d2
	moveq #0, d1
selectedComponentScan
	tst.l d2
	beq.s selectedComponentReady
	cmpi.b #'.', (a1)+
	bne.s selectedComponentNext
	movea.l a1, a0
	move.l d2, d1
	subq.l #1, d1
selectedComponentNext
	subq.l #1, d2
	bra.s selectedComponentScan
selectedComponentReady
	tst.l d1
	beq.s selectedComponentFail
	move.l d1, d0
	jsr eng.opasmEngineResolveLabelValueV1
	bra.s selectedComponentReturn
selectedComponentFail
	moveq #1, d0
selectedComponentReturn
	movem.l (sp)+, d1-d2/a0-a1
	rts
	.bend  ; resolveSelectedLastComponentV1

; Architecture-neutral opForge identifier boundaries used only to query the
; embedding import resolver. Registers, literals, and unmapped names remain
; unchanged in the package request.
selectedImportTokenStart	.block
	cmpi.b #'A', d0
	blo.s punctuation
	cmpi.b #'Z', d0
	bls.s accepted
	cmpi.b #'a', d0
	blo.s punctuation
	cmpi.b #'z', d0
	bls.s accepted
punctuation
	cmpi.b #'_', d0
	beq.s accepted
	cmpi.b #'.', d0
	beq.s accepted
	cmpi.b #'$', d0
	beq.s accepted
	moveq #0, d0
	rts
accepted
	moveq #1, d0
	rts
	.bend  ; selectedImportTokenStart

selectedImportTokenContinue	.block
	move.l d0, -(sp)
	bsr.s selectedImportTokenStart
	tst.l d0
	bne.s accepted
	move.l (sp), d0
	cmpi.b #'0', d0
	blo.s rejected
	cmpi.b #'9', d0
	bls.s accepted
rejected
	addq.l #4, sp
	moveq #0, d0
	rts
accepted
	addq.l #4, sp
	moveq #1, d0
	rts
	.bend  ; selectedImportTokenContinue

; Copy the engine label snapshot and prepend unqualified aliases owned by the
; complete active scope.  Scoped aliases precede globals so Rust-compatible
; module/block shadowing is preserved without exposing scope state to tkpkg.
; Inputs: A0 = evaluation extension base; A1 = service frame; D0 = request
; bytes; D1 = 1 only for a selected CPU instruction request.
; Outputs: extension label pointers/count reference the bounded snapshot.
materializeScopedSnapshot	.block
	movem.l d0-d7/a0-a6, -(sp)
	move.l d0, d6
	move.l d1, d5
	movea.l a0, a3
	movea.l a1, a6
	movea.l 0(a3), a4
	movea.l 4(a3), a5
	move.l 8(a3), d0
	cmpi.w #SCOPED_SNAPSHOT_SOURCE_CAPACITY, d0
	bhi.w done
	move.w d0, ScopedSnapshotSourceCount.l
	clr.w ScopedSnapshotCount.l
	moveq #0, d7

aliasLoop
	cmp.w ScopedSnapshotSourceCount.l, d7
	bhs.s importedAliasBegin
	move.l d7, d0
	lsl.l #6, d0
	lea 0(a4, d0.l), a0
	bsr.w snapshotNameLen
	jsr scopes.activeLabelAliasV1
	tst.l d0
	beq.s aliasNext
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	cmpi.w #SCOPED_SNAPSHOT_CAPACITY, d1
	bhs.s copyOriginalBegin
	move.l d1, d2
	lsl.l #6, d2
	lea ScopedSnapshotNames.l, a1
	adda.l d2, a1
	bsr.w copySnapshotName
	move.l d7, d2
	lsl.l #2, d2
	move.l 0(a5, d2.l), d3
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	lsl.l #2, d1
	lea ScopedSnapshotValues.l, a0
	adda.l d1, a0
	move.l d3, (a0)
	lea ScopedSnapshotCount.l, a0
	addq.w #1, (a0)

aliasNext
	addq.w #1, d7
	bra.s aliasLoop

importedAliasBegin
	tst.l d5
	beq.s copyOriginalBegin
	move.l d6, d0
	bsr.w materializeSelectedImportAliases

copyOriginalBegin
	moveq #0, d7

copyOriginalLoop
	cmp.w ScopedSnapshotSourceCount.l, d7
	bhs.s publish
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	cmpi.w #SCOPED_SNAPSHOT_CAPACITY, d1
	bhs.s publish
	move.l d7, d2
	lsl.l #6, d2
	lea 0(a4, d2.l), a0
	move.l d1, d3
	lsl.l #6, d3
	lea ScopedSnapshotNames.l, a1
	adda.l d3, a1
	moveq #SCOPED_SNAPSHOT_NAME_BYTES, d0
	bsr.w copySnapshotName
	move.l d7, d2
	lsl.l #2, d2
	move.l 0(a5, d2.l), d3
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	lsl.l #2, d1
	lea ScopedSnapshotValues.l, a0
	adda.l d1, a0
	move.l d3, (a0)
	lea ScopedSnapshotCount.l, a0
	addq.w #1, (a0)
	addq.w #1, d7
	bra.s copyOriginalLoop

publish
	move.l #ScopedSnapshotNames, 0(a3)
	move.l #ScopedSnapshotValues, 4(a3)
	moveq #0, d0
	move.w ScopedSnapshotCount.l, d0
	move.l d0, 8(a3)

done
	movem.l (sp)+, d0-d7/a0-a6
	rts
	.bend  ; materializeScopedSnapshot

; Inputs: A0 = null-terminated fixed-width label name.
; Outputs: D0 = byte length, capped at the fixed-width name capacity.
snapshotNameLen	.block
	moveq #0, d0

loop
	cmpi.w #SCOPED_SNAPSHOT_NAME_BYTES, d0
	bhs.s done
	tst.b 0(a0, d0.w)
	beq.s done
	addq.w #1, d0
	bra.s loop

done
	rts
	.bend  ; snapshotNameLen

; Inputs: A0/D0 = source name/bytes; A1 = destination fixed-width slot.
; Outputs: destination contains the copied bytes and a trailing null when the
;          source is shorter than the fixed-width slot.
copySnapshotName	.block
	move.l d0, d1
	beq.s terminate
	subq.w #1, d1

loop
	move.b (a0)+, (a1)+
	dbf d1, loop
	cmpi.w #SCOPED_SNAPSHOT_NAME_BYTES, d0
	bhs.s done

terminate
	clr.b (a1)

done
	rts
	.bend  ; copySnapshotName

	.endsection

	.section bss, kind=bss
	.align 4

ScopedSnapshotSourceCount
	.res word, 1
ScopedSnapshotCount
	.res word, 1
SelectedStatementIndex
	.res word, 1
ScopedSnapshotNames
	.res byte, SCOPED_SNAPSHOT_CAPACITY * SCOPED_SNAPSHOT_NAME_BYTES
ScopedSnapshotValues
	.res long, SCOPED_SNAPSHOT_CAPACITY

	.endsection
	.endmodule
