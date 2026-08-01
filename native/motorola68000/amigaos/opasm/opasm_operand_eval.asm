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
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a1), a2
	movea.l a2, a1
	jsr eng.prepareEvaluateExpressionRequestV1
	rts
	.bend  ; prepareExpressionRequestV1

; Append the evaluation extension to a prepared request.
; Inputs: A0 = OPASM_SERVICE_* frame; D0.W = request bytes.
; Outputs: D0 = engine status.
prepareExpressionExtensionV1	.block
	movem.l a0-a3, -(sp)
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a2
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a3
	movea.l a3, a1
	movea.l a2, a0
	jsr eng.prepareEvaluateExpressionExtensionV1
	movea.l a3, a0
	bsr.w materializeScopedSnapshot
	movem.l (sp)+, a0-a3
	rts
	.bend  ; prepareExpressionExtensionV1

; Prepare the directive-specific evaluation extension.
; Inputs: A0 = OPASM_SERVICE_* frame.
; Outputs: D0 = engine status.
prepareDirectiveExpressionExtensionV1	.block
	movem.l a0-a1, -(sp)
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a1
	movea.l a1, a0
	jsr eng.prepareDirectiveEvaluateExpressionExtensionV1
	movea.l a1, a0
	bsr.w materializeScopedSnapshot
	movem.l (sp)+, a0-a1
	rts
	.bend  ; prepareDirectiveExpressionExtensionV1

	.priv

; Copy the engine label snapshot and prepend unqualified aliases owned by the
; complete active scope.  Scoped aliases precede globals so Rust-compatible
; module/block shadowing is preserved without exposing scope state to tkpkg.
; Inputs: A0 = evaluation extension base.
; Outputs: extension label pointers/count reference the bounded snapshot.
materializeScopedSnapshot	.block
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a3
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
	bhs.s copyOriginalBegin
	move.l d7, d0
	lsl.l #6, d0
	lea 0(a4, d0.l), a0
	bsr.w snapshotNameLen
	jsr scopes.activeLabelAliasV1
	tst.l d0
	beq.s aliasNext
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
	addq.w #1, ScopedSnapshotCount.l

aliasNext
	addq.w #1, d7
	bra.s aliasLoop

copyOriginalBegin
	moveq #0, d7

copyOriginalLoop
	cmp.w ScopedSnapshotSourceCount.l, d7
	bhs.s publish
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
	addq.w #1, ScopedSnapshotCount.l
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
ScopedSnapshotNames
	.res byte, SCOPED_SNAPSHOT_CAPACITY * SCOPED_SNAPSHOT_NAME_BYTES
ScopedSnapshotValues
	.res long, SCOPED_SNAPSHOT_CAPACITY

	.endsection
	.endmodule
