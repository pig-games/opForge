; Native opasm assembly-engine staging for the AmigaOS CLI.
;
; This module owns the transitional two-pass loop. The CLI supplies a compact
; context with session pointers and host callbacks for the currently supported
; smoke semantics while opasm owns pass ordering and statement iteration.

	.module opasm.amigaos.engine
	.cpu 68020
	.pub

TOKEN_BUFFER_CAPACITY           = 64
SOURCE_LINE_BUFFER_CAPACITY     = 512
NATIVE_SOURCE_RECORD_CAPACITY   = 512
NATIVE_STATEMENT_TABLE_CAPACITY = 160
NATIVE_LABEL_TABLE_CAPACITY     = 16
NATIVE_IMAGE_BUFFER_CAPACITY    = 4096
OPASM_ENGINE_ASSEMBLY_SESSION_BYTES = (5 * 2) + TOKEN_BUFFER_CAPACITY + (2 * 4) + (NATIVE_SOURCE_RECORD_CAPACITY * 4) + (NATIVE_SOURCE_RECORD_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * SOURCE_LINE_BUFFER_CAPACITY) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_LABEL_TABLE_CAPACITY * 4) + (NATIVE_LABEL_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + NATIVE_LABEL_TABLE_CAPACITY + NATIVE_IMAGE_BUFFER_CAPACITY

	.section code

OPASM_ENGINE_CTX_SESSION_PASS_PTR = 0
OPASM_ENGINE_CTX_STMT_COUNT_PTR   = 4
OPASM_ENGINE_CTX_BIN_REQUESTED_PTR = 8
OPASM_ENGINE_CTX_PASS1_BEGIN_CB   = 12
OPASM_ENGINE_CTX_PASS2_BEGIN_CB   = 16
OPASM_ENGINE_CTX_PASS1_OK_CB      = 20
OPASM_ENGINE_CTX_PASS2_OK_CB      = 24
OPASM_ENGINE_CTX_RECORD_LABEL_CB  = 28
OPASM_ENGINE_CTX_ADVANCE_PC_CB    = 32
OPASM_ENGINE_CTX_EMIT_IMAGE_CB    = 36

; A4: opasm engine context pointer.
; Returns D0=0 on success, non-zero on failure.
	.pub
; Initialize opasm-owned assembly-session state.
;
; Inputs:
; - A0: null-terminated CPU name to copy into the session.
;
; Outputs:
; - D0: 0 on success.
opasmEngineInitSessionV1	.block
	movem.l d1/a0-a1, -(sp)
	lea OpasmEngineAssemblySessionStart.l, a1
	move.l #OPASM_ENGINE_ASSEMBLY_SESSION_BYTES, d0
	bsr.w clearBytes
	lea OpasmEngineSessionCpuName.l, a1
	move.l #TOKEN_BUFFER_CAPACITY - 1, d0

copyCpuLoop
	move.b (a0)+, d1
	move.b d1, (a1)+
	beq.s copyCpuDone
	subq.l #1, d0
	bne.s copyCpuLoop
	clr.b -(a1)

copyCpuDone
	movem.l (sp)+, d1/a0-a1
	moveq #0, d0
	rts
	.bend  ; opasmEngineInitSessionV1

; Reset statement collection state before parsing input.
;
; Outputs:
; - D0: 0 on success.
opasmEngineResetStatementCollectionV1	.block
	clr.w OpasmEngineStmtCount.l
	moveq #0, d0
	rts
	.bend  ; opasmEngineResetStatementCollectionV1

; Record one logical source line in opasm-owned session tables.
;
; Inputs:
; - D0: source line number.
; - D1: source line length.
;
; Outputs:
; - D0: 0 on success.
opasmEngineRecordSourceLineV1	.block
	movem.l d2/a0, -(sp)
	moveq #0, d2
	move.w OpasmEngineSourceRecordCount.l, d2
	cmpi.w #NATIVE_SOURCE_RECORD_CAPACITY, d2
	bhs.s done
	lsl.l #2, d2
	lea OpasmEngineSourceLineNumTable.l, a0
	move.l d0, 0(a0, d2.l)
	moveq #0, d2
	move.w OpasmEngineSourceRecordCount.l, d2
	add.w d2, d2
	lea OpasmEngineSourceLineLenTable.l, a0
	move.w d1, 0(a0, d2.l)
	addq.w #1, OpasmEngineSourceRecordCount.l

done
	movem.l (sp)+, d2/a0
	moveq #0, d0
	rts
	.bend  ; opasmEngineRecordSourceLineV1

opasmEngineRunTwoPassV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l a4, a5
	bsr.w runPassOne
	tst.l d0
	bne.s done
	bsr.w runPassTwo

done
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; opasmEngineRunTwoPassV1
	.priv

clearBytes	.block
	tst.l d0
	beq.s done

loop
	clr.b (a1)+
	subq.l #1, d0
	bne.s loop

done
	rts
	.bend  ; clearBytes

runPassOne	.block
	movea.l OPASM_ENGINE_CTX_SESSION_PASS_PTR(a5), a0
	move.w #1, (a0)
	movea.l OPASM_ENGINE_CTX_PASS1_BEGIN_CB(a5), a0
	jsr (a0)
	tst.l d0
	bne.s return
	clr.w d7

loop
	movea.l OPASM_ENGINE_CTX_STMT_COUNT_PTR(a5), a0
	move.w (a0), d0
	cmp.w d0, d7
	bhs.s ok
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_RECORD_LABEL_CB(a5), a0
	jsr (a0)
	tst.l d0
	bne.s return
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_ADVANCE_PC_CB(a5), a0
	jsr (a0)
	tst.l d0
	bne.s return
	addq.w #1, d7
	bra.s loop

ok
	movea.l OPASM_ENGINE_CTX_PASS1_OK_CB(a5), a0
	jsr (a0)

return
	rts
	.bend  ; runPassOne

runPassTwo	.block
	movea.l OPASM_ENGINE_CTX_SESSION_PASS_PTR(a5), a0
	move.w #2, (a0)
	movea.l OPASM_ENGINE_CTX_PASS2_BEGIN_CB(a5), a0
	jsr (a0)
	tst.l d0
	bne.s return
	clr.w d7

loop
	movea.l OPASM_ENGINE_CTX_STMT_COUNT_PTR(a5), a0
	move.w (a0), d0
	cmp.w d0, d7
	bhs.s ok
	movea.l OPASM_ENGINE_CTX_BIN_REQUESTED_PTR(a5), a0
	tst.w (a0)
	beq.s advanceOnly
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_EMIT_IMAGE_CB(a5), a0
	jsr (a0)
	tst.l d0
	bne.s return

advanceOnly
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_ADVANCE_PC_CB(a5), a0
	jsr (a0)
	tst.l d0
	bne.s return
	addq.w #1, d7
	bra.s loop

ok
	movea.l OPASM_ENGINE_CTX_PASS2_OK_CB(a5), a0
	jsr (a0)

return
	rts
	.bend  ; runPassTwo

	.endsection

	.pub
	
	.section bss, kind=bss
	.align 4

OpasmEngineAssemblySessionStart
OpasmEngineStmtCount
	.res word, 1
OpasmEngineSessionPass
	.res word, 1
OpasmEngineSourceRecordCount
	.res word, 1
OpasmEngineLabelCount
	.res word, 1
OpasmEngineImageByteCount
	.res word, 1
OpasmEngineSessionCpuName
	.res byte, TOKEN_BUFFER_CAPACITY
OpasmEngineSessionOrigin
	.res long, 1
OpasmEngineSessionCurrentPc
	.res long, 1
OpasmEngineSourceLineNumTable
	.res long, NATIVE_SOURCE_RECORD_CAPACITY
OpasmEngineSourceLineLenTable
	.res word, NATIVE_SOURCE_RECORD_CAPACITY
OpasmEngineStmtLineTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtSourceLineLenTable
	.res word, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtSourceLineTextTable
	.res byte, NATIVE_STATEMENT_TABLE_CAPACITY * SOURCE_LINE_BUFFER_CAPACITY
OpasmEngineStmtLabelLenTable
	.res word, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtMnemLenTable
	.res word, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtOperandLenTable
	.res word, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtDirectiveKindTable
	.res word, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtMnemOffTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtLabelNameTable
	.res byte, NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
OpasmEngineStmtMnemNameTable
	.res byte, NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
OpasmEngineStmtOperandNameTable
	.res byte, NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
OpasmEngineStmtExprFlagsTable
	.res word, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtExprOperandIndexTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtExprSlotIndexTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtExprStartTokenTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtExprEndTokenTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtExprSpanLineTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtExprSpanStartTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtExprSpanEndTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineLabelValueTable
	.res long, NATIVE_LABEL_TABLE_CAPACITY
OpasmEngineLabelNameTable
	.res byte, NATIVE_LABEL_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
OpasmEngineLabelFinalizedTable
	.res byte, NATIVE_LABEL_TABLE_CAPACITY
OpasmEngineImageBuffer
	.res byte, NATIVE_IMAGE_BUFFER_CAPACITY
opasmEngineAssemblySessionEnd

	.endsection
	.endmodule
