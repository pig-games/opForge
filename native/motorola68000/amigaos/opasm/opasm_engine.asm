; Native opasm assembly-engine staging for the AmigaOS CLI.
;
; This module owns the transitional two-pass loop. The CLI supplies a compact
; context with session pointers and host callbacks for the currently supported
; smoke semantics while opasm owns pass ordering and statement iteration.

	.module opasm.amigaos.engine
	.cpu 68020

	.use opasm.amigaos.events

	.pub

TOKEN_BUFFER_CAPACITY           = 64
SOURCE_LINE_BUFFER_CAPACITY     = 512
NATIVE_SOURCE_RECORD_CAPACITY   = 512
NATIVE_STATEMENT_TABLE_CAPACITY = 160
NATIVE_LABEL_TABLE_CAPACITY     = 16
NATIVE_IMAGE_BUFFER_CAPACITY    = 4096
OPASM_ENGINE_CONTEXT_LONGS      = 10
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
OPASM_ENGINE_CALLBACK_REQ_BIN_REQUESTED_PTR = 0
OPASM_ENGINE_CALLBACK_REQ_PASS1_BEGIN_CB = 4
OPASM_ENGINE_CALLBACK_REQ_PASS2_BEGIN_CB = 8
OPASM_ENGINE_CALLBACK_REQ_PASS1_OK_CB = 12
OPASM_ENGINE_CALLBACK_REQ_PASS2_OK_CB = 16
OPASM_ENGINE_CALLBACK_REQ_RECORD_LABEL_CB = 20
OPASM_ENGINE_CALLBACK_REQ_ADVANCE_PC_CB = 24
OPASM_ENGINE_CALLBACK_REQ_EMIT_IMAGE_CB = 28
OPASM_ENGINE_CALLBACK_REQ_BYTES = 32
OPASM_ENGINE_STMT_REQ_SOURCE_LINE_NUM = 0
OPASM_ENGINE_STMT_REQ_SOURCE_LINE_LEN = 4
OPASM_ENGINE_STMT_REQ_DIRECTIVE_KIND  = 6
OPASM_ENGINE_STMT_REQ_LABEL_START     = 8
OPASM_ENGINE_STMT_REQ_LABEL_LEN       = 12
OPASM_ENGINE_STMT_REQ_LABEL_LEN_WORD  = 14
OPASM_ENGINE_STMT_REQ_MNEM_START      = 16
OPASM_ENGINE_STMT_REQ_MNEM_OFF        = 20
OPASM_ENGINE_STMT_REQ_MNEM_LEN        = 24
OPASM_ENGINE_STMT_REQ_MNEM_LEN_WORD   = 26
OPASM_ENGINE_STMT_REQ_OPERAND_START   = 28
OPASM_ENGINE_STMT_REQ_OPERAND_END     = 32
OPASM_ENGINE_STMT_REQ_EXPR_FOUND      = 36
OPASM_ENGINE_STMT_REQ_EXPR_OPERAND_INDEX = 40
OPASM_ENGINE_STMT_REQ_EXPR_SLOT_INDEX = 44
OPASM_ENGINE_STMT_REQ_EXPR_START_TOKEN = 48
OPASM_ENGINE_STMT_REQ_EXPR_END_TOKEN  = 52
OPASM_ENGINE_STMT_REQ_EXPR_SPAN_LINE  = 56
OPASM_ENGINE_STMT_REQ_EXPR_SPAN_START = 60
OPASM_ENGINE_STMT_REQ_EXPR_SPAN_END   = 64
OPASM_ENGINE_STMT_RECORD_REQUEST_BYTES = 68
OPASM_ENGINE_LABEL_EVENT_NONE      = 0
OPASM_ENGINE_LABEL_EVENT_STORED    = 1
OPASM_ENGINE_LABEL_EVENT_DUPLICATE = 2
OPASM_ENGINE_LABEL_EVENT_CAPACITY  = 3
OPASM_ENGINE_EXPR_META_OPERAND_INDEX = 0
OPASM_ENGINE_EXPR_META_SLOT_INDEX    = 4
OPASM_ENGINE_EXPR_META_START_TOKEN   = 8
OPASM_ENGINE_EXPR_META_END_TOKEN     = 12
OPASM_ENGINE_EXPR_META_SPAN_LINE     = 16
OPASM_ENGINE_EXPR_META_SPAN_START    = 20
OPASM_ENGINE_EXPR_META_SPAN_END      = 24
OPASM_ENGINE_EXPR_META_BYTES         = 28
OPASM_ENGINE_STMT_TEXT_MNEM_PTR      = 0
OPASM_ENGINE_STMT_TEXT_MNEM_LEN      = 4
OPASM_ENGINE_STMT_TEXT_OPERAND_PTR   = 8
OPASM_ENGINE_STMT_TEXT_OPERAND_LEN   = 12
OPASM_ENGINE_STMT_TEXT_BYTES         = 16
OPASM_ENGINE_SELECTED_REQ_TEXT_META  = 0
OPASM_ENGINE_SELECTED_REQ_EXPR_META  = OPASM_ENGINE_STMT_TEXT_BYTES
OPASM_ENGINE_SELECTED_REQ_SCRATCH_BYTES = OPASM_ENGINE_STMT_TEXT_BYTES + OPASM_ENGINE_EXPR_META_BYTES
OPASM_ENGINE_SELECTED_REQ_MNEM_PTR   = 0
OPASM_ENGINE_SELECTED_REQ_MNEM_LEN   = 4
OPASM_ENGINE_SELECTED_REQ_OPERAND_PTR = 8
OPASM_ENGINE_SELECTED_REQ_OPERAND_LEN = 12
OPASM_ENGINE_SELECTED_REQ_EXPR_OPERAND_INDEX = 16
OPASM_ENGINE_SELECTED_REQ_EXPR_SLOT_INDEX = 20
OPASM_ENGINE_SELECTED_REQ_EXPR_START_TOKEN = 24
OPASM_ENGINE_SELECTED_REQ_EXPR_END_TOKEN = 28
OPASM_ENGINE_SELECTED_REQ_EXPR_SPAN_LINE = 32
OPASM_ENGINE_SELECTED_REQ_EXPR_SPAN_START = 36
OPASM_ENGINE_SELECTED_REQ_EXPR_SPAN_END = 40
OPASM_ENGINE_EVAL_REQ_TEXT_META  = 0
OPASM_ENGINE_EVAL_REQ_EXPR_META  = OPASM_ENGINE_STMT_TEXT_BYTES
OPASM_ENGINE_EVAL_REQ_SCRATCH_BYTES = OPASM_ENGINE_STMT_TEXT_BYTES + OPASM_ENGINE_EXPR_META_BYTES
OPASM_ENGINE_EVAL_REQ_EXPR_SPAN_LINE  = OPASM_ENGINE_EVAL_REQ_EXPR_META + OPASM_ENGINE_EXPR_META_SPAN_LINE
OPASM_ENGINE_EVAL_REQ_EXPR_SPAN_START = OPASM_ENGINE_EVAL_REQ_EXPR_META + OPASM_ENGINE_EXPR_META_SPAN_START
OPASM_ENGINE_EVAL_REQ_EXPR_SPAN_END   = OPASM_ENGINE_EVAL_REQ_EXPR_META + OPASM_ENGINE_EXPR_META_SPAN_END

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
initSessionV1	.block
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
	.bend  ; initSessionV1

; Reset statement collection state before parsing input.
;
; Outputs:
; - D0: 0 on success.
resetStatementCollectionV1	.block
	clr.w OpasmEngineStmtCount.l
	moveq #0, d0
	rts
	.bend  ; resetStatementCollectionV1

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

; Store one parsed statement into opasm-owned statement tables.
;
; Inputs:
; - A0: source-line bytes.
; - A1: mnemonic/token scratch bytes.
; - A2: statement-record request using OPASM_ENGINE_STMT_REQ_* offsets.
;
; Outputs:
; - D0: 0 on success, non-zero on capacity or field failure.
opasmEngineStoreStatementRecordV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	movea.l a2, a5
	move.l OPASM_ENGINE_STMT_REQ_MNEM_LEN(a5), d0
	cmp.l #TOKEN_BUFFER_CAPACITY - 1, d0
	bhi.w fail
	move.w OpasmEngineStmtCount.l, d0
	cmpi.w #NATIVE_STATEMENT_TABLE_CAPACITY, d0
	bhs.w fail
	bsr.w storeStatementRecord
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; opasmEngineStoreStatementRecordV1

; Commit a successfully stored statement record.
;
; Outputs:
; - D0: 0 on success.
opasmEngineCommitStatementRecordV1	.block
	addq.w #1, OpasmEngineStmtCount.l
	moveq #0, d0
	rts
	.bend  ; opasmEngineCommitStatementRecordV1

; Build the opasm-owned two-pass engine context from host callbacks.
;
; Inputs:
; - A0: OPASM_ENGINE_CALLBACK_REQ_* request buffer.
;
; Outputs:
; - A4: opasm engine context pointer.
; - D0: 0 on success.
opasmEngineBuildCallbackContextV1	.block
	movem.l d1/a0-a1, -(sp)
	lea OpasmEngineContext.l, a1
	move.l #OpasmEngineSessionPass, (a1)+
	move.l #OpasmEngineStmtCount, (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_BIN_REQUESTED_PTR(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_PASS1_BEGIN_CB(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_PASS2_BEGIN_CB(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_PASS1_OK_CB(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_PASS2_OK_CB(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_RECORD_LABEL_CB(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_ADVANCE_PC_CB(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_EMIT_IMAGE_CB(a0), (a1)+
	lea OpasmEngineContext.l, a4
	movem.l (sp)+, d1/a0-a1
	moveq #0, d0
	rts
	.bend  ; opasmEngineBuildCallbackContextV1

; Initialize opasm-owned pass-one state.
;
; Outputs:
; - D0: 0 on success.
opasmEngineBeginPassOneV1	.block
	movem.l d1/a0, -(sp)
	clr.w OpasmEngineLabelCount.l
	lea OpasmEngineLabelFinalizedTable.l, a0
	moveq #NATIVE_LABEL_TABLE_CAPACITY - 1, d0

clearLoop
	clr.b (a0)+
	dbf d0, clearLoop
	clr.w OpasmEngineImageByteCount.l
	move.l #$00000800, OpasmEngineSessionOrigin.l
	move.l OpasmEngineSessionOrigin.l, d1
	move.l d1, OpasmEngineSessionCurrentPc.l
	movem.l (sp)+, d1/a0
	moveq #0, d0
	rts
	.bend  ; opasmEngineBeginPassOneV1

; Initialize opasm-owned pass-two state.
;
; Outputs:
; - D0: 0 on success.
opasmEngineBeginPassTwoV1	.block
	movem.l d1/a0, -(sp)
	moveq #0, d0
	move.w OpasmEngineLabelCount.l, d0
	subq.w #1, d0
	bmi.s finalizeDone
	lea OpasmEngineLabelFinalizedTable.l, a0

finalizeLoop
	move.b #1, (a0)+
	dbf d0, finalizeLoop

finalizeDone
	clr.w OpasmEngineImageByteCount.l
	move.l OpasmEngineSessionOrigin.l, d1
	move.l d1, OpasmEngineSessionCurrentPc.l
	movem.l (sp)+, d1/a0
	moveq #0, d0
	rts
	.bend  ; opasmEngineBeginPassTwoV1

; Record the label attached to one statement during pass one.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: 0 on success/no label, non-zero on duplicate/capacity failure.
; - D1: OPASM_ENGINE_LABEL_EVENT_*.
; - D2: current PC for stored-label diagnostics.
; - A0: label text for stored/duplicate diagnostics.
opasmEngineRecordStatementLabelV1	.block
	movem.l d3-d7/a1-a3, -(sp)
	move.l d0, d7
	lsl.l #6, d7
	lea OpasmEngineStmtLabelNameTable.l, a1
	adda.l d7, a1
	tst.b (a1)
	beq.w noLabel
	moveq #0, d0
	move.w OpasmEngineLabelCount.l, d0
	cmpi.w #NATIVE_LABEL_TABLE_CAPACITY, d0
	bhs.w capacity
	moveq #0, d6

duplicateLoop
	move.w OpasmEngineLabelCount.l, d0
	cmp.w d0, d6
	bhs.s storeLabel
	moveq #0, d5
	move.w d6, d5
	lsl.l #6, d5
	lea OpasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	moveq #0, d0
	move.l d7, d5
	lsr.l #6, d5
	add.w d5, d5
	lea OpasmEngineStmtLabelLenTable.l, a2
	move.w 0(a2, d5.l), d0
	bne.s haveExistingLabelLen
	movea.l a1, a0
	bsr.w tokenLen
	moveq #0, d5
	move.w d6, d5
	lsl.l #6, d5
	lea OpasmEngineLabelNameTable.l, a0
	adda.l d5, a0

haveExistingLabelLen
	bsr.w labelEquals
	bne.w duplicate
	addq.w #1, d6
	bra.s duplicateLoop

storeLabel
	moveq #0, d6
	move.w OpasmEngineLabelCount.l, d6
	move.l d6, d5
	lsl.l #2, d5
	lea OpasmEngineLabelValueTable.l, a0
	move.l OpasmEngineSessionCurrentPc.l, 0(a0, d5.l)
	lea OpasmEngineLabelFinalizedTable.l, a0
	clr.b 0(a0, d6.l)
	move.l d6, d5
	lsl.l #6, d5
	lea OpasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	move.l a0, d4
	movea.l a1, a3
	movea.l a0, a1
	movea.l a3, a0
	moveq #0, d0
	move.l d7, d5
	lsr.l #6, d5
	add.w d5, d5
	lea OpasmEngineStmtLabelLenTable.l, a2
	move.w 0(a2, d5.l), d0
	bne.s haveStoreLabelLen
	movea.l a3, a0
	bsr.w tokenLen
	movea.l a3, a0

haveStoreLabelLen
	bsr.w copyFixedString
	clr.b (a1)
	addq.w #1, OpasmEngineLabelCount.l
	moveq #0, d0
	moveq #OPASM_ENGINE_LABEL_EVENT_STORED, d1
	move.l OpasmEngineSessionCurrentPc.l, d2
	movea.l d4, a0
	bra.s return

duplicate
	moveq #1, d0
	moveq #OPASM_ENGINE_LABEL_EVENT_DUPLICATE, d1
	movea.l a1, a0
	bra.s return

capacity
	moveq #1, d0
	moveq #OPASM_ENGINE_LABEL_EVENT_CAPACITY, d1
	movea.l a1, a0
	bra.s return

noLabel
	moveq #0, d0
	moveq #OPASM_ENGINE_LABEL_EVENT_NONE, d1
	suba.l a0, a0

return
	movem.l (sp)+, d3-d7/a1-a3
	rts
	.bend  ; opasmEngineRecordStatementLabelV1

; Set origin and current PC.
;
; Inputs:
; - D0: new origin/current PC.
;
; Outputs:
; - D0: 0 on success.
opasmEngineSetOriginV1	.block
	move.l d0, OpasmEngineSessionOrigin.l
	move.l d0, OpasmEngineSessionCurrentPc.l
	moveq #0, d0
	rts
	.bend  ; opasmEngineSetOriginV1

; Advance current PC by an encoded byte size.
;
; Inputs:
; - D0: encoded byte size.
;
; Outputs:
; - D0: 0 on success.
opasmEngineAdvancePcBySizeV1	.block
	add.l d0, OpasmEngineSessionCurrentPc.l
	moveq #0, d0
	rts
	.bend  ; opasmEngineAdvancePcBySizeV1

; Append encoded bytes to the opasm-owned image buffer.
;
; Inputs:
; - A0: encoded byte source.
; - D0: encoded byte count.
;
; Outputs:
; - D0: 0 on success, non-zero on image capacity failure.
opasmEngineAppendImageBytesV1	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.w d0, d3
	moveq #0, d1
	move.w OpasmEngineImageByteCount.l, d1
	add.w d3, d1
	cmpi.w #NATIVE_IMAGE_BUFFER_CAPACITY, d1
	bhi.s fail
	moveq #0, d1
	move.w OpasmEngineImageByteCount.l, d1
	lea OpasmEngineImageBuffer.l, a1
	adda.l d1, a1
	move.w d3, d1
	beq.s done

copyLoop
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.s copyLoop

done
	add.w d3, OpasmEngineImageByteCount.l
	movem.l (sp)+, d1-d3/a0-a1
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d1-d3/a0-a1
	moveq #1, d0
	rts
	.bend  ; opasmEngineAppendImageBytesV1

; Return the current opasm-owned image byte count.
;
; Outputs:
; - D0: image byte count.
opasmEngineGetImageByteCountV1	.block
	moveq #0, d0
	move.w OpasmEngineImageByteCount.l, d0
	rts
	.bend  ; opasmEngineGetImageByteCountV1

; Return a pointer to the opasm-owned image buffer.
;
; Outputs:
; - A0: image buffer pointer.
opasmEngineGetImageBufferPtrV1	.block
	lea OpasmEngineImageBuffer.l, a0
	rts
	.bend  ; opasmEngineGetImageBufferPtrV1

; Return a pointer to the opasm-owned session CPU name.
;
; Outputs:
; - A0: CPU name pointer.
opasmEngineGetSessionCpuNamePtrV1	.block
	lea OpasmEngineSessionCpuName.l, a0
	rts
	.bend  ; opasmEngineGetSessionCpuNamePtrV1

; Return the current opasm pass number.
;
; Outputs:
; - D0: pass number.
opasmEngineGetSessionPassV1	.block
	moveq #0, d0
	move.w OpasmEngineSessionPass.l, d0
	rts
	.bend  ; opasmEngineGetSessionPassV1

; Return the current opasm session origin.
;
; Outputs:
; - D0: origin.
opasmEngineGetSessionOriginV1	.block
	move.l OpasmEngineSessionOrigin.l, d0
	rts
	.bend  ; opasmEngineGetSessionOriginV1

; Return the current opasm session PC.
;
; Outputs:
; - D0: current PC.
opasmEngineGetSessionCurrentPcV1	.block
	move.l OpasmEngineSessionCurrentPc.l, d0
	rts
	.bend  ; opasmEngineGetSessionCurrentPcV1

; Return the source record count.
;
; Outputs:
; - D0: source record count.
opasmEngineGetSourceRecordCountV1	.block
	moveq #0, d0
	move.w OpasmEngineSourceRecordCount.l, d0
	rts
	.bend  ; opasmEngineGetSourceRecordCountV1

; Return the statement count.
;
; Outputs:
; - D0: statement count.
opasmEngineGetStatementCountV1	.block
	moveq #0, d0
	move.w OpasmEngineStmtCount.l, d0
	rts
	.bend  ; opasmEngineGetStatementCountV1

; Return the label count.
;
; Outputs:
; - D0: label count.
opasmEngineGetLabelCountV1	.block
	moveq #0, d0
	move.w OpasmEngineLabelCount.l, d0
	rts
	.bend  ; opasmEngineGetLabelCountV1

; Write the opasm-owned expression-evaluation environment extension fields.
;
; Inputs:
; - A1: extension buffer base.
;
; Outputs:
; - D0: 0 on success.
opasmEngineWriteEvaluateExpressionExtensionBaseV1	.block
	movem.l d1/a1, -(sp)
	move.l #OpasmEngineLabelNameTable, (a1)+
	move.l #OpasmEngineLabelValueTable, (a1)+
	moveq #0, d1
	move.w OpasmEngineLabelCount.l, d1
	move.l d1, (a1)+
	move.l OpasmEngineSessionCurrentPc.l, (a1)+
	clr.l (a1)+
	clr.l (a1)
	movem.l (sp)+, d1/a1
	moveq #0, d0
	rts
	.bend  ; opasmEngineWriteEvaluateExpressionExtensionBaseV1

; Prepare the expression-evaluation environment extension.
;
; Inputs:
; - A0: evaluate request buffer.
; - D0: evaluate request byte length.
; - A1: extension buffer base.
;
; Outputs:
; - D0: 0 on success.
; - selected-shape metadata written at the extension tail when inferred.
;
; Clobbers:
; - D0-D1/A0-A1/CCR
;
; CCR:
; - reflects D0 on return.
prepareEvaluateExpressionExtensionV1	.block
	movem.l d1/a0-a1, -(sp)
	jsr opasmEngineWriteEvaluateExpressionExtensionBaseV1
	jsr opasmEngineInferSelectedShapeForEvalRequestV1
	; Keep the explicit length probe: this branches on whether a selected-shape
	; string exists, not on a status-return contract.
	tst.w d0
	beq.s done
	adda.w #16, a1
	move.l a0, (a1)
	move.l d0, 4(a1)

done
	movem.l (sp)+, d1/a0-a1
	moveq #0, d0
	rts
	.bend  ; prepareEvaluateExpressionExtensionV1

; Return the stored source line number for one statement.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: source line number.
opasmEngineGetStatementLineNumberV1	.block
	move.l d1, -(sp)
	moveq #0, d1
	move.w d0, d1
	lsl.l #2, d1
	lea OpasmEngineStmtLineTable.l, a0
	move.l 0(a0, d1.l), d0
	move.l (sp)+, d1
	rts
	.bend  ; opasmEngineGetStatementLineNumberV1

; Return stored source-line text for one statement.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: source-line text length, or 0 when absent.
; - A0: source-line text pointer when D0 is non-zero.
;
; Clobbers:
; - D0-D2/A0/CCR
;
; CCR:
; - reflects D0 on return.
getStatementSourceLineTextV1	.block
	movem.l d1-d2, -(sp)
	moveq #0, d1
	move.w d0, d1
	move.l d1, d2
	add.w d2, d2
	lea OpasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d0
	move.w 0(a0, d2.l), d0
	beq.s fail
	lsl.l #8, d1
	add.l d1, d1
	lea OpasmEngineStmtSourceLineTextTable.l, a0
	adda.l d1, a0
	movem.l (sp)+, d1-d2
	rts

fail
	suba.l a0, a0
	movem.l (sp)+, d1-d2
	rts
	.bend  ; getStatementSourceLineTextV1

; Return a source-line slice for a stored statement expression span.
;
; Inputs:
; - D0: statement index.
; - D1: expression span start column, one-based.
; - D2: expression span end column, one-based exclusive.
;
; Outputs:
; - D0: slice length, or 0 when invalid/unavailable.
; - A0: slice pointer when D0 is non-zero.
opasmEngineGetStatementExprTextSliceV1	.block
	movem.l d1-d4, -(sp)
	moveq #0, d3
	move.w d0, d3
	move.l d3, d4
	add.w d4, d4
	lea OpasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d0
	move.w 0(a0, d4.l), d0
	beq.s fail
	tst.l d1
	beq.s fail
	cmp.l d1, d2
	bls.s fail
	move.l d1, d4
	subq.l #1, d4
	cmp.l d0, d4
	bhs.s fail
	lsl.l #8, d3
	add.l d3, d3
	lea OpasmEngineStmtSourceLineTextTable.l, a0
	adda.l d3, a0
	adda.l d4, a0
	sub.l d4, d0
	move.l d2, d3
	sub.l d1, d3
	cmp.l d0, d3
	bls.s useDesired
	movem.l (sp)+, d1-d4
	rts

useDesired
	move.l d3, d0
	movem.l (sp)+, d1-d4
	rts

fail
	clr.l d0
	suba.l a0, a0
	movem.l (sp)+, d1-d4
	rts
	.bend  ; opasmEngineGetStatementExprTextSliceV1

; Return stored expression metadata for one statement.
;
; Inputs:
; - D0: statement index.
; - A0: OPASM_ENGINE_EXPR_META_* output buffer.
;
; Outputs:
; - D0: 1 when expression metadata exists, 0 when absent.
opasmEngineGetStatementExprMetadataV1	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.w d0, d1
	add.w d1, d1
	lea OpasmEngineStmtExprFlagsTable.l, a1
	tst.w 0(a1, d1.l)
	beq.s empty
	lsr.w #1, d1
	lsl.l #2, d1
	lea OpasmEngineStmtExprOperandIndexTable.l, a1
	move.l 0(a1, d1.l), OPASM_ENGINE_EXPR_META_OPERAND_INDEX(a0)
	lea OpasmEngineStmtExprSlotIndexTable.l, a1
	move.l 0(a1, d1.l), OPASM_ENGINE_EXPR_META_SLOT_INDEX(a0)
	lea OpasmEngineStmtExprStartTokenTable.l, a1
	move.l 0(a1, d1.l), OPASM_ENGINE_EXPR_META_START_TOKEN(a0)
	lea OpasmEngineStmtExprEndTokenTable.l, a1
	move.l 0(a1, d1.l), OPASM_ENGINE_EXPR_META_END_TOKEN(a0)
	lea OpasmEngineStmtExprSpanLineTable.l, a1
	move.l 0(a1, d1.l), OPASM_ENGINE_EXPR_META_SPAN_LINE(a0)
	lea OpasmEngineStmtExprSpanStartTable.l, a1
	move.l 0(a1, d1.l), OPASM_ENGINE_EXPR_META_SPAN_START(a0)
	lea OpasmEngineStmtExprSpanEndTable.l, a1
	move.l 0(a1, d1.l), OPASM_ENGINE_EXPR_META_SPAN_END(a0)
	movem.l (sp)+, d1-d2/a0-a1
	moveq #1, d0
	rts

empty
	moveq #0, d2
	move.l d2, OPASM_ENGINE_EXPR_META_OPERAND_INDEX(a0)
	move.l d2, OPASM_ENGINE_EXPR_META_SLOT_INDEX(a0)
	move.l d2, OPASM_ENGINE_EXPR_META_START_TOKEN(a0)
	move.l d2, OPASM_ENGINE_EXPR_META_END_TOKEN(a0)
	move.l d2, OPASM_ENGINE_EXPR_META_SPAN_LINE(a0)
	move.l d2, OPASM_ENGINE_EXPR_META_SPAN_START(a0)
	move.l d2, OPASM_ENGINE_EXPR_META_SPAN_END(a0)
	movem.l (sp)+, d1-d2/a0-a1
	moveq #0, d0
	rts
	.bend  ; opasmEngineGetStatementExprMetadataV1

; Return whether a statement has stored expression metadata.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: 1 when expression metadata exists, 0 when absent.
;
; Clobbers:
; - A0/CCR.
;
; CCR:
; - Reflects D0 on return. The final wrapper tail uses only CCR-neutral `adda`/`rts`.
statementHasExprMetadataV1	.block
	suba.l #OPASM_ENGINE_EXPR_META_BYTES, sp
	movea.l sp, a0
	jsr opasmEngineGetStatementExprMetadataV1
	adda.l #OPASM_ENGINE_EXPR_META_BYTES, sp
	rts
	.bend  ; statementHasExprMetadataV1

; Return stored mnemonic and operand text metadata for one statement.
;
; Inputs:
; - D0: statement index.
; - A0: OPASM_ENGINE_STMT_TEXT_* output buffer.
;
; Outputs:
; - D0: 0 on success, non-zero when the statement has no mnemonic text.
opasmEngineGetStatementTextMetadataV1	.block
	movem.l d1-d3/a0-a2, -(sp)
	movea.l a0, a2
	moveq #0, d1
	move.w d0, d1
	move.l d1, d2
	lsl.l #6, d2
	lea OpasmEngineStmtMnemNameTable.l, a1
	adda.l d2, a1
	move.l a1, d0
	move.l d0, OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a2)
	move.l d1, d3
	add.w d3, d3
	lea OpasmEngineStmtMnemLenTable.l, a1
	moveq #0, d0
	move.w 0(a1, d3.l), d0
	bne.s haveMnemLen
	movea.l OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a2), a1
	movea.l a1, a0
	bsr.w tokenLen

haveMnemLen
	tst.w d0
	beq.s fail
	move.l d0, OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a2)
	lea OpasmEngineStmtOperandLenTable.l, a1
	moveq #0, d0
	move.w 0(a1, d3.l), d0
	move.l d0, OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(a2)
	lea OpasmEngineStmtOperandNameTable.l, a1
	adda.l d2, a1
	move.l a1, d0
	move.l d0, OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(a2)
	movem.l (sp)+, d1-d3/a0-a2
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d1-d3/a0-a2
	moveq #1, d0
	rts
	.bend  ; opasmEngineGetStatementTextMetadataV1

; Prepare an evaluate-expression request for statement text.
;
; Inputs:
; - A0: operand/expression text.
; - D0: operand/expression text length.
; - D1: statement index.
; - A1: output request buffer.
;
; Outputs:
; - D0: 0 on success, non-zero on failure.
; - D1: request byte length when successful.
prepareEvaluateExpressionRequestV1	.block
	movem.l d2-d7/a0-a5, -(sp)
	suba.l #OPASM_ENGINE_EVAL_REQ_SCRATCH_BYTES, sp
	movea.l a0, a2
	move.l d0, d4
	move.w d1, d7
	movea.l a1, a5
	lea OPASM_ENGINE_EVAL_REQ_TEXT_META(sp), a0
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementTextMetadataV1
	bne.w fail
	move.l OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d6
	cmpi.l #255, d6
	bhi.w fail
	lea OPASM_ENGINE_EVAL_REQ_EXPR_META(sp), a0
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementExprMetadataV1
	move.l d0, d5
	movea.l a5, a1
	tst.l d5
	beq.s useStatementLine
	move.l OPASM_ENGINE_EVAL_REQ_EXPR_SPAN_LINE(sp), d2
	bne.s writeLine

useStatementLine
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementLineNumberV1
	move.l d0, d2

writeLine
	move.l d2, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	tst.l d5
	beq.s syntheticSpan
	move.l OPASM_ENGINE_EVAL_REQ_EXPR_SPAN_START(sp), d2
	move.l OPASM_ENGINE_EVAL_REQ_EXPR_SPAN_END(sp), d3
	bra.s writeSpan

syntheticSpan
	tst.l d4
	bne.s syntheticNonEmptySpan
	clr.l d2
	clr.l d3
	bra.s writeSpan

syntheticNonEmptySpan
	moveq #1, d2
	move.l d4, d3
	addq.l #1, d3

writeSpan
	move.w d2, d0
	move.b d0, (a1)+
	lsr.w #8, d0
	move.b d0, (a1)+
	move.w d3, d0
	move.b d0, (a1)+
	lsr.w #8, d0
	move.b d0, (a1)+
	move.l d6, d3
	move.b d6, (a1)+
	tst.l d6
	beq.s copyOperand
	movea.l OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	move.w d6, d0
	bsr.w copyFixedString

copyOperand
	movea.l a2, a0
	move.w d4, d0
	bsr.w copyFixedString
	move.w d4, d1
	add.w d3, d1
	addi.w #9, d1
	adda.l #OPASM_ENGINE_EVAL_REQ_SCRATCH_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a5
	moveq #0, d0
	rts

fail
	adda.l #OPASM_ENGINE_EVAL_REQ_SCRATCH_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a5
	moveq #1, d0
	rts
	.bend  ; prepareEvaluateExpressionRequestV1

; Prepare an evaluate-expression request for a selected statement.
;
; Inputs:
; - D0: statement index.
; - A1: output request buffer.
;
; Outputs:
; - D0: 0 on success, non-zero on failure.
; - D1: request byte length when successful.
prepareSelectedEvaluateRequestV1	.block
	movem.l d2-d7/a0-a5, -(sp)
	suba.l #OPASM_ENGINE_SELECTED_REQ_SCRATCH_BYTES, sp
	move.w d0, d7
	movea.l a1, a5
	lea OPASM_ENGINE_SELECTED_REQ_TEXT_META(sp), a0
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementTextMetadataV1
	bne.w fail
	move.l OPASM_ENGINE_SELECTED_REQ_MNEM_LEN(sp), d6
	cmpi.l #255, d6
	bhi.w fail
	movea.l OPASM_ENGINE_SELECTED_REQ_OPERAND_PTR(sp), a2
	move.l OPASM_ENGINE_SELECTED_REQ_OPERAND_LEN(sp), d4
	lea OPASM_ENGINE_SELECTED_REQ_EXPR_META(sp), a0
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementExprMetadataV1
	move.l d0, d5
	beq.s syntheticRequest
	move.l OPASM_ENGINE_SELECTED_REQ_EXPR_SPAN_START(sp), d2
	move.l OPASM_ENGINE_SELECTED_REQ_EXPR_SPAN_END(sp), d3
	cmp.l d2, d3
	bls.s syntheticRequest
	moveq #0, d0
	move.w d7, d0
	jsr getStatementSourceLineTextV1
	tst.l d0
	beq.s syntheticRequest
	move.l d0, d1
	move.l d2, d0
	subq.l #1, d0
	cmp.l d1, d0
	bhs.s syntheticRequest
	move.l d3, d0
	subq.l #1, d0
	cmp.l d1, d0
	bhi.s syntheticRequest
	movea.l a0, a2
	move.l d1, d4
	moveq #1, d5
	bra.s buildRequest

syntheticRequest
	moveq #0, d5

buildRequest
	movea.l a5, a1
	tst.l d5
	beq.s useStatementLine
	move.l OPASM_ENGINE_SELECTED_REQ_EXPR_SPAN_LINE(sp), d2
	bne.s writeLine

useStatementLine
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementLineNumberV1
	move.l d0, d2

writeLine
	move.l d2, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	tst.l d5
	beq.s syntheticSpan
	move.l OPASM_ENGINE_SELECTED_REQ_EXPR_SPAN_START(sp), d2
	move.l OPASM_ENGINE_SELECTED_REQ_EXPR_SPAN_END(sp), d3
	bra.s writeSpan

syntheticSpan
	tst.l d4
	bne.s syntheticNonEmptySpan
	clr.l d2
	clr.l d3
	bra.s writeSpan

syntheticNonEmptySpan
	moveq #1, d2
	move.l d4, d3
	addq.l #1, d3

writeSpan
	move.w d2, d0
	move.b d0, (a1)+
	lsr.w #8, d0
	move.b d0, (a1)+
	move.w d3, d0
	move.b d0, (a1)+
	lsr.w #8, d0
	move.b d0, (a1)+
	move.l d6, d3
	move.b d6, (a1)+
	tst.l d6
	beq.s copyOperand
	movea.l OPASM_ENGINE_SELECTED_REQ_MNEM_PTR(sp), a0
	move.w d6, d0
	bsr.w copyFixedString

copyOperand
	movea.l a2, a0
	move.w d4, d0
	bsr.w copyFixedString
	move.w d4, d1
	add.w d3, d1
	addi.w #9, d1
	adda.l #OPASM_ENGINE_SELECTED_REQ_SCRATCH_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a5
	moveq #0, d0
	rts

fail
	adda.l #OPASM_ENGINE_SELECTED_REQ_SCRATCH_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a5
	moveq #1, d0
	rts
	.bend  ; prepareSelectedEvaluateRequestV1

; Prepare an encode-instruction request for a mnemonic.
;
; Inputs:
; - A0: mnemonic text.
; - D0: mnemonic length.
; - A1: output request buffer.
;
; Outputs:
; - D0: 0 on success, non-zero on failure.
; - D1: request byte length when successful.
prepareEncodeInstructionRequestV1	.block
	movem.l d2/a0-a1, -(sp)
	move.l d0, d2
	cmpi.l #255, d2
	bhi.s fail
	move.b d2, (a1)+
	tst.l d2
	beq.s candidateCount
	move.w d2, d0
	bsr.w copyFixedString

candidateCount
	clr.b (a1)+
	move.w d2, d1
	addq.w #2, d1
	movem.l (sp)+, d2/a0-a1
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d2/a0-a1
	moveq #1, d0
	rts
	.bend  ; prepareEncodeInstructionRequestV1

; Infer the selected-shape text for an evaluate request.
;
; Inputs:
; - A0: evaluate request buffer.
; - D0: evaluate request byte length.
;
; Outputs:
; - A0: selected-shape text when D0 is non-zero.
; - D0: selected-shape text length, or 0 when none applies.
;
; Clobbers:
; - D1-D7/A1-A2/CCR
;
; CCR:
; - Reflects D0 on return.
opasmEngineInferSelectedShapeForEvalRequestV1	.block
	movem.l d1-d7/a1-a2, -(sp)
	movea.l a0, a2
	move.w d0, d7
	moveq #0, d0
	move.b 8(a2), d0
	bsr.w inferSelectedShapeBranchMnemonic
	bne.w direct
	movea.l a2, a0
	moveq #0, d0
	move.b 8(a0), d0
	moveq #0, d2
	move.w d7, d2
	subi.w #9, d2
	bcs.w none
	sub.w d0, d2
	bcs.w none
	lea 9(a0, d0.w), a0

trimLeading
	tst.w d2
	beq.w none
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s trimLeadingOne
	cmpi.b #9, d3
	bne.s trimTrailing

trimLeadingOne
	addq.l #1, a0
	subq.w #1, d2
	bra.s trimLeading

trimTrailing
	tst.w d2
	beq.w none
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #' ', d3
	beq.s trimTrailingOne
	cmpi.b #9, d3
	bne.s ready

trimTrailingOne
	subq.w #1, d2
	bra.s trimTrailing

ready
	cmpi.w #1, d2
	bne.s checkPrefix
	move.b (a0), d3
	ori.b #$20, d3
	cmpi.b #'a', d3
	beq.w accumulator

checkPrefix
	move.b (a0), d3
	cmpi.b #'#', d3
	beq.w immediate
	cmpi.b #'(', d3
	beq.w paren
	bsr.w inferSelectedShapeSuffix
	cmpi.b #'x', d0
	beq.w directX
	cmpi.b #'y', d0
	beq.w directY
	bra.w direct

paren
	bsr.w inferSelectedShapeSuffix
	cmpi.b #'y', d0
	beq.w indirectIndexedY
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #')', d3
	bne.w indirect
	cmpi.w #4, d2
	bcs.w indirect
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	ori.b #$20, d3
	cmpi.b #'x', d3
	bne.w indirect
	tst.w d4
	beq.w indirect
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #',', d3
	beq.w indexedIndirectX
	bra.w indirect

accumulator
	lea OpasmEngineSelectedShapeAccumulatorText, a0
	moveq #11, d0
	bra.s return

immediate
	lea OpasmEngineSelectedShapeImmediateText, a0
	moveq #9, d0
	bra.s return

direct
	lea OpasmEngineSelectedShapeDirectText, a0
	moveq #6, d0
	bra.s return

directX
	lea OpasmEngineSelectedShapeDirectXText, a0
	moveq #8, d0
	bra.s return

directY
	lea OpasmEngineSelectedShapeDirectYText, a0
	moveq #8, d0
	bra.s return

indirect
	lea OpasmEngineSelectedShapeIndirectText, a0
	moveq #8, d0
	bra.s return

indexedIndirectX
	lea OpasmEngineSelectedShapeIndexedIndirectXText, a0
	moveq #18, d0
	bra.s return

indirectIndexedY
	lea OpasmEngineSelectedShapeIndirectIndexedYText, a0
	moveq #18, d0
	bra.s return

none
	moveq #0, d0

return
	movem.l (sp)+, d1-d7/a1-a2
	rts
	.bend  ; opasmEngineInferSelectedShapeForEvalRequestV1

; Check whether a statement mnemonic duplicates that statement's label.
;
; Inputs:
; - D0: statement index.
; - D1: mnemonic length.
; - A0: mnemonic text.
;
; Outputs:
; - D0: 1 when mnemonic text equals the statement label, else 0.
opasmEngineStatementMnemonicDuplicatesLabelV1	.block
	movem.l d1-d4/a0-a2, -(sp)
	move.l d0, d2
	add.w d2, d2
	lea OpasmEngineStmtLabelLenTable.l, a2
	moveq #0, d3
	move.w 0(a2, d2.l), d3
	beq.s no
	cmp.w d1, d3
	bne.s no
	move.l d0, d4
	lsl.l #6, d4
	lea OpasmEngineStmtLabelNameTable.l, a1
	adda.l d4, a1
	move.l d1, d0
	bsr.w labelEquals
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; opasmEngineStatementMnemonicDuplicatesLabelV1

; Check whether a statement looks like a bare column-one token.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: 1 when the source text is a bare column-one token, else 0.
opasmEngineStatementLooksBareColumnOneV1	.block
	movem.l d1-d4/a0, -(sp)
	move.l d0, d1
	add.w d1, d1
	lea OpasmEngineStmtOperandLenTable.l, a0
	tst.w 0(a0, d1.l)
	bne.w no
	lea OpasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d4
	move.w 0(a0, d1.l), d4
	beq.w no
	move.l d0, d2
	lsl.l #8, d2
	add.l d2, d2
	lea OpasmEngineStmtSourceLineTextTable.l, a0
	adda.l d2, a0
	move.b (a0), d3
	beq.w no
	cmpi.b #10, d3
	beq.w no
	cmpi.b #13, d3
	beq.w no
	cmpi.b #' ', d3
	beq.w no
	cmpi.b #9, d3
	beq.w no
	cmpi.b #'.', d3
	beq.w no
	cmpi.b #';', d3
	beq.w no

tokenLoop
	tst.l d4
	beq.s yes
	move.b (a0), d3
	beq.s yes
	cmpi.b #10, d3
	beq.s yes
	cmpi.b #13, d3
	beq.s yes
	cmpi.b #';', d3
	beq.s yes
	cmpi.b #' ', d3
	beq.s trailingLoop
	cmpi.b #9, d3
	beq.s trailingLoop
	addq.l #1, a0
	subq.l #1, d4
	bra.s tokenLoop

trailingLoop
	tst.l d4
	beq.s yes
	move.b (a0), d3
	beq.s yes
	cmpi.b #10, d3
	beq.s yes
	cmpi.b #13, d3
	beq.s yes
	cmpi.b #';', d3
	beq.s yes
	cmpi.b #' ', d3
	beq.s trailingOne
	cmpi.b #9, d3
	beq.s trailingOne
	bra.s no

trailingOne
	addq.l #1, a0
	subq.l #1, d4
	bra.s trailingLoop

yes
	moveq #1, d0
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d4/a0
	rts
	.bend  ; opasmEngineStatementLooksBareColumnOneV1

; Inputs:
; - callback context fields in OPASM_ENGINE_CTX_* and callback pointers in A4/A5.
;
; Outputs:
; - D0: 0 on success, non-zero when either pass callback chain reports failure.
;
; Clobbers:
; - D0-D7/A0-A5/CCR
;
; CCR:
; - reflects D0 on return.
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

copyFixedString	.block
	move.w d0, d6
	beq.s done

loop
	move.b (a0)+, (a1)+
	subq.w #1, d6
	bne.s loop

done
	rts
	.bend  ; copyFixedString

inferSelectedShapeSuffix	.block
	moveq #0, d0
	cmpi.w #3, d2
	bcs.s return
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	ori.b #$20, d3
	cmpi.b #'x', d3
	beq.s maybe
	cmpi.b #'y', d3
	bne.s return

maybe
	move.b d3, d0
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #',', d3
	beq.s return
	moveq #0, d0

return
	rts
	.bend  ; inferSelectedShapeSuffix

inferSelectedShapeBranchMnemonic	.block
	cmpi.w #3, d0
	beq.s lenOk
	moveq #0, d0
	rts

lenOk
	lea 9(a2), a1
	move.b (a1)+, d1
	ori.b #$20, d1
	cmpi.b #'b', d1
	beq.s haveB
	moveq #0, d0
	rts

haveB
	move.b (a1)+, d1
	move.b (a1), d2
	ori.b #$20, d1
	ori.b #$20, d2
	cmpi.b #'c', d1
	beq.s checkC
	cmpi.b #'e', d1
	beq.s checkEq
	cmpi.b #'n', d1
	beq.s checkNe
	cmpi.b #'m', d1
	beq.s checkMi
	cmpi.b #'p', d1
	beq.s checkPl
	cmpi.b #'v', d1
	beq.s checkV
	cmpi.b #'r', d1
	beq.s checkRa
	moveq #0, d0
	rts

checkC
	cmpi.b #'c', d2
	beq.s yes
	cmpi.b #'s', d2
	beq.s yes
	moveq #0, d0
	rts

checkEq
	cmpi.b #'q', d2
	beq.s yes
	moveq #0, d0
	rts

checkNe
	cmpi.b #'e', d2
	beq.s yes
	moveq #0, d0
	rts

checkMi
	cmpi.b #'i', d2
	beq.s yes
	moveq #0, d0
	rts

checkPl
	cmpi.b #'l', d2
	beq.s yes
	moveq #0, d0
	rts

checkV
	cmpi.b #'c', d2
	beq.s yes
	cmpi.b #'s', d2
	beq.s yes
	moveq #0, d0
	rts

checkRa
	cmpi.b #'a', d2
	beq.s yes
	moveq #0, d0
	rts

yes
	moveq #1, d0
	rts
	.bend  ; inferSelectedShapeBranchMnemonic

OpasmEngineSelectedShapeAccumulatorText
	.byte "accumulator", 0
OpasmEngineSelectedShapeImmediateText
	.byte "immediate", 0
OpasmEngineSelectedShapeDirectText
	.byte "direct", 0
OpasmEngineSelectedShapeDirectXText
	.byte "direct_x", 0
OpasmEngineSelectedShapeDirectYText
	.byte "direct_y", 0
OpasmEngineSelectedShapeIndirectText
	.byte "indirect", 0
OpasmEngineSelectedShapeIndexedIndirectXText
	.byte "indexed_indirect_x", 0
OpasmEngineSelectedShapeIndirectIndexedYText
	.byte "indirect_indexed_y", 0

tokenLen	.block
	movem.l d1/a0, -(sp)
	moveq #0, d0

loop
	move.b (a0)+, d1
	beq.s done
	addq.w #1, d0
	cmpi.w #TOKEN_BUFFER_CAPACITY, d0
	bhs.s done
	bra.s loop

done
	movem.l (sp)+, d1/a0
	rts
	.bend  ; tokenLen

labelEquals	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.w d0, d3
	beq.s no

loop
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmp.b d1, d2
	bne.s no
	subq.w #1, d3
	bne.s loop
	tst.b (a0)
	bne.s no
	moveq #1, d0
	bra.s done

no
	moveq #0, d0

done
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; labelEquals

skipLineWhitespace	.block
	tst.l d0
	beq.s done
	cmpi.b #' ', (a0)
	beq.s one
	cmpi.b #9, (a0)
	bne.s done

one
	addq.l #1, a0
	subq.l #1, d0
	bra.s skipLineWhitespace

done
	rts
	.bend  ; skipLineWhitespace

copyOperandText	.block
	movem.l d0-d4/a0-a1, -(sp)
	clr.w d5
	move.l #TOKEN_BUFFER_CAPACITY - 1, d4

loop
	tst.l d0
	beq.s done
	moveq #0, d2
	move.b (a0), d2
	beq.s done
	cmpi.b #';', d2
	beq.s done
	cmpi.b #10, d2
	beq.s done
	cmpi.b #13, d2
	beq.s done
	tst.l d4
	beq.s done
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d5
	subq.l #1, d4
	bra.s loop

done
	bsr.w trimOperandText
	clr.b (a1)
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend  ; copyOperandText

trimOperandText	.block
	tst.w d5
	beq.s done
	movea.l a1, a0

loop
	tst.w d5
	beq.s setEnd
	subq.l #1, a0
	move.b (a0), d0
	cmpi.b #' ', d0
	beq.s trimOne
	cmpi.b #9, d0
	beq.s trimOne
	bra.s setEnd

trimOne
	subq.w #1, d5
	bra.s loop

setEnd
	movea.l a0, a1
	addq.l #1, a1

done
	rts
	.bend  ; trimOperandText

storeStatementRecord	.block
	moveq #0, d1
	move.w OpasmEngineStmtCount.l, d1
	lsl.l #2, d1
	lea OpasmEngineStmtLineTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_SOURCE_LINE_NUM(a5), 0(a0, d1.l)
	lea OpasmEngineStmtMnemOffTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_MNEM_OFF(a5), 0(a0, d1.l)
	moveq #0, d2
	move.w OpasmEngineStmtCount.l, d2
	add.w d2, d2
	lea OpasmEngineStmtSourceLineLenTable.l, a0
	clr.w 0(a0, d2.l)
	lea OpasmEngineStmtLabelLenTable.l, a0
	move.w OPASM_ENGINE_STMT_REQ_LABEL_LEN_WORD(a5), 0(a0, d2.l)
	lea OpasmEngineStmtMnemLenTable.l, a0
	move.w OPASM_ENGINE_STMT_REQ_MNEM_LEN_WORD(a5), 0(a0, d2.l)
	lea OpasmEngineStmtOperandLenTable.l, a0
	clr.w 0(a0, d2.l)
	lea OpasmEngineStmtDirectiveKindTable.l, a0
	move.w OPASM_ENGINE_STMT_REQ_DIRECTIVE_KIND(a5), 0(a0, d2.l)
	lea OpasmEngineStmtExprFlagsTable.l, a0
	clr.w 0(a0, d2.l)
	lea OpasmEngineStmtExprOperandIndexTable.l, a0
	clr.l 0(a0, d1.l)
	lea OpasmEngineStmtExprSlotIndexTable.l, a0
	clr.l 0(a0, d1.l)
	lea OpasmEngineStmtExprStartTokenTable.l, a0
	clr.l 0(a0, d1.l)
	lea OpasmEngineStmtExprEndTokenTable.l, a0
	clr.l 0(a0, d1.l)
	lea OpasmEngineStmtExprSpanLineTable.l, a0
	clr.l 0(a0, d1.l)
	lea OpasmEngineStmtExprSpanStartTable.l, a0
	clr.l 0(a0, d1.l)
	lea OpasmEngineStmtExprSpanEndTable.l, a0
	clr.l 0(a0, d1.l)
	moveq #0, d3
	move.w OpasmEngineStmtCount.l, d3
	lsl.l #6, d3
	moveq #0, d4
	move.w OpasmEngineStmtCount.l, d4
	lsl.l #8, d4
	add.l d4, d4
	lea OpasmEngineStmtSourceLineTextTable.l, a1
	adda.l d4, a1
	clr.b (a1)
	moveq #0, d0
	move.w OPASM_ENGINE_STMT_REQ_SOURCE_LINE_LEN(a5), d0
	beq.s sourceLineDone
	cmp.l #SOURCE_LINE_BUFFER_CAPACITY - 1, d0
	bls.s sourceLineLenOk
	move.l #SOURCE_LINE_BUFFER_CAPACITY - 1, d0

sourceLineLenOk
	movea.l a3, a0
	move.l d2, -(sp)
	bsr.w copyFixedString
	move.l (sp)+, d2
	clr.b (a1)
	lea OpasmEngineStmtSourceLineLenTable.l, a0
	move.w d0, 0(a0, d2.l)

sourceLineDone
	lea OpasmEngineStmtLabelNameTable.l, a1
	adda.l d3, a1
	clr.b (a1)
	move.l OPASM_ENGINE_STMT_REQ_LABEL_LEN(a5), d0
	beq.s mnemText
	move.l OPASM_ENGINE_STMT_REQ_LABEL_START(a5), d1
	beq.s mnemText
	subq.l #1, d1
	movea.l a3, a0
	adda.l d1, a0
	bsr.w copyFixedString
	clr.b (a1)

mnemText
	lea OpasmEngineStmtMnemNameTable.l, a1
	adda.l d3, a1
	clr.b (a1)
	move.l OPASM_ENGINE_STMT_REQ_MNEM_LEN(a5), d0
	beq.w done
	movea.l a4, a0
	adda.l OPASM_ENGINE_STMT_REQ_MNEM_OFF(a5), a0
	bsr.w copyFixedString
	clr.b (a1)

operandText
	lea OpasmEngineStmtOperandNameTable.l, a1
	adda.l d3, a1
	clr.b (a1)
	move.l OPASM_ENGINE_STMT_REQ_MNEM_START(a5), d0
	bne.s operandFallback
	move.l OPASM_ENGINE_STMT_REQ_OPERAND_START(a5), d0
	beq.s operandFallback
	move.l OPASM_ENGINE_STMT_REQ_OPERAND_END(a5), d1
	cmp.l d0, d1
	bls.s operandFallback
	move.l d0, d2
	subq.l #1, d2
	sub.l d0, d1
	movea.l a3, a0
	adda.l d2, a0
	move.l d1, d0
	bsr.w copyOperandText
	moveq #0, d0
	move.w OpasmEngineStmtCount.l, d0
	add.w d0, d0
	lea OpasmEngineStmtOperandLenTable.l, a0
	move.w d5, 0(a0, d0.l)
	bra.s exprMetadata

operandFallback
	move.l OPASM_ENGINE_STMT_REQ_MNEM_START(a5), d0
	beq.w exprMetadata
	move.l OPASM_ENGINE_STMT_REQ_MNEM_LEN(a5), d2
	beq.w exprMetadata
	add.l d2, d0
	beq.w exprMetadata
	moveq #0, d1
	move.w OPASM_ENGINE_STMT_REQ_SOURCE_LINE_LEN(a5), d1
	cmp.l d1, d0
	bhs.w exprMetadata
	movea.l a3, a0
	adda.l d0, a0
	sub.l d0, d1
	move.l d1, d0
	bsr.w skipLineWhitespace
	bsr.w copyOperandText
	moveq #0, d0
	move.w OpasmEngineStmtCount.l, d0
	add.w d0, d0
	lea OpasmEngineStmtOperandLenTable.l, a0
	move.w d5, 0(a0, d0.l)

exprMetadata
	tst.w OPASM_ENGINE_STMT_REQ_EXPR_FOUND(a5)
	beq.w done
	moveq #0, d1
	move.w OpasmEngineStmtCount.l, d1
	lsl.l #2, d1
	moveq #0, d2
	move.w OpasmEngineStmtCount.l, d2
	add.w d2, d2
	lea OpasmEngineStmtExprFlagsTable.l, a0
	move.w #1, 0(a0, d2.l)
	lea OpasmEngineStmtExprOperandIndexTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_EXPR_OPERAND_INDEX(a5), 0(a0, d1.l)
	lea OpasmEngineStmtExprSlotIndexTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_EXPR_SLOT_INDEX(a5), 0(a0, d1.l)
	lea OpasmEngineStmtExprStartTokenTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_EXPR_START_TOKEN(a5), 0(a0, d1.l)
	lea OpasmEngineStmtExprEndTokenTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_EXPR_END_TOKEN(a5), 0(a0, d1.l)
	lea OpasmEngineStmtExprSpanLineTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_EXPR_SPAN_LINE(a5), 0(a0, d1.l)
	lea OpasmEngineStmtExprSpanStartTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_EXPR_SPAN_START(a5), 0(a0, d1.l)
	lea OpasmEngineStmtExprSpanEndTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_EXPR_SPAN_END(a5), 0(a0, d1.l)

done
	rts
	.bend  ; storeStatementRecord

; Inputs:
; - pass-one callback pointers and session context in OPASM_ENGINE_CTX_*.
;
; Outputs:
; - D0: 0 on success, non-zero when any pass-one callback reports failure.
;
; Clobbers:
; - D0/D7/A0/CCR
;
; CCR:
; - reflects D0 on return.
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

; Inputs:
; - pass-two callback pointers and session context in OPASM_ENGINE_CTX_*.
;
; Outputs:
; - D0: 0 on success, non-zero when any pass-two callback reports failure.
;
; Clobbers:
; - D0/D7/A0/CCR
;
; CCR:
; - reflects D0 on return.
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

OpasmEngineContext
	.res long, OPASM_ENGINE_CONTEXT_LONGS
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
