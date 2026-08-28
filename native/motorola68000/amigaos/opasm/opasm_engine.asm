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
NATIVE_STATEMENT_TABLE_CAPACITY = NATIVE_SOURCE_RECORD_CAPACITY
NATIVE_LABEL_TABLE_CAPACITY     = NATIVE_SOURCE_RECORD_CAPACITY
NATIVE_IMAGE_BUFFER_CAPACITY    = 65535
NATIVE_SOURCE_TEXT_BYTES        = NATIVE_SOURCE_RECORD_CAPACITY * SOURCE_LINE_BUFFER_CAPACITY
OPASM_ENGINE_LAYOUT_PASS_LIMIT  = 8
OPASM_ENGINE_CONTEXT_LONGS      = 12
; Exact byte count from OpasmEngineAssemblySessionStart through the two image
; buffers. Keep this explicit: the native bootstrap must not depend on a
; forward label subtraction while forward-reference stability is under proof.
OPASM_ENGINE_ASSEMBLY_SESSION_BYTES = 862302

	.section code

OPASM_ENGINE_CTX_SESSION_PASS_PTR = 0
OPASM_ENGINE_CTX_STMT_COUNT_PTR   = 4
OPASM_ENGINE_CTX_BIN_REQUESTED_PTR = 8
OPASM_ENGINE_CTX_PASS1_BEGIN_CB   = 12
OPASM_ENGINE_CTX_PASS2_BEGIN_CB   = 16
OPASM_ENGINE_CTX_PASS1_OK_CB      = 20
OPASM_ENGINE_CTX_PASS2_OK_CB      = 24
OPASM_ENGINE_CTX_RECORD_LABEL_CB  = 28
OPASM_ENGINE_CTX_REFRESH_LABEL_CB = 32
OPASM_ENGINE_CTX_ADVANCE_PC_CB    = 36
OPASM_ENGINE_CTX_FLOW_CONTROL_CB  = 40
OPASM_ENGINE_CTX_EMIT_IMAGE_CB    = 44
OPASM_ENGINE_CALLBACK_REQ_BIN_REQUESTED_PTR = 0
OPASM_ENGINE_CALLBACK_REQ_PASS1_BEGIN_CB = 4
OPASM_ENGINE_CALLBACK_REQ_PASS2_BEGIN_CB = 8
OPASM_ENGINE_CALLBACK_REQ_PASS1_OK_CB = 12
OPASM_ENGINE_CALLBACK_REQ_PASS2_OK_CB = 16
OPASM_ENGINE_CALLBACK_REQ_RECORD_LABEL_CB = 20
OPASM_ENGINE_CALLBACK_REQ_REFRESH_LABEL_CB = 24
OPASM_ENGINE_CALLBACK_REQ_ADVANCE_PC_CB = 28
OPASM_ENGINE_CALLBACK_REQ_FLOW_CONTROL_CB = 32
OPASM_ENGINE_CALLBACK_REQ_EMIT_IMAGE_CB = 36
OPASM_ENGINE_CALLBACK_REQ_BYTES = 40
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
OPASM_ENGINE_STMT_REQ_OWNER_PTR        = 68
OPASM_ENGINE_STMT_REQ_OWNER_LEN        = 72
OPASM_ENGINE_STMT_REQ_OWNER_LEN_WORD   = 74
OPASM_ENGINE_STMT_RECORD_REQUEST_BYTES = 76
; CPU-neutral parser/assembler statement kinds. Structural kinds are consumed
; before instruction selection, matching Rust's directive-first line pipeline.
OPASM_ENGINE_STMT_KIND_NONE       = 0
OPASM_ENGINE_STMT_KIND_MODULE     = 1
OPASM_ENGINE_STMT_KIND_ENDMODULE  = 2
OPASM_ENGINE_STMT_KIND_USE        = 3
OPASM_ENGINE_STMT_KIND_GENERIC    = 4
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

; Update the opasm-owned session CPU name without resetting statement state.
;
; Inputs:
; - A0: null-terminated CPU name to copy into the session.
;
; Outputs:
; - D0: 0 on success.
setSessionCpuNameV1	.block
	movem.l d1/a0-a1, -(sp)
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
	.bend  ; setSessionCpuNameV1

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
; - A0: source line bytes.
; - D0: source line number.
; - D1: source line length.
;
; Outputs:
; - D0: 0 on success.
opasmEngineRecordSourceLineV1	.block
	movem.l d2-d4/a0-a2, -(sp)
	movea.l a0, a2
	move.l d1, d4
	cmp.l #SOURCE_LINE_BUFFER_CAPACITY - 1, d4
	bls.s lengthReady
	move.l #SOURCE_LINE_BUFFER_CAPACITY - 1, d4

lengthReady
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
	move.w d4, 0(a0, d2.l)
	moveq #0, d2
	move.w OpasmEngineSourceRecordCount.l, d2
	lsl.l #8, d2
	add.l d2, d2
	lea OpasmEngineSourceLineTextTable.l, a1
	adda.l d2, a1
	move.l d4, d3

copyReady
	move.l d3, d1
	beq.s copyDone
	subq.l #1, d1

copyLoop
	move.b (a2)+, (a1)+
	dbra d1, copyLoop

copyDone
	clr.b (a1)
	move.w OpasmEngineSourceRecordCount.l, d2
	addq.w #1, d2
	move.w d2, OpasmEngineSourceRecordCount.l

done
	movem.l (sp)+, d2-d4/a0-a2
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
	move.l OPASM_ENGINE_STMT_REQ_OWNER_LEN(a5), d0
	cmp.l #TOKEN_BUFFER_CAPACITY - 1, d0
	bhi.w fail
	tst.l d0
	beq.s ownerRequestReady
	tst.l OPASM_ENGINE_STMT_REQ_OWNER_PTR(a5)
	beq.w fail
ownerRequestReady
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
	move.w OpasmEngineStmtCount.l, d0
	addq.w #1, d0
	move.w d0, OpasmEngineStmtCount.l
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
	move.l OPASM_ENGINE_CALLBACK_REQ_REFRESH_LABEL_CB(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_ADVANCE_PC_CB(a0), (a1)+
	move.l OPASM_ENGINE_CALLBACK_REQ_FLOW_CONTROL_CB(a0), (a1)+
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
	move.w #-1, OpasmEngineLastResolvedLabelIndex.l
	lea OpasmEngineLabelFinalizedTable.l, a0
	lea OpasmEngineLabelAbsoluteConstantTable.l, a1
	move.w #NATIVE_LABEL_TABLE_CAPACITY - 1, d0

clearLoop
	clr.b (a0)+
	clr.b (a1)+
	dbf d0, clearLoop
	clr.w OpasmEngineImageByteCount.l
	clr.l OpasmEngineImageWriteOffset.l
	clr.l OpasmEngineSessionOrigin.l
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
	clr.w OpasmEngineLayoutChanged.l
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
	clr.w OpasmEngineMappedImageByteCount.l
	clr.w OpasmEngineImageRoute.l
	clr.l OpasmEngineImageWriteOffset.l
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
	add.l d5, d5
	lea OpasmEngineLabelStatementIndexTable.l, a0
	move.l d7, d0
	lsr.l #6, d0
	move.w d0, 0(a0, d5.l)
	lea OpasmEngineLabelPcBackedTable.l, a0
	move.b #1, 0(a0, d6.l)
	lea OpasmEngineLabelAbsoluteConstantTable.l, a0
	clr.b 0(a0, d6.l)
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
	move.w OpasmEngineLabelCount.l, d5
	addq.w #1, d5
	move.w d5, OpasmEngineLabelCount.l
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

; Record or update the value-backed label attached to one symbol directive.
;
; Inputs:
; - D0: statement index.
; - D3: symbol value.
; - D4.W: non-zero to update an existing symbol value instead of reporting a
;         duplicate.
;
; Outputs:
; - D0: 0 on success/no label, non-zero on duplicate/capacity failure.
; - D1: OPASM_ENGINE_LABEL_EVENT_*.
; - D2: stored value for stored-label diagnostics.
; - A0: label text for stored/duplicate diagnostics.
opasmEngineRecordStatementLabelValueV1	.block
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
	add.l d5, d5
	lea OpasmEngineLabelStatementIndexTable.l, a0
	move.l d7, d0
	lsr.l #6, d0
	move.w d0, 0(a0, d5.l)
	lea OpasmEngineLabelPcBackedTable.l, a0
	clr.b 0(a0, d6.l)
	lea OpasmEngineLabelAbsoluteConstantTable.l, a0
	clr.b 0(a0, d6.l)
	move.l d6, d5
	lsl.l #2, d5
	lea OpasmEngineLabelValueTable.l, a0
	move.l d3, 0(a0, d5.l)
	lea OpasmEngineLabelFinalizedTable.l, a0
	move.b #1, 0(a0, d6.l)
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
	move.w OpasmEngineLabelCount.l, d5
	addq.w #1, d5
	move.w d5, OpasmEngineLabelCount.l
	moveq #0, d0
	moveq #OPASM_ENGINE_LABEL_EVENT_STORED, d1
	move.l d3, d2
	movea.l d4, a0
	bra.s return

duplicate
	tst.w d4
	bne.s updateExisting
	moveq #1, d0
	moveq #OPASM_ENGINE_LABEL_EVENT_DUPLICATE, d1
	movea.l a1, a0
	bra.s return

updateExisting
	moveq #0, d5
	move.w d6, d5
	move.l d5, d0
	add.l d0, d0
	lea OpasmEngineLabelStatementIndexTable.l, a0
	move.l d7, d1
	lsr.l #6, d1
	move.w d1, 0(a0, d0.l)
	lea OpasmEngineLabelPcBackedTable.l, a0
	clr.b 0(a0, d5.l)
	lea OpasmEngineLabelAbsoluteConstantTable.l, a0
	clr.b 0(a0, d5.l)
	lsl.l #2, d5
	lea OpasmEngineLabelValueTable.l, a0
	move.l d3, 0(a0, d5.l)
	lea OpasmEngineLabelFinalizedTable.l, a0
	move.b #1, 0(a0, d6.l)
	moveq #0, d5
	move.w d6, d5
	lsl.l #6, d5
	lea OpasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	moveq #0, d0
	moveq #OPASM_ENGINE_LABEL_EVENT_STORED, d1
	move.l d3, d2
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
	.bend  ; opasmEngineRecordStatementLabelValueV1

; Resolve one exact label token from the opasm-owned symbol table.
;
; Inputs:
; - A0: label token text.
; - D0: label token byte length.
;
; Outputs:
; - D0: 0 on success, 1 when no label matches.
; - D3: label value on success.
opasmEngineResolveLabelValueV1	.block
	movem.l d1-d2/d4-d6/a0-a2, -(sp)
	movea.l a0, a2
	move.l d0, d6
	clr.w d4

loop
	cmp.w OpasmEngineLabelCount.l, d4
	bhs.s fail
	moveq #0, d5
	move.w d4, d5
	lsl.l #6, d5
	lea OpasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	movea.l a2, a1
	move.l d6, d0
	bsr.w labelEquals
	bne.s found
	addq.w #1, d4
	bra.s loop

found
	move.w d4, OpasmEngineLastResolvedLabelIndex.l
	moveq #0, d5
	move.w d4, d5
	lsl.l #2, d5
	lea OpasmEngineLabelValueTable.l, a0
	move.l 0(a0, d5.l), d3
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d6/a0-a2
	rts
	.bend  ; opasmEngineResolveLabelValueV1

; Resolve exactly one stored label by the final component of an already
; authorized qualified token. Ambiguity is a hard miss.
; Inputs: A0/D0 = authorized token. Outputs: D0 = status; D3 = value.
opasmEngineResolveUniqueLabelFinalComponentV1	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	movea.l a0, a2
	move.l d0, d6
	movea.l a0, a1
	move.l d0, d5
authorizedComponentScan
	tst.l d5
	beq.s authorizedComponentReady
	cmpi.b #'.', (a1)+
	bne.s authorizedComponentNext
	movea.l a1, a2
	move.l d5, d6
	subq.l #1, d6
authorizedComponentNext
	subq.l #1, d5
	bra.s authorizedComponentScan
authorizedComponentReady
	tst.l d6
	beq.w uniqueComponentFail
	moveq #-1, d7
	moveq #0, d4
uniqueLabelLoop
	cmp.w OpasmEngineLabelCount.l, d4
	bhs.s uniqueLabelDone
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea OpasmEngineLabelNameTable.l, a0
	adda.l d0, a0
	bsr.w tokenLen
	movea.l a0, a3
	movea.l a0, a1
	move.l d0, d2
	move.l d0, d5
storedComponentScan
	tst.l d5
	beq.s storedComponentReady
	cmpi.b #'.', (a1)+
	bne.s storedComponentNext
	movea.l a1, a3
	move.l d5, d2
	subq.l #1, d2
storedComponentNext
	subq.l #1, d5
	bra.s storedComponentScan
storedComponentReady
	cmp.l d6, d2
	bne.s uniqueLabelNext
	movea.l a2, a0
	movea.l a3, a1
	move.l d6, d5
uniqueComponentCompare
	move.b (a0)+, d1
	cmp.b (a1)+, d1
	bne.s uniqueLabelNext
	subq.l #1, d5
	bne.s uniqueComponentCompare
	tst.l d7
	bpl.s uniqueComponentFail
	move.w d4, d7
uniqueLabelNext
	addq.w #1, d4
	bra.s uniqueLabelLoop
uniqueLabelDone
	tst.l d7
	bmi.s uniqueComponentFail
	move.w d7, OpasmEngineLastResolvedLabelIndex.l
	move.l d7, d0
	lsl.l #2, d0
	lea OpasmEngineLabelValueTable.l, a0
	move.l 0(a0, d0.l), d3
	moveq #0, d0
	bra.s uniqueComponentReturn
uniqueComponentFail
	moveq #1, d0
uniqueComponentReturn
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; opasmEngineResolveUniqueLabelFinalComponentV1

; Reset or read the label index most recently resolved by this session.
opasmEngineResetLastResolvedLabelV1	.block
	move.w #-1, OpasmEngineLastResolvedLabelIndex.l
	moveq #0, d0
	rts
	.bend  ; opasmEngineResetLastResolvedLabelV1

opasmEngineGetLastResolvedLabelV1	.block
	moveq #0, d0
	move.w OpasmEngineLastResolvedLabelIndex.l, d0
	rts
	.bend  ; opasmEngineGetLastResolvedLabelV1

; Report Rust's target-reference property for the most recently resolved
; symbol.  PC-backed labels are targets; const/var/set labels are scalar
; values and therefore remain position-independent.
; Outputs: D0 = 1 for a PC-backed resolved label, otherwise 0.
; @opforge-owner: opasm.amigaos.engine
; @opforge-slice: documentation/plans/slices/native-porting-slice-motorola68000-reference-matrix-v1.toml
; @opforge-role: context
opasmEngineLastResolvedLabelIsTargetReferenceV1	.block
	moveq #0, d0
	move.w OpasmEngineLastResolvedLabelIndex.l, d0
	cmpi.w #-1, d0
	beq.s notTarget
	cmp.w OpasmEngineLabelCount.l, d0
	bhs.s notTarget
	lea OpasmEngineLabelPcBackedTable.l, a0
	moveq #0, d1
	move.b 0(a0, d0.w), d1
	move.l d1, d0
	rts
notTarget
	moveq #0, d0
	rts
	.bend  ; opasmEngineLastResolvedLabelIsTargetReferenceV1

; Retain and query Rust's absolute-constant provenance for a value-backed
; symbol. The expression owner computes the property from the complete source
; expression after pass-one labels have been finalized.
; @opforge-owner: opasm.amigaos.engine
; @opforge-slice: documentation/plans/slices/native-porting-slice-hunk-fixup-relocation-v1.toml
; @opforge-role: context
; Inputs: D0.W = label index; D1.B = zero/nonzero property.
; Outputs: D0 = 0 on success, 1 for an invalid or PC-backed label.
; Clobbers: D0-D2/A0/CCR.
; CCR: reflects D0 on return.
opasmEngineSetLabelAbsoluteConstantV1	.block
	moveq #0, d2
	move.w d0, d2
	cmp.w OpasmEngineLabelCount.l, d2
	bhs.s absoluteSetFail
	lea OpasmEngineLabelPcBackedTable.l, a0
	tst.b 0(a0, d2.l)
	bne.s absoluteSetFail
	lea OpasmEngineLabelAbsoluteConstantTable.l, a0
	move.b d1, 0(a0, d2.l)
	moveq #0, d0
	rts
absoluteSetFail
	moveq #1, d0
	rts
	.bend  ; opasmEngineSetLabelAbsoluteConstantV1

; Inputs: D0.W = label index. Outputs: D0 = 1 only for a retained absolute
; constant, otherwise 0. Clobbers: D0-D1/A0/CCR. CCR reflects D0.
; @opforge-owner: opasm.amigaos.engine
; @opforge-slice: documentation/plans/slices/native-porting-slice-hunk-fixup-relocation-v1.toml
; @opforge-role: context
opasmEngineLabelIsAbsoluteConstantV1	.block
	moveq #0, d1
	move.w d0, d1
	cmp.w OpasmEngineLabelCount.l, d1
	bhs.s absoluteNo
	lea OpasmEngineLabelAbsoluteConstantTable.l, a0
	moveq #0, d0
	move.b 0(a0, d1.l), d0
	rts
absoluteNo
	moveq #0, d0
	rts
	.bend  ; opasmEngineLabelIsAbsoluteConstantV1

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

; Lower the retained image origin without changing the current source PC.
; Pass one uses this when a later `.org` precedes the first observed origin;
; pass two then starts at the finalized minimum address.
; @opforge-owner: opasm.amigaos.image
; @opforge-slice: documentation/plans/slices/native-porting-slice-overlapping-origin-image.toml
; @opforge-role: implementation
; Inputs: D0 = new image origin. Outputs: D0 = 0.
opasmEngineSetImageOriginV1	.block
	move.l d0, OpasmEngineSessionOrigin.l
	moveq #0, d0
	rts
	.bend  ; opasmEngineSetImageOriginV1

; Set current PC while preserving the session origin.
;
; Inputs:
; - D0: new current PC.
;
; Outputs:
; - D0: 0 on success.
opasmEngineSetCurrentPcV1	.block
	move.l d0, OpasmEngineSessionCurrentPc.l
	moveq #0, d0
	rts
	.bend  ; opasmEngineSetCurrentPcV1

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
; @opforge-owner: opasm.amigaos.image
; @opforge-slice: documentation/plans/slices/native-porting-slice-overlapping-origin-image.toml
; @opforge-role: implementation
;
; Inputs:
; - A0: encoded byte source.
; - D0: encoded byte count.
;
; Outputs:
; - D0: 0 on success, non-zero on image capacity failure.
opasmEngineAppendImageBytesV1	.block
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d3
	move.w d0, d3
	cmpi.l #NATIVE_IMAGE_BUFFER_CAPACITY, d3
	bhi.w fail
	move.w OpasmEngineImageRoute.l, d1
	cmpi.w #1, d1
	beq.w success
	tst.l d3
	beq.w success
	cmpi.w #2, d1
	beq.w mapped
	move.l OpasmEngineImageWriteOffset.l, d1
	move.l d1, d2
	add.l d3, d2
	cmpi.l #NATIVE_IMAGE_BUFFER_CAPACITY, d2
	bhi.w fail
	; A forward origin leaves an address gap. Clear only the newly exposed
	; range so bytes from a prior assembly can never become current evidence.
	moveq #0, d3
	move.w OpasmEngineImageByteCount.l, d3
	cmp.l d3, d1
	bls.s mainCopyReady
	lea OpasmEngineImageBuffer.l, a1
	adda.l d3, a1
	sub.l d3, d1
mainGapLoop
	clr.b (a1)+
	subq.l #1, d1
	bne.s mainGapLoop
	move.l OpasmEngineImageWriteOffset.l, d1
mainCopyReady
	lea OpasmEngineImageBuffer.l, a1
	adda.l d1, a1
	moveq #0, d3
	move.w d0, d3
	move.w d3, d1
	beq.s done

copyLoop
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.s copyLoop

done
	add.l d3, OpasmEngineImageWriteOffset.l
	move.l OpasmEngineImageWriteOffset.l, d1
	moveq #0, d2
	move.w OpasmEngineImageByteCount.l, d2
	cmp.l d2, d1
	bls.w success
	move.w d1, OpasmEngineImageByteCount.l
	bra.w success

mapped
	moveq #0, d1
	move.w OpasmEngineMappedImageByteCount.l, d1
	add.l d3, d1
	cmpi.l #NATIVE_IMAGE_BUFFER_CAPACITY, d1
	bhi.w fail
	moveq #0, d1
	move.w OpasmEngineMappedImageByteCount.l, d1
	lea OpasmEngineMappedImageBuffer.l, a1
	adda.l d1, a1
	move.w d3, d1
	beq.s mappedDone

mappedCopyLoop
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.s mappedCopyLoop

mappedDone
	add.w d3, OpasmEngineMappedImageByteCount.l

success
	movem.l (sp)+, d1-d3/a0-a1
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d1-d3/a0-a1
	moveq #1, d0
	rts
	.bend  ; opasmEngineAppendImageBytesV1

; Select where subsequent statement bytes are emitted.
;
; Inputs: D0.W = 0 main image, 1 discard, 2 mapped logical tail.
; Outputs: D0.L = 0 on success, 1 for an invalid route.
opasmEngineSetImageRouteV1	.block
	cmpi.w #2, d0
	bhi.s fail
	move.w d0, OpasmEngineImageRoute.l
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opasmEngineSetImageRouteV1

; Append the retained reachable logical-section tail to the main image.
;
; Outputs: D0.L = 0 on success, 1 on main-image capacity failure.
opasmEngineFlushMappedImageV1	.block
	clr.w OpasmEngineImageRoute.l
	moveq #0, d0
	move.w OpasmEngineImageByteCount.l, d0
	move.l d0, OpasmEngineImageWriteOffset.l
	lea OpasmEngineMappedImageBuffer.l, a0
	moveq #0, d0
	move.w OpasmEngineMappedImageByteCount.l, d0
	jsr opasmEngineAppendImageBytesV1
	rts
	.bend  ; opasmEngineFlushMappedImageV1

; Return the defining statement index retained for a label.
;
; Inputs: D0.W = label index.
; Outputs: D0.L = statement index, or $0000ffff for an invalid label index.
opasmEngineGetLabelStatementIndexV1	.block
	moveq #0, d1
	move.w d0, d1
	cmp.w OpasmEngineLabelCount.l, d1
	bhs.s invalid
	add.w d1, d1
	lea OpasmEngineLabelStatementIndexTable.l, a0
	moveq #0, d0
	move.w 0(a0, d1.w), d0
	rts
invalid
	moveq #0, d0
	move.w #$ffff, d0
	rts
	.bend  ; opasmEngineGetLabelStatementIndexV1

; Mark the image byte offset/address where one statement starts emitting bytes.
; @opforge-owner: opasm.amigaos.image
; @opforge-slice: documentation/plans/slices/native-porting-slice-overlapping-origin-image.toml
; @opforge-role: implementation
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: 0 on success.
opasmEngineBeginStatementOutputV1	.block
	movem.l d1-d2/a0, -(sp)
	moveq #0, d1
	move.w d0, d1
	lsl.l #2, d1
	move.l OpasmEngineSessionCurrentPc.l, d2
	sub.l OpasmEngineSessionOrigin.l, d2
	move.l d2, OpasmEngineImageWriteOffset.l
	lea OpasmEngineStmtOutputOffsetTable.l, a0
	move.l d2, 0(a0, d1.l)
	lea OpasmEngineStmtOutputByteCountTable.l, a0
	clr.l 0(a0, d1.l)
	move.l OpasmEngineSessionCurrentPc.l, d2
	lea OpasmEngineStmtOutputAddrTable.l, a0
	move.l d2, 0(a0, d1.l)
	movem.l (sp)+, d1-d2/a0
	moveq #0, d0
	rts
	.bend  ; opasmEngineBeginStatementOutputV1

; Mark the image byte count emitted by one statement.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: 0 on success.
opasmEngineEndStatementOutputV1	.block
	movem.l d1-d2/a0, -(sp)
	moveq #0, d1
	move.w d0, d1
	lsl.l #2, d1
	move.l OpasmEngineImageWriteOffset.l, d2
	lea OpasmEngineStmtOutputOffsetTable.l, a0
	sub.l 0(a0, d1.l), d2
	lea OpasmEngineStmtOutputByteCountTable.l, a0
	move.l d2, 0(a0, d1.l)
	movem.l (sp)+, d1-d2/a0
	moveq #0, d0
	rts
	.bend  ; opasmEngineEndStatementOutputV1

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

; Return whether the current statement was reached by a flow-control redirect.
; Outputs: D0.W = 1 after a skipped/jumped callback transition, 0 after ordinary
; sequential processing.
opasmEngineGetFlowRedirectedV1	.block
	moveq #0, d0
	move.w OpasmEngineFlowRedirected.l, d0
	rts
	.bend  ; opasmEngineGetFlowRedirectedV1

; Record the next statement selected by the flow-control callback.
; Inputs: D0.W = next statement index.
opasmEngineSetFlowNextV1	.block
	move.w d0, OpasmEngineFlowNext.l
	move.w #1, OpasmEngineFlowPending.l
	rts
	.bend  ; opasmEngineSetFlowNextV1

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

; Restore the observable collection state to an earlier bounded checkpoint.
; Inputs: D0.W = source records; D1.W = statements; D2.W = image bytes;
;         D3.L = current PC. Each count must not exceed its current value.
; Outputs: D0 = 0 on success, 1 when the checkpoint is not a rollback.
; Clobbers: D0-D2/CCR.
; CCR: reflects D0 on return.
opasmEngineRollbackCollectionV1	.block
	cmp.w OpasmEngineSourceRecordCount.l, d0
	bhi.s fail
	cmp.w OpasmEngineStmtCount.l, d1
	bhi.s fail
	cmp.w OpasmEngineImageByteCount.l, d2
	bhi.s fail
	move.w d0, OpasmEngineSourceRecordCount.l
	move.w d1, OpasmEngineStmtCount.l
	move.w d2, OpasmEngineImageByteCount.l
	move.l d3, OpasmEngineSessionCurrentPc.l
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opasmEngineRollbackCollectionV1

; Return the source line number for one source record.
; Inputs: D0 = source record index.
; Outputs: D0 = source line number.
opasmEngineGetSourceRecordLineNumberV1	.block
	move.l d1, -(sp)
	moveq #0, d1
	move.w d0, d1
	lsl.l #2, d1
	lea OpasmEngineSourceLineNumTable.l, a0
	move.l 0(a0, d1.l), d0
	move.l (sp)+, d1
	rts
	.bend  ; opasmEngineGetSourceRecordLineNumberV1

; Return exact text for one source record.
; Inputs: D0 = source record index.
; Outputs: D0 = text length; A0 = text pointer.
opasmEngineGetSourceRecordTextV1	.block
	movem.l d1-d2, -(sp)
	moveq #0, d1
	move.w d0, d1
	move.l d1, d2
	add.w d2, d2
	lea OpasmEngineSourceLineLenTable.l, a0
	moveq #0, d0
	move.w 0(a0, d2.l), d0
	lsl.l #8, d1
	add.l d1, d1
	lea OpasmEngineSourceLineTextTable.l, a0
	adda.l d1, a0
	movem.l (sp)+, d1-d2
	rts
	.bend  ; opasmEngineGetSourceRecordTextV1

; Return the statement count.
;
; Outputs:
; - D0: statement count.
opasmEngineGetStatementCountV1	.block
	moveq #0, d0
	move.w OpasmEngineStmtCount.l, d0
	rts
	.bend  ; opasmEngineGetStatementCountV1

; Return the CPU-neutral parser kind retained with a statement.
; Inputs: D0.W = statement index.
; Outputs: D0.W = OPASM_ENGINE_STMT_KIND_*.
; Clobbers: D0/CCR.
opasmEngineGetStatementKindV1	.block
	movem.l d1/a0, -(sp)
	moveq #0, d1
	move.w d0, d1
	add.w d1, d1
	lea OpasmEngineStmtDirectiveKindTable.l, a0
	moveq #0, d0
	move.w 0(a0, d1.l), d0
	movem.l (sp)+, d1/a0
	rts
	.bend  ; opasmEngineGetStatementKindV1

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
	clr.l (a1)+
	moveq #0, d1
	move.w OpasmEngineSessionPass.l, d1
	move.l d1, (a1)
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
	movem.l d1-d2/a0-a1/a3, -(sp)
	move.w d0, d2
	movea.l a1, a3
	jsr opasmEngineWriteEvaluateExpressionExtensionBaseV1
	move.w d2, d0
	jsr opasmEngineInferSelectedShapeForEvalRequestV1
	; Keep the explicit length probe: this branches on whether a selected-shape
	; string exists, not on a status-return contract.
	tst.w d0
	beq.s done
	movea.l a3, a1
	adda.w #16, a1
	move.l a0, (a1)
	move.l d0, 4(a1)

done
	movem.l (sp)+, d1-d2/a0-a1/a3
	moveq #0, d0
	rts
	.bend  ; prepareEvaluateExpressionExtensionV1

; Prepare the base expression-evaluation environment extension without selected-shape metadata.
;
; Inputs:
; - A1: extension buffer base.
;
; Outputs:
; - D0: 0 on success.
prepareDirectiveEvaluateExpressionExtensionV1	.block
	jsr opasmEngineWriteEvaluateExpressionExtensionBaseV1
	moveq #0, d0
	rts
	.bend  ; prepareDirectiveEvaluateExpressionExtensionV1

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

; Return the recorded output address for one statement.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: output address.
opasmEngineGetStatementOutputAddrV1	.block
	move.l d1, -(sp)
	moveq #0, d1
	move.w d0, d1
	lsl.l #2, d1
	lea OpasmEngineStmtOutputAddrTable.l, a0
	move.l 0(a0, d1.l), d0
	move.l (sp)+, d1
	rts
	.bend  ; opasmEngineGetStatementOutputAddrV1

; Return the recorded output image offset for one statement.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: image offset.
opasmEngineGetStatementOutputOffsetV1	.block
	move.l d1, -(sp)
	moveq #0, d1
	move.w d0, d1
	lsl.l #2, d1
	lea OpasmEngineStmtOutputOffsetTable.l, a0
	move.l 0(a0, d1.l), d0
	move.l (sp)+, d1
	rts
	.bend  ; opasmEngineGetStatementOutputOffsetV1

; Return the recorded output byte count for one statement.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: byte count.
opasmEngineGetStatementOutputByteCountV1	.block
	move.l d1, -(sp)
	moveq #0, d1
	move.w d0, d1
	lsl.l #2, d1
	lea OpasmEngineStmtOutputByteCountTable.l, a0
	move.l 0(a0, d1.l), d0
	move.l (sp)+, d1
	rts
	.bend  ; opasmEngineGetStatementOutputByteCountV1

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
	beq.w fail
	move.l d0, OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a2)
	; Prefer the original statement source span for operands.  The legacy token
	; snapshot remains the fallback for synthesized records, but source-backed
	; operands must retain the full 511-byte line contract instead of silently
	; losing byte 64 and any closing quote stored there.
	move.l d1, d3
	add.w d3, d3
	lea OpasmEngineStmtOperandLenTable.l, a1
	cmpi.w #TOKEN_BUFFER_CAPACITY - 1, 0(a1, d3.l)
	blo.s copiedOperand
	move.l d1, -(sp)
	move.l d1, d3
	lsl.l #2, d3
	lea OpasmEngineStmtOperandStartTable.l, a1
	move.l 0(a1, d3.l), d1
	lea OpasmEngineStmtOperandEndTable.l, a1
	move.l 0(a1, d3.l), d2
	move.l (sp), d0
	bsr.w opasmEngineGetStatementExprTextSliceV1
	move.l (sp)+, d1
	tst.l d0
	beq.s copiedOperand
	move.l d0, OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(a2)
	move.l a0, OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(a2)
	bra.s textReady

copiedOperand
	move.l d1, d3
	add.w d3, d3
	move.l d1, d2
	lsl.l #6, d2
	lea OpasmEngineStmtOperandLenTable.l, a1
	moveq #0, d0
	move.w 0(a1, d3.l), d0
	move.l d0, OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(a2)
	lea OpasmEngineStmtOperandNameTable.l, a1
	adda.l d2, a1
	move.l a1, d0
	move.l d0, OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(a2)

textReady
	movem.l (sp)+, d1-d3/a0-a2
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d1-d3/a0-a2
	moveq #1, d0
	rts
	.bend  ; opasmEngineGetStatementTextMetadataV1

; Return the original source text captured for one statement.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: source-text byte length (zero when unavailable).
; - A0: source-text pointer when D0 is non-zero.
; Clobbers: D1-D2/A0/CCR.
opasmEngineGetStatementSourceTextV1	.block
	moveq #0, d1
	move.w d0, d1
	add.w d1, d1
	lea OpasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d0
	move.w 0(a0, d1.l), d0
	beq.s done
	move.l d1, d2
	lsl.l #8, d2
	lea OpasmEngineStmtSourceLineTextTable.l, a0
	adda.l d2, a0

done
	rts
	.bend  ; opasmEngineGetStatementSourceTextV1

; Return or replace one stored statement label name.
; Inputs: D0 = statement index; A0/D1 = replacement text/length for setter.
; Outputs: getter returns A0 = label text and D0 = label length.
; Clobbers: D0-D3/A0-A2/CCR.
; CCR: reflects D0 on return.
opasmEngineGetStatementLabelTextV1	.block
	moveq #0, d1
	move.w d0, d1
	move.l d1, d2
	add.w d2, d2
	lea OpasmEngineStmtLabelLenTable.l, a0
	moveq #0, d0
	move.w 0(a0, d2.l), d0
	lsl.l #6, d1
	lea OpasmEngineStmtLabelNameTable.l, a0
	adda.l d1, a0
	rts
	.bend  ; opasmEngineGetStatementLabelTextV1

; Return the module/namespace owner captured with one stored statement.
; Inputs: D0 = statement index.
; Outputs: A0 = owner text and D0 = owner length.
; Clobbers: D0-D2/A0/CCR.
; CCR: reflects D0 on return.
opasmEngineGetStatementOwnerTextV1	.block
	moveq #0, d1
	move.w d0, d1
	move.l d1, d2
	add.w d2, d2
	lea OpasmEngineStmtOwnerLenTable.l, a0
	moveq #0, d0
	move.w 0(a0, d2.l), d0
	lsl.l #6, d1
	lea OpasmEngineStmtOwnerNameTable.l, a0
	adda.l d1, a0
	rts
	.bend  ; opasmEngineGetStatementOwnerTextV1

; Replace one stored statement owner with an authoritative bounded name.
; Inputs: D0 = statement index; A0/D1 = owner text/length.
; Outputs: D0 = 0 on success, 1 on invalid index/capacity failure.
; Clobbers: D0-D4/A0-A2/CCR.
opasmEngineSetStatementOwnerTextV1	.block
	cmpi.l #TOKEN_BUFFER_CAPACITY - 1, d1
	bhi.s ownerSetFail
	cmp.w OpasmEngineStmtCount.l, d0
	bhi.s ownerSetFail
	move.l d0, d2
	lsl.l #6, d2
	lea OpasmEngineStmtOwnerNameTable.l, a1
	adda.l d2, a1
	move.l d1, d3
ownerSetCopy
	tst.l d3
	beq.s ownerSetTerminate
	move.b (a0)+, (a1)+
	subq.l #1, d3
	bra.s ownerSetCopy
ownerSetTerminate
	clr.b (a1)
	add.w d0, d0
	lea OpasmEngineStmtOwnerLenTable.l, a2
	move.w d1, 0(a2, d0.l)
	moveq #0, d0
	rts
ownerSetFail
	moveq #1, d0
	rts
	.bend  ; opasmEngineSetStatementOwnerTextV1

; Inputs: D0 = statement index; A0/D1 = replacement label text/length.
; Outputs: D0 = 0 on success, 1 on capacity failure.
; Clobbers: D0-D4/A0-A2/CCR.
; CCR: reflects D0 on return.
opasmEngineSetStatementLabelTextV1	.block
	cmpi.l #TOKEN_BUFFER_CAPACITY - 1, d1
	bhs.s fail
	move.l d0, d2
	lsl.l #6, d2
	lea OpasmEngineStmtLabelNameTable.l, a1
	adda.l d2, a1
	move.l d1, d3
copy
	tst.l d3
	beq.s terminate
	move.b (a0)+, (a1)+
	subq.l #1, d3
	bra.s copy
terminate
	clr.b (a1)
	add.w d0, d0
	lea OpasmEngineStmtLabelLenTable.l, a2
	move.w d1, 0(a2, d0.l)
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opasmEngineSetStatementLabelTextV1

; Replace one stored statement operand with an authoritative bounded token.
; Inputs: D0 = statement index; A0/D1 = replacement operand text/length.
; Outputs: D0 = 0 on success, 1 on invalid index/capacity failure.
; Clobbers: D0-D4/A0-A2/CCR.
; CCR: reflects D0 on return.
opasmEngineSetStatementOperandTextV1	.block
	cmpi.l #TOKEN_BUFFER_CAPACITY - 1, d1
	bhs.s operandSetFail
	cmp.w OpasmEngineStmtCount.l, d0
	bhi.s operandSetFail
	move.l d0, d2
	lsl.l #6, d2
	lea OpasmEngineStmtOperandNameTable.l, a1
	adda.l d2, a1
	move.l d1, d3
operandSetCopy
	tst.l d3
	beq.s operandSetTerminate
	move.b (a0)+, (a1)+
	subq.l #1, d3
	bra.s operandSetCopy
operandSetTerminate
	clr.b (a1)
	add.w d0, d0
	lea OpasmEngineStmtOperandLenTable.l, a2
	move.w d1, 0(a2, d0.l)
	moveq #0, d0
	rts
operandSetFail
	moveq #1, d0
	rts
	.bend  ; opasmEngineSetStatementOperandTextV1

; Report whether one stored statement is the generic `.org` directive.
; Inputs: D0 = statement index.
; Outputs: D0 = 1 for `.org`, 0 otherwise.
; CCR: reflects D0 on return.
opasmEngineStatementIsOrgV1	.block
	movem.l d1-d3/a0, -(sp)
	moveq #0, d1
	move.w d0, d1
	move.l d1, d2
	add.w d2, d2
	lea OpasmEngineStmtMnemLenTable.l, a0
	move.w 0(a0, d2.l), d3
	cmpi.w #3, d3
	beq.s haveLength
	cmpi.w #4, d3
	bne.s no

haveLength
	lsl.l #6, d1
	lea OpasmEngineStmtMnemNameTable.l, a0
	adda.l d1, a0
	cmpi.w #4, d3
	bne.s firstLetter
	cmpi.b #'.', (a0)+
	bne.s no

firstLetter
	move.b (a0)+, d3
	ori.b #$20, d3
	cmpi.b #'o', d3
	bne.s no
	move.b (a0)+, d3
	ori.b #$20, d3
	cmpi.b #'r', d3
	bne.s no
	move.b (a0), d3
	ori.b #$20, d3
	cmpi.b #'g', d3
	bne.s no
	moveq #1, d0
	movem.l (sp)+, d1-d3/a0
	rts

no
	moveq #0, d0
	movem.l (sp)+, d1-d3/a0
	rts
	.bend  ; opasmEngineStatementIsOrgV1

; Return one native label name.
; Inputs: D0 = label index.
; Outputs: A0 = NUL-terminated label name.
opasmEngineGetLabelNameV1	.block
	lsl.l #6, d0
	lea OpasmEngineLabelNameTable.l, a0
	adda.l d0, a0
	rts
	.bend  ; opasmEngineGetLabelNameV1

; Return one native label value.
; Inputs: D0 = label index.
; Outputs: D0 = label value.
opasmEngineGetLabelValueV1	.block
	move.l d1, -(sp)
	moveq #0, d1
	move.w d0, d1
	lsl.l #2, d1
	lea OpasmEngineLabelValueTable.l, a0
	move.l 0(a0, d1.l), d0
	move.l (sp)+, d1
	rts
	.bend  ; opasmEngineGetLabelValueV1

; Return a PC-backed label's current value for pass-boundary layout rebasing.
; Inputs: D0 = label index. Outputs: D0 = value, D1 = 1 when PC-backed;
; otherwise D0/D1 = 0.
opasmEngineGetPcBackedLabelValueV1	.block
	movem.l d2/a0, -(sp)
	moveq #0, d2
	move.w d0, d2
	cmp.w OpasmEngineLabelCount.l, d2
	bhs.s notPcBacked
	lea OpasmEngineLabelPcBackedTable.l, a0
	tst.b 0(a0, d2.l)
	beq.s notPcBacked
	move.l d2, d0
	jsr opasmEngineGetLabelValueV1
	moveq #1, d1
	bra.s return
notPcBacked
	clr.l d0
	clr.l d1
return
	movem.l (sp)+, d2/a0
	rts
	.bend  ; opasmEngineGetPcBackedLabelValueV1

; Replace one PC-backed label value after pass-one layout placement.
; Inputs: D0 = label index; D1 = final placed value.
; Outputs: D0 = 0 on success, 1 for invalid/non-PC-backed label.
opasmEngineSetPcBackedLabelValueV1	.block
	movem.l d2-d3/a0, -(sp)
	moveq #0, d2
	move.w d0, d2
	cmp.w OpasmEngineLabelCount.l, d2
	bhs.s fail
	lea OpasmEngineLabelPcBackedTable.l, a0
	tst.b 0(a0, d2.l)
	beq.s fail
	move.l d2, d3
	lsl.l #2, d3
	lea OpasmEngineLabelValueTable.l, a0
	move.l d1, 0(a0, d3.l)
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d2-d3/a0
	rts
	.bend  ; opasmEngineSetPcBackedLabelValueV1

; Refresh one exact PC-backed statement label during a neutral layout retry.
; Mutable/value symbols are deliberately ignored. A changed address requests
; another whole layout pass, matching Rust's snapshot-convergence boundary.
; @opforge-owner: opasm.amigaos.engine
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68000-branch-stability-v1.toml
; @opforge-role: facade
; Inputs: D0 = statement index. Outputs: D0 = 0 success, 1 missing label.
opasmEngineRefreshStatementPcLabelV1	.block
	movem.l d1-d7/a0-a3, -(sp)
	moveq #0, d6
	move.w d0, d6
	cmp.w OpasmEngineStmtCount.l, d6
	bhs.s refreshFail
	move.l d6, d7
	lsl.l #6, d7
	lea OpasmEngineStmtLabelNameTable.l, a1
	adda.l d7, a1
	move.l d6, d5
	add.l d5, d5
	lea OpasmEngineStmtLabelLenTable.l, a2
	moveq #0, d7
	move.w 0(a2, d5.l), d7
	beq.s refreshOk
	moveq #0, d4

refreshFindLoop
	cmp.w OpasmEngineLabelCount.l, d4
	bhs.s refreshFail
	move.l d4, d5
	lsl.l #6, d5
	lea OpasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	move.w d7, d0
	bsr.w labelEquals
	tst.l d0
	bne.s refreshFound
	addq.w #1, d4
	bra.s refreshFindLoop

refreshFound
	lea OpasmEngineLabelPcBackedTable.l, a0
	tst.b 0(a0, d4.l)
	beq.s refreshOk
	move.l d4, d5
	lsl.l #2, d5
	lea OpasmEngineLabelValueTable.l, a0
	move.l OpasmEngineSessionCurrentPc.l, d3
	cmp.l 0(a0, d5.l), d3
	beq.s refreshOk
	move.l d3, 0(a0, d5.l)
	move.w #1, OpasmEngineLayoutChanged.l

refreshOk
	moveq #0, d0
	bra.s refreshReturn
refreshFail
	moveq #1, d0
refreshReturn
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; opasmEngineRefreshStatementPcLabelV1

; Return the final placed value for one label. PC-backed labels use the owning
; statement's pass-two output address; const/var/set labels retain their stored
; value. This keeps artifact rendering tied to the same assembled statement
; state without changing expression-resolution ownership.
; Inputs: D0 = label index. Outputs: D0 = final value.
opasmEngineGetLabelPlacedValueV1	.block
	movem.l d1-d2/a0, -(sp)
	moveq #0, d1
	move.w d0, d1
	cmp.w OpasmEngineLabelCount.l, d1
	bhs.s invalid
	lea OpasmEngineLabelPcBackedTable.l, a0
	tst.b 0(a0, d1.l)
	beq.s storedValue
	move.l d1, d2
	add.l d2, d2
	lea OpasmEngineLabelStatementIndexTable.l, a0
	moveq #0, d1
	move.w 0(a0, d2.l), d1
	lsl.l #2, d1
	lea OpasmEngineStmtOutputAddrTable.l, a0
	move.l 0(a0, d1.l), d0
	bra.s return
storedValue
	move.l d1, d0
	jsr opasmEngineGetLabelValueV1
	bra.s return
invalid
	clr.l d0
return
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; opasmEngineGetLabelPlacedValueV1

; Return whether one native label is finalized.
; Inputs: D0 = label index.
; Outputs: D0 = 1 when finalized, 0 otherwise.
opasmEngineIsLabelFinalV1	.block
	move.l d1, -(sp)
	moveq #0, d1
	move.w d0, d1
	cmp.w OpasmEngineLabelCount.l, d1
	bhs.s notFinal
	lea OpasmEngineLabelFinalizedTable.l, a0
	moveq #0, d0
	move.b 0(a0, d1.l), d0
	move.l (sp)+, d1
	rts

notFinal
	moveq #0, d0
	move.l (sp)+, d1
	rts
	.bend  ; opasmEngineIsLabelFinalV1

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
	movem.l d2-d4/a0-a2, -(sp)
	movea.l a0, a2
	move.l d0, d4
	beq.s fail
	moveq #0, d0
	move.w d1, d0
	jsr opasmEngineGetStatementLineNumberV1
	move.l d0, d2
	move.l d2, d0
	move.b d0, (a1)+
	lsr.l #8, d0
	move.b d0, (a1)+
	lsr.l #8, d0
	move.b d0, (a1)+
	lsr.l #8, d0
	move.b d0, (a1)+
	moveq #1, d3
	move.w d3, d0
	move.b d0, (a1)+
	lsr.w #8, d0
	move.b d0, (a1)+
	move.l d4, d3
	addq.l #1, d3
	move.w d3, d0
	move.b d0, (a1)+
	lsr.w #8, d0
	move.b d0, (a1)+
	clr.b (a1)+
	movea.l a2, a0
	move.w d4, d0
	bsr.w copyFixedString
	move.w d4, d1
	addi.w #9, d1
	movem.l (sp)+, d2-d4/a0-a2
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d2-d4/a0-a2
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

buildRequest
	movea.l a5, a1
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

; Return the one-based operand span for one stored statement.
;
; Inputs:
; - D0: statement index.
;
; Outputs:
; - D0: operand start column, or 0 when unavailable.
; - D1: operand end column, one-based exclusive, on success.
statementOperandSpanV1	.block
	movem.l d2-d3/a0, -(sp)
	moveq #0, d2
	move.w d0, d2
	lsl.l #2, d2
	lea OpasmEngineStmtOperandStartTable.l, a0
	move.l 0(a0, d2.l), d0
	beq.s fail
	lea OpasmEngineStmtOperandEndTable.l, a0
	move.l 0(a0, d2.l), d1
	cmp.l d0, d1
	bls.s fail
	movem.l (sp)+, d2-d3/a0
	rts

fail
	moveq #0, d0
	moveq #0, d1
	movem.l (sp)+, d2-d3/a0
	rts
	.bend  ; statementOperandSpanV1

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
	moveq #0, d1
	move.b 4(a0), d1
	moveq #0, d2
	move.b 5(a0), d2
	lsl.w #8, d2
	or.w d2, d1
	moveq #0, d2
	move.b 6(a0), d2
	moveq #0, d3
	move.b 7(a0), d3
	lsl.w #8, d3
	or.w d3, d2
	moveq #0, d5
	move.w d7, d5
	subi.w #9, d5
	bcs.w none
	sub.w d0, d5
	bcs.w none
	lea 9(a0, d0.w), a0
	tst.w d1
	beq.w none
	cmp.w d1, d2
	bls.w none
	move.w d1, d4
	subq.w #1, d4
	cmp.w d5, d4
	bhs.w none
	move.w d2, d3
	subq.w #1, d3
	cmp.w d5, d3
	bhi.w none
	movea.l a0, a1
	adda.w d4, a1
	move.w d2, d5
	sub.w d1, d5
	move.w d5, d2
	move.w d4, d6
	movea.l a1, a0

scanBack
	tst.w d6
	beq.s trimLeading
	move.b -1(a1), d3
	cmpi.b #' ', d3
	beq.s scanBackOne
	cmpi.b #9, d3
	beq.s scanBackOne
	cmpi.b #'#', d3
	beq.s includePrefix
	cmpi.b #'(', d3
	beq.s includePrefix
	bra.s trimLeading

scanBackOne
	subq.l #1, a1
	subq.w #1, d6
	bra.s scanBack

includePrefix
	subq.l #1, a0
	addq.w #1, d2

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
	bne.s checkTopLevelComma

trimTrailingOne
	subq.w #1, d2
	bra.s trimTrailing

checkTopLevelComma
	movea.l a0, a1
	move.w d2, d4
	moveq #0, d5
	moveq #0, d6

commaScan
	tst.w d4
	beq.s ready
	move.b (a1)+, d3
	cmpi.b #'(', d3
	beq.s commaOpen
	cmpi.b #')', d3
	beq.s commaClose
	cmpi.b #',', d3
	bne.s commaNext
	tst.w d5
	bne.s commaNext
	moveq #1, d6
	bra.s commaNext

commaOpen
	addq.w #1, d5
	bra.s commaNext

commaClose
	tst.w d5
	beq.s commaNext
	subq.w #1, d5

commaNext
	subq.w #1, d4
	bra.s commaScan

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
	bne.s checkParenPrefix
	; A top-level comma means this is a composite operand list.  Do not label
	; the whole list with the legacy single-immediate shape; the package-owned
	; semantic input plan resolves its components and register classes.
	tst.w d6
	bne.w none
	bra.w immediate

checkParenPrefix
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
	; A top-level second operand makes this a composite package expression,
	; not one legacy indirect operand.  Keep the established `(expr),Y` and
	; `(expr,X)` forms above/below; otherwise let package projections decide.
	tst.w d6
	bne.w none
	; Rust's package_shape_input classifies every single non-register operand as
	; direct, including a Member whose base is parenthesized.  Do not let the
	; legacy leading-parenthesis hint misclassify `(expr).field` as indirect;
	; the package-owned member projection will validate the field itself.
	bsr.w inferSelectedShapeParenMember
	tst.b d0
	bne.w direct
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
	bra.w none

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
	move.w #OPASM_ENGINE_LAYOUT_PASS_LIMIT, OpasmEngineLayoutPassesRemaining.l

layoutPass
	bsr.w runPassTwo
	tst.l d0
	bne.s done
	tst.w OpasmEngineLayoutChanged.l
	beq.s done
	subq.w #1, OpasmEngineLayoutPassesRemaining.l
	bne.s layoutPass
	moveq #1, d0

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

; Identify a neutral member suffix following the close of the leading
; parenthesized expression.  The field spelling remains package-owned.
; Inputs: A0/D2.W = trimmed operand text. Output: D0.B = 1 on match, else 0.
inferSelectedShapeParenMember	.block
	movem.l d3-d5/a1, -(sp)
	movea.l a0, a1
	move.w d2, d4
	moveq #0, d5
	moveq #0, d0

scan
	tst.w d4
	beq.s return
	move.b (a1)+, d3
	subq.w #1, d4
	cmpi.b #'(', d3
	beq.s open
	cmpi.b #')', d3
	bne.s scan
	tst.w d5
	beq.s return
	subq.w #1, d5
	bne.s scan
	cmpi.w #2, d4
	blo.s return
	cmpi.b #'.', (a1)
	bne.s return
	moveq #1, d0
	bra.s return

open
	addq.w #1, d5
	bra.s scan

return
	movem.l (sp)+, d3-d5/a1
	rts
	.bend  ; inferSelectedShapeParenMember

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
	lea OpasmEngineStmtOwnerLenTable.l, a0
	clr.w 0(a0, d2.l)
	lea OpasmEngineStmtDirectiveKindTable.l, a0
	move.w OPASM_ENGINE_STMT_REQ_DIRECTIVE_KIND(a5), 0(a0, d2.l)
	lea OpasmEngineStmtOutputAddrTable.l, a0
	clr.l 0(a0, d1.l)
	lea OpasmEngineStmtOperandStartTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_OPERAND_START(a5), 0(a0, d1.l)
	lea OpasmEngineStmtOperandEndTable.l, a0
	move.l OPASM_ENGINE_STMT_REQ_OPERAND_END(a5), 0(a0, d1.l)
	lea OpasmEngineStmtOutputOffsetTable.l, a0
	clr.l 0(a0, d1.l)
	lea OpasmEngineStmtOutputByteCountTable.l, a0
	clr.l 0(a0, d1.l)
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
	lea OpasmEngineStmtOwnerNameTable.l, a1
	adda.l d3, a1
	clr.b (a1)
	move.l OPASM_ENGINE_STMT_REQ_OWNER_LEN(a5), d0
	beq.s ownerDone
	movea.l OPASM_ENGINE_STMT_REQ_OWNER_PTR(a5), a0
	bsr.w copyFixedString
	clr.b (a1)
	moveq #0, d0
	move.w OpasmEngineStmtCount.l, d0
	add.w d0, d0
	lea OpasmEngineStmtOwnerLenTable.l, a0
	move.w OPASM_ENGINE_STMT_REQ_OWNER_LEN_WORD(a5), 0(a0, d0.l)

ownerDone
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
	bne.w return
	clr.w d7
	clr.w OpasmEngineFlowRedirected.l
	clr.w OpasmEngineFlowPending.l

loop
	movea.l OPASM_ENGINE_CTX_STMT_COUNT_PTR(a5), a0
	move.w (a0), d0
	cmp.w d0, d7
	bhs.w ok
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_FLOW_CONTROL_CB(a5), a0
	move.l d7, -(sp)
	jsr (a0)
	move.l (sp)+, d7
	tst.l d0
	bne.w return
	tst.w OpasmEngineFlowPending.l
	beq.w process
	move.w OpasmEngineFlowNext.l, d2
	clr.w OpasmEngineFlowPending.l
	tst.w d2
	bpl.s redirected
	andi.w #$7fff, d2
	clr.w OpasmEngineFlowRedirected.l
	bra.s setNext
redirected
	move.w #1, OpasmEngineFlowRedirected.l
setNext
	move.w d2, d7
	bra.w loop

process
	clr.w OpasmEngineFlowRedirected.l
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_RECORD_LABEL_CB(a5), a0
	move.l d7, -(sp)
	jsr (a0)
	move.l (sp)+, d7
	tst.l d0
	bne.w return
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_ADVANCE_PC_CB(a5), a0
	move.l d7, -(sp)
	jsr (a0)
	move.l (sp)+, d7
	tst.l d0
	bne.w return
	addq.w #1, d7
	bra.w loop

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
	bne.w return
	clr.w d7
	clr.w OpasmEngineFlowRedirected.l
	clr.w OpasmEngineFlowPending.l

loop
	movea.l OPASM_ENGINE_CTX_STMT_COUNT_PTR(a5), a0
	move.w (a0), d0
	cmp.w d0, d7
	bhs.w ok
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_FLOW_CONTROL_CB(a5), a0
	move.l d7, -(sp)
	jsr (a0)
	move.l (sp)+, d7
	tst.l d0
	bne.w return
	tst.w OpasmEngineFlowPending.l
	beq.w process
	move.w OpasmEngineFlowNext.l, d2
	clr.w OpasmEngineFlowPending.l
	tst.w d2
	bpl.s redirected
	andi.w #$7fff, d2
	clr.w OpasmEngineFlowRedirected.l
	bra.s setNext
redirected
	move.w #1, OpasmEngineFlowRedirected.l
setNext
	move.w d2, d7
	bra.w loop

process
	clr.w OpasmEngineFlowRedirected.l
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_REFRESH_LABEL_CB(a5), a0
	move.l d7, -(sp)
	jsr (a0)
	move.l (sp)+, d7
	tst.l d0
	bne.w return
	movea.l OPASM_ENGINE_CTX_BIN_REQUESTED_PTR(a5), a0
	tst.w (a0)
	beq.s advanceOnly
	moveq #0, d0
	move.w d7, d0
	move.l d7, -(sp)
	jsr opasmEngineBeginStatementOutputV1
	move.l (sp)+, d7
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_EMIT_IMAGE_CB(a5), a0
	move.l d7, -(sp)
	jsr (a0)
	move.l (sp)+, d7
	tst.l d0
	bne.w return
	moveq #0, d0
	move.w d7, d0
	move.l d7, -(sp)
	jsr opasmEngineEndStatementOutputV1
	move.l (sp)+, d7

advanceOnly
	moveq #0, d0
	move.w d7, d0
	movea.l OPASM_ENGINE_CTX_ADVANCE_PC_CB(a5), a0
	move.l d7, -(sp)
	jsr (a0)
	move.l (sp)+, d7
	tst.l d0
	bne.w return
	addq.w #1, d7
	bra.w loop

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
OpasmEngineFlowRedirected
	.res word, 1
OpasmEngineFlowNext
	.res word, 1
OpasmEngineFlowPending
	.res word, 1
OpasmEngineAssemblySessionStart
OpasmEngineStmtCount
	.res word, 1
OpasmEngineSessionPass
	.res word, 1
OpasmEngineLayoutPassesRemaining
	.res word, 1
OpasmEngineLayoutChanged
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
OpasmEngineSourceLineTextTable
	.res byte, NATIVE_SOURCE_TEXT_BYTES
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
OpasmEngineStmtOwnerLenTable
	.res word, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtDirectiveKindTable
	.res word, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtOutputAddrTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtOperandStartTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtOperandEndTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtOutputOffsetTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtOutputByteCountTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtMnemOffTable
	.res long, NATIVE_STATEMENT_TABLE_CAPACITY
OpasmEngineStmtLabelNameTable
	.res byte, NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
OpasmEngineStmtMnemNameTable
	.res byte, NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
OpasmEngineStmtOperandNameTable
	.res byte, NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
OpasmEngineStmtOwnerNameTable
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
OpasmEngineLabelStatementIndexTable
	.res word, NATIVE_LABEL_TABLE_CAPACITY
OpasmEngineLabelPcBackedTable
	.res byte, NATIVE_LABEL_TABLE_CAPACITY
OpasmEngineLabelAbsoluteConstantTable
	.res byte, NATIVE_LABEL_TABLE_CAPACITY
OpasmEngineLastResolvedLabelIndex
	.res word, 1
OpasmEngineImageRoute
	.res word, 1
OpasmEngineMappedImageByteCount
	.res word, 1
OpasmEngineImageWriteOffset
	.res long, 1
OpasmEngineImageBuffer
	.res byte, NATIVE_IMAGE_BUFFER_CAPACITY
OpasmEngineMappedImageBuffer
	.res byte, NATIVE_IMAGE_BUFFER_CAPACITY
opasmEngineAssemblySessionEnd

	.endsection
	.endmodule
