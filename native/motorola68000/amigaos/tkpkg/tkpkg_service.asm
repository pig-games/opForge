; Request dispatch and lifecycle scaffolding for the first tkpkg native slice.

	.module tkpkg.amigaos.service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use opasm.amigaos.engine
	.use opcore.amigaos.expr_bridge
	.use tkpkg.amigaos.package_loader
	.use tkpkg.amigaos.pipeline
	.use tkpkg.amigaos.tokenizer_vm
	.use prvm.amigaos.line_router

TKPKG_PARSE_ROUTE_FRAME_SIZE         = 116
TKPKG_EVAL_EXPR_REQUEST_FIXED_SIZE   = 9
TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE = 16
TKPKG_SELECTED_EXTENSION_INPUT_SIZE  = 24
TKPKG_SELECTED_STATUS_OK             = 0
TKPKG_SELECTED_STATUS_NO_OUTPUT      = 1
TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC = 2
TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS = 3
TKPKG_SELECTED_STATUS_OPERAND_ERROR  = 4
TKPKG_EVAL_EXPR_EXTENSION_RESULT_OFF = 16
TKPKG_EVAL_EXPR_EXTENSION_TOTAL_SIZE = 20
EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN    = 45
EVAL_EXPR_FAILED_TEXT_LEN            = 36
EVAL_EXPR_VALUE_PREFIX_LEN           = 6
EVAL_EXPR_MIN_I32_TEXT_LEN           = 11
EVAL_EXPR_MISSING_EXPR_TEXT_LEN      = 45
EVAL_EXPR_MISSING_EXVM_TEXT_LEN      = 42
EVAL_EXPR_BAD_EXPR_VERSION_TEXT_LEN  = 46
EVAL_EXPR_BAD_EXVM_VERSION_TEXT_LEN  = 44
SELECTED_SELECTOR_UNKNOWN_TEXT_LEN   = 33
SELECTED_SELECTOR_UNSUPPORTED_TEXT_LEN = 36
SELECTED_SELECTOR_OPERAND_TEXT_LEN   = 30
SELECTED_OPERAND_BAD_EXVM_TEXT_LEN   = 33
SELECTED_OPERAND_EMPTY_TEXT_LEN      = 30
SELECTED_OPERAND_UNEXPECTED_TEXT_LEN = 40
SELECTED_OPERAND_BRIDGE_TEXT_LEN     = 38
SELECTED_OPERAND_LENGTH_TEXT_LEN     = 38
SELECTED_OPERAND_COMPILE_TEXT_LEN    = 39
SELECTED_OPERAND_FINALIZE_TEXT_LEN   = 40
SELECTED_OPERAND_EVAL_TEXT_LEN       = 38
SELECTED_OPERAND_HEX_PARSE_TEXT_LEN  = 41
SELECTED_OPERAND_LITERAL_EMIT_TEXT_LEN = 44
SELECTED_OPERAND_TRAILING_TEXT_LEN   = 38
SELECTED_OPERAND_SINGLE_TEXT_LEN     = 38
EXPRVM_MISSING_END_TEXT_LEN          = 26
EXPRVM_UNKNOWN_OPCODE_TEXT_LEN       = 29
EXPRVM_LITERAL_READ_TEXT_LEN         = 34
EXPRVM_LITERAL_PUSH_TEXT_LEN         = 34
EXPRVM_REQUIRE_SCALAR_TEXT_LEN       = 36
EXPRVM_END_STACK_TEXT_LEN            = 31
EXPRVM_POP_TEXT_LEN                  = 25
ENCODE_ENVELOPE_MALFORMED_TEXT_LEN  = 33
ENCODE_TABLE_MALFORMED_TEXT_LEN     = 30
SCOPED_OWNER_FAMILY                  = 0
SCOPED_OWNER_CPU                     = 1
SCOPED_OWNER_DIALECT                 = 2

	.section data, kind=data

EvaluateExprNeedsPipelineText
	.byte "OTR001: evaluate_expression requires pipeline", 0

EvaluateExprFailedText
	.byte "OTR901: expression evaluation failed", 0

EvaluateExprMissingExprText
	.byte "OTR001: missing expression evaluator contract", 0

EvaluateExprMissingExvmText
	.byte "OTR001: missing expression parser contract", 0

EvaluateExprBadExprVersionText
	.byte "OTR901: unsupported expression contract opcode", 0

EvaluateExprBadExvmVersionText
	.byte "OTR901: unsupported expression parser opcode", 0

EvaluateExprValuePrefixText
	.byte "VALUE ", 0

EvaluateExprMinI32Text
	.byte "-2147483648", 0

SelectedSelectorUnknownText
	.byte "OTR901: selector unknown mnemonic", 0

SelectedSelectorUnsupportedText
	.byte "OTR901: selector unsupported address", 0

SelectedSelectorOperandText
	.byte "OTR901: selector operand error", 0

SelectedOperandBadExvmText
	.byte "OTR901: selected operand bad exvm", 0

SelectedOperandEmptyText
	.byte "OTR901: selected operand empty", 0

SelectedOperandUnexpectedText
	.byte "OTR901: selected operand unexpected text", 0

SelectedOperandBridgeText
	.byte "OTR901: selected operand bridge failed", 0

SelectedOperandLengthText
	.byte "OTR901: selected operand length mismatch", 0

SelectedOperandCompileText
	.byte "OTR901: selected operand compile failed", 0

SelectedOperandFinalizeText
	.byte "OTR901: selected operand finalize failed", 0

SelectedOperandEvalText
	.byte "OTR901: selected operand exprvm failed", 0

SelectedOperandHexParseText
	.byte "OTR901: selected operand hex parse failed", 0

SelectedOperandLiteralEmitText
	.byte "OTR901: selected operand literal emit failed", 0

SelectedOperandTrailingText
	.byte "OTR901: selected operand trailing text", 0

SelectedOperandSingleText
	.byte "OTR901: selected operand single failed", 0

TkpkgMselPlanNoneText
	.byte "none", 0
TkpkgMselPlanU8Text
	.byte "u8", 0
TkpkgMselPlanU16Text
	.byte "u16", 0
TkpkgMselPlanBranch8Text
	.byte "rel8", 0
TkpkgMselPlanPairU8Rel8Text
	.byte "pair_u8_rel8", 0
TkpkgMselShapeImmediateText
	.byte "immediate", 0
TkpkgMselShapeAccumulatorText
	.byte "accumulator", 0
TkpkgMselShapeDirectXText
	.byte "direct_x", 0
TkpkgMselShapeDirectYText
	.byte "direct_y", 0
TkpkgMselShapeIndirectText
	.byte "indirect", 0
TkpkgMselShapeIndexedIndirectXText
	.byte "indexed_indirect_x", 0
TkpkgMselShapeIndirectIndexedYText
	.byte "indirect_indexed_y", 0
TkpkgMselModeIndexedIndirectXText
	.byte "indexedindirectx", 0
TkpkgMselModeIndirectIndexedYText
	.byte "indirectindexedy", 0

ExprVmMissingEndText
	.byte "OTR901: exprvm missing end", 0

ExprVmUnknownOpcodeText
	.byte "OTR901: exprvm unknown opcode", 0

ExprVmLiteralReadText
	.byte "OTR901: exprvm literal read failed", 0

ExprVmLiteralPushText
	.byte "OTR901: exprvm literal push failed", 0

ExprVmRequireScalarText
	.byte "OTR901: exprvm require scalar failed", 0

ExprVmEndStackText
	.byte "OTR901: exprvm end stack failed", 0

ExprVmPopText
	.byte "OTR901: exprvm pop failed", 0

EncodeEnvelopeMalformedText
	.byte "OTR901: encode envelope malformed", 0

EncodeTableMalformedText
	.byte "OTR901: encode table malformed", 0

EvaluateExprDecimalPowers
	.long 1000000000
	.long 100000000
	.long 10000000
	.long 1000000
	.long 100000
	.long 10000
	.long 1000
	.long 100
	.long 10
	.long 1

	.endsection

	.section bss, kind=bss

EvaluateExprDecimalBuffer
	.res byte, 16
EvaluateExprDecimalBufferEnd

EncodeSelectedSelectorContext
	.res long, 2
EncodeSelectedMnemonicPtr
	.res long, 1
EncodeSelectedSourceLinePtr
	.res long, 1
EncodeSelectedLabelNamePtr
	.res long, 1
EncodeSelectedLabelValuePtr
	.res long, 1
EncodeSelectedLabelCount
	.res long, 1
EncodeSelectedCurrentPc
	.res long, 1
EncodeSelectedExvmOpcodeVersion
	.res word, 1
EncodeSelectedExprOpcodeVersion
	.res word, 1
EncodeSelectedOperandDiag
	.res word, 1
EncodeSelectedMselShapePtr
	.res long, 1
EncodeSelectedMselShapeLen
	.res word, 1
EncodeSelectedCurrentShapePtr
	.res long, 1
EncodeSelectedCurrentShapeLen
	.res word, 1
EncodeSelectedMselMnemonicLen
	.res word, 1
EncodeSelectedMselExprPtr
	.res long, 1
EncodeSelectedMselExprLen
	.res word, 1
EncodeSelectedMselModePtr
	.res long, 1
EncodeSelectedMselModeLen
	.res word, 1
EncodeSelectedMselPlanPtr
	.res long, 1
EncodeSelectedMselPlanLen
	.res word, 1
EncodeSelectedMselValue
	.res long, 1
EncodeSelectedMselUnstable
	.res byte, 1
PairAPtr
	.res long, 1
PairALen
	.res word, 1
PairBPtr
	.res long, 1
PairBLen
	.res word, 1
PairAVal
	.res long, 1
PairBVal
	.res long, 1

	.endsection

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Initialize the shared tkpkg service control block.
;
; This is the local bootstrap convenience entry used by native AmigaOS callers
; that link the service directly. It routes through the same dispatch surface as
; external calls so the initialized control block matches the public ABI.
;
; Inputs:
; - none; uses the shared ControlBlockV1 buffer.
;
; Outputs:
; - ControlBlockV1 contains ABI magic/version/capability fields.
; - D0/D1 follow tkpkg_service_dispatch_v1 for ENTRY_ORD_INIT.
; ---------------------------------------------------------------------------
bootstrapV1	.block
	lea buffers.ControlBlockV1, a0  ; shared in-module CB used by the direct native bootstrap
	moveq #abi.ENTRY_ORD_INIT, d0  ; exercise the public init ordinal, not a private initializer
	bsr.w dispatchV1  ; keep bootstrap behavior identical to an external init call
	rts
	.bend  ; bootstrapV1

; ---------------------------------------------------------------------------
; Public tkpkg service dispatcher.
;
; This is the stable native runtime boundary for package-backed VM services. It
; validates the v1 control block for every non-init request, then dispatches by
; ENTRY_ORD_* without exposing package internals to the CLI.
;
; Inputs:
; - A0: NATIVE_CONTROL_BLOCK_V1 pointer.
; - D0: ENTRY_ORD_* request ordinal.
;
; Outputs:
; - D0/D1 are request-specific immediate results.
; - CB_STATUS_CODE reports STATUS_*_V1 for the caller-visible service result.
; - CB_OUTPUT_PTR/CB_OUTPUT_LEN identify any payload written in the control
;   block output window.
; - Last-error fields are updated for bad-control, bad-request, and runtime
;   error paths.
; ---------------------------------------------------------------------------
dispatchV1	.block
	cmpi.b #abi.ENTRY_ORD_INIT, d0
	beq.s handleInitEntry
	bsr.w tkpkgServicePrepareRequestV1  ; assign a request id before validation/status reporting
	bsr.w tkpkgServiceValidateHeaderV1  ; reject stale or foreign control blocks early
	bne.s dispatchDone
	cmpi.b #abi.ENTRY_ORD_LAST_ERROR, d0
	beq.s handleLastError
	cmpi.b #abi.ENTRY_ORD_LOAD_PACKAGE, d0
	beq.w handleLoadPackage
	cmpi.b #abi.ENTRY_ORD_SET_PIPELINE, d0
	beq.w handleSetPipeline
	cmpi.b #abi.ENTRY_ORD_TOKENIZE_LINE, d0
	beq.w handleTokenizeLine
	cmpi.b #abi.ENTRY_ORD_PARSE_LINE, d0
	beq.w handleParseLine
	cmpi.b #abi.ENTRY_ORD_ENCODE_INSTRUCTION, d0
	beq.w handleEncodeInstruction
	cmpi.b #abi.ENTRY_ORD_EVALUATE_EXPRESSION, d0
	beq.w handleEvaluateExpression
	cmpi.b #abi.ENTRY_ORD_SELECT_INSTRUCTION, d0
	beq.w handleSelectInstruction
	cmpi.b #abi.ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0
	beq.w handleEncodeSelectedInstruction
	bsr.w tkpkgServiceSetBadRequestV1
	rts

handleInitEntry
	bsr.w tkpkgServicePrepareRequestV1
	bsr.w tkpkgServiceWriteHeaderV1
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceWriteClearExtensionFieldsV1
	bra.s handleInit

dispatchDone
	rts

handleInit
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	rts

handleLastError
	tst.b abi.CB_INPUT_LEN(a0)
	bne.s lastErrorBadRequest
	tst.b 19(a0)
	bne.s lastErrorBadRequest
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	move.b buffers.StoredLastErrorLen, abi.CB_OUTPUT_LEN(a0)
	move.b buffers.StoredLastErrorLenHi, 23(a0)
	tst.b buffers.StoredLastErrorLen
	beq.s lastErrorDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1

lastErrorDone
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	rts

lastErrorBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

handleLoadPackage
	move.l a0, -(sp)
	bsr.w package_loader.tkpkgPackageLoaderLoadV1
	movea.l (sp)+, a0
	tst.b d0
	bne.s loadPackageError
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	rts

loadPackageError
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

handleSetPipeline
	move.l a0, -(sp)
	bsr.w pipeline.tkpkgPipelineSetActiveV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s setPipelineOk
	cmpi.b #abi.STATUS_BAD_REQUEST_V1, d0
	beq.s setPipelineBadRequest
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

setPipelineBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

setPipelineOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	rts

handleTokenizeLine
	move.l a0, -(sp)
	bsr.w tokenizer_vm.tkpkgTokenizerVmTokenizeLineV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s tokenizeLineOk
	cmpi.b #abi.STATUS_BAD_REQUEST_V1, d0
	beq.s tokenizeLineBadRequest
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

tokenizeLineBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

tokenizeLineOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	tst.w d1
	beq.s tokenizeLineDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1
	move.b d1, abi.CB_OUTPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 23(a0)

tokenizeLineDone
	rts

handleParseLine
	move.l a0, -(sp)
	bsr.w tkpkgServiceParseLineV1
	movea.l (sp)+, a0
	tst.b d2
	beq.s parseLineOk
	bsr.w tkpkgServiceSetBadRequestV1
	rts

parseLineOk
	movem.l d0-d1, -(sp)
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	movem.l (sp)+, d0-d1
	rts

handleEncodeInstruction
	move.l a0, -(sp)
	bsr.w tkpkgServiceEncodeInstructionV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s encodeInstructionOk
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

encodeInstructionOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	tst.w d1
	beq.s encodeInstructionDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1
	move.b d1, abi.CB_OUTPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 23(a0)

encodeInstructionDone
	rts

handleSelectInstruction
	move.l a0, -(sp)
	bsr.w selectInstructionV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s selectInstructionOk
	cmpi.b #abi.STATUS_BAD_REQUEST_V1, d0
	beq.s selectInstructionBadRequest
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

selectInstructionBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

selectInstructionOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	tst.w d1
	beq.s selectInstructionDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1
	move.b d1, abi.CB_OUTPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 23(a0)

selectInstructionDone
	rts

handleEncodeSelectedInstruction
	move.l a0, -(sp)
	bsr.w encodeSelectedInstructionV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s encodeSelectedInstructionOk
	cmpi.b #abi.STATUS_BAD_REQUEST_V1, d0
	beq.s encodeSelectedInstructionBadRequest
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

encodeSelectedInstructionBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

encodeSelectedInstructionOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	tst.w d1
	beq.s encodeSelectedInstructionDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1
	move.b d1, abi.CB_OUTPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 23(a0)

encodeSelectedInstructionDone
	rts

handleEvaluateExpression
	move.l a0, -(sp)
	bsr.w evaluateExpressionV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s evaluateExpressionOk
	cmpi.b #abi.STATUS_BAD_REQUEST_V1, d0
	beq.s evaluateExpressionBadRequest
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

evaluateExpressionBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

evaluateExpressionOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	tst.w d1
	beq.s evaluateExpressionDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1
	move.b d1, abi.CB_OUTPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 23(a0)

evaluateExpressionDone
	rts
	.bend  ; dispatchV1

	.priv

; ---------------------------------------------------------------------------
; Evaluate one operand expression request through the current native bridge.
;
; Request payload shape:
; - 4-byte little-endian line number (reserved for future richer context)
; - 2-byte little-endian operand_start_col (1-based, inclusive)
; - 2-byte little-endian operand_end_col (1-based, exclusive)
; - 1-byte mnemonic length followed by mnemonic bytes
; - remaining bytes: UTF-8 source line text
;
; Current implementation note: this service now slices the requested operand
; from the source-line/span envelope and runs it through the EXVM-shaped native
; expression bridge selected by the loaded EXPR/EXVM contracts.
;
; Inputs:
; - A0: validated control block whose input window points at the request.
;
; Outputs:
; - D0: 0 on success, STATUS_BAD_REQUEST_V1 on malformed payload, or
;   STATUS_RUNTIME_ERROR_V1 on evaluation/runtime failure.
; - D1: output payload byte length on success, or runtime message length on
;   failure.
; - A1: runtime failure message pointer when D0 is STATUS_RUNTIME_ERROR_V1.
; ---------------------------------------------------------------------------
evaluateExpressionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	btst #1, buffers.PackageStateFlags
	bne.s havePipeline
	lea EvaluateExprNeedsPipelineText, a1
	moveq #EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

havePipeline
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0
	moveq #0, d1
	move.b 17(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.W), a4
	moveq #0, d7
	move.b abi.CB_INPUT_LEN(a0), d7
	moveq #0, d0
	move.b 19(a0), d0
	lsl.w #8, d0
	or.w d0, d7
	cmpi.w #TKPKG_EVAL_EXPR_REQUEST_FIXED_SIZE, d7
	bcs.w badPayload
	adda.w #4, a4
	subi.w #4, d7
	moveq #0, d2
	move.b (a4)+, d2
	moveq #0, d3
	move.b (a4)+, d3
	lsl.w #8, d3
	or.w d3, d2
	moveq #0, d4
	move.b (a4)+, d4
	moveq #0, d5
	move.b (a4)+, d5
	lsl.w #8, d5
	or.w d5, d4
	subi.w #4, d7
	moveq #0, d6
	move.b (a4)+, d6
	subq.w #1, d7
	cmp.w d7, d6
	bhi.w badPayload
	adda.w d6, a4
	sub.w d6, d7
	beq.w badPayload
	tst.w d2
	beq.w badPayload
	cmp.w d2, d4
	bls.w badPayload
	move.w d2, d0
	subq.w #1, d0
	cmp.w d7, d0
	bhs.w badPayload
	move.w d4, d1
	subq.w #1, d1
	cmp.w d7, d1
	bhi.w badPayload
	move.l d2, d3
	moveq #0, d1
	moveq #0, d2
	moveq #0, d6
	moveq #0, d5
	moveq #0, d0
	move.b abi.CB_EXTENSION_PTR(a0), d0
	moveq #0, d3
	move.b 25(a0), d3
	lsl.w #8, d3
	or.w d3, d0
	moveq #0, d3
	move.b abi.CB_EXTENSION_LEN(a0), d3
	moveq #0, d5
	move.b 27(a0), d5
	lsl.w #8, d5
	or.w d5, d3
	move.l d4, -(sp)
	move.l d3, -(sp)
	cmpi.w #TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE, d3
	bcs.s noExtension
	lea 0(a0, d0.W), a3
	movea.l a3, a5
	movea.l (a3)+, a1
	movea.l (a3)+, a2
	move.l (a3)+, d1
	move.l (a3)+, d2
	bset #0, d6

noExtension
	move.l d6, -(sp)
	bsr.w resolveExpressionContractVersionsV1
	bne.s resolveFail
	move.l (sp)+, d6
	move.l (sp)+, d3
	move.l (sp)+, d4
	movea.l a4, a0
	move.l d3, d0
	subq.l #1, d0
	adda.w d0, a0
	move.l d4, d0
	sub.l d3, d0
	beq.s badPayload
	move.l d6, -(sp)
	moveq #0, d4
	move.w d6, d4
	moveq #0, d5
	move.w d7, d5
	moveq #0, d6
	move.w engine.opasmEngineSessionPass.l, d6
	lea engine.opasmEngineLabelFinalizedTable.l, a6
	jsr expr_bridge.opcoreExvmEvalOperandV1
	move.l (sp)+, d6
	tst.b d0
	bne.s bridgeFail
	btst #0, d6
	beq.s noExtensionWrite
	move.l d3, TKPKG_EVAL_EXPR_EXTENSION_RESULT_OFF(a5)

noExtensionWrite
	bsr.w writeExpressionValueOutputV1
	moveq #0, d0
	bra.s return

badPayload
	moveq #abi.STATUS_BAD_REQUEST_V1, d0
	moveq #0, d1
	bra.s return

resolveFail
	addq.l #4, sp
	addq.l #8, sp
	bra.s return

bridgeFail
	lea EvaluateExprFailedText, a1
	moveq #EVAL_EXPR_FAILED_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; evaluateExpressionV1

encodeSelectedInstructionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	btst #1, buffers.PackageStateFlags
	bne.s havePipeline
	lea EvaluateExprNeedsPipelineText, a1
	moveq #EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

havePipeline
	bsr.w buildSelectedEnvelopeV1
	bne.s return
	tst.w d1
	beq.s return

haveEnvelope
	lea buffers.TokenScratchBuffer, a4
	move.w d1, d7
	bsr.w tkpkgEncodeInstructionEnvelopeV1

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; encodeSelectedInstructionV1

; Inputs:
; - A0: selected-instruction service request control block.
;
; Outputs:
; - D0: 0 on success, nonzero ABI/runtime status on failure.
; - D1: 1 when a selectable instruction exists, 0 when no output is available.
;
; Clobbers:
; - D0-D1/D2-D7/A1-A6/CCR
;
; CCR:
; - Reflects D0 on return.
selectInstructionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	btst #1, buffers.PackageStateFlags
	bne.s havePipeline
	lea EvaluateExprNeedsPipelineText, a1
	moveq #EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.s return

havePipeline
	bsr.w buildSelectedEnvelopeV1
	bne.s return
	tst.w d1
	beq.s return
	moveq #1, d1
	moveq #0, d0

return
	movem.l (sp)+, d2-d7/a2-a6
	rts

	.bend  ; selectInstructionV1

; Inputs:
; - A0: selected-instruction or selection-probe service request control block.
;
; Outputs:
; - D0: 0 on success, nonzero ABI/runtime status on failure.
; - D1: built envelope length on success, 0 when no matching output is available.
; - A1: diagnostic text pointer on failure.
;
; Clobbers:
; - D0-D7/A1/A3-A5/CCR
;
; CCR:
; - Reflects D0 on return.
buildSelectedEnvelopeV1	.block
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0
	moveq #0, d1
	move.b 17(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.W), a4
	moveq #0, d7
	move.b abi.CB_INPUT_LEN(a0), d7
	moveq #0, d0
	move.b 19(a0), d0
	lsl.w #8, d0
	or.w d0, d7
	cmpi.w #TKPKG_EVAL_EXPR_REQUEST_FIXED_SIZE, d7
	bcs.w badPayload
	adda.w #4, a4
	subi.w #4, d7
	moveq #0, d2
	move.b (a4)+, d2
	moveq #0, d3
	move.b (a4)+, d3
	lsl.w #8, d3
	or.w d3, d2
	moveq #0, d4
	move.b (a4)+, d4
	moveq #0, d5
	move.b (a4)+, d5
	lsl.w #8, d5
	or.w d5, d4
	subi.w #4, d7
	moveq #0, d6
	move.b (a4)+, d6
	subq.w #1, d7
	cmp.w d7, d6
	bhi.w badPayload
	movea.l a4, a3
	adda.w d6, a4
	sub.w d6, d7
	move.l a3, EncodeSelectedMnemonicPtr
	move.l a4, EncodeSelectedSourceLinePtr
	moveq #0, d0
	move.b abi.CB_EXTENSION_PTR(a0), d0
	moveq #0, d1
	move.b 25(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	move.b abi.CB_EXTENSION_LEN(a0), d1
	moveq #0, d5
	move.b 27(a0), d5
	lsl.w #8, d5
	or.w d5, d1
	move.w d1, d5
	move.l d4, -(sp)
	move.l d2, -(sp)
	cmpi.w #TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE, d1
	bcs.s noExtension
	lea 0(a0, d0.W), a5
	movea.l (a5)+, a1
	movea.l (a5)+, a2
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l a1, EncodeSelectedLabelNamePtr
	move.l a2, EncodeSelectedLabelValuePtr
	move.l d1, EncodeSelectedLabelCount
	move.l d2, EncodeSelectedCurrentPc
	bset #0, d6
	clr.l EncodeSelectedMselShapePtr
	clr.w EncodeSelectedMselShapeLen
	cmpi.w #TKPKG_SELECTED_EXTENSION_INPUT_SIZE, d5
	bcs.s resolveVersions
	movea.l (a5)+, a1
	move.l (a5)+, d0
	move.l a1, EncodeSelectedMselShapePtr
	move.w d0, EncodeSelectedMselShapeLen
	bra.s resolveVersions

noExtension
	clr.l EncodeSelectedLabelNamePtr
	clr.l EncodeSelectedLabelValuePtr
	clr.l EncodeSelectedLabelCount
	clr.l EncodeSelectedCurrentPc
	clr.l EncodeSelectedMselShapePtr
	clr.w EncodeSelectedMselShapeLen

resolveVersions
	move.l d7, -(sp)
	move.l d6, -(sp)
	move.l d5, -(sp)
	bsr.w resolveExpressionContractVersionsV1
	bne.w resolveFail
	move.w d6, EncodeSelectedExvmOpcodeVersion
	move.w d7, EncodeSelectedExprOpcodeVersion
	move.l (sp)+, d5
	move.l (sp)+, d6
	move.l (sp)+, d7
	move.l (sp)+, d2
	move.l (sp)+, d4
	movea.l EncodeSelectedSourceLinePtr, a4
	tst.w d2
	beq.s noOperandSpan
	tst.w d2
	beq.w badPayload
	cmp.w d2, d4
	bls.w badPayload
	move.w d2, d0
	subq.w #1, d0
	cmp.w d7, d0
	bhs.w badPayload
	move.w d4, d1
	subq.w #1, d1
	cmp.w d7, d1
	bhi.w badPayload
	movea.l a4, a1
	move.l d2, d0
	subq.l #1, d0
	adda.w d0, a1
	move.l d4, d1
	sub.l d2, d1
	move.l a1, EncodeSelectedMselExprPtr
	move.w d1, EncodeSelectedMselExprLen
	bra.s buildCandidate

noOperandSpan
	tst.w d4
	bne.w badPayload
	clr.l EncodeSelectedMselExprPtr
	clr.w EncodeSelectedMselExprLen
	movea.l EncodeSelectedSourceLinePtr, a1
	moveq #0, d1

buildCandidate
	move.l a1, -(sp)
	move.l d1, -(sp)
	movea.l EncodeSelectedMnemonicPtr, a0
	move.w d6, d0
	bsr.w tkpkgBuildSelectedEnvelopeFromMselV1
	move.w d1, d7
	move.l (sp)+, d1
	movea.l (sp)+, a1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	beq.w haveOutput
	cmpi.l #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	beq.w noOutput
	cmpi.l #TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC, d0
	beq.w unknownMnemonic
	cmpi.l #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0
	beq.w unsupportedAddress
	cmpi.l #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	beq.w operandError
	lea buffers.RuntimeErrorText, a1
	moveq #buffers.RUNTIME_ERROR_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

unknownMnemonic
	lea SelectedSelectorUnknownText, a1
	moveq #SELECTED_SELECTOR_UNKNOWN_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

unsupportedAddress
	lea SelectedSelectorUnsupportedText, a1
	moveq #SELECTED_SELECTOR_UNSUPPORTED_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandError
	move.w EncodeSelectedOperandDiag, d0
	cmpi.w #1, d0
	beq.w operandBadExvm
	cmpi.w #2, d0
	beq.w operandEmpty
	cmpi.w #3, d0
	beq.w operandUnexpected
	cmpi.w #4, d0
	beq.w operandBridge
	cmpi.w #5, d0
	beq.w operandLength
	cmpi.w #6, d0
	beq.w operandCompile
	cmpi.w #7, d0
	beq.w operandFinalize
	cmpi.w #8, d0
	beq.w operandEval
	cmpi.w #31, d0
	beq.w operandHexParse
	cmpi.w #32, d0
	beq.w operandLiteralEmit
	cmpi.w #33, d0
	beq.w operandTrailing
	cmpi.w #34, d0
	beq.w operandSingle
	cmpi.w #51, d0
	beq.w exprVmMissingEnd
	cmpi.w #52, d0
	beq.w exprVmUnknownOpcode
	cmpi.w #53, d0
	beq.w exprVmLiteralRead
	cmpi.w #54, d0
	beq.w exprVmLiteralPush
	cmpi.w #55, d0
	beq.w exprVmRequireScalar
	cmpi.w #56, d0
	beq.w exprVmEndStack
	cmpi.w #57, d0
	beq.w exprVmPop
	lea SelectedSelectorOperandText, a1
	moveq #SELECTED_SELECTOR_OPERAND_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandBadExvm
	lea SelectedOperandBadExvmText, a1
	moveq #SELECTED_OPERAND_BAD_EXVM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandEmpty
	lea SelectedOperandEmptyText, a1
	moveq #SELECTED_OPERAND_EMPTY_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandUnexpected
	lea SelectedOperandUnexpectedText, a1
	moveq #SELECTED_OPERAND_UNEXPECTED_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandBridge
	lea SelectedOperandBridgeText, a1
	moveq #SELECTED_OPERAND_BRIDGE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandLength
	lea SelectedOperandLengthText, a1
	moveq #SELECTED_OPERAND_LENGTH_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandCompile
	lea SelectedOperandCompileText, a1
	moveq #SELECTED_OPERAND_COMPILE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandFinalize
	lea SelectedOperandFinalizeText, a1
	moveq #SELECTED_OPERAND_FINALIZE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandEval
	lea SelectedOperandEvalText, a1
	moveq #SELECTED_OPERAND_EVAL_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandHexParse
	lea SelectedOperandHexParseText, a1
	moveq #SELECTED_OPERAND_HEX_PARSE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandLiteralEmit
	lea SelectedOperandLiteralEmitText, a1
	moveq #SELECTED_OPERAND_LITERAL_EMIT_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandTrailing
	lea SelectedOperandTrailingText, a1
	moveq #SELECTED_OPERAND_TRAILING_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

operandSingle
	lea SelectedOperandSingleText, a1
	moveq #SELECTED_OPERAND_SINGLE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

exprVmMissingEnd
	lea ExprVmMissingEndText, a1
	moveq #EXPRVM_MISSING_END_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

exprVmUnknownOpcode
	lea ExprVmUnknownOpcodeText, a1
	moveq #EXPRVM_UNKNOWN_OPCODE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

exprVmLiteralRead
	lea ExprVmLiteralReadText, a1
	moveq #EXPRVM_LITERAL_READ_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

exprVmLiteralPush
	lea ExprVmLiteralPushText, a1
	moveq #EXPRVM_LITERAL_PUSH_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

exprVmRequireScalar
	lea ExprVmRequireScalarText, a1
	moveq #EXPRVM_REQUIRE_SCALAR_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

exprVmEndStack
	lea ExprVmEndStackText, a1
	moveq #EXPRVM_END_STACK_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

exprVmPop
	lea ExprVmPopText, a1
	moveq #EXPRVM_POP_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

haveOutput
	move.w d7, d1
	moveq #0, d0
	bra.w return

noOutput
	moveq #0, d1
	moveq #0, d0
	bra.w return

badPayload
	moveq #abi.STATUS_BAD_REQUEST_V1, d0
	moveq #0, d1
	bra.w return

resolveFail
	lea 12(sp), sp
	addq.l #8, sp

return
	tst.l d0
	rts
	.bend  ; buildSelectedEnvelopeV1

tkpkgBuildSelectedEnvelopeFromMselV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.w d0, d2
	move.w d2, EncodeSelectedMselMnemonicLen
	tst.w d2
	beq.w noOutput
	lea buffers.MselChunkOffsetLo, a3
	bsr.w tkpkgServiceChunkPtrFromLocatorV1
	bne.w noOutput
	bsr.w tkpkgServiceReadU32LeLow16V1
	bne.w noOutput
	tst.w d0
	beq.w noOutput
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

entryLoop
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w unsupported
	move.b (a2)+, d6
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	bsr.w tkpkgSelectedMselOwnerMatchesV1
	move.b d0, d5
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	tst.b d5
	beq.s skipMnemonicCompare
	move.l a2, -(sp)
	move.w d2, d1
	movea.l a5, a2
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	and.b d0, d5

skipMnemonicCompare
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	tst.b d5
	beq.s skipShapeCompare
	move.l a1, EncodeSelectedCurrentShapePtr
	move.w d0, EncodeSelectedCurrentShapeLen
	tst.b d5
	beq.s skipShapeCompare
	tst.w EncodeSelectedMselShapeLen
	beq.s skipShapeCompare
	tst.l EncodeSelectedMselShapePtr
	beq.s skipShapeCompare
	move.l a2, -(sp)
	move.w EncodeSelectedMselShapeLen, d1
	movea.l EncodeSelectedMselShapePtr, a2
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	and.b d0, d5

skipShapeCompare
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	tst.b d5
	beq.s skipModeStore
	move.l a1, EncodeSelectedMselModePtr
	move.w d0, EncodeSelectedMselModeLen

skipModeStore
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	tst.b d5
	beq.s skipPlanStore
	move.l a1, EncodeSelectedMselPlanPtr
	move.w d0, EncodeSelectedMselPlanLen
	move.l a2, -(sp)
	move.w d7, -(sp)
	bsr.w tkpkgMselTryBuildCandidateV1
	move.w (sp)+, d7
	movea.l (sp)+, a2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	beq.s return
	cmpi.l #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	beq.s maybeReturnOperandError
	bra.s skipPlanStore

maybeReturnOperandError
	tst.w EncodeSelectedMselShapeLen
	beq.s skipPlanStore
	tst.l EncodeSelectedMselShapePtr
	beq.s skipPlanStore
	bra.s return

skipPlanStore
	moveq #4, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w unsupported
	lea 4(a2), a2
	dbf d7, entryLoop

noOutput
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s return

unsupported
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0

return
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildSelectedEnvelopeFromMselV1

tkpkgMselTryBuildCandidateV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea TkpkgMselPlanNoneText, a2
	moveq #4, d1
	bsr.w tkpkgMselPlanEqualsV1
	bne.w buildNone
	lea TkpkgMselPlanU8Text, a2
	moveq #2, d1
	bsr.w tkpkgMselPlanEqualsV1
	bne.s tryU8
	lea TkpkgMselPlanU16Text, a2
	moveq #3, d1
	bsr.w tkpkgMselPlanEqualsV1
	bne.s tryU16
	lea TkpkgMselPlanBranch8Text, a2
	moveq #4, d1
	bsr.w tkpkgMselPlanEqualsV1
	bne.s tryBranchOffset8
	lea TkpkgMselPlanPairU8Rel8Text, a2
	moveq #12, d1
	bsr.w tkpkgMselPlanEqualsV1
	bne.w tryPairU8Rel8
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.w return

tryU8
	bsr.w tkpkgMselEvalOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	tst.b EncodeSelectedMselUnstable
	beq.s tryU8Stable
	moveq #1, d6
	bra.w tryUnstablePassOneOperand

tryU8Stable
	move.l EncodeSelectedMselValue, d3
	bpl.s tryU8NonNegative
	bra.w operandError

tryU8NonNegative
	cmpi.l #$000000FF, d3
	bls.s tryU8Fits
	bra.w noOutput

tryU8Fits
	moveq #1, d6
	bra.w buildOperand

tryU16
	bsr.w tkpkgMselEvalOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	move.l EncodeSelectedMselValue, d3
	bpl.s tryU16NonNegative
	bra.w operandError

tryU16NonNegative
	cmpi.l #$0000FFFF, d3
	bls.s tryU16Fits
	bra.w operandError

tryU16Fits
	moveq #2, d6
	bra.w buildOperand

tryBranchOffset8
	cmpi.w #1, engine.opasmEngineSessionPass.l
	beq.w tryUnstablePassOneOperand
	bsr.w tkpkgMselEvalOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	tst.b EncodeSelectedMselUnstable
	beq.s tryBranchStable
	moveq #1, d6
	bra.w tryUnstablePassOneOperand

tryBranchStable
	move.l EncodeSelectedMselValue, d3
	move.l EncodeSelectedCurrentPc, d4
	addq.l #2, d4
	sub.l d4, d3
	cmpi.l #-128, d3
	bge.s tryBranchMinFits
	bra.w operandError

tryBranchMinFits
	cmpi.l #127, d3
	ble.s tryBranchFits
	bra.w operandError

tryBranchFits
	move.l d3, EncodeSelectedMselValue
	moveq #1, d6
	bra.w buildOperand

tryPairU8Rel8
	clr.l PairAPtr.l
	clr.w PairALen.l
	clr.l PairBPtr.l
	clr.w PairBLen.l
	movea.l EncodeSelectedMselExprPtr, a1
	move.w EncodeSelectedMselExprLen, d7
	beq.w operandError
	moveq #0, d5
	moveq #0, d6

pairScanLoop
	tst.w d7
	beq.w operandError
	move.b (a1)+, d4
	cmpi.b #'(', d4
	beq.s pairOpenParen
	cmpi.b #')', d4
	beq.s pairCloseParen
	cmpi.b #',', d4
	bne.s pairNextChar
	tst.w d6
	beq.s pairFoundComma
	bra.s pairNextChar

pairOpenParen
	addq.w #1, d6
	bra.s pairNextChar

pairCloseParen
	tst.w d6
	beq.s pairNextChar
	subq.w #1, d6

pairNextChar
	addq.w #1, d5
	subq.w #1, d7
	bra.s pairScanLoop

pairFoundComma
	movea.l EncodeSelectedMselExprPtr, a0
	moveq #0, d0
	move.w d5, d0
	move.l d0, d2

pairFirstTrimStartLoop
	tst.l d2
	beq.w operandError
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s pairFirstTrimStartOne
	cmpi.b #9, d3
	bne.s pairFirstTrimEndInit

pairFirstTrimStartOne
	addq.l #1, a0
	subq.l #1, d2
	bra.s pairFirstTrimStartLoop

pairFirstTrimEndInit
	lea 0(a0, d2.l), a1
	subq.l #1, a1

pairFirstTrimEndLoop
	tst.l d2
	beq.w operandError
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s pairFirstTrimEndOne
	cmpi.b #9, d3
	bne.s pairFirstTrimOk

pairFirstTrimEndOne
	subq.l #1, a1
	subq.l #1, d2
	bra.s pairFirstTrimEndLoop

pairFirstTrimOk
	move.l a0, PairAPtr.l
	move.w d2, PairALen.l
	movea.l EncodeSelectedMselExprPtr, a0
	adda.w d5, a0
	addq.l #1, a0
	moveq #0, d0
	move.w EncodeSelectedMselExprLen, d0
	sub.w d5, d0
	subq.w #1, d0
	move.l d0, d2

pairSecondTrimStartLoop
	tst.l d2
	beq.w operandError
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s pairSecondTrimStartOne
	cmpi.b #9, d3
	bne.s pairSecondTrimEndInit

pairSecondTrimStartOne
	addq.l #1, a0
	subq.l #1, d2
	bra.s pairSecondTrimStartLoop

pairSecondTrimEndInit
	lea 0(a0, d2.l), a1
	subq.l #1, a1

pairSecondTrimEndLoop
	tst.l d2
	beq.w operandError
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s pairSecondTrimEndOne
	cmpi.b #9, d3
	bne.s pairSecondTrimOk

pairSecondTrimEndOne
	subq.l #1, a1
	subq.l #1, d2
	bra.s pairSecondTrimEndLoop

pairSecondTrimOk
	move.l a0, d0
	move.l d0, PairBPtr.l
	move.w d2, PairBLen.l
	move.l PairAPtr.l, d0
	move.l d0, EncodeSelectedMselExprPtr
	move.w PairALen.l, d0
	move.w d0, EncodeSelectedMselExprLen
	bsr.w tkpkgMselEvalOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	move.l EncodeSelectedMselValue, d3
	bpl.s tryPairFirstNonNegative
	bra.w operandError

tryPairFirstNonNegative
	cmpi.l #$000000FF, d3
	bls.s tryPairFirstFits
	bra.w operandError

tryPairFirstFits
	move.l d3, PairAVal.l
	cmpi.w #1, engine.opasmEngineSessionPass.l
	bne.s tryPairSecondStable
	clr.l PairBVal.l
	bra.w buildPairOperand

tryPairSecondStable
	move.l PairBPtr.l, d0
	move.l d0, EncodeSelectedMselExprPtr
	move.w PairBLen.l, d0
	move.w d0, EncodeSelectedMselExprLen
	bsr.w tkpkgMselEvalOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	move.l EncodeSelectedMselValue, d3
	move.l EncodeSelectedCurrentPc, d4
	addq.l #3, d4
	sub.l d4, d3
	cmpi.l #-128, d3
	bge.s tryPairSecondMinFits
	bra.w operandError

tryPairSecondMinFits
	cmpi.l #127, d3
	ble.s tryPairSecondFits
	bra.w operandError

tryPairSecondFits
	move.l d3, PairBVal.l
	bra.w buildPairOperand

tryUnstablePassOneOperand
	cmpi.w #1, engine.opasmEngineSessionPass.l
	bne.w noOutput
	clr.l EncodeSelectedMselValue
	bra.w buildOperand

buildNone
	tst.w EncodeSelectedMselExprLen
	beq.s buildNoneOperand
	bsr.w tkpkgMselCurrentShapeAccumulatorV1
	beq.s noOutput
	bsr.w tkpkgMselExprIsAccumulatorAV1
	beq.s noOutput

buildNoneOperand
	moveq #0, d6
	bra.w buildOperand

noOutput
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s return

operandError
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	bra.s return

buildOperand
	bsr.w tkpkgMselWriteCandidateEnvelopeV1
	bra.s return

buildPairOperand
	lea buffers.TokenScratchBuffer, a4
	move.w EncodeSelectedMselMnemonicLen, d0
	cmpi.w #255, d0
	bhi.w operandError
	move.b d0, (a4)+
	movea.l a5, a0
	bsr.w tkpkgMselCopyBytesV1
	move.b #1, (a4)+
	move.w EncodeSelectedMselModeLen, d0
	cmpi.w #255, d0
	bhi.w operandError
	move.b d0, (a4)+
	movea.l EncodeSelectedMselModePtr, a0
	bsr.w tkpkgMselCopyBytesV1
	move.b #2, (a4)+
	move.b #1, (a4)+
	move.l PairAVal.l, d3
	move.b d3, (a4)+
	move.b #1, (a4)+
	move.l PairBVal.l, d3
	move.b d3, (a4)+
	move.l a4, d1
	lea buffers.TokenScratchBuffer, a0
	sub.l a0, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0

return
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgMselTryBuildCandidateV1

; Compare current selected-plan text against the caller-supplied plan tag.
; Inputs: A2 = expected plan text; D1 = expected plan length.
; Outputs: D0 = 1 when plan matches, 0 otherwise.
; Clobbers: D0/D4/CCR.
; CCR: reflects D0 on return.
tkpkgMselPlanEqualsV1	.block
	movem.l d1/a2, -(sp)
	movea.l EncodeSelectedMselPlanPtr, a1
	move.w EncodeSelectedMselPlanLen, d0
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movem.l (sp)+, d1/a2
	rts
	.bend  ; tkpkgMselPlanEqualsV1

tkpkgMselEvalOperandV1	.block
	bsr.w tkpkgMselCurrentShapeImmediateV1
	move.b d0, d7
	bsr.w tkpkgMselCurrentShapeIndexSuffixV1
	move.b d0, d6
	bsr.w tkpkgMselCurrentShapeParenModeV1
	bne.s haveParenMode
	bsr.w tkpkgMselCurrentModeParenModeV1

haveParenMode
	move.b d0, d5
	movea.l EncodeSelectedMselExprPtr, a0
	moveq #0, d0
	move.w EncodeSelectedMselExprLen, d0
	tst.b d7
	beq.s haveOperandText
	tst.l d0
	beq.s haveOperandText
	cmpi.b #'#', (a0)
	bne.s haveOperandText
	addq.l #1, a0
	subq.l #1, d0

haveOperandText
	tst.b d6
	beq.s evalParenOperandText
	bsr.w tkpkgMselStripIndexSuffixV1
	tst.b d1
	bne.s operandError

evalParenOperandText
	tst.b d5
	beq.s evalOperandText
	cmpi.b #2, d5
	beq.s stripIndexedIndirectX
	cmpi.b #3, d5
	beq.s stripIndirectIndexedY
	bsr.w tkpkgMselStripOuterParensV1
	bra.s evalOperandText

stripIndexedIndirectX
	bsr.w tkpkgMselStripOuterParensV1
	tst.b d1
	bne.s evalOperandText
	moveq #'x', d6
	bsr.w tkpkgMselStripIndexSuffixV1
	tst.b d1
	bne.s operandError
	bra.s evalOperandText

stripIndirectIndexedY
	moveq #'y', d6
	bsr.w tkpkgMselStripIndexSuffixV1
	tst.b d1
	bne.s evalOperandText
	bsr.w tkpkgMselStripOuterParensV1

evalOperandText
	bsr.w encodeSelectedOperandV1
	bne.s operandError
	move.l d3, EncodeSelectedMselValue
	clr.b EncodeSelectedMselUnstable
	tst.l d5
	beq.s ok
	move.b #1, EncodeSelectedMselUnstable

ok
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	rts

operandError
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	rts
	.bend  ; tkpkgMselEvalOperandV1

tkpkgMselCurrentShapeImmediateV1	.block
	movea.l EncodeSelectedCurrentShapePtr, a1
	move.w EncodeSelectedCurrentShapeLen, d0
	lea TkpkgMselShapeImmediateText, a2
	moveq #9, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	rts
	.bend  ; tkpkgMselCurrentShapeImmediateV1

; Inputs:
; - Uses the current selected-shape text stored in EncodeSelectedCurrentShapePtr/Len.
;
; Outputs:
; - D0: 1 when the current shape is `accumulator`, 0 otherwise.
;
; Clobbers:
; - D0-D4/A1-A2/CCR
;
; CCR:
; - Reflects D0 on return.
tkpkgMselCurrentShapeAccumulatorV1	.block
	movea.l EncodeSelectedCurrentShapePtr, a1
	move.w EncodeSelectedCurrentShapeLen, d0
	lea TkpkgMselShapeAccumulatorText, a2
	moveq #11, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	rts
	.bend  ; tkpkgMselCurrentShapeAccumulatorV1

; Inputs:
; - Uses the current selected operand text stored in EncodeSelectedMselExprPtr/Len.
;
; Outputs:
; - D0: 1 when the trimmed operand text is exactly `a`, 0 otherwise.
;
; Clobbers:
; - D0-D2/A1/CCR
;
; CCR:
; - Reflects D0 on return.
tkpkgMselExprIsAccumulatorAV1	.block
	movem.l d1-d2/a1, -(sp)
	movea.l EncodeSelectedMselExprPtr, a1
	move.w EncodeSelectedMselExprLen, d0
trimLeading
	tst.w d0
	beq.s notAccumulator
	move.b (a1), d1
	cmpi.b #' ', d1
	beq.s skipLeading
	cmpi.b #9, d1
	beq.s skipLeading
	bra.s trimTrailing

skipLeading
	addq.l #1, a1
	subq.w #1, d0
	bra.s trimLeading

trimTrailing
	tst.w d0
	beq.s notAccumulator
	move.w d0, d1
	subq.w #1, d1
	move.b 0(a1, d1.w), d2
	cmpi.b #' ', d2
	beq.s skipTrailing
	cmpi.b #9, d2
	beq.s skipTrailing
	bra.s compareAccumulator

skipTrailing
	subq.w #1, d0
	bra.s trimTrailing

compareAccumulator
	cmpi.w #1, d0
	bne.s notAccumulator
	move.b (a1), d0
	ori.b #$20, d0
	cmpi.b #'a', d0
	bne.s notAccumulator
	moveq #1, d0
	bra.s return

notAccumulator
	moveq #0, d0

return
	movem.l (sp)+, d1-d2/a1
	rts
	.bend  ; tkpkgMselExprIsAccumulatorAV1

tkpkgMselCurrentShapeIndexSuffixV1	.block
	movea.l EncodeSelectedCurrentShapePtr, a1
	move.w EncodeSelectedCurrentShapeLen, d0
	lea TkpkgMselShapeDirectXText, a2
	moveq #8, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s directX
	movea.l EncodeSelectedCurrentShapePtr, a1
	move.w EncodeSelectedCurrentShapeLen, d0
	lea TkpkgMselShapeDirectYText, a2
	moveq #8, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s directY
	moveq #0, d0
	rts

directX
	moveq #'x', d0
	rts

directY
	moveq #'y', d0
	rts
	.bend  ; tkpkgMselCurrentShapeIndexSuffixV1

; Inputs:
; - Uses the current selected-shape text stored in EncodeSelectedCurrentShapePtr/Len.
;
; Outputs:
; - D0: 0 when no paren mode is active, 1 for indirect, 2 for indexed-indirect-X,
;   3 for indirect-indexed-Y.
;
; Clobbers:
; - D0-D4/A1-A2/CCR
;
; CCR:
; - Reflects D0 on return.
tkpkgMselCurrentShapeParenModeV1	.block
	movea.l EncodeSelectedCurrentShapePtr, a1
	move.w EncodeSelectedCurrentShapeLen, d0
	lea TkpkgMselShapeIndirectText, a2
	moveq #8, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s indirect
	movea.l EncodeSelectedCurrentShapePtr, a1
	move.w EncodeSelectedCurrentShapeLen, d0
	lea TkpkgMselShapeIndexedIndirectXText, a2
	moveq #18, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s indexedIndirectX
	movea.l EncodeSelectedCurrentShapePtr, a1
	move.w EncodeSelectedCurrentShapeLen, d0
	lea TkpkgMselShapeIndirectIndexedYText, a2
	moveq #18, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s indirectIndexedY
	moveq #0, d0
	rts

indirect
	moveq #1, d0
	rts

indexedIndirectX
	moveq #2, d0
	rts

indirectIndexedY
	moveq #3, d0
	rts
	.bend  ; tkpkgMselCurrentShapeParenModeV1

tkpkgMselCurrentModeParenModeV1	.block
	movea.l EncodeSelectedMselModePtr, a1
	move.w EncodeSelectedMselModeLen, d0
	lea TkpkgMselShapeIndirectText, a2
	moveq #8, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s indirect
	movea.l EncodeSelectedMselModePtr, a1
	move.w EncodeSelectedMselModeLen, d0
	lea TkpkgMselModeIndexedIndirectXText, a2
	moveq #16, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s indexedIndirectX
	movea.l EncodeSelectedMselModePtr, a1
	move.w EncodeSelectedMselModeLen, d0
	lea TkpkgMselModeIndirectIndexedYText, a2
	moveq #16, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s indirectIndexedY
	moveq #0, d0
	rts

indirect
	moveq #1, d0
	rts

indexedIndirectX
	moveq #2, d0
	rts

indirectIndexedY
	moveq #3, d0
	rts
	.bend  ; tkpkgMselCurrentModeParenModeV1

tkpkgMselStripOuterParensV1	.block
	movem.l d2-d3/a1, -(sp)
	moveq #1, d1
	move.l d0, d2

trimStartLoop
	tst.l d2
	beq.s return
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s trimStartOne
	cmpi.b #9, d3
	bne.s trimEndInit

trimStartOne
	addq.l #1, a0
	subq.l #1, d2
	bra.s trimStartLoop

trimEndInit
	cmpi.l #2, d2
	bcs.s return
	lea 0(a0, d2.l), a1
	subq.l #1, a1

trimEndLoop
	tst.l d2
	beq.s return
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s trimEndOne
	cmpi.b #9, d3
	bne.s haveEnd

trimEndOne
	subq.l #1, a1
	subq.l #1, d2
	bra.s trimEndLoop

haveEnd
	cmpi.l #2, d2
	bcs.s return
	cmpi.b #'(', (a0)
	bne.s return
	cmpi.b #')', (a1)
	bne.s return
	addq.l #1, a0
	subq.l #2, d2
	move.l d2, d0
	moveq #0, d1

return
	movem.l (sp)+, d2-d3/a1
	rts
	.bend  ; tkpkgMselStripOuterParensV1

tkpkgMselStripIndexSuffixV1	.block
	movem.l d2-d4/a1, -(sp)
	moveq #1, d1
	move.l d0, d2
	cmpi.l #2, d2
	bcs.s return
	lea 0(a0, d2.l), a1
	subq.l #1, a1

trimEndLoop
	tst.l d2
	beq.s return
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s trimEndOne
	cmpi.b #9, d3
	bne.s haveSuffixChar

trimEndOne
	subq.l #1, a1
	subq.l #1, d2
	bra.s trimEndLoop

haveSuffixChar
	move.b d3, d4
	cmpi.b #'A', d4
	bcs.s suffixFolded
	cmpi.b #'Z', d4
	bhi.s suffixFolded
	addi.b #32, d4

suffixFolded
	cmp.b d6, d4
	bne.s return
	cmpi.l #2, d2
	bcs.s return
	subq.l #1, a1
	subq.l #1, d2
	cmpi.b #',', (a1)
	bne.s return
	subq.l #1, d2
	beq.s return
	move.l d2, d0
	lea 0(a0, d0.l), a1
	subq.l #1, a1

trimBeforeCommaLoop
	tst.l d0
	beq.s return
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s trimBeforeCommaOne
	cmpi.b #9, d3
	bne.s ok

trimBeforeCommaOne
	subq.l #1, a1
	subq.l #1, d0
	bra.s trimBeforeCommaLoop

ok
	moveq #0, d1

return
	movem.l (sp)+, d2-d4/a1
	rts
	.bend  ; tkpkgMselStripIndexSuffixV1

tkpkgMselWriteCandidateEnvelopeV1	.block
	lea buffers.TokenScratchBuffer, a4
	move.w EncodeSelectedMselMnemonicLen, d0
	cmpi.w #255, d0
	bhi.w operandError
	move.b d0, (a4)+
	movea.l a5, a0
	bsr.w tkpkgMselCopyBytesV1
	move.b #1, (a4)+
	move.w EncodeSelectedMselModeLen, d0
	cmpi.w #255, d0
	bhi.w operandError
	move.b d0, (a4)+
	movea.l EncodeSelectedMselModePtr, a0
	bsr.w tkpkgMselCopyBytesV1
	tst.w d6
	beq.s writeNoOperands
	move.b #1, (a4)+
	move.b d6, (a4)+
	move.l EncodeSelectedMselValue, d3
	move.b d3, (a4)+
	cmpi.w #2, d6
	bne.s done
	lsr.l #8, d3
	move.b d3, (a4)+
	bra.s done

writeNoOperands
	move.b #0, (a4)+

done
	move.l a4, d1
	lea buffers.TokenScratchBuffer, a0
	sub.l a0, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	rts

operandError
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	rts
	.bend  ; tkpkgMselWriteCandidateEnvelopeV1

tkpkgMselCopyBytesV1	.block
	tst.w d0
	beq.s done
	subq.w #1, d0

loop
	move.b (a0)+, (a4)+
	dbf d0, loop

done
	rts
	.bend  ; tkpkgMselCopyBytesV1

tkpkgSelectedMselOwnerMatchesV1	.block
	movem.l d2-d4/a2-a4, -(sp)
	move.w d0, d3
	movea.l a1, a3
	cmpi.b #SCOPED_OWNER_DIALECT, d6
	bne.s checkCpu
	lea buffers.ActiveDialectBuffer.l, a2
	tst.b (a2)
	beq.s noMatch
	bra.s compare

checkCpu
	cmpi.b #SCOPED_OWNER_CPU, d6
	bne.s checkFamily
	lea buffers.ActiveCpuBuffer.l, a2
	bra.s compare

checkFamily
	cmpi.b #SCOPED_OWNER_FAMILY, d6
	bne.s noMatch
	lea buffers.ActiveFamilyBuffer.l, a2

compare
	bsr.w tkpkgServiceActiveOwnerLenV1
	move.w d3, d0
	movea.l a3, a1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bra.s return

noMatch
	moveq #0, d0

return
	movem.l (sp)+, d2-d4/a2-a4
	rts
	.bend  ; tkpkgSelectedMselOwnerMatchesV1

writeCandidateOutputV1	.block
	movem.l d2-d7/a2-a4, -(sp)
	lea buffers.TokenScratchBuffer, a4
	moveq #0, d4
	move.w d7, d4
	cmpi.w #4, d4
	bcs.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d4
	cmp.w d4, d5
	bhi.w fail
	adda.w d5, a4
	sub.w d5, d4
	beq.w fail
	moveq #0, d3
	move.b (a4)+, d3
	subq.w #1, d4
	beq.w noOutput
	tst.w d3
	beq.w noOutput
	lea buffers.LastErrorBuffer, a2
	moveq #0, d1

loop
	tst.w d4
	beq.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d4
	cmp.w d4, d5
	bhi.w fail

modeLoop
	tst.w d5
	beq.s modeDone
	move.b (a4)+, (a2)+
	addq.w #1, d1
	subq.w #1, d4
	subq.w #1, d5
	bne.s modeLoop

modeDone
	tst.w d4
	beq.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d4

operandLoop
	tst.w d5
	beq.s newline
	tst.w d4
	beq.w fail
	move.b #' ', (a2)+
	addq.w #1, d1
	moveq #0, d6
	move.b (a4)+, d6
	subq.w #1, d4
	cmp.w d4, d6
	bhi.w fail

operandBytesLoop
	tst.w d6
	beq.s nextOperand
	moveq #0, d7
	move.b (a4)+, d7
	subq.w #1, d4
	bsr.w appendHexByteV1
	subq.w #1, d6
	bne.s operandBytesLoop

nextOperand
	subq.w #1, d5
	bne.s operandLoop

newline
	move.b #10, (a2)+
	addq.w #1, d1
	subq.w #1, d3
	bne.s loop
	moveq #0, d0
	bra.s return

noOutput
	moveq #0, d1
	moveq #0, d0
	bra.s return

fail
	lea buffers.RuntimeErrorText, a1
	moveq #buffers.RUNTIME_ERROR_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0

return
	movem.l (sp)+, d2-d7/a2-a4
	rts
	.bend  ; writeCandidateOutputV1

appendHexByteV1	.block
	moveq #0, d2
	move.b d7, d2
	move.b d2, d6
	lsr.b #4, d6
	bsr.s hexDigitFromNibbleV1
	move.b d0, (a2)+
	addq.w #1, d1
	move.b d2, d6
	andi.b #$0f, d6
	bsr.s hexDigitFromNibbleV1
	move.b d0, (a2)+
	addq.w #1, d1
	rts
	.bend  ; appendHexByteV1

hexDigitFromNibbleV1	.block
	moveq #0, d0
	move.b d6, d0
	cmpi.b #9, d0
	ble.s decimal
	addi.b #'A' - 10, d0
	rts

decimal
	addi.b #'0', d0
	rts
	.bend  ; hexDigitFromNibbleV1

; Inputs:
; - A0/D0: operand text pointer and length.
; - Selected-envelope EXVM opcode version/current-PC/label context fields.
;
; Outputs:
; - D0: 0 on success, nonzero on operand parse/eval failure.
; - D3: evaluated operand value on success.
; - EncodeSelectedOperandDiag updated on failure.
;
; Clobbers:
; - D0-D2/D4-D7/A0-A2/A6/CCR
;
; CCR:
; - Reflects D0 on return.
encodeSelectedOperandV1	.block
	movem.l d1-d2/d6-d7/a1-a2/a6, -(sp)
	clr.w EncodeSelectedOperandDiag
	movea.l EncodeSelectedLabelNamePtr, a1
	movea.l EncodeSelectedLabelValuePtr, a2
	move.l EncodeSelectedLabelCount, d1
	move.l EncodeSelectedCurrentPc, d2
	moveq #0, d4
	move.w EncodeSelectedExvmOpcodeVersion, d4
	cmpi.w #1, d4
	beq.s haveExvm
	move.w #1, EncodeSelectedOperandDiag
	moveq #1, d0
	bra.w return

haveExvm
	tst.l d0
	bne.s haveText
	move.w #2, EncodeSelectedOperandDiag
	moveq #1, d0
	bra.w return

haveText
	moveq #0, d7
	move.b (a0), d7
	cmpi.b #'$', d7
	beq.s textOk

checkPercent
	cmpi.b #'%', d7
	beq.s textOk
	cmpi.b #'*', d7
	beq.s textOk
	cmpi.b #'+', d7
	beq.s textOk
	cmpi.b #'-', d7
	beq.s textOk
	cmpi.b #'0', d7
	bcs.s maybeLetter
	cmpi.b #'9', d7
	bls.s textOk

maybeLetter
	cmpi.b #'A', d7
	bcs.s unexpectedText
	cmpi.b #'Z', d7
	bls.s textOk
	cmpi.b #'a', d7
	bcs.s unexpectedText
	cmpi.b #'z', d7
	bls.s textOk

unexpectedText
	move.w #3, EncodeSelectedOperandDiag
	moveq #1, d0
	bra.w return

textOk
	bsr.w encodeSelectedOperandTryLabelV1
	tst.l d7
	bne.w return
	moveq #0, d5
	moveq #1, d5
	moveq #0, d6
	move.w engine.opasmEngineSessionPass.l, d6
	lea engine.opasmEngineLabelFinalizedTable.l, a6
	jsr expr_bridge.opcoreExvmEvalOperandV1
	beq.w return
	cmpi.b #3, d0
	beq.s compileFail
	cmpi.b #4, d0
	beq.s finalizeFail
	cmpi.b #5, d0
	beq.s evalFail
	cmpi.b #31, d0
	beq.s hexParseFail
	cmpi.b #32, d0
	beq.s literalEmitFail
	cmpi.b #33, d0
	beq.s trailingFail
	cmpi.b #34, d0
	beq.s singleFail
	cmpi.b #51, d0
	bhs.s exprVmFail
	move.w #4, EncodeSelectedOperandDiag
	bra.w return

compileFail
	move.w #6, EncodeSelectedOperandDiag
	bra.w return

finalizeFail
	move.w #7, EncodeSelectedOperandDiag
	bra.w return

evalFail
	move.w #8, EncodeSelectedOperandDiag
	bra.w return

hexParseFail
	move.w #31, EncodeSelectedOperandDiag
	bra.w return

literalEmitFail
	move.w #32, EncodeSelectedOperandDiag
	bra.w return

trailingFail
	move.w #33, EncodeSelectedOperandDiag
	bra.w return

singleFail
	move.w #34, EncodeSelectedOperandDiag
	bra.w return

exprVmFail
	move.w d0, EncodeSelectedOperandDiag

return
	movem.l (sp)+, d1-d2/d6-d7/a1-a2/a6
	tst.l d0
	rts
	.bend  ; encodeSelectedOperandV1

encodeSelectedOperandTryLabelV1	.block
	movem.l d0-d2/d4/d6/a0-a2/a6, -(sp)
	moveq #0, d7
	tst.l d1
	bne.s haveContext
	lea engine.opasmEngineLabelNameTable.l, a1
	lea engine.opasmEngineLabelValueTable.l, a2
	moveq #0, d1
	move.w engine.opasmEngineLabelCount.l, d1
	tst.l d1
	beq.s return

haveContext
	moveq #0, d6

loop
	cmp.l d1, d6
	bhs.s return
	move.l d6, d2
	lsl.l #6, d2
	movea.l a1, a6
	adda.l d2, a6
	bsr.s encodeSelectedOperandLabelEqualsV1
	tst.l d7
	bne.s found
	addq.l #1, d6
	bra.s loop

found
	move.l d6, d2
	lsl.l #2, d2
	move.l 0(a2, d2.l), d3
	moveq #0, d5
	moveq #0, d0

return
	movem.l (sp)+, d0-d2/d4/d6/a0-a2/a6
	tst.l d7
	beq.s done
	moveq #0, d0

done
	rts
	.bend  ; encodeSelectedOperandTryLabelV1

encodeSelectedOperandLabelEqualsV1	.block
	movem.l d0-d2/a0-a1/a6, -(sp)
	move.l d0, d2
	beq.s no

loop
	move.b (a0)+, d1
	move.b (a6)+, d0
	cmp.b d0, d1
	bne.s no
	subq.l #1, d2
	bne.s loop
	tst.b (a6)
	bne.s no
	moveq #1, d7
	bra.s return

no
	moveq #0, d7

return
	movem.l (sp)+, d0-d2/a0-a1/a6
	rts
	.bend  ; encodeSelectedOperandLabelEqualsV1

writeExpressionValueOutputV1	.block
	movem.l d0/d2-d7/a0-a2, -(sp)
	lea buffers.LastErrorBuffer, a2
	lea EvaluateExprValuePrefixText, a1
	moveq #EVAL_EXPR_VALUE_PREFIX_LEN, d6
	move.w d6, d5

prefixLoop
	move.b (a1)+, (a2)+
	subq.w #1, d5
	bne.s prefixLoop
	move.l d3, d0
	bpl.s positive
	cmpi.l #$80000000, d0
	bne.s negative
	lea EvaluateExprMinI32Text, a1
	moveq #EVAL_EXPR_MIN_I32_TEXT_LEN, d2
	add.w d2, d6
	bra.s copyDigits

negative
	move.b #'-', (a2)+
	addq.w #1, d6
	neg.l d0

positive
	bsr.w appendUnsignedDecimalV1
	add.w d2, d6
	lea EvaluateExprDecimalBuffer, a1

copyDigits
	tst.w d2
	beq.s done

digitsLoop
	move.b (a1)+, (a2)+
	subq.w #1, d2
	bne.s digitsLoop

done
	clr.b (a2)
	move.w d6, d1
	movem.l (sp)+, d0/d2-d7/a0-a2
	rts
	.bend  ; writeExpressionValueOutputV1

appendUnsignedDecimalV1	.block
	lea EvaluateExprDecimalPowers, a1
	lea EvaluateExprDecimalBuffer, a0
	moveq #9, d3
	moveq #0, d2
	moveq #0, d4

loop
	move.l (a1)+, d6
	moveq #0, d7

countLoop
	cmp.l d6, d0
	bcs.s digitReady
	sub.l d6, d0
	addq.b #1, d7
	bra.s countLoop

digitReady
	tst.b d4
	bne.s emit
	tst.b d7
	bne.s startEmit
	tst.w d3
	bne.s next

startEmit
	moveq #1, d4

emit
	addi.b #'0', d7
	move.b d7, (a0)+
	addq.w #1, d2

next
	dbf d3, loop
	rts
	.bend  ; appendUnsignedDecimalV1

; Resolve both EXVM and ExprVM opcode versions for the current package selection.
; Inputs: active EXVM/EXPR package locators plus D6 preserved for caller state.
; Outputs: D0 = 0 on success, nonzero on version lookup failure; D6 restored on success.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resolveExpressionContractVersionsV1	.block
	bsr.w resolveExvmOpcodeVersionV1
	bne.s return
	move.l d6, -(sp)
	bsr.w resolveExprOpcodeVersionV1
	bne.s exprFail
	move.l (sp)+, d6
	bra.s return

exprFail
	addq.l #4, sp

return
	tst.l d0
	rts
	.bend  ; resolveExpressionContractVersionsV1

resolveExvmOpcodeVersionV1	.block
	lea buffers.ActiveDialectBuffer.l, a4
	tst.b (a4)
	beq.s skipDialect
	moveq #SCOPED_OWNER_DIALECT, d6
	bsr.w findExvmOpcodeVersionV1
	beq.s found

skipDialect
	lea buffers.ActiveCpuBuffer.l, a4
	moveq #SCOPED_OWNER_CPU, d6
	bsr.w findExvmOpcodeVersionV1
	beq.s found
	lea buffers.ActiveFamilyBuffer.l, a4
	moveq #SCOPED_OWNER_FAMILY, d6
	bsr.w findExvmOpcodeVersionV1
	beq.s found
	lea EvaluateExprMissingExvmText, a1
	moveq #EVAL_EXPR_MISSING_EXVM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

found
	cmpi.w #1, d6
	beq.s ok
	lea EvaluateExprBadExvmVersionText, a1
	moveq #EVAL_EXPR_BAD_EXVM_VERSION_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

ok
	moveq #0, d0
	rts
	.bend  ; resolveExvmOpcodeVersionV1

resolveExprOpcodeVersionV1	.block
	lea buffers.ActiveDialectBuffer.l, a4
	tst.b (a4)
	beq.s skipDialect
	moveq #SCOPED_OWNER_DIALECT, d6
	bsr.w findExprOpcodeVersionV1
	beq.s found

skipDialect
	lea buffers.ActiveCpuBuffer.l, a4
	moveq #SCOPED_OWNER_CPU, d6
	bsr.w findExprOpcodeVersionV1
	beq.s found
	lea buffers.ActiveFamilyBuffer.l, a4
	moveq #SCOPED_OWNER_FAMILY, d6
	bsr.w findExprOpcodeVersionV1
	beq.s found
	lea EvaluateExprMissingExprText, a1
	moveq #EVAL_EXPR_MISSING_EXPR_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

found
	move.w d7, d6
	cmpi.w #1, d6
	beq.s ok
	cmpi.w #2, d6
	beq.s ok
	lea EvaluateExprBadExprVersionText, a1
	moveq #EVAL_EXPR_BAD_EXPR_VERSION_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

ok
	moveq #0, d0
	rts
	.bend  ; resolveExprOpcodeVersionV1

findExvmOpcodeVersionV1	.block
	move.b d6, d5
	lea buffers.ExvmChunkOffsetLo, a3
	bsr.w tkpkgServiceChunkPtrFromLocatorV1
	bne.s missing
	bsr.w tkpkgServiceReadU32LeLow16V1
	bne.s missing
	tst.w d0
	beq.s missing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

loop
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s missing
	move.b (a2)+, d6
	bsr.w tkpkgServiceLocateStringV1
	bne.s missing
	cmp.b d5, d6
	bne.s skipEntry
	move.w d0, d2
	move.l a2, -(sp)
	movea.l a1, a3
	move.w d2, d0
	movea.l a4, a2
	bsr.w tkpkgServiceActiveOwnerLenV1
	movea.l a3, a1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	bne.s readVersion

skipEntry
	bsr.w skipExvmEntryTailV1
	bne.s missing
	dbf d7, loop

missing
	moveq #1, d0
	rts

readVersion
	bsr.w tkpkgServiceReadU16LeV1
	bne.s missing
	move.w d0, d6
	moveq #0, d0
	rts
	.bend  ; findExvmOpcodeVersionV1

findExprOpcodeVersionV1	.block
	move.b d6, d5
	lea buffers.ExprChunkOffsetLo, a3
	bsr.w tkpkgServiceChunkPtrFromLocatorV1
	bne.s missing
	bsr.w tkpkgServiceReadU32LeLow16V1
	bne.s missing
	tst.w d0
	beq.s missing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

loop
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s missing
	move.b (a2)+, d6
	bsr.w tkpkgServiceLocateStringV1
	bne.s missing
	cmp.b d5, d6
	bne.s skipEntry
	move.w d0, d2
	move.l a2, -(sp)
	movea.l a1, a3
	move.w d2, d0
	movea.l a4, a2
	bsr.w tkpkgServiceActiveOwnerLenV1
	movea.l a3, a1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	bne.s readVersion

skipEntry
	bsr.w tkpkgServiceSkipExprEntryTailV1
	bne.s missing
	dbf d7, loop

missing
	moveq #1, d0
	rts

readVersion
	bsr.w tkpkgServiceReadU16LeV1
	bne.s missing
	move.w d0, d7
	moveq #0, d0
	rts
	.bend  ; findExprOpcodeVersionV1

; Skip the EXVM entry tail after the owner string.
; Inputs: A2/A6 = current EXVM entry cursor/exclusive end.
; Outputs: A2 advanced past the tail when present; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/CCR.
; CCR: reflects D1 on return.
skipExvmEntryTailV1	.block
	bsr.w tkpkgServiceReadU16LeV1
	bne.s done
	bsr.w tkpkgServiceSkipStringV1

done
	rts
	.bend  ; skipExvmEntryTailV1

; Skip the EXPR entry tail after the owner string and fixed metadata.
; Inputs: A2/A6 = current EXPR entry cursor/exclusive end.
; Outputs: A2 advanced past the tail when present; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/D7/CCR.
; CCR: reflects D1 on return.
tkpkgServiceSkipExprEntryTailV1	.block
	bsr.w tkpkgServiceReadU16LeV1
	bne.s tailDone
	moveq #16, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s tailDone
	lea 16(a2), a2
	moveq #7, d7

stringLoop
	bsr.w tkpkgServiceSkipStringV1
	bne.s tailDone
	dbf d7, stringLoop

tailDone
	rts
	.bend  ; tkpkgServiceSkipExprEntryTailV1

tkpkgServiceActiveOwnerLenV1	.block
	moveq #0, d1

loop
	tst.b 0(a2, d1.W)
	beq.s done
	addq.w #1, d1
	bra.s loop

done
	rts
	.bend  ; tkpkgServiceActiveOwnerLenV1

tkpkgServiceChunkPtrFromLocatorV1	.block
	moveq #0, d0
	move.b (a3)+, d0
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d7
	move.b (a3)+, d7
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d7
	beq.s missing
	lea buffers.PackageStorage, a6
	lea 0(a6, d0.W), a2
	lea 0(a2, d7.W), a6
	moveq #0, d1
	rts

missing
	moveq #1, d1
	rts
	.bend  ; tkpkgServiceChunkPtrFromLocatorV1

; Read one little-endian u16 from the current package cursor.
; Inputs: A2 = current package cursor; A6 = exclusive package end.
; Outputs: D0 = decoded value; A2 advanced by 2 on success; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/CCR.
; CCR: reflects D1 on return.
tkpkgServiceReadU16LeV1	.block
	moveq #2, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts

fail
	moveq #1, d1
	rts
	.bend  ; tkpkgServiceReadU16LeV1

; Resolve one length-prefixed string locator into A1 and advance A2 past it.
; Inputs: A2 = current string cursor; A6 = exclusive package end.
; Outputs: D0 = string byte length; A1 = string bytes; A2 advanced past the record; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D3/A1/CCR.
; CCR: reflects D1 on return.
tkpkgServiceLocateStringV1	.block
	bsr.w tkpkgServiceReadU32LeLow16V1
	bne.s boundsFail
	move.l d0, d2
	move.l d0, d3
	addq.l #4, d3
	move.l d3, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s boundsFail
	move.l d2, d0
	lea 4(a2), a1
	lea 4(a2), a2
	adda.l d0, a2
	moveq #0, d1
	rts

boundsFail
	moveq #0, d0
	movea.l d0, a1
	moveq #1, d1
	rts
	.bend  ; tkpkgServiceLocateStringV1

; Skip one length-prefixed string at the current package cursor.
; Inputs: A2/A6 = current string cursor/exclusive end.
; Outputs: A2 advanced past the string; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/D2-D3/A1/CCR.
; CCR: reflects D1 on return.
tkpkgServiceSkipStringV1	.block
	bsr.w tkpkgServiceLocateStringV1
	rts
	.bend  ; tkpkgServiceSkipStringV1

; Read one little-endian u32 field and return its low 16 bits.
; Inputs: A2 = current field cursor; A6 = exclusive package end.
; Outputs: D0 = decoded low-16 value; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/CCR.
; CCR: reflects D1 on return.
tkpkgServiceReadU32LeLow16V1	.block
	moveq #4, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s fail
	moveq #0, d0
	move.b (a2), d0
	moveq #0, d1
	move.b 1(a2), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts

fail
	moveq #1, d1
	rts
	.bend  ; tkpkgServiceReadU32LeLow16V1

; Verify that D0 bytes remain between A2 and the exclusive end pointer in A6.
; Inputs: D0 = required byte count; A2 = current package cursor; A6 = exclusive end.
; Outputs: D1 = 0 when enough bytes remain, 1 on bounds failure.
; Clobbers: D1/CCR.
; CCR: reflects D1 on return.
tkpkgServiceRequireBytesV1	.block
	movea.l a2, a1
	adda.l d0, a1
	cmpa.l a6, a1
	bhi.s fail
	moveq #0, d1
	rts

fail
	moveq #1, d1
	rts
	.bend  ; tkpkgServiceRequireBytesV1

; Compare two ASCII strings case-insensitively.
; Inputs: A1 = first string bytes; A2 = second string bytes; D0/D1 = lengths.
; Outputs: D0 = 1 when strings match, 0 otherwise.
; Clobbers: D0/D2-D4/CCR.
; CCR: reflects D0 on return.
tkpkgServiceStringEqAsciiCasefoldV1	.block
	cmp.w d1, d0
	bne.s noMatch
	move.w d0, d4
	beq.s match
	subq.w #1, d4

loop
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d3
	move.b (a2)+, d3
	move.b d2, d0
	bsr.w tkpkgServiceFoldAsciiLowerV1
	move.b d0, d2
	move.b d3, d0
	bsr.w tkpkgServiceFoldAsciiLowerV1
	cmp.b d0, d2
	bne.s noMatch
	dbf d4, loop

match
	moveq #1, d0
	rts

noMatch
	moveq #0, d0
	rts
	.bend  ; tkpkgServiceStringEqAsciiCasefoldV1

tkpkgServiceFoldAsciiLowerV1	.block
	cmpi.b #'A', d0
	blo.s done
	cmpi.b #'Z', d0
	bhi.s done
	ori.b #$20, d0

done
	rts
	.bend  ; tkpkgServiceFoldAsciiLowerV1

; ---------------------------------------------------------------------------
; Route one parser request frame through PRVM.
;
; Current implementation note: this entry accepts only the fixed
; TKPKG_PARSE_ROUTE_FRAME_SIZE route frame built by the native CLI. It is the
; intended service boundary for parse-line behavior even while the CLI still
; owns some transitional parser/assembler state.
;
; Inputs:
; - A0: validated control block whose input window points at a PRVM route frame.
;
; Outputs:
; - D0/D1: PRVM status/result-count values.
; - D2: 0 on accepted request, 1 on malformed service payload.
; ---------------------------------------------------------------------------
tkpkgServiceParseLineV1	.block
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0  ; low byte of CB-relative route-frame offset
	moveq #0, d1
	move.b 17(a0), d1  ; high byte of CB_INPUT_PTR; direct offset avoids a temp struct
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.W), a1  ; A1 now points at the caller-supplied PRVM route frame
	moveq #0, d0
	move.b abi.CB_INPUT_LEN(a0), d0  ; low byte of route-frame byte length
	moveq #0, d1
	move.b 19(a0), d1  ; high byte of CB_INPUT_LEN
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #TKPKG_PARSE_ROUTE_FRAME_SIZE, d0
	bne.s badRequest
	movea.l a1, a0  ; PRVM router ABI expects its route frame in A0
	jsr line_router.prvmRouteLine68000  ; D0/D1 become the parser service's immediate return pair
	moveq #0, d2
	rts

badRequest
	moveq #1, d2
	moveq #0, d0
	moveq #0, d1
	rts
	.bend  ; tkpkgServiceParseLineV1

; ---------------------------------------------------------------------------
; Encode one package-backed instruction request.
;
; The request payload is the compact selector/encoder envelope currently built
; by native opasm staging code:
; - mnemonic length + mnemonic bytes
; - candidate count
; - per-candidate addressing-mode and operand bytes
;
; Current implementation note: this entry still decodes only the small native
; 6502 smoke envelope used by the first CLI slice. The architectural contract is
; still correct: the CLI asks the package service to encode instead of writing
; opcodes directly.
;
; Inputs:
; - A0: validated control block whose input window points at the encode request.
;
; Outputs:
; - D0: 0 on success, nonzero on runtime error.
; - D1: encoded byte count on success.
; - output bytes are written in the service output window.
; ---------------------------------------------------------------------------
tkpkgServiceEncodeInstructionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0  ; low byte of CB-relative encode-request offset
	moveq #0, d1
	move.b 17(a0), d1  ; high byte of CB_INPUT_PTR
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.W), a4  ; A4 walks the request envelope in-place
	moveq #0, d7
	move.b abi.CB_INPUT_LEN(a0), d7  ; D7 tracks remaining request bytes as fields are consumed
	moveq #0, d0
	move.b 19(a0), d0
	lsl.w #8, d0
	or.w d0, d7
	bsr.w tkpkgEncodeInstructionEnvelopeV1

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; tkpkgServiceEncodeInstructionV1

tkpkgEncodeInstructionEnvelopeV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	cmpi.w #4, d7
	bcs.w fail
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d7
	tst.w d2
	beq.w fail
	cmp.w d7, d2
	bhi.w fail
	movea.l a4, a5
	adda.w d2, a4
	sub.w d2, d7
	beq.w fail
	moveq #0, d3
	move.b (a4)+, d3
	subq.w #1, d7
	tst.w d3
	beq.w noMatch
	tst.w d7
	beq.w fail
	moveq #0, d4
	move.b (a4)+, d4
	subq.w #1, d7
	tst.w d4
	beq.w fail
	cmp.w d7, d4
	bhi.w fail
	movea.l a4, a6
	adda.w d4, a4
	sub.w d4, d7
	beq.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d7
	tst.w d5
	beq.s noOperandRecord
	tst.w d7
	beq.w fail
	moveq #0, d6
	move.b (a4)+, d6
	subq.w #1, d7
	cmp.w d7, d6
	bhi.w fail
	movea.l a4, a3
	move.w d6, -(sp)
	move.w d5, d0
	move.w d7, d1
	movea.l a4, a2

validateOperandRecord
	cmp.w d1, d6
	bhi.s validateOperandFail
	adda.w d6, a2
	sub.w d6, d1
	subq.w #1, d0
	beq.s validateOperandDone
	tst.w d1
	beq.s validateOperandFail
	moveq #0, d6
	move.b (a2)+, d6
	subq.w #1, d1
	bra.s validateOperandRecord

validateOperandFail
	addq.l #2, sp
	bra.w fail

validateOperandDone
	move.w (sp)+, d6
	bra.w encodeCandidate

noOperandRecord
	moveq #0, d6
	movea.l a4, a3

encodeCandidate
	bsr.w tkpkgEncodeFindAndExecuteTableProgram
	bra.w return

noMatch
	cmpi.w #4, d2
	bne.s noMatchReturn
	cmpi.w #2, d5
	bne.s noMatchReturn
	cmpi.w #1, d6
	bne.s noMatchReturn
	movea.l a5, a0
	move.b (a0)+, d0
	ori.b #$20, d0
	cmpi.b #'b', d0
	bne.s noMatchReturn
	move.b (a0)+, d0
	ori.b #$20, d0
	cmpi.b #'b', d0
	bne.s noMatchReturn
	move.b (a0)+, d0
	ori.b #$20, d0
	cmpi.b #'r', d0
	beq.s directBitBranchReset
	cmpi.b #'s', d0
	bne.s noMatchReturn
	moveq #0, d0
	move.b #$8F, d0
	bra.s directBitBranchOpcode

directBitBranchReset
	moveq #$0F, d0

directBitBranchOpcode
	moveq #0, d1
	move.b (a0), d1
	cmpi.b #'0', d1
	blo.s noMatchReturn
	cmpi.b #'7', d1
	bhi.s noMatchReturn
	sub.b #'0', d1
	lsl.b #4, d1
	add.b d1, d0
	lea buffers.LastErrorBuffer, a2
	move.b d0, (a2)+
	move.b (a3), (a2)+
	movea.l a3, a0
	adda.w d6, a0
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.w #1, d1
	bne.s noMatchReturn
	move.b (a0), (a2)+
	moveq #3, d1
	moveq #0, d0
	bra.w return

noMatchReturn
	moveq #0, d1
	moveq #2, d0
	bra.w return

fail
	lea EncodeEnvelopeMalformedText, a1
	moveq #ENCODE_ENVELOPE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; tkpkgEncodeInstructionEnvelopeV1

tkpkgEncodeFindAndExecuteTableProgram	.block
	movem.l d2-d7/a0-a6, -(sp)
	moveq #0, d0
	move.b buffers.TablChunkOffsetLo, d0
	moveq #0, d1
	move.b buffers.TablChunkOffsetHi, d1
	lsl.w #8, d1
	or.w d1, d0
	beq.w fail
	lea buffers.PackageStorage, a0
	adda.w d0, a0
	bsr.w tkpkgEncodeReadU32Low16
	beq.w noMatch
	move.w d0, d7
	subq.w #1, d7

loop
	move.b (a0)+, d0
	bsr.w tkpkgEncodeSkipString
	movea.l a0, a1
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a2
	adda.w d1, a0
	movea.l a5, a1
	move.w d2, d0
	bsr.w tkpkgEncodeStringEqIgnoreCase
	beq.s skipModeCheck
	movea.l a0, a1
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a2
	adda.w d1, a0
	movea.l a6, a1
	move.w d4, d0
	bsr.w tkpkgEncodeStringEqIgnoreCase
	beq.s skipProgram
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a1
	bsr.w tkpkgEncodeExecuteProgram
	bra.s return

skipModeCheck
	bsr.w tkpkgEncodeSkipString

skipProgram
	bsr.w tkpkgEncodeSkipBytes
	dbra d7, loop

noMatch
	moveq #0, d1
	moveq #0, d0
	bra.s return

fail
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgEncodeFindAndExecuteTableProgram

tkpkgEncodeExecuteProgram	.block
	movem.l d2-d7/a0-a4, -(sp)
	movea.l a1, a0
	move.w d1, d7
	lea buffers.LastErrorBuffer, a2
	clr.w d1

loop
	tst.w d7
	beq.s fail
	move.b (a0)+, d0
	subq.w #1, d7
	cmpi.b #$FF, d0
	beq.s ok
	cmpi.b #$01, d0
	beq.s emitU8
	cmpi.b #$02, d0
	beq.s emitOperand
	bra.w fail

emitU8
	tst.w d7
	beq.s fail
	move.b (a0)+, (a2)+
	subq.w #1, d7
	addq.w #1, d1
	bra.s loop

emitOperand
	tst.w d7
	beq.s fail
	moveq #0, d3
	move.b (a0)+, d3
	subq.w #1, d7
	cmp.w d5, d3
	bhs.s fail
	movea.l a3, a4
	move.w d6, d2
	tst.w d3
	beq.s operandCopyStart

operandSelectLoop
	adda.w d2, a4
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d3
	bne.s operandSelectLoop

operandCopyStart
	move.w d2, d0
	beq.s loop

operandLoop
	move.b (a4)+, (a2)+
	addq.w #1, d1
	subq.w #1, d0
	bne.s operandLoop
	bra.s loop

ok
	moveq #0, d0
	bra.s return

fail
	lea buffers.RuntimeErrorText, a1
	moveq #buffers.RUNTIME_ERROR_TEXT_LEN, d1
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0-a4
	rts
	.bend  ; tkpkgEncodeExecuteProgram

; Inputs:
; - A0: points at a 32-bit little-endian table field whose low 16 bits are consumed.
;
; Outputs:
; - D0.W: decoded low 16-bit value.
; - A0: advanced past the 4-byte field.
;
; Clobbers:
; - D0-D1/A0/CCR
;
; CCR:
; - Reflects D0.W on return.
tkpkgEncodeReadU32Low16	.block
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d1
	move.b (a0)+, d1
	lsl.w #8, d1
	or.w d1, d0
	addq.l #2, a0
	rts
	.bend  ; tkpkgEncodeReadU32Low16

tkpkgEncodeSkipString	.block
	bsr.w tkpkgEncodeReadU32Low16
	adda.w d0, a0
	rts
	.bend  ; tkpkgEncodeSkipString

tkpkgEncodeSkipBytes	.block
	bsr.w tkpkgEncodeReadU32Low16
	adda.w d0, a0
	rts
	.bend  ; tkpkgEncodeSkipBytes

tkpkgEncodeStringEqIgnoreCase	.block
	movem.l d1-d4/a1-a2, -(sp)
	cmp.w d1, d0
	bne.s no
	tst.w d0
	beq.s yes
	move.w d0, d4
	subq.w #1, d4

loop
	move.b (a1)+, d2
	move.b (a2)+, d3
	cmpi.b #'A', d2
	bcs.s leftOk
	cmpi.b #'Z', d2
	bhi.s leftOk
	addi.b #32, d2

leftOk
	cmpi.b #'A', d3
	bcs.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	addi.b #32, d3

compare
	cmp.b d3, d2
	bne.s no
	dbra d4, loop

yes
	moveq #1, d0
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d4/a1-a2
	rts
	.bend  ; tkpkgEncodeStringEqIgnoreCase

tkpkgServicePrepareRequestV1	.block
	bsr.w tkpkgServiceIncrementRequestIdV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	rts
	.bend  ; tkpkgServicePrepareRequestV1

; Validate the native service control block header.
; Inputs: A0 = candidate control block.
; Outputs: D1 = 0 when the header is valid, 1 when the control block is rejected.
; Clobbers: D1/CCR.
; CCR: reflects D1 on return.
tkpkgServiceValidateHeaderV1	.block
	moveq #0, d1
	cmpi.b #$4f, (a0)
	bne.s badControlBlock
	cmpi.b #$54, 1(a0)
	bne.s badControlBlock
	cmpi.b #$36, 2(a0)
	bne.s badControlBlock
	cmpi.b #$35, 3(a0)
	bne.s badControlBlock
	cmpi.b #$01, abi.CB_ABI_VERSION(a0)
	bne.s badControlBlock
	tst.b 5(a0)
	bne.s badControlBlock
	cmpi.b #abi.NATIVE_CONTROL_BLOCK_SIZE_V1, abi.CB_STRUCT_SIZE(a0)
	bne.s badControlBlock
	tst.b 7(a0)
	bne.s badControlBlock
	moveq #0, d1
	rts

badControlBlock
	bsr.w tkpkgServiceSetBadControlBlockV1
	moveq #1, d1
	rts
	.bend  ; tkpkgServiceValidateHeaderV1

tkpkgServiceWriteHeaderV1	.block
	move.b #$4f, (a0)
	move.b #$54, 1(a0)
	move.b #$36, 2(a0)
	move.b #$35, 3(a0)
	move.b #$01, abi.CB_ABI_VERSION(a0)
	clr.b 5(a0)
	move.b #abi.NATIVE_CONTROL_BLOCK_SIZE_V1, abi.CB_STRUCT_SIZE(a0)
	clr.b 7(a0)
	move.b #abi.CAPABILITY_FLAGS_V1, abi.CB_CAPABILITY_FLAGS(a0)
	clr.b 9(a0)
	clr.b abi.CB_RESERVED0(a0)
	clr.b 15(a0)
	bsr.w tkpkgServiceSetStatusOkV1
	rts
	.bend  ; tkpkgServiceWriteHeaderV1

tkpkgServiceIncrementRequestIdV1	.block
	move.b buffers.NextRequestIdLo, d1
	addq.b #1, d1
	move.b d1, buffers.NextRequestIdLo
	bne.s done
	move.b buffers.NextRequestIdHi, d2
	addq.b #1, d2
	move.b d2, buffers.NextRequestIdHi

done
	move.b buffers.NextRequestIdLo, abi.CB_REQUEST_ID(a0)
	move.b buffers.NextRequestIdHi, 13(a0)
	rts
	.bend  ; tkpkgServiceIncrementRequestIdV1

tkpkgServiceSetBadRequestV1	.block
	bsr.w tkpkgServiceSetStatusBadRequestV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	lea buffers.BadRequestText, a1
	moveq #buffers.BAD_REQUEST_TEXT_LEN, d1
	bsr.w tkpkgServiceCopyLastErrorMessageV1
	bsr.w tkpkgServiceWriteLastErrorBufferOffsetV1
	move.b #buffers.BAD_REQUEST_TEXT_LEN, abi.CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b #buffers.BAD_REQUEST_TEXT_LEN, buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	move.b #buffers.LAST_ERROR_KIND_BAD_REQUEST, buffers.StoredLastErrorKind
	rts
	.bend  ; tkpkgServiceSetBadRequestV1

tkpkgServiceSetBadControlBlockV1	.block
	bsr.w tkpkgServiceSetStatusBadControlBlockV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	lea buffers.ControlBlockErrorText, a1
	moveq #buffers.CONTROL_BLOCK_ERROR_TEXT_LEN, d1
	bsr.w tkpkgServiceCopyLastErrorMessageV1
	bsr.w tkpkgServiceWriteLastErrorBufferOffsetV1
	move.b #buffers.CONTROL_BLOCK_ERROR_TEXT_LEN, abi.CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b #buffers.CONTROL_BLOCK_ERROR_TEXT_LEN, buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	move.b #buffers.LAST_ERROR_KIND_BAD_CONTROL, buffers.StoredLastErrorKind
	rts
	.bend  ; tkpkgServiceSetBadControlBlockV1

tkpkgServiceSetRuntimeErrorV1	.block
	bsr.w tkpkgServiceSetStatusRuntimeErrorV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	lea buffers.RuntimeErrorText, a1
	moveq #buffers.RUNTIME_ERROR_TEXT_LEN, d1
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	move.b #buffers.LAST_ERROR_KIND_RUNTIME, buffers.StoredLastErrorKind
	move.b #buffers.RUNTIME_ERROR_TEXT_LEN, buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	rts
	.bend  ; tkpkgServiceSetRuntimeErrorV1

tkpkgServiceSetRuntimeErrorMessageV1	.block
	bsr.w tkpkgServiceSetStatusRuntimeErrorV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	bsr.w tkpkgServiceCopyLastErrorMessageV1
	bsr.w tkpkgServiceWriteLastErrorBufferOffsetV1
	move.b d1, abi.CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b d1, buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	move.b #buffers.LAST_ERROR_KIND_RUNTIME, buffers.StoredLastErrorKind
	rts
	.bend  ; tkpkgServiceSetRuntimeErrorMessageV1

tkpkgServiceClearStoredLastErrorV1	.block
	clr.b buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	move.b #buffers.LAST_ERROR_KIND_NONE, buffers.StoredLastErrorKind
	rts
	.bend  ; tkpkgServiceClearStoredLastErrorV1

tkpkgServiceWriteClearOutputFieldsV1	.block
	clr.b abi.CB_OUTPUT_PTR(a0)
	clr.b 21(a0)
	clr.b abi.CB_OUTPUT_LEN(a0)
	clr.b 23(a0)
	rts
	.bend  ; tkpkgServiceWriteClearOutputFieldsV1

tkpkgServiceWriteClearExtensionFieldsV1	.block
	clr.b abi.CB_EXTENSION_PTR(a0)
	clr.b 25(a0)
	clr.b abi.CB_EXTENSION_LEN(a0)
	clr.b 27(a0)
	rts
	.bend  ; tkpkgServiceWriteClearExtensionFieldsV1

tkpkgServiceWriteClearInputFieldsV1	.block
	clr.b abi.CB_INPUT_PTR(a0)
	clr.b 17(a0)
	clr.b abi.CB_INPUT_LEN(a0)
	clr.b 19(a0)
	rts
	.bend  ; tkpkgServiceWriteClearInputFieldsV1

tkpkgServiceWriteClearLastErrorFieldsV1	.block
	clr.b abi.CB_LAST_ERROR_PTR(a0)
	clr.b 29(a0)
	clr.b abi.CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	rts
	.bend  ; tkpkgServiceWriteClearLastErrorFieldsV1

tkpkgServiceWriteLastErrorBufferOffsetV1	.block
	move.b #buffers.LAST_ERROR_BUFFER_PTR_V1, abi.CB_LAST_ERROR_PTR(a0)
	clr.b 29(a0)
	rts
	.bend  ; tkpkgServiceWriteLastErrorBufferOffsetV1

tkpkgServiceWriteOutputBufferOffsetV1	.block
	move.b #buffers.LAST_ERROR_BUFFER_PTR_V1, abi.CB_OUTPUT_PTR(a0)
	clr.b 21(a0)
	rts
	.bend  ; tkpkgServiceWriteOutputBufferOffsetV1

tkpkgServiceCopyLastErrorMessageV1	.block
	lea buffers.LastErrorBuffer, a2
	move.w d1, d2
	beq.s done

loop
	move.b (a1)+, (a2)+
	subq.w #1, d2
	bne.s loop

done
	clr.b (a2)
	rts
	.bend  ; tkpkgServiceCopyLastErrorMessageV1

tkpkgServiceSetStatusOkV1	.block
	clr.b abi.CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts
	.bend  ; tkpkgServiceSetStatusOkV1

tkpkgServiceSetStatusBadControlBlockV1	.block
	move.b #abi.STATUS_BAD_CONTROL_BLOCK_V1, abi.CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts
	.bend  ; tkpkgServiceSetStatusBadControlBlockV1

tkpkgServiceSetStatusBadRequestV1	.block
	move.b #abi.STATUS_BAD_REQUEST_V1, abi.CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts
	.bend  ; tkpkgServiceSetStatusBadRequestV1

tkpkgServiceSetStatusRuntimeErrorV1	.block
	move.b #abi.STATUS_RUNTIME_ERROR_V1, abi.CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts
	.bend  ; tkpkgServiceSetStatusRuntimeErrorV1

	.endsection
	.endmodule
