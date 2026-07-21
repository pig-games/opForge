; Request dispatch and lifecycle scaffolding for the first tkpkg native slice.

	.module tkpkg.amigaos.service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.service_request as request
	.use tkpkg.amigaos.service_status as status
	.use opasm.amigaos.engine
	.use opcore.amigaos.expr_bridge
	.use tkpkg.amigaos.expression_service as expression
	.use tkpkg.amigaos.selection_service as selection
	.use tkpkg.amigaos.encode_service as encoding
	.use tkpkg.amigaos.package_loader
	.use tkpkg.amigaos.parse_service as parser
	.use tkpkg.amigaos.pipeline
	.use tkpkg.amigaos.tokenizer_vm

TKPKG_EVAL_EXPR_REQUEST_FIXED_SIZE   = 9
TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE = 16
TKPKG_SELECTED_EXTENSION_INPUT_SIZE  = 24
TKPKG_SELECTED_EXTENSION_PASS_INPUT_SIZE = 28
TKPKG_SELECTED_STATUS_OK             = 0
TKPKG_SELECTED_STATUS_NO_OUTPUT      = 1
TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC = 2
TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS = 3
TKPKG_SELECTED_STATUS_OPERAND_ERROR  = 4
TKPKG_MSEL_SURFACE_NONE             = 0
TKPKG_MSEL_SURFACE_IMMEDIATE        = 1
TKPKG_MSEL_SURFACE_ACCUMULATOR      = 2
TKPKG_MSEL_SURFACE_DIRECT_X         = 3
TKPKG_MSEL_SURFACE_DIRECT_Y         = 4
TKPKG_MSEL_SURFACE_INDIRECT         = 5
TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X = 6
TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y = 7
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
EVAL_EXPR_ZERO_OUTPUT_TEXT_LEN       = 39
EVAL_EXPR_OK_ZERO_LEN_TEXT_LEN       = 40
EVAL_EXPR_BRIDGE_CODE1_TEXT_LEN      = 41
EVAL_EXPR_BRIDGE_CODE3_TEXT_LEN      = 50
EVAL_EXPR_BRIDGE_CODE4_TEXT_LEN      = 51
EVAL_EXPR_BRIDGE_CODE5_TEXT_LEN      = 49
EVAL_EXPR_BRIDGE_CODE33_TEXT_LEN     = 48
EVAL_EXPR_BRIDGE_CODE34_TEXT_LEN     = 47
EVAL_EXPR_NO_LABEL_CONTEXT_TEXT_LEN  = 49
EVAL_EXPR_SLICE_START_TEXT_LEN       = 55
EVAL_EXPR_SLICE_OTHER_TEXT_LEN       = 59
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

EvaluateExprZeroOutputText
	.byte "OTR904: evaluate expression returned no output", 0

EvaluateExprOkZeroLenText
	.byte "OTR905: evaluate expression ok saw zero len", 0

EvaluateExprBridgeCode1Text
	.byte "OTR920: expression bridge returned code 1", 0

EvaluateExprBridgeCode3Text
	.byte "OTR923: expression bridge reported compile failure", 0

EvaluateExprBridgeCode4Text
	.byte "OTR924: expression bridge reported finalize failure", 0

EvaluateExprBridgeCode5Text
	.byte "OTR925: expression bridge reported exprvm failure", 0

EvaluateExprBridgeCode33Text
	.byte "OTR921: expression bridge reported trailing text", 0

EvaluateExprBridgeCode34Text
	.byte "OTR922: expression bridge reported missing term", 0

EvaluateExprNoLabelContextText
	.byte "OTR930: evaluate expression had no label context", 0

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

	.res word, 1
EvaluateExpressionOutputLen
	.res word, 1
EncodeSelectedOutputLen
	.res word, 1
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
	jsr encoding.encodeInstructionV1
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
	jsr selection.selectInstructionV1
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
	jsr encoding.encodeSelectedInstructionV1
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
	move.w d1, EncodeSelectedOutputLen.l
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	move.w EncodeSelectedOutputLen.l, d1
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
	move.w d1, EvaluateExpressionOutputLen.l
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	move.w EvaluateExpressionOutputLen.l, d1
	bne.s haveEvaluateExpressionOutput
	lea EvaluateExprOkZeroLenText, a1
	moveq #EVAL_EXPR_OK_ZERO_LEN_TEXT_LEN, d1
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

haveEvaluateExpressionOutput
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
; Compatibility transition for Item 5.5.1. The expression service owns the
; request envelope and bridge execution; this facade step retains only package
; contract validation until the neutral context and contract owners land.
evaluateExpressionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	jsr expression.prepareV1
	bne.s return
	bsr.w resolveExpressionContractVersionsV1
	bne.s return
	moveq #0, d4
	move.w d6, d4
	moveq #0, d5
	move.w d7, d5
	jsr expression.executePreparedV1

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; evaluateExpressionV1

; Inputs:
; - A0/D0: operand text pointer and length.
; - Selected-envelope EXVM opcode version/current-PC/label context fields.
;
; Outputs:
; - D0: 0 on success, nonzero on operand parse/eval failure.
; - D3: evaluated operand value on success.
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
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a3)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a3)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d7
	move.b (a3)+, d7
	moveq #0, d1
	move.b (a3)+, d1
	lsl.l #8, d1
	or.l d1, d7
	moveq #0, d1
	move.b (a3)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d7
	moveq #0, d1
	move.b (a3)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d7
	beq.s missing
	lea buffers.PackageStorage, a6
	lea 0(a6, d0.l), a2
	lea 0(a2, d7.l), a6
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

; Compatibility delegate for the Item 5.5 facade-to-parser adapter. It contains
; no parser request-envelope implementation and is deleted when callers migrate.
tkpkgServiceParseLineV1	.block
	jmp parser.parseLineV1
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

; Compatibility delegates are the Item 5.4.1 facade-to-request adapter. They
; contain no lifecycle implementation and are deleted when callers migrate.
tkpkgServicePrepareRequestV1	.block
	jmp request.prepareRequestV1
	.bend  ; tkpkgServicePrepareRequestV1

tkpkgServiceValidateHeaderV1	.block
	jmp request.validateHeaderV1
	.bend  ; tkpkgServiceValidateHeaderV1

tkpkgServiceWriteHeaderV1	.block
	jmp request.writeHeaderV1
	.bend  ; tkpkgServiceWriteHeaderV1

tkpkgServiceIncrementRequestIdV1	.block
	jmp request.incrementRequestIdV1
	.bend  ; tkpkgServiceIncrementRequestIdV1

tkpkgServiceWriteClearExtensionFieldsV1	.block
	jmp request.writeClearExtensionFieldsV1
	.bend  ; tkpkgServiceWriteClearExtensionFieldsV1

tkpkgServiceWriteClearInputFieldsV1	.block
	jmp request.writeClearInputFieldsV1
	.bend  ; tkpkgServiceWriteClearInputFieldsV1

; Compatibility delegates are the Item 5.4 facade-to-status adapter.  They
; contain no status/error implementation and are deleted when callers migrate.
tkpkgServiceSetBadRequestV1	.block
	jmp status.setBadRequestV1
	.bend  ; tkpkgServiceSetBadRequestV1

tkpkgServiceSetBadControlBlockV1	.block
	jmp status.setBadControlBlockV1
	.bend  ; tkpkgServiceSetBadControlBlockV1

tkpkgServiceSetRuntimeErrorV1	.block
	jmp status.setRuntimeErrorV1
	.bend  ; tkpkgServiceSetRuntimeErrorV1

tkpkgServiceSetRuntimeErrorMessageV1	.block
	jmp status.setRuntimeErrorMessageV1
	.bend  ; tkpkgServiceSetRuntimeErrorMessageV1

tkpkgServiceClearStoredLastErrorV1	.block
	jmp status.clearStoredLastErrorV1
	.bend  ; tkpkgServiceClearStoredLastErrorV1

tkpkgServiceWriteClearOutputFieldsV1	.block
	jmp status.writeClearOutputFieldsV1
	.bend  ; tkpkgServiceWriteClearOutputFieldsV1

tkpkgServiceWriteClearLastErrorFieldsV1	.block
	jmp status.writeClearLastErrorFieldsV1
	.bend  ; tkpkgServiceWriteClearLastErrorFieldsV1

tkpkgServiceWriteLastErrorBufferOffsetV1	.block
	jmp status.writeLastErrorBufferOffsetV1
	.bend  ; tkpkgServiceWriteLastErrorBufferOffsetV1

tkpkgServiceWriteOutputBufferOffsetV1	.block
	jmp status.writeOutputBufferOffsetV1
	.bend  ; tkpkgServiceWriteOutputBufferOffsetV1

tkpkgServiceCopyLastErrorMessageV1	.block
	jmp status.copyLastErrorMessageV1
	.bend  ; tkpkgServiceCopyLastErrorMessageV1

tkpkgServiceSetStatusOkV1	.block
	jmp status.setStatusOkV1
	.bend  ; tkpkgServiceSetStatusOkV1

tkpkgServiceSetStatusBadControlBlockV1	.block
	jmp status.setStatusBadControlBlockV1
	.bend  ; tkpkgServiceSetStatusBadControlBlockV1

tkpkgServiceSetStatusBadRequestV1	.block
	jmp status.setStatusBadRequestV1
	.bend  ; tkpkgServiceSetStatusBadRequestV1

tkpkgServiceSetStatusRuntimeErrorV1	.block
	jmp status.setStatusRuntimeErrorV1
	.bend  ; tkpkgServiceSetStatusRuntimeErrorV1

	.endsection
	.endmodule
