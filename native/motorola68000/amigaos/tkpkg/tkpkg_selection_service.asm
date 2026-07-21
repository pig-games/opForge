; Selection and candidate construction for the tkpkg service facade.
;
; This module preserves existing selection order, plan tags, and emitted bytes.
; It intentionally adds no CPU, family, dialect, or instruction semantics.

	.module tkpkg.amigaos.selection_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use opasm.amigaos.engine
	.use opcore.amigaos.expr_bridge

TKPKG_EVAL_EXPR_REQUEST_FIXED_SIZE = 9
TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE = 16
TKPKG_SELECTED_EXTENSION_INPUT_SIZE = 24
TKPKG_SELECTED_EXTENSION_PASS_INPUT_SIZE = 28
TKPKG_SELECTED_STATUS_OK = 0
TKPKG_SELECTED_STATUS_NO_OUTPUT = 1
TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC = 2
TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS = 3
TKPKG_SELECTED_STATUS_OPERAND_ERROR = 4
TKPKG_MSEL_SURFACE_NONE = 0
TKPKG_MSEL_SURFACE_IMMEDIATE = 1
TKPKG_MSEL_SURFACE_ACCUMULATOR = 2
TKPKG_MSEL_SURFACE_DIRECT_X = 3
TKPKG_MSEL_SURFACE_DIRECT_Y = 4
TKPKG_MSEL_SURFACE_INDIRECT = 5
TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X = 6
TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y = 7
EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN = 45
EVAL_EXPR_MISSING_EXPR_TEXT_LEN = 45
EVAL_EXPR_MISSING_EXVM_TEXT_LEN = 42
EVAL_EXPR_BAD_EXPR_VERSION_TEXT_LEN = 46
EVAL_EXPR_BAD_EXVM_VERSION_TEXT_LEN = 44
SELECTED_SELECTOR_UNKNOWN_TEXT_LEN = 33
SELECTED_SELECTOR_UNSUPPORTED_TEXT_LEN = 36
SELECTED_SELECTOR_OPERAND_TEXT_LEN = 30
SELECTED_OPERAND_BAD_EXVM_TEXT_LEN = 33
SELECTED_OPERAND_EMPTY_TEXT_LEN = 30
SELECTED_OPERAND_UNEXPECTED_TEXT_LEN = 40
SELECTED_OPERAND_BRIDGE_TEXT_LEN = 38
SELECTED_OPERAND_LENGTH_TEXT_LEN = 38
SELECTED_OPERAND_COMPILE_TEXT_LEN = 39
SELECTED_OPERAND_FINALIZE_TEXT_LEN = 40
SELECTED_OPERAND_EVAL_TEXT_LEN = 38
SELECTED_OPERAND_HEX_PARSE_TEXT_LEN = 41
SELECTED_OPERAND_LITERAL_EMIT_TEXT_LEN = 44
SELECTED_OPERAND_TRAILING_TEXT_LEN = 38
SELECTED_OPERAND_SINGLE_TEXT_LEN = 38
EXPRVM_MISSING_END_TEXT_LEN = 26
EXPRVM_UNKNOWN_OPCODE_TEXT_LEN = 29
EXPRVM_LITERAL_READ_TEXT_LEN = 34
EXPRVM_LITERAL_PUSH_TEXT_LEN = 34
EXPRVM_REQUIRE_SCALAR_TEXT_LEN = 36
EXPRVM_END_STACK_TEXT_LEN = 31
EXPRVM_POP_TEXT_LEN = 25
SCOPED_OWNER_FAMILY = 0
SCOPED_OWNER_CPU = 1
SCOPED_OWNER_DIALECT = 2

	.section data, kind=data
	.priv

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

	.endsection

	.section bss, kind=bss

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
EncodeSelectedSessionPass
	.res word, 1
EncodeSelectedExvmOpcodeVersion
	.res word, 1
EncodeSelectedExprOpcodeVersion
	.res word, 1
EncodeSelectedOperandStatus
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
EncodeSelectedMselOwnerPtr
	.res long, 1
EncodeSelectedMselOwnerLen
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
	.align 2
EncodeSelectedMselMatchFlags

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

	.endsection

	.section code, kind=code
	.pub

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

; Convert a no-output candidate result into the existing public diagnostic.
; Inputs: D2 = 0 for no mnemonic match, nonzero for an unsupported match.
; Outputs: D0 = runtime-error status; D1/A1 = diagnostic length/text.
; Clobbers: D0-D2/A1/CCR.
noOutputErrorV1	.block
	tst.b d2
	beq.s unknown
	lea SelectedSelectorUnsupportedText, a1
	moveq #SELECTED_SELECTOR_UNSUPPORTED_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

unknown
	lea SelectedSelectorUnknownText, a1
	moveq #SELECTED_SELECTOR_UNKNOWN_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts
	.bend  ; noOutputErrorV1

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
	moveq #0, d0
	move.w engine.opasmEngineSessionPass.l, d0
	move.w d0, EncodeSelectedSessionPass
	clr.l EncodeSelectedMselShapePtr
	clr.w EncodeSelectedMselShapeLen
	cmpi.w #TKPKG_SELECTED_EXTENSION_INPUT_SIZE, d5
	bcs.s resolveVersions
	movea.l (a5)+, a1
	move.l (a5)+, d0
	move.l a1, EncodeSelectedMselShapePtr
	move.w d0, EncodeSelectedMselShapeLen
	cmpi.w #TKPKG_SELECTED_EXTENSION_PASS_INPUT_SIZE, d5
	bcs.s resolveVersions
	move.l (a5)+, d0
	move.w d0, EncodeSelectedSessionPass
	bra.s resolveVersions

noExtension
	clr.l EncodeSelectedLabelNamePtr
	clr.l EncodeSelectedLabelValuePtr
	clr.l EncodeSelectedLabelCount
	clr.l EncodeSelectedCurrentPc
	moveq #0, d0
	move.w engine.opasmEngineSessionPass.l, d0
	move.w d0, EncodeSelectedSessionPass
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
	move.w EncodeSelectedOperandStatus, d0
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
	moveq #0, d2
	moveq #0, d0
	bra.w return

noOutput
	moveq #0, d1
	moveq #0, d2
	btst #0, EncodeSelectedMselMatchFlags
	sne d2
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
	clr.w EncodeSelectedMselMatchFlags
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
	move.l a1, EncodeSelectedMselOwnerPtr
	move.w d0, EncodeSelectedMselOwnerLen
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	move.l a2, -(sp)
	move.w d2, d1
	movea.l a5, a2
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	move.b d0, d5
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	tst.b d5
	beq.s skipShapeCompare
	move.l a1, EncodeSelectedCurrentShapePtr
	move.w d0, EncodeSelectedCurrentShapeLen
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
	move.l a1, -(sp)
	move.w d0, -(sp)

	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	tst.b d5
	beq.w skipPlanStore
	move.l a1, -(sp)
	move.w d0, -(sp)
	move.w EncodeSelectedMselOwnerLen, d0
	movea.l EncodeSelectedMselOwnerPtr, a1
	bsr.w tkpkgSelectedMselOwnerMatchesV1
	tst.b d0
	beq.w skipPlanStoreWithPlanFrame
	bset #0, EncodeSelectedMselMatchFlags
	move.w 6(sp), d0
	move.w d0, EncodeSelectedMselModeLen
	move.l 8(sp), d0
	move.l d0, EncodeSelectedMselModePtr
	move.w (sp), d0
	move.w d0, EncodeSelectedMselPlanLen
	move.l 2(sp), d0
	move.l d0, EncodeSelectedMselPlanPtr
	move.l a2, -(sp)
	move.w d7, -(sp)
	bsr.w tkpkgMselTryBuildCandidateV1
	move.w (sp)+, d7
	movea.l (sp)+, a2
	addq.l #6, sp
	addq.l #6, sp
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	beq.w return
	cmpi.l #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	beq.w maybeReturnOperandError
	bra.w skipPlanRecordNoFrame

maybeReturnOperandError
	tst.w EncodeSelectedMselShapeLen
	beq.w skipPlanRecordNoFrame
	tst.l EncodeSelectedMselShapePtr
	beq.w skipPlanRecordNoFrame
	bra.w return

skipPlanStoreWithPlanFrame
	addq.l #6, sp

skipPlanStore
	addq.l #6, sp

skipPlanRecordNoFrame
	moveq #4, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w unsupported
	lea 4(a2), a2
	dbf d7, entryLoop

noOutput
	moveq #0, d1
	btst #0, EncodeSelectedMselMatchFlags
	beq.s unknownMnemonic
	moveq #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0
	bra.s return

unknownMnemonic
	moveq #TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC, d0
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
	lea PlanDispatchTable(pc), a4
	moveq #4, d7

dispatchPlanLoop
	movea.l (a4)+, a2
	moveq #0, d1
	move.w (a4)+, d1
	addq.l #2, a4
	bsr.w tkpkgMselPlanEqualsV1
	beq.s dispatchPlanNext
	movea.l (a4), a0
	jmp (a0)

dispatchPlanNext
	adda.w #4, a4
	dbf d7, dispatchPlanLoop
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
	bne.s tryBranchEvaluate
	moveq #1, d6
	bra.w tryUnstablePassOneOperand

tryBranchEvaluate
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
	moveq #0, d6
	cmpi.w #1, engine.opasmEngineSessionPass.l
	beq.s pairPassCaptured
	moveq #1, d6

pairPassCaptured
	move.w d6, -(sp)
	move.l PairAPtr.l, d0
	move.l d0, EncodeSelectedMselExprPtr
	move.w PairALen.l, d0
	move.w d0, EncodeSelectedMselExprLen
	bsr.w tkpkgMselEvalPairPartOperandV1
	move.w (sp)+, d6
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
	tst.w d6
	bne.s tryPairSecondStable
	clr.l PairBVal.l
	bra.w buildPairOperand

tryPairSecondStable
	move.l PairBPtr.l, d0
	move.l d0, EncodeSelectedMselExprPtr
	move.w PairBLen.l, d0
	move.w d0, EncodeSelectedMselExprLen
	bsr.w tkpkgMselEvalPairPartOperandV1
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
	bsr.w tkpkgMselCurrentShapeCodeV1
	cmpi.b #TKPKG_MSEL_SURFACE_ACCUMULATOR, d0
	bne.s noOutput
	bsr.w tkpkgMselExprIsAccumulatorAV1
	beq.s noOutput

buildNoneOperand
	moveq #0, d6
	bra.w buildOperand

noOutput
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.w return

operandError
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	bra.w return

buildOperand
	bsr.w tkpkgMselWriteCandidateEnvelopeV1
	bra.w return

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
	bra.w return

	.align 2
PlanDispatchTable
	.long TkpkgMselPlanNoneText
	.word 4
	.word 0
	.long buildNone
	.long TkpkgMselPlanU8Text
	.word 2
	.word 0
	.long tryU8
	.long TkpkgMselPlanU16Text
	.word 3
	.word 0
	.long tryU16
	.long TkpkgMselPlanBranch8Text
	.word 4
	.word 0
	.long tryBranchOffset8
	.long TkpkgMselPlanPairU8Rel8Text
	.word 12
	.word 0
	.long tryPairU8Rel8

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

; Evaluate one operand part from a package-owned pair plan.
; Inputs: EncodeSelectedMselExprPtr/Len identify the part to evaluate.
; Outputs: D0 = selected status; EncodeSelectedMselValue set on success.
; Clobbers: D0-D1/CCR plus tkpkgMselEvalOperandV1 clobbers.
; CCR: reflects D0 on return.
tkpkgMselEvalPairPartOperandV1	.block
	move.l EncodeSelectedMselModePtr, -(sp)
	move.w EncodeSelectedMselModeLen, -(sp)
	move.l EncodeSelectedCurrentShapePtr, -(sp)
	move.w EncodeSelectedCurrentShapeLen, -(sp)
	clr.l EncodeSelectedCurrentShapePtr
	clr.w EncodeSelectedCurrentShapeLen
	clr.l EncodeSelectedMselModePtr
	clr.w EncodeSelectedMselModeLen
	bsr.w tkpkgMselEvalOperandV1
	move.w (sp)+, d1
	move.w d1, EncodeSelectedCurrentShapeLen
	move.l (sp)+, d1
	move.l d1, EncodeSelectedCurrentShapePtr
	move.w (sp)+, d1
	move.w d1, EncodeSelectedMselModeLen
	move.l (sp)+, d1
	move.l d1, EncodeSelectedMselModePtr
	tst.l d0
	rts
	.bend  ; tkpkgMselEvalPairPartOperandV1

tkpkgMselEvalOperandV1	.block
	bsr.w tkpkgMselCurrentShapeCodeV1
	moveq #0, d7
	moveq #0, d6
	moveq #0, d5
	cmpi.b #TKPKG_MSEL_SURFACE_IMMEDIATE, d0
	bne.s checkDirectX
	moveq #1, d7
	bra.s haveShapeSurface

checkDirectX
	cmpi.b #TKPKG_MSEL_SURFACE_DIRECT_X, d0
	bne.s checkDirectY
	moveq #'x', d6
	bra.s haveShapeSurface

checkDirectY
	cmpi.b #TKPKG_MSEL_SURFACE_DIRECT_Y, d0
	bne.s checkIndirect
	moveq #'y', d6
	bra.s haveShapeSurface

checkIndirect
	cmpi.b #TKPKG_MSEL_SURFACE_INDIRECT, d0
	bne.s checkIndexedIndirectX
	moveq #1, d5
	bra.s haveShapeSurface

checkIndexedIndirectX
	cmpi.b #TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X, d0
	bne.s checkSurfaceIndexedY
	moveq #2, d5
	bra.s haveShapeSurface

checkSurfaceIndexedY
	cmpi.b #TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y, d0
	bne.s haveShapeSurface
	moveq #3, d5

haveShapeSurface
	tst.b d5
	bne.s haveOperandSurface
	bsr.w tkpkgMselCurrentModeParenCodeV1
	move.b d0, d5
	tst.b d6
	bne.s haveOperandSurface
	tst.b d5
	bne.s haveOperandSurface
	bsr.w tkpkgMselCurrentModeIndexSuffixV1
	move.b d0, d6

haveOperandSurface
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
	moveq #1, d6
	bra.s evalOperandText

stripIndirectIndexedY
	moveq #'y', d6
	bsr.w tkpkgMselStripIndexSuffixV1
	tst.b d1
	bne.s evalOperandText
	bsr.w tkpkgMselStripOuterParensV1
	moveq #1, d6

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

; Transitional native seam:
; - Package-owned shape and mode tags are collapsed into compact local surface
;   codes here before operand evaluation mutates source spans.
; - Keep this lookup table-driven; do not reintroduce per-shape compare ladders
;   elsewhere in tkpkg selector runtime code.
;
; Inputs:
; - A0: table of (`.long text`, `.word len`, `.byte code`, `.byte pad`) entries.
; - A1/D0: active text pointer and length.
; - D7: entry count minus one for DBF iteration.
;
; Outputs:
; - D0: matched surface code or `TKPKG_MSEL_SURFACE_NONE`.
;
; Clobbers:
; - D0-D5/D7/A0-A4/CCR
;
; CCR:
; - Reflects D0 on return.
tkpkgMselLookupTaggedTextCodeV1	.block
	movem.l d1-d5/a1-a4, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	move.w d0, d5
	move.w d7, d4

loop
	movea.l a4, a1
	move.w d5, d0
	movea.l (a3)+, a2
	moveq #0, d1
	move.w (a3)+, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	bne.s match
	addq.l #2, a3
	dbf d4, loop
	moveq #TKPKG_MSEL_SURFACE_NONE, d0
	bra.s return

match
	moveq #0, d0
	move.b (a3), d0

return
	movem.l (sp)+, d1-d5/a1-a4
	rts
	.bend  ; tkpkgMselLookupTaggedTextCodeV1

tkpkgMselCurrentShapeCodeV1	.block
	movea.l EncodeSelectedCurrentShapePtr, a1
	move.w EncodeSelectedCurrentShapeLen, d0
	lea CurrentShapeCodeTable(pc), a0
	moveq #6, d7
	bsr.w tkpkgMselLookupTaggedTextCodeV1
	rts

	.align 2
CurrentShapeCodeTable
	.long TkpkgMselShapeImmediateText
	.word 9
	.byte TKPKG_MSEL_SURFACE_IMMEDIATE, 0
	.long TkpkgMselShapeAccumulatorText
	.word 11
	.byte TKPKG_MSEL_SURFACE_ACCUMULATOR, 0
	.long TkpkgMselShapeDirectXText
	.word 8
	.byte TKPKG_MSEL_SURFACE_DIRECT_X, 0
	.long TkpkgMselShapeDirectYText
	.word 8
	.byte TKPKG_MSEL_SURFACE_DIRECT_Y, 0
	.long TkpkgMselShapeIndirectText
	.word 8
	.byte TKPKG_MSEL_SURFACE_INDIRECT, 0
	.long TkpkgMselShapeIndexedIndirectXText
	.word 18
	.byte TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X, 0
	.long TkpkgMselShapeIndirectIndexedYText
	.word 18
	.byte TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y, 0
	.bend  ; tkpkgMselCurrentShapeCodeV1

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

tkpkgMselCurrentModeParenCodeV1	.block
	movea.l EncodeSelectedMselModePtr, a1
	move.w EncodeSelectedMselModeLen, d0
	lea CurrentModeParenCodeTable(pc), a0
	moveq #2, d7
	bsr.w tkpkgMselLookupTaggedTextCodeV1
	rts

	.align 2
CurrentModeParenCodeTable
	.long TkpkgMselShapeIndirectText
	.word 8
	.byte TKPKG_MSEL_SURFACE_INDIRECT, 0
	.long TkpkgMselModeIndexedIndirectXText
	.word 16
	.byte TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X, 0
	.long TkpkgMselModeIndirectIndexedYText
	.word 16
	.byte TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y, 0
	.bend  ; tkpkgMselCurrentModeParenCodeV1

tkpkgMselCurrentModeIndexSuffixV1	.block
	movea.l EncodeSelectedMselModePtr, a1
	move.w EncodeSelectedMselModeLen, d0
	cmpi.w #2, d0
	bcs.s none
	subq.w #1, d0
	move.b 0(a1, d0.w), d0
	ori.b #$20, d0
	cmpi.b #'x', d0
	beq.s return
	cmpi.b #'y', d0
	beq.s return

none
	moveq #0, d0

return
	rts
	.bend  ; tkpkgMselCurrentModeIndexSuffixV1

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

; - EncodeSelectedOperandStatus updated on failure.
;
; Clobbers:
; - D0-D2/D4-D7/A0-A2/A6/CCR
;
; CCR:
; - Reflects D0 on return.
encodeSelectedOperandV1	.block
	movem.l d1-d2/d6-d7/a1-a2/a6, -(sp)
	clr.w EncodeSelectedOperandStatus
	movea.l EncodeSelectedLabelNamePtr, a1
	movea.l EncodeSelectedLabelValuePtr, a2
	move.l EncodeSelectedLabelCount, d1
	move.l EncodeSelectedCurrentPc, d2
	moveq #0, d4
	move.w EncodeSelectedExvmOpcodeVersion, d4
	cmpi.w #1, d4
	beq.s haveExvm
	move.w #1, EncodeSelectedOperandStatus
	moveq #1, d0
	bra.w return

haveExvm
	tst.l d0
	bne.s haveText
	move.w #2, EncodeSelectedOperandStatus
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
	move.w #3, EncodeSelectedOperandStatus
	moveq #1, d0
	bra.w return

textOk
	bsr.w encodeSelectedOperandTryLabelV1
	tst.l d7
	bne.w return
	moveq #0, d5
	moveq #1, d5
	moveq #0, d6
	move.w EncodeSelectedSessionPass.l, d6
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
	move.w #4, EncodeSelectedOperandStatus
	bra.w return

compileFail
	move.w #6, EncodeSelectedOperandStatus
	bra.w return

finalizeFail
	move.w #7, EncodeSelectedOperandStatus
	bra.w return

evalFail
	move.w #8, EncodeSelectedOperandStatus
	bra.w return

hexParseFail
	move.w #31, EncodeSelectedOperandStatus
	bra.w return

literalEmitFail
	move.w #32, EncodeSelectedOperandStatus
	bra.w return

trailingFail
	move.w #33, EncodeSelectedOperandStatus
	bra.w return

singleFail
	move.w #34, EncodeSelectedOperandStatus
	bra.w return

exprVmFail
	move.w d0, EncodeSelectedOperandStatus

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

	.endsection
	.endmodule
