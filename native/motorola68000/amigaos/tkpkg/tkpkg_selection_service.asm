; Selection and candidate construction for the tkpkg service facade.
;
; This module preserves existing selection order, plan tags, and emitted bytes.
; It intentionally adds no CPU, family, dialect, or instruction semantics.

	.module tkpkg.amigaos.selection_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.selection_state as state
	.use tkpkg.amigaos.operand_runtime as operand
	.use tkpkg.amigaos.runtime_context as context
	.use opcore.amigaos.expr_bridge

TKPKG_EVAL_EXPR_REQUEST_FIXED_SIZE = 9
TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE = 16
TKPKG_SELECTED_EXTENSION_INPUT_SIZE = 24
TKPKG_SELECTED_EXTENSION_PASS_INPUT_SIZE = 28
TKPKG_SELECTED_EXTENSION_RESOLVER_INPUT_SIZE = 32
TKPKG_SELECTED_STATUS_OK = 0
TKPKG_SELECTED_STATUS_NO_OUTPUT = 1
TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC = 2
TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS = 3
TKPKG_SELECTED_STATUS_OPERAND_ERROR = 4
TKPKG_SELECTED_STATUS_RUNTIME_ERROR = 5
TKPKG_SELECTED_STATUS_SEMANTIC_REJECT = 6
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

RequiredValuePrefixText
	.byte "required_value_program:"

ValuePrefixText
	.byte "value_program:"

IndirectTupleRegisterPrefixText
	.byte "indirect_tuple_reg"

IndirectTupleQualifiedRegisterPrefixText
	.byte "indirect_tuple_qualified_reg"

IndirectTupleValuePrefixText
	.byte "indirect_tuple_value"

IndirectTupleArityPrefixText
	.byte "indirect_tuple_arity"

UnaryPlusIndirectRegisterPrefixText
	.byte "unary_plus_indirect_reg"

UnaryMinusIndirectRegisterPrefixText
	.byte "unary_minus_indirect_reg"

MemberShapePrefixText
	.byte "member_shape"

MemberPrefixText
	.byte "member"

TargetPrefixText
	.byte "target:"

RejectMnemonicPlaceholderText
	.byte "{mnemonic}"

RejectFormPlaceholderText
	.byte "{form}"

ImmediateRegisterShapeText
	.byte "immediate_register"

ImmediateDirectShapeText
	.byte "immediate_direct"

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
	move.l a0, -(sp)
	clr.l state.EncodeSelectedSemanticPlanPtr
	clr.b state.EncodeSelectedSemanticPlanKind
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
	move.l a3, state.EncodeSelectedMnemonicPtr
	move.l a4, state.EncodeSelectedSourceLinePtr
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
	bcc.s haveBaseExtension
	bra.w noExtension
haveBaseExtension
	lea 0(a0, d0.W), a5
	movea.l (a5)+, a1
	movea.l (a5)+, a2
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l a1, state.EncodeSelectedLabelNamePtr
	move.l a2, state.EncodeSelectedLabelValuePtr
	move.l d1, state.EncodeSelectedLabelCount
	move.l d2, state.EncodeSelectedCurrentPc
	jsr context.getPassV1
	move.w d0, state.EncodeSelectedSessionPass
	clr.l state.EncodeSelectedMselShapePtr
	clr.w state.EncodeSelectedMselShapeLen
	clr.l state.EncodeSelectedSymbolResolverPtr
	cmpi.w #TKPKG_SELECTED_EXTENSION_INPUT_SIZE, d5
	bcc.s haveShapeExtension
	bra.w resolveVersions
haveShapeExtension
	movea.l (a5)+, a1
	move.l (a5)+, d0
	move.l a1, state.EncodeSelectedMselShapePtr
	move.w d0, state.EncodeSelectedMselShapeLen
	cmpi.w #TKPKG_SELECTED_EXTENSION_PASS_INPUT_SIZE, d5
	bcc.s havePassExtension
	bra.w resolveVersions
havePassExtension
	move.l (a5)+, d0
	move.w d0, state.EncodeSelectedSessionPass
	cmpi.w #TKPKG_SELECTED_EXTENSION_RESOLVER_INPUT_SIZE, d5
	bcc.s haveResolverExtension
	bra.w resolveVersions
haveResolverExtension
	move.l (a5)+, d0
	move.l d0, state.EncodeSelectedSymbolResolverPtr
	bra.s resolveVersions

noExtension
	clr.l state.EncodeSelectedLabelNamePtr
	clr.l state.EncodeSelectedLabelValuePtr
	clr.l state.EncodeSelectedLabelCount
	clr.l state.EncodeSelectedCurrentPc
	jsr context.getPassV1
	move.w d0, state.EncodeSelectedSessionPass
	clr.l state.EncodeSelectedMselShapePtr
	clr.w state.EncodeSelectedMselShapeLen
	clr.l state.EncodeSelectedSymbolResolverPtr

resolveVersions
	move.l d7, -(sp)
	move.l d6, -(sp)
	move.l d5, -(sp)
	bsr.w resolveExpressionContractVersionsV1
	bne.w resolveFail
	move.w d6, state.EncodeSelectedExvmOpcodeVersion
	move.w d7, state.EncodeSelectedExprOpcodeVersion
	move.l (sp)+, d5
	move.l (sp)+, d6
	move.l (sp)+, d7
	move.l (sp)+, d2
	move.l (sp)+, d4
	movea.l state.EncodeSelectedSourceLinePtr, a4
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
	move.l a1, state.EncodeSelectedMselExprPtr
	move.w d1, state.EncodeSelectedMselExprLen
	bra.s buildCandidate

noOperandSpan
	tst.w d4
	bne.w badPayload
	clr.l state.EncodeSelectedMselExprPtr
	clr.w state.EncodeSelectedMselExprLen
	movea.l state.EncodeSelectedSourceLinePtr, a1
	moveq #0, d1

buildCandidate
	move.l a1, -(sp)
	move.l d1, -(sp)
	movea.l state.EncodeSelectedMnemonicPtr, a0
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
	cmpi.l #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	beq.w semanticReject
	lea buffers.RuntimeErrorText, a1
	moveq #buffers.RUNTIME_ERROR_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

semanticReject
	lea buffers.TokenScratchBuffer, a1
	move.w d7, d1
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
	move.w state.EncodeSelectedOperandStatus, d0
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
	btst #2, state.EncodeSelectedMselMatchFlags
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
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; buildSelectedEnvelopeV1

tkpkgBuildSelectedEnvelopeFromMselV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	clr.w state.EncodeSelectedMselMatchFlags
	clr.w state.EncodeSelectedMselFallbackLen
	movea.l a0, a5
	move.w d0, d2
	move.w d2, state.EncodeSelectedMselMnemonicLen
	tst.w d2
	beq.w noOutput
	bsr.w tkpkgInferSelectedPackageShapeV1
	lea buffers.MselChunkOffsetLo, a3
	bsr.w tkpkgServiceChunkPtrFromLocatorV1
	bne.w compactSelector
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
	move.l a1, state.EncodeSelectedMselOwnerPtr
	move.w d0, state.EncodeSelectedMselOwnerLen
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	move.l a2, -(sp)
	move.w d2, d1
	movea.l a5, a2
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	move.b d0, d5
	beq.s locateShape
	move.w state.EncodeSelectedMselOwnerLen, d0
	movea.l state.EncodeSelectedMselOwnerPtr, a1
	bsr.w tkpkgSelectedMselOwnerMatchesV1
	move.b d0, d5
	beq.s locateShape
	bset #2, state.EncodeSelectedMselMatchFlags

locateShape
	bsr.w tkpkgServiceLocateStringV1
	bne.w unsupported
	tst.b d5
	beq.s skipShapeCompare
	move.l a1, state.EncodeSelectedCurrentShapePtr
	move.w d0, state.EncodeSelectedCurrentShapeLen
	tst.w state.EncodeSelectedMselShapeLen
	beq.s skipShapeCompare
	tst.l state.EncodeSelectedMselShapePtr
	beq.s skipShapeCompare
	move.l a2, -(sp)
	move.w state.EncodeSelectedMselShapeLen, d1
	movea.l state.EncodeSelectedMselShapePtr, a2
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
	bset #0, state.EncodeSelectedMselMatchFlags
	move.w 6(sp), d0
	move.w d0, state.EncodeSelectedMselModeLen
	move.l 8(sp), d0
	move.l d0, state.EncodeSelectedMselModePtr
	move.w (sp), d0
	move.w d0, state.EncodeSelectedMselPlanLen
	move.l 2(sp), d0
	move.l d0, state.EncodeSelectedMselPlanPtr
	moveq #4, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w skipPlanStoreWithPlanFrame
	moveq #0, d4
	move.b 2(a2), d4
	move.l a2, -(sp)
	move.w d7, -(sp)
	jsr operand.tkpkgMselTryBuildCandidateV1
	move.w (sp)+, d7
	movea.l (sp)+, a2
	addq.l #6, sp
	addq.l #6, sp
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s candidateNotOk
	tst.b d4
	beq.w return
	tst.b state.EncodeSelectedMselUnstable
	beq.w return
	bset #1, state.EncodeSelectedMselMatchFlags
	move.w d1, state.EncodeSelectedMselFallbackLen
	bra.w skipPlanRecordNoFrame

candidateNotOk
	cmpi.l #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	beq.w maybeReturnOperandError
	bra.w skipPlanRecordNoFrame

maybeReturnOperandError
	tst.w state.EncodeSelectedMselShapeLen
	beq.w skipPlanRecordNoFrame
	tst.l state.EncodeSelectedMselShapePtr
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
	btst #1, state.EncodeSelectedMselMatchFlags
	beq.s noFallback
	moveq #0, d1
	move.w state.EncodeSelectedMselFallbackLen, d1
	moveq #0, d2
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s return

noFallback
	moveq #0, d1
	btst #2, state.EncodeSelectedMselMatchFlags
	beq.s unknownMnemonic
	moveq #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0
	bra.s return

unknownMnemonic
	moveq #TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC, d0
	bra.s return

unsupported
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0
	bra.s return

compactSelector
	movea.l a5, a0
	moveq #0, d0
	move.w state.EncodeSelectedMselMnemonicLen, d0
	bsr.w tkpkgBuildSelectedEnvelopeFromCmseV7

return
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildSelectedEnvelopeFromMselV1

; Infer the two-operand immediate shape at the package boundary when the
; frontend supplied no legacy selected-shape metadata.  This mirrors Rust's
; package_shape_input classification: an Immediate wrapper is removed from
; expr0, then the package register map decides between immediate_register and
; immediate_direct.  The classification is package-owned and CPU-neutral.
;
; Inputs: state.EncodeSelectedMselExprPtr/Len and resolved package context.
; Outputs: state.EncodeSelectedMselShapePtr/Len when inference applies.
; Clobbers: D0-D3/A0-A1/CCR.
tkpkgInferSelectedPackageShapeV1	.block
	movem.l d2-d3, -(sp)
	tst.w state.EncodeSelectedMselShapeLen
	bne.s return
	tst.l state.EncodeSelectedMselShapePtr
	bne.s return
	moveq #0, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s return
	tst.l d0
	beq.s return
	cmpi.b #'#', (a0)
	bne.s return
	moveq #1, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s return
	move.l a0, d2
	move.l d0, d3
	moveq #2, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	beq.s return
	movea.l d2, a0
	move.l d3, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.s immediateDirect
	lea ImmediateRegisterShapeText, a0
	move.l a0, state.EncodeSelectedMselShapePtr
	move.w #18, state.EncodeSelectedMselShapeLen
	bra.s return

immediateDirect
	lea ImmediateDirectShapeText, a0
	move.l a0, state.EncodeSelectedMselShapePtr
	move.w #16, state.EncodeSelectedMselShapeLen

return
	movem.l (sp)+, d2-d3
	rts
	.bend  ; tkpkgInferSelectedPackageShapeV1

; Decode the Rust CMSE v7 wire format and recover raw or v2 scalar-input plans.
; Both paths preserve package ownership: this module only transports opaque
; program ids and neutral scalar projections into the candidate envelope.
; Inputs: A0/D0 = selected mnemonic text/length.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D1 = envelope length on success.
tkpkgBuildSelectedEnvelopeFromCmseV7	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea -24(sp), sp
	clr.w state.EncodeSelectedMselMatchFlags
	clr.w state.EncodeSelectedMselFallbackLen
	movea.l a0, a5
	move.w d0, d2
	move.w d2, state.EncodeSelectedMselMnemonicLen
	tst.w d2
	beq.w cmseNoOutput
	cmpi.w #buffers.COMPACT_SELECTOR_TEXT_CAPACITY, d2
	bhi.w cmseMalformed
	lea buffers.CompactSelectorMnemonicText, a3
	movea.l a5, a1
	move.w d2, d3
	beq.s cmseMnemonicCopied
	subq.w #1, d3
cmseMnemonicCopyLoop
	move.b (a1)+, (a3)+
	dbf d3, cmseMnemonicCopyLoop
cmseMnemonicCopied
	moveq #0, d0
	move.w state.EncodeSelectedMselShapeLen, d0
	cmpi.w #buffers.COMPACT_SELECTOR_TEXT_CAPACITY, d0
	bhi.w cmseMalformed
	tst.w d0
	beq.s cmseShapeCopied
	movea.l state.EncodeSelectedMselShapePtr, a1
	move.l a1, d1
	beq.w cmseMalformed
	lea buffers.CompactSelectorShapeText, a3
	move.w d0, d3
	subq.w #1, d3
cmseShapeCopyLoop
	move.b (a1)+, (a3)+
	dbf d3, cmseShapeCopyLoop
	lea buffers.CompactSelectorShapeText, a1
	move.l a1, state.EncodeSelectedMselShapePtr
cmseShapeCopied
	lea buffers.CompactSelectorMnemonicText, a5
	lea buffers.CmseChunkOffsetLo, a3
	bsr.w tkpkgServiceChunkPtrFromLocatorV1
	bne.w cmseNoOutput
	move.l a6, buffers.CompactSelectorChunkEndPtr
	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
	cmpi.w #7, d0
	bne.w cmseMalformed
	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
	tst.w d0
	beq.w cmseMalformed
	move.w d0, d7
	move.w #$FFFF, 16(sp)
	move.w #$FFFF, 18(sp)
	move.w #$FFFF, 20(sp)
	moveq #0, d5

cmseOwnerLoop
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w cmseMalformed
	moveq #0, d6
	move.b (a2)+, d6
	cmpi.b #SCOPED_OWNER_DIALECT, d6
	bhi.w cmseMalformed
	bsr.w tkpkgServiceLocateStringV1
	bne.w cmseMalformed
	move.l a2, -(sp)
	bsr.w tkpkgSelectedMselOwnerMatchesV1
	movea.l (sp)+, a2
	tst.b d0
	beq.s cmseOwnerNext
	tst.b d6
	beq.s cmseOwnerFamily
	cmpi.b #SCOPED_OWNER_CPU, d6
	beq.s cmseOwnerCpu
	move.w d5, 20(sp)
	bra.s cmseOwnerNext
cmseOwnerCpu
	move.w d5, 18(sp)
	bra.s cmseOwnerNext
cmseOwnerFamily
	move.w d5, 16(sp)
cmseOwnerNext
	addq.w #1, d5
	subq.w #1, d7
	bne.w cmseOwnerLoop

	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
	tst.w d0
	beq.w cmseMalformed
	move.w d0, buffers.CompactSelectorStringCount
	move.l a2, buffers.CompactSelectorStringsPtr
	move.w #$FFFF, (sp)
	move.w #$FFFF, 2(sp)
	clr.w 22(sp)
	move.w d0, d7
	moveq #0, d5

cmseStringLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
	move.w d0, d6
	cmp.w 22(sp), d6
	bhi.w cmseMalformed
	bsr.w tkpkgServiceLocateStringV1
	bne.w cmseMalformed
	move.w d0, d3
	add.w d6, d0
	bcs.w cmseMalformed
	cmpi.w #buffers.COMPACT_STRING_SCRATCH_CAPACITY, d0
	bhi.w cmseMalformed
	move.w d0, 22(sp)
	lea buffers.CompactStringScratchBuffer, a3
	adda.w d6, a3
	tst.w d3
	beq.s cmseStringCompare
	subq.w #1, d3
cmseStringCopy
	move.b (a1)+, (a3)+
	dbf d3, cmseStringCopy

cmseStringCompare
	move.l a2, -(sp)
	move.w state.EncodeSelectedMselMnemonicLen, d1
	lea buffers.CompactStringScratchBuffer, a1
	movea.l a5, a2
	move.w 26(sp), d0
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.s cmseStringShape
	move.w d5, (sp)

cmseStringShape
	tst.w state.EncodeSelectedMselShapeLen
	beq.s cmseStringNext
	tst.l state.EncodeSelectedMselShapePtr
	beq.s cmseStringNext
	move.l a2, -(sp)
	move.w state.EncodeSelectedMselShapeLen, d1
	lea buffers.CompactStringScratchBuffer, a1
	movea.l state.EncodeSelectedMselShapePtr, a2
	move.w 26(sp), d0
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.s cmseStringNext
	move.w d5, 2(sp)

cmseStringNext
	addq.w #1, d5
	subq.w #1, d7
	bne.w cmseStringLoop
	move.w (sp), d0
	cmpi.w #$FFFF, d0
	beq.w cmseUnknown
	tst.w state.EncodeSelectedMselShapeLen
	beq.s cmseHaveStringKeys
	move.w 2(sp), d0
	cmpi.w #$FFFF, d0
	beq.w cmseNoOutput

cmseHaveStringKeys
	moveq #4, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w cmseMalformed
	tst.b 2(a2)
	bne.w cmseMalformed
	tst.b 3(a2)
	bne.w cmseMalformed
	moveq #0, d7
	move.b (a2)+, d7
	moveq #0, d0
	move.b (a2)+, d0
	lsl.w #8, d0
	or.w d0, d7
	addq.l #2, a2
	tst.w d7
	beq.w cmseMalformed
	subq.w #1, d7

cmseSelectorLoop
	clr.l state.EncodeSelectedSemanticPlanPtr
	clr.b state.EncodeSelectedSemanticPlanKind
	moveq #0, d6
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w cmseMalformed
	move.b (a2)+, d6
	move.w d6, 10(sp)
	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
	move.w d0, 12(sp)
	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
	move.w d0, 14(sp)
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w cmseMalformed
	moveq #0, d0
	move.b (a2)+, d0
	cmpi.b #0, d0
	beq.s cmseSemanticMode
	cmpi.b #1, d0
	bne.w cmseMalformed
	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
	move.w d0, 4(sp)
	bra.s cmseModeReady
cmseSemanticMode
	move.w #$FFFF, 4(sp)
cmseModeReady
	clr.b 8(sp)
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w cmseMalformed
	moveq #0, d0
	move.b (a2)+, d0
	bne.s cmseSkipStructuredPlan
	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
	move.w d0, 6(sp)
	move.b #1, 8(sp)
	bra.s cmsePlanReady
cmseSkipStructuredPlan
	move.l a2, state.EncodeSelectedSemanticPlanPtr
	move.b d0, state.EncodeSelectedSemanticPlanKind
	bsr.w skipCompactSelectorPlanBodyV7
	bne.w cmseMalformed
cmsePlanReady
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w cmseMalformed
	moveq #0, d0
	move.b (a2)+, d0
	cmpi.b #$FF, d0
	bne.s cmsePriorityReady
	bsr.w tkpkgServiceReadU16LeV1
	bne.w cmseMalformed
cmsePriorityReady
	moveq #2, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w cmseMalformed
	move.b (a2)+, d0
	cmpi.b #1, d0
	bhi.w cmseMalformed
	move.b d0, 9(sp)
	addq.l #1, a2

	move.w 10(sp), d6
	cmp.w 20(sp), d6
	beq.w cmseOwnerMatches
	cmp.w 18(sp), d6
	beq.w cmseOwnerMatches
	cmp.w 16(sp), d6
	bne.w cmseSelectorNext
cmseOwnerMatches
	move.w 12(sp), d3
	move.w 14(sp), d4
	moveq #0, d0
	move.w (sp), d0
	cmp.w d0, d3
	bne.w cmseSelectorNext
	bset #2, state.EncodeSelectedMselMatchFlags
	tst.w state.EncodeSelectedMselShapeLen
	beq.w cmseShapeMatches
	moveq #0, d0
	move.w 2(sp), d0
	cmp.w d0, d4
	bne.w cmseSelectorNext
cmseShapeMatches
	moveq #0, d4
	move.b 9(sp), d4
	tst.b 8(sp)
	beq.w cmseStructuredCandidate
	move.w 4(sp), d0
	cmpi.w #$FFFF, d0
	beq.w cmseSelectorNext
	lea buffers.CompactSelectorModeText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w cmseMalformed
	lea buffers.CompactSelectorModeText, a1
	move.l a1, state.EncodeSelectedMselModePtr
	move.w d0, state.EncodeSelectedMselModeLen
	move.w 6(sp), d0
	lea buffers.CompactSelectorPlanText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w cmseMalformed
	lea buffers.CompactSelectorPlanText, a1
	move.l a1, state.EncodeSelectedMselPlanPtr
	move.w d0, state.EncodeSelectedMselPlanLen
	move.l state.EncodeSelectedMselShapePtr, d0
	move.l d0, state.EncodeSelectedCurrentShapePtr
	move.w state.EncodeSelectedMselShapeLen, d0
	move.w d0, state.EncodeSelectedCurrentShapeLen
	bset #0, state.EncodeSelectedMselMatchFlags
	move.l a2, -(sp)
	move.w d7, -(sp)
	jsr operand.tkpkgMselTryBuildCandidateV1
	move.w (sp)+, d7
	movea.l (sp)+, a2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s cmseCandidateReady
	clr.l state.EncodeSelectedSemanticPlanPtr
	clr.b state.EncodeSelectedSemanticPlanKind
	bra.w cmseCandidateReady

cmseStructuredCandidate
	moveq #0, d0
	move.b state.EncodeSelectedSemanticPlanKind, d0
	cmpi.b #1, d0
	beq.w cmseBuildStructured
	cmpi.b #2, d0
	beq.w cmseBuildStructured
	cmpi.b #4, d0
	beq.s cmseBuildSequence
	cmpi.b #6, d0
	bne.w cmseSelectorNext
	movea.l state.EncodeSelectedSemanticPlanPtr, a1
	move.l a2, -(sp)
	move.w d7, -(sp)
	bsr.w tkpkgBuildCompactSemanticRejectCandidateV2
	move.w (sp)+, d7
	movea.l (sp)+, a2
	bra.s cmseCandidateReady
cmseBuildSequence
	movea.l state.EncodeSelectedSemanticPlanPtr, a1
	move.l a2, -(sp)
	move.w d7, -(sp)
	bsr.w tkpkgBuildCompactSemanticSequenceCandidateV2
	move.w (sp)+, d7
	movea.l (sp)+, a2
	bra.s cmseCandidateReady
cmseBuildStructured
	movea.l state.EncodeSelectedSemanticPlanPtr, a1
	move.l a2, -(sp)
	move.w d7, -(sp)
	bsr.w tkpkgBuildCompactSemanticCandidateV2
	move.w (sp)+, d7
	movea.l (sp)+, a2

cmseCandidateReady
	cmpi.l #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	beq.w cmseReturn
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w cmseCandidateNotOk
	tst.b d4
	beq.w cmseReturn
	tst.b state.EncodeSelectedMselUnstable
	beq.w cmseReturn
	bset #1, state.EncodeSelectedMselMatchFlags
	move.w d1, state.EncodeSelectedMselFallbackLen
	bra.w cmseSelectorNext

cmseCandidateNotOk
	cmpi.l #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	bne.w cmseSelectorNext
	tst.w state.EncodeSelectedMselShapeLen
	beq.w cmseSelectorNext
	tst.l state.EncodeSelectedMselShapePtr
	beq.w cmseSelectorNext
	bra.w cmseReturn

cmseSelectorNext
	dbf d7, cmseSelectorLoop
	btst #1, state.EncodeSelectedMselMatchFlags
	beq.s cmseNoFallback
	moveq #0, d1
	move.w state.EncodeSelectedMselFallbackLen, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s cmseReturn
cmseNoFallback
	moveq #0, d1
	btst #2, state.EncodeSelectedMselMatchFlags
	beq.s cmseUnknown
	moveq #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0
	bra.s cmseReturn
cmseUnknown
	moveq #TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC, d0
	bra.s cmseReturn
cmseNoOutput
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC, d0
	bra.s cmseReturn
cmseMissingChunk
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s cmseReturn
cmseMalformed
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
cmseReturn
	lea 24(sp), sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; tkpkgBuildSelectedEnvelopeFromCmseV7

	.priv
; Build the existing candidate envelope from one CMSE v7 scalar/input plan.
; The envelope carries an opaque CSEM program id and four-byte neutral scalar
; records.  No family spelling or encoding meaning is interpreted here.
; Inputs: D0.B = compact plan kind (1 inputs, 2 scalar); A1 = plan body;
;         A5/D2 = selected mnemonic; A6 = CMSE chunk end.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D1 = envelope length on success.
tkpkgBuildCompactSemanticCandidateV2	.block
	movem.l d2-d7/a0-a6, -(sp)
	moveq #0, d7
	move.b d0, d7
	movea.l a1, a2
	bsr.w tkpkgServiceReadU16LeV1
	bne.w semanticMalformed
	lea buffers.CompactSelectorPlanText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w semanticMalformed
	tst.w d0
	beq.w semanticMalformed
	cmpi.w #buffers.COMPACT_SELECTOR_TEXT_CAPACITY, d0
	bhi.w semanticMalformed
	lea buffers.CompactSelectorPlanText, a1
	move.l a1, state.EncodeSelectedMselModePtr
	move.w d0, state.EncodeSelectedMselModeLen
	move.l state.EncodeSelectedMselShapePtr, d1
	move.l d1, state.EncodeSelectedCurrentShapePtr
	move.w state.EncodeSelectedMselShapeLen, d1
	move.w d1, state.EncodeSelectedCurrentShapeLen
	bset #0, state.EncodeSelectedMselMatchFlags

	lea buffers.TokenScratchBuffer, a4
	move.w state.EncodeSelectedMselMnemonicLen, d0
	cmpi.w #255, d0
	bhi.w semanticMalformed
	move.b d0, (a4)+
	movea.l a5, a0
	jsr operand.tkpkgMselCopyBytesV1
	move.b #1, (a4)+
	move.w state.EncodeSelectedMselModeLen, d0
	cmpi.w #255, d0
	bhi.w semanticMalformed
	move.b d0, (a4)+
	movea.l state.EncodeSelectedMselModePtr, a0
	jsr operand.tkpkgMselCopyBytesV1

	cmpi.b #2, d7
	beq.w semanticScalar
	cmpi.b #1, d7
	bne.w semanticMalformed
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w semanticMalformed
	moveq #0, d6
	move.b (a2)+, d6
	tst.w d6
	beq.w semanticMalformed
	cmpi.w #32, d6
	bhi.w semanticMalformed
	move.b d6, (a4)+

semanticInputLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.w semanticMalformed
	lea buffers.CompactSelectorInputText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w semanticMalformed
	tst.w d0
	beq.w semanticMalformed
	cmpi.w #buffers.COMPACT_SELECTOR_TEXT_CAPACITY, d0
	bhi.w semanticMalformed
	lea buffers.CompactSelectorInputText, a1
	bsr.w tkpkgProjectCompactSemanticInputV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w semanticReturn
	move.b #4, (a4)+
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	subq.w #1, d6
	bne.w semanticInputLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.w semanticMalformed
	bra.s semanticDone

semanticScalar
	bsr.w tkpkgServiceReadU16LeV1
	bne.w semanticMalformed
	move.b #1, (a4)+
	moveq #0, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w semanticOperand
	moveq #0, d1
	cmpi.b #'#', (a0)
	bne.s semanticScalarEvaluate
	moveq #1, d1
semanticScalarEvaluate
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s semanticReturn
	move.b #4, (a4)+
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+

semanticDone
	move.l a4, d1
	lea buffers.TokenScratchBuffer, a0
	sub.l a0, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s semanticReturn

semanticOperand
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	bra.s semanticReturn
semanticMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	moveq #0, d1
semanticReturn
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildCompactSemanticCandidateV2

; Build one bounded native envelope for Rust CMSE v7 semantic sequences.
; Match steps only prove that every neutral input projects. Encode and fixup
; steps retain their opaque program id and projected scalar records in package
; order, matching Rust selector_encoding.rs sequence execution.
; Inputs: A1 = kind-4 plan body; A5/D2 = mnemonic; A6 = CMSE chunk end.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D1 = envelope length on success.
tkpkgBuildCompactSemanticSequenceCandidateV2	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a1, a2
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w sequenceMalformed
	moveq #0, d7
	move.b (a2)+, d7
	cmpi.w #2, d7
	bcs.w sequenceMalformed

	lea buffers.TokenScratchBuffer, a4
	move.w d2, d0
	cmpi.w #255, d0
	bhi.w sequenceMalformed
	move.b d0, (a4)+
	movea.l a5, a0
	jsr operand.tkpkgMselCopyBytesV1
	movea.l a4, a3
	clr.b (a4)+
	moveq #0, d6

sequenceStepLoop
	moveq #3, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w sequenceMalformed
	moveq #0, d5
	move.b (a2)+, d5
	cmpi.b #2, d5
	bhi.w sequenceMalformed
	bsr.w tkpkgServiceReadU16LeV1
	bne.w sequenceMalformed
	move.w d0, -(sp)
	lea buffers.CompactSelectorPlanText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w sequenceProgramStackMalformed
	tst.w d0
	beq.w sequenceProgramStackMalformed
	cmpi.w #255, d0
	bhi.w sequenceProgramStackMalformed
	move.w d0, d4
	move.w (sp)+, d0

	tst.b d5
	beq.s sequenceInputs
	moveq #1, d0
	add.w d4, d0
	addq.w #1, d0
	bsr.w tkpkgSequenceRequireCandidateBytesV2
	bne.w sequenceMalformed
	move.b d4, (a4)+
	lea buffers.CompactSelectorPlanText, a0
	move.w d4, d0
	jsr operand.tkpkgMselCopyBytesV1
	movea.l a4, a0
	clr.b (a4)+

sequenceInputs
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w sequenceMalformed
	moveq #0, d4
	move.b (a2)+, d4
	tst.w d4
	beq.w sequenceMalformed
	tst.b d5
	beq.s sequenceInputLoop
	move.b d4, (a0)

sequenceInputLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.w sequenceMalformed
	lea buffers.CompactSelectorInputText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w sequenceMalformed
	tst.w d0
	beq.w sequenceMalformed
	lea buffers.CompactSelectorInputText, a1
	moveq #0, d2
	move.l a2, -(sp)
	move.l d0, -(sp)
	lea TargetPrefixText, a2
	moveq #7, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s sequenceNoTargetPrefix
	move.l (sp)+, d0
	lea 7(a1), a1
	subi.w #7, d0
	beq.s sequenceInputCursorMalformed
	moveq #1, d2
	tst.b d5
	bne.s sequenceProjectInput
	bsr.w tkpkgProjectDirectSemanticTargetV2
	bra.s sequenceInputProjected
sequenceNoTargetPrefix
	move.l (sp)+, d0
sequenceProjectInput
	bsr.w tkpkgProjectCompactSemanticInputV2
sequenceInputProjected
	movea.l (sp)+, a2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w sequenceReturn
	tst.b d5
	beq.s sequenceInputNext
	moveq #5, d0
	cmpi.b #2, d5
	bne.s sequenceRequireInputRecord
	addq.w #1, d0
sequenceRequireInputRecord
	bsr.w tkpkgSequenceRequireCandidateBytesV2
	bne.w sequenceMalformed
	moveq #4, d0
	cmpi.b #2, d5
	bne.s sequenceWriteInputLength
	addq.w #1, d0
sequenceWriteInputLength
	move.b d0, (a4)+
	cmpi.b #2, d5
	bne.s sequenceWriteInputValue
	move.b d2, (a4)+
sequenceWriteInputValue
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+

sequenceInputNext
	subq.w #1, d4
	bne.w sequenceInputLoop
	tst.b d5
	beq.s sequenceStepNext
	addq.w #1, d6
	cmpi.w #255, d6
	bhi.w sequenceMalformed
	bra.s sequenceStepNext

sequenceInputCursorMalformed
	addq.l #4, sp
	bra.w sequenceMalformed

sequenceStepNext
	subq.w #1, d7
	bne.w sequenceStepLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.w sequenceMalformed
	tst.w d6
	beq.w sequenceMalformed
	move.b d6, (a3)
	move.l a4, d1
	lea buffers.TokenScratchBuffer, a0
	sub.l a0, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s sequenceReturn

sequenceProgramStackMalformed
	addq.l #2, sp
sequenceMalformed
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
sequenceReturn
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildCompactSemanticSequenceCandidateV2

; Execute Rust CMSE v7 semantic rejection for neutral input sources.  A row is
; rejected only when every declared projection matches.  The package DIAG
; table owns the message template; this runtime substitutes the two standard
; selector captures supplied by Rust (`mnemonic` and `form`).
; Inputs: A1 = kind-6 plan body; A5/D2 = mnemonic; A6 = CMSE chunk end.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D1 = diagnostic length on rejection.
tkpkgBuildCompactSemanticRejectCandidateV2	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a1, a2
	bsr.w tkpkgServiceReadU16LeV1
	bne.s rejectMalformed
	lea buffers.CompactSelectorPlanText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.s rejectMalformed
	tst.w d0
	beq.s rejectMalformed
	move.w d0, d6
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s rejectMalformed
	moveq #0, d7
	move.b (a2)+, d7
	tst.w d7
	beq.s rejectMalformed

rejectInputLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.s rejectMalformed
	lea buffers.CompactSelectorInputText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.s rejectMalformed
	tst.w d0
	beq.s rejectMalformed
	lea buffers.CompactSelectorInputText, a1
	bsr.w tkpkgProjectCompactSemanticInputV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s rejectReturn
	subq.w #1, d7
	bne.s rejectInputLoop

	lea buffers.CompactSelectorPlanText, a1
	move.w d6, d0
	move.w state.EncodeSelectedMselMnemonicLen, d2
	bsr.w tkpkgRenderRejectMessageCodeV1
	tst.l d0
	bne.s rejectMalformed
	moveq #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	bra.s rejectReturn

rejectMalformed
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
rejectReturn
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildCompactSemanticRejectCandidateV2

; Resolve one package-owned diagnostic code and render its template into the
; bounded candidate scratch buffer.  DIAG is the Rust simple-schema sequence:
; u32 count followed by code/template length-prefixed UTF-8 string pairs.
; Inputs: A1/D0 = diagnostic code; A5/D2 = selected full mnemonic.
; Outputs: D0 = 0 success, 1 malformed/missing; D1 = rendered byte length.
tkpkgRenderRejectMessageCodeV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea -4(sp), sp
	move.w d2, (sp)
	move.w d0, 2(sp)
	movea.l a1, a4
	lea buffers.MessageChunkOffsetLo, a3
	bsr.w tkpkgServiceChunkPtrFromLocatorV1
	bne.w rejectMessageFail
	move.w 2(sp), d7
	moveq #4, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w rejectMessageFail
	tst.b 2(a2)
	bne.w rejectMessageFail
	tst.b 3(a2)
	bne.w rejectMessageFail
	moveq #0, d6
	move.b (a2), d6
	moveq #0, d0
	move.b 1(a2), d0
	lsl.w #8, d0
	or.w d0, d6
	lea 4(a2), a2
	tst.w d6
	beq.w rejectMessageFail

rejectMessageLoop
	bsr.w tkpkgServiceLocateStringV1
	bne.w rejectMessageFail
	move.l a2, -(sp)
	movea.l a4, a2
	move.w d7, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	move.b d0, d5
	bsr.w tkpkgServiceLocateStringV1
	bne.s rejectMessageFail
	tst.b d5
	bne.s rejectMessageFound
	subq.w #1, d6
	bne.s rejectMessageLoop
	bra.s rejectMessageFail

rejectMessageFound
	move.w (sp), d2
	bsr.w tkpkgRenderRejectMessageTemplateV1
	bra.s rejectMessageReturn

rejectMessageFail
	moveq #0, d1
	moveq #1, d0
rejectMessageReturn
	lea 4(sp), sp
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgRenderRejectMessageCodeV1

; Render Rust's standard selector diagnostic captures.  Literal template bytes
; remain package-owned; replacement mnemonics are ASCII-uppercased exactly as
; Rust's selector boundary does before supplying `mnemonic` and `form`.
; Inputs: A1/D0 = template; A5/D2 = selected full mnemonic.
; Outputs: D0 = 0 success, 1 overflow; D1 = rendered length.
tkpkgRenderRejectMessageTemplateV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a1, a3
	move.w d0, d7
	lea buffers.TokenScratchBuffer, a4
	moveq #0, d6
	moveq #0, d5

rejectMessageBaseScan
	cmp.w d2, d5
	bhs.s rejectMessageTemplateLoop
	cmpi.b #'.', 0(a5, d5.w)
	beq.s rejectMessageTemplateLoop
	addq.w #1, d5
	bra.s rejectMessageBaseScan

rejectMessageTemplateLoop
	tst.w d7
	beq.w rejectMessageTemplateDone
	cmpi.w #10, d7
	bcs.s rejectMessageCheckForm
	movea.l a3, a1
	move.w d7, d0
	lea RejectMnemonicPlaceholderText, a2
	moveq #10, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s rejectMessageCheckForm
	movea.l a5, a0
	move.w d5, d4
	lea 10(a3), a3
	subi.w #10, d7
	bra.s rejectMessageCopyReplacement

rejectMessageCheckForm
	cmpi.w #6, d7
	bcs.s rejectMessageCopyLiteral
	movea.l a3, a1
	move.w d7, d0
	lea RejectFormPlaceholderText, a2
	moveq #6, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s rejectMessageCopyLiteral
	movea.l a5, a0
	move.w d2, d4
	lea 6(a3), a3
	subq.w #6, d7

rejectMessageCopyReplacement
	move.w d6, d0
	add.w d4, d0
	bcs.s rejectMessageTemplateFail
	cmpi.w #buffers.TOKEN_SCRATCH_CAPACITY, d0
	bhi.s rejectMessageTemplateFail
	tst.w d4
	beq.s rejectMessageTemplateLoop
rejectMessageReplacementLoop
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #'a', d3
	blo.s rejectMessageReplacementReady
	cmpi.b #'z', d3
	bhi.s rejectMessageReplacementReady
	subi.b #$20, d3
rejectMessageReplacementReady
	move.b d3, (a4)+
	addq.w #1, d6
	subq.w #1, d4
	bne.s rejectMessageReplacementLoop
	bra.w rejectMessageTemplateLoop

rejectMessageCopyLiteral
	cmpi.w #buffers.TOKEN_SCRATCH_CAPACITY, d6
	bhs.s rejectMessageTemplateFail
	move.b (a3)+, (a4)+
	addq.w #1, d6
	subq.w #1, d7
	bra.w rejectMessageTemplateLoop

rejectMessageTemplateDone
	move.w d6, d1
	moveq #0, d0
	bra.s rejectMessageTemplateReturn
rejectMessageTemplateFail
	moveq #0, d1
	moveq #1, d0
rejectMessageTemplateReturn
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgRenderRejectMessageTemplateV1

; Require D0.W more bytes in the bounded selected-candidate buffer.
tkpkgSequenceRequireCandidateBytesV2	.block
	move.l a4, d1
	lea buffers.TokenScratchBuffer, a0
	sub.l a0, d1
	add.l d0, d1
	cmpi.l #buffers.TOKEN_SCRATCH_CAPACITY, d1
	bhi.s sequenceCandidateFail
	moveq #0, d1
	rts
sequenceCandidateFail
	moveq #1, d1
	rts
	.bend  ; tkpkgSequenceRequireCandidateBytesV2

; Project the CPU-neutral CMSE input grammar.  Package strings choose literal,
; expression, immediate, direct/indirect register, or value-program sources.
; Inputs: A1/D0 = resolved input source text.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = projected scalar on success.
tkpkgProjectCompactSemanticInputV2	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	movea.l a1, a5
	move.w d0, d7
	movea.l a5, a1
	move.w d7, d0
	lea MemberShapePrefixText, a2
	moveq #12, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckMember
	lea 12(a5), a1
	move.w d7, d0
	subi.w #12, d0
	bsr.w tkpkgParseMemberSpecV2
	bne.w semanticProjectMalformed
	movea.l a2, a1
	move.w d4, d1
	move.w d3, d0
	jsr operand.tkpkgMselLocateMemberBaseV2
	bne.w semanticProjectNoMatch
	moveq #0, d3
	bra.w semanticProjectOk

semanticCheckMember
	movea.l a5, a1
	move.w d7, d0
	lea MemberPrefixText, a2
	moveq #6, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckLiteral
	lea 6(a5), a1
	move.w d7, d0
	subi.w #6, d0
	bsr.w tkpkgParseMemberSpecV2
	bne.w semanticProjectMalformed
	movea.l a2, a1
	move.w d4, d1
	move.w d3, d0
	jsr operand.tkpkgMselLocateMemberBaseV2
	bne.w semanticProjectNoMatch
	jsr operand.tkpkgMselStripOuterParensV1
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	bra.w semanticProjectReturn

semanticCheckLiteral
	cmpi.w #8, d7
	bcs.s semanticCheckExpr
	cmpi.b #'l', (a5)
	bne.s semanticCheckExpr
	cmpi.b #'i', 1(a5)
	bne.s semanticCheckExpr
	cmpi.b #'t', 2(a5)
	bne.s semanticCheckExpr
	cmpi.b #'e', 3(a5)
	bne.s semanticCheckExpr
	cmpi.b #'r', 4(a5)
	bne.s semanticCheckExpr
	cmpi.b #'a', 5(a5)
	bne.s semanticCheckExpr
	cmpi.b #'l', 6(a5)
	bne.s semanticCheckExpr
	cmpi.b #':', 7(a5)
	bne.s semanticCheckExpr
	lea 8(a5), a1
	subi.w #8, d7
	bsr.w tkpkgParseSignedDecimalV2
	bne.w semanticProjectMalformed
	bra.w semanticProjectOk

semanticCheckExpr
	cmpi.w #4, d7
	bcs.s semanticCheckImmediate
	cmpi.b #'e', (a5)
	bne.s semanticCheckImmediate
	cmpi.b #'x', 1(a5)
	bne.s semanticCheckImmediate
	cmpi.b #'p', 2(a5)
	bne.s semanticCheckImmediate
	cmpi.b #'r', 3(a5)
	bne.s semanticCheckImmediate
	lea 4(a5), a1
	move.w d7, d0
	subi.w #4, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w semanticProjectMalformed
	move.w d3, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w semanticProjectNoMatch
	moveq #0, d1
	cmpi.b #'#', (a0)
	bne.s semanticExprEvaluate
	moveq #1, d1
semanticExprEvaluate
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	bra.w semanticProjectReturn

semanticCheckImmediate
	cmpi.w #9, d7
	bcs.w semanticCheckTupleRegister
	cmpi.b #'i', (a5)
	bne.w semanticCheckTupleRegister
	cmpi.b #'m', 1(a5)
	bne.w semanticCheckTupleRegister
	cmpi.b #'m', 2(a5)
	bne.w semanticCheckTupleRegister
	cmpi.b #'e', 3(a5)
	bne.w semanticCheckTupleRegister
	cmpi.b #'d', 4(a5)
	bne.w semanticCheckTupleRegister
	cmpi.b #'i', 5(a5)
	bne.w semanticCheckTupleRegister
	cmpi.b #'a', 6(a5)
	bne.w semanticCheckTupleRegister
	cmpi.b #'t', 7(a5)
	bne.w semanticCheckTupleRegister
	cmpi.b #'e', 8(a5)
	bne.w semanticCheckTupleRegister
	lea 9(a5), a1
	move.w d7, d0
	subi.w #9, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w semanticProjectMalformed
	move.w d3, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w semanticProjectNoMatch
	moveq #1, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	bra.w semanticProjectReturn

semanticCheckTupleRegister
	movea.l a5, a1
	move.w d7, d0
	lea IndirectTupleRegisterPrefixText, a2
	moveq #18, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckTupleQualifiedRegister
	lea 18(a5), a1
	move.w d7, d0
	subi.w #18, d0
	bsr.w tkpkgParseTupleItemClassSpecV2
	bne.w semanticProjectMalformed
	tst.w d6
	bne.w semanticProjectMalformed
	cmpi.w #$FFFF, d5
	beq.w semanticProjectMalformed
	move.w d3, d0
	move.w d4, d1
	jsr operand.tkpkgMselLocateIndirectTupleItemV2
	bne.w semanticProjectNoMatch
	move.w d5, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w semanticProjectNoMatch
	bra.w semanticProjectOk

semanticCheckTupleQualifiedRegister
	movea.l a5, a1
	move.w d7, d0
	lea IndirectTupleQualifiedRegisterPrefixText, a2
	moveq #28, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckTupleValue
	lea 28(a5), a1
	move.w d7, d0
	subi.w #28, d0
	bsr.w tkpkgParseTupleItemClassSpecV2
	bne.w semanticProjectMalformed
	tst.w d6
	beq.w semanticProjectMalformed
	cmpi.w #$FFFF, d5
	beq.w semanticProjectMalformed
	movea.l d5, a6
	move.w d3, d0
	move.w d4, d1
	jsr operand.tkpkgMselLocateIndirectTupleItemV2
	bne.w semanticProjectNoMatch
	movea.l a2, a1
	move.w d6, d1
	bsr.w tkpkgMselStripExpectedQualifierV2
	bne.w semanticProjectNoMatch
	move.l a6, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w semanticProjectNoMatch
	bra.w semanticProjectOk

semanticCheckTupleValue
	movea.l a5, a1
	move.w d7, d0
	lea IndirectTupleValuePrefixText, a2
	moveq #20, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckTupleArity
	lea 20(a5), a1
	move.w d7, d0
	subi.w #20, d0
	bsr.w tkpkgParseTupleItemClassSpecV2
	bne.w semanticProjectMalformed
	cmpi.w #$FFFF, d5
	bne.w semanticProjectMalformed
	move.w d3, d0
	move.w d4, d1
	jsr operand.tkpkgMselLocateIndirectTupleItemV2
	bne.w semanticProjectNoMatch
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	bra.w semanticProjectReturn

semanticCheckTupleArity
	movea.l a5, a1
	move.w d7, d0
	lea IndirectTupleArityPrefixText, a2
	moveq #20, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckUnaryPlusIndirectRegister
	lea 20(a5), a1
	move.w d7, d0
	subi.w #20, d0
	bsr.w tkpkgParseTupleAritySpecV2
	bne.w semanticProjectMalformed
	move.w d3, d0
	moveq #0, d1
	jsr operand.tkpkgMselLocateIndirectTupleItemV2
	bne.w semanticProjectNoMatch
	cmp.w d4, d2
	bne.w semanticProjectNoMatch
	moveq #0, d3
	move.w d2, d3
	bra.w semanticProjectOk

semanticCheckUnaryPlusIndirectRegister
	movea.l a5, a1
	move.w d7, d0
	lea UnaryPlusIndirectRegisterPrefixText, a2
	moveq #23, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckUnaryMinusIndirectRegister
	movea.l #2, a6
	lea 23(a5), a1
	move.w d7, d0
	subi.w #23, d0
	bra.w semanticRegisterSpec

semanticCheckUnaryMinusIndirectRegister
	movea.l a5, a1
	move.w d7, d0
	lea UnaryMinusIndirectRegisterPrefixText, a2
	moveq #24, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckIndirectRegister
	movea.l #3, a6
	lea 24(a5), a1
	move.w d7, d0
	subi.w #24, d0
	bra.w semanticRegisterSpec

semanticCheckIndirectRegister
	cmpi.w #20, d7
	bcs.s semanticCheckRegister
	cmpi.b #'i', (a5)
	bne.s semanticCheckRegister
	cmpi.b #'n', 1(a5)
	bne.s semanticCheckRegister
	cmpi.b #'d', 2(a5)
	bne.s semanticCheckRegister
	cmpi.b #'i', 3(a5)
	bne.s semanticCheckRegister
	cmpi.b #'r', 4(a5)
	bne.s semanticCheckRegister
	cmpi.b #'e', 5(a5)
	bne.s semanticCheckRegister
	cmpi.b #'c', 6(a5)
	bne.s semanticCheckRegister
	cmpi.b #'t', 7(a5)
	bne.s semanticCheckRegister
	cmpi.b #'_', 8(a5)
	bne.s semanticCheckRegister
	cmpi.b #'r', 9(a5)
	bne.s semanticCheckRegister
	cmpi.b #'e', 10(a5)
	bne.s semanticCheckRegister
	cmpi.b #'g', 11(a5)
	bne.s semanticCheckRegister
	movea.l #1, a6
	lea 12(a5), a1
	move.w d7, d0
	subi.w #12, d0
	bra.s semanticRegisterSpec

semanticCheckRegister
	cmpi.w #10, d7
	bcs.w semanticCheckRequiredValue
	cmpi.b #'r', (a5)
	bne.w semanticCheckRequiredValue
	cmpi.b #'e', 1(a5)
	bne.w semanticCheckRequiredValue
	cmpi.b #'g', 2(a5)
	bne.w semanticCheckRequiredValue
	suba.l a6, a6
	lea 3(a5), a1
	move.w d7, d0
	subi.w #3, d0

semanticRegisterSpec
	movea.l a1, a2
	move.w d0, d6
	moveq #0, d4
semanticRegisterIndexScan
	tst.w d6
	beq.w semanticProjectMalformed
	cmpi.b #'.', (a2)
	beq.s semanticRegisterIndexReady
	addq.l #1, a2
	addq.w #1, d4
	subq.w #1, d6
	bra.s semanticRegisterIndexScan
semanticRegisterIndexReady
	move.w d4, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w semanticProjectMalformed
	move.w d3, d7
	cmpi.w #6, d6
	bcs.w semanticProjectMalformed
	cmpi.b #'c', 1(a2)
	bne.w semanticProjectMalformed
	cmpi.b #'l', 2(a2)
	bne.w semanticProjectMalformed
	cmpi.b #'a', 3(a2)
	bne.w semanticProjectMalformed
	cmpi.b #'s', 4(a2)
	bne.w semanticProjectMalformed
	cmpi.b #'s', 5(a2)
	bne.w semanticProjectMalformed
	lea 6(a2), a1
	move.w d6, d0
	subi.w #6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w semanticProjectMalformed
	move.w d3, d6
	move.w d7, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w semanticProjectNoMatch
	move.l a6, d5
	beq.s semanticRegisterLookup
	cmpi.l #1, d5
	beq.s semanticRegisterStripParens
	cmpi.l #2, d5
	beq.s semanticRegisterStripUnaryPlus
	tst.l d0
	beq.w semanticProjectNoMatch
	cmpi.b #'-', (a0)
	bne.w semanticProjectNoMatch
	addq.l #1, a0
	subq.l #1, d0
	bra.s semanticRegisterStripParens

semanticRegisterStripUnaryPlus
	tst.l d0
	beq.w semanticProjectNoMatch
	subq.l #1, d0
	cmpi.b #'+', 0(a0, d0.l)
	bne.w semanticProjectNoMatch

semanticRegisterStripParens
	jsr operand.tkpkgMselStripOuterParensV1
	bne.w semanticProjectNoMatch

semanticRegisterLookup
	move.w d6, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w semanticProjectNoMatch
	bra.s semanticProjectOk

semanticCheckRequiredValue
	; Value-program projections are handled by the neutral VALP interpreter in
	; the next helper.  Required and optional programs share the frozen v1
	; projection grammar and differ only on a value constraint violation.
	movea.l a5, a1
	move.w d7, d0
	bsr.w tkpkgProjectRequiredValueV1
	bra.s semanticProjectReturn

semanticProjectMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s semanticProjectReturn
semanticProjectNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s semanticProjectReturn
semanticProjectOk
	moveq #TKPKG_SELECTED_STATUS_OK, d0
semanticProjectReturn
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectCompactSemanticInputV2

; Project Rust's `target:exprN` match predicate for a direct identifier.  The
; native selector boundary retains source spans rather than Rust Expr nodes, so
; this slice accepts the equivalent architecture-neutral identifier spelling
; and rejects package-owned register names.  More complex target expressions
; remain non-matches until their structured projection is ported.
; Inputs: A1/D0 = `exprN` suffix. Outputs: D0 selected status; D3=0 on match.
tkpkgProjectDirectSemanticTargetV2	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	movea.l a1, a5
	move.w d0, d7
	cmpi.w #5, d7
	bcs.w targetMalformed
	cmpi.b #'e', (a5)
	bne.w targetMalformed
	cmpi.b #'x', 1(a5)
	bne.w targetMalformed
	cmpi.b #'p', 2(a5)
	bne.w targetMalformed
	cmpi.b #'r', 3(a5)
	bne.w targetMalformed
	lea 4(a5), a1
	move.w d7, d0
	subi.w #4, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w targetMalformed
	move.w d3, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w targetNoMatch
	movea.l a0, a4
	move.l d0, d6
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	beq.w targetNoMatch
	movea.l a4, a0
	move.l d6, d7
	beq.w targetNoMatch
	moveq #0, d4
	move.b (a0)+, d4
	subq.l #1, d7
	cmpi.b #'_', d4
	beq.s targetIdentifierRest
	cmpi.b #'.', d4
	beq.s targetIdentifierRest
	cmpi.b #'A', d4
	bcs.w targetNoMatch
	cmpi.b #'Z', d4
	bls.s targetIdentifierRest
	cmpi.b #'a', d4
	bcs.w targetNoMatch
	cmpi.b #'z', d4
	bhi.w targetNoMatch

targetIdentifierRest
	tst.l d7
	beq.s targetOk
	moveq #0, d4
	move.b (a0)+, d4
	subq.l #1, d7
	cmpi.b #'_', d4
	beq.s targetIdentifierRest
	cmpi.b #'.', d4
	beq.s targetIdentifierRest
	cmpi.b #'0', d4
	bcs.s targetIdentifierLetter
	cmpi.b #'9', d4
	bls.s targetIdentifierRest
targetIdentifierLetter
	cmpi.b #'A', d4
	bcs.s targetNoMatch
	cmpi.b #'Z', d4
	bls.s targetIdentifierRest
	cmpi.b #'a', d4
	bcs.s targetNoMatch
	cmpi.b #'z', d4
	bls.s targetIdentifierRest
	bra.s targetNoMatch

targetOk
	moveq #0, d3
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s targetReturn
targetNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s targetReturn
targetMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
targetReturn
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectDirectSemanticTargetV2

; Parse `N.FIELD` for neutral member/member-shape projections.
; Inputs: A1/D0 = suffix. Outputs: D3=operand, A2/D4=field, D1=0/1.
tkpkgParseMemberSpecV2	.block
	movem.l d2/d5-d7/a0-a1/a3-a6, -(sp)
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d2
	tst.w d7
	beq.s memberSpecFail
memberSpecIndexScan
	tst.w d7
	beq.s memberSpecFail
	cmpi.b #'.', (a3)
	beq.s memberSpecIndexReady
	addq.l #1, a3
	addq.w #1, d2
	subq.w #1, d7
	bra.s memberSpecIndexScan
memberSpecIndexReady
	move.w d2, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.s memberSpecFail
	movea.l d3, a4
	cmpi.w #7, d7
	bcs.s memberSpecFail
	cmpi.b #'.', (a3)
	bne.s memberSpecFail
	cmpi.b #'f', 1(a3)
	bne.s memberSpecFail
	cmpi.b #'i', 2(a3)
	bne.s memberSpecFail
	cmpi.b #'e', 3(a3)
	bne.s memberSpecFail
	cmpi.b #'l', 4(a3)
	bne.s memberSpecFail
	cmpi.b #'d', 5(a3)
	bne.s memberSpecFail
	lea 6(a3), a3
	subi.w #6, d7
	beq.s memberSpecFail
	move.l a4, d3
	movea.l a3, a2
	moveq #0, d4
	move.w d7, d4
	moveq #0, d1
	bra.s memberSpecReturn
memberSpecFail
	moveq #1, d1
memberSpecReturn
	movem.l (sp)+, d2/d5-d7/a0-a1/a3-a6
	tst.l d1
	rts
	.bend  ; tkpkgParseMemberSpecV2

; Exact prefix comparison for package-owned semantic projection tags.
; Inputs: A1/D0 = source; A2/D1 = prefix. Output: D0.B = 1 match, 0 no match.
tkpkgSemanticPrefixMatchesV2	.block
	movem.l d2-d4/a1-a2, -(sp)
	moveq #0, d2
	cmp.w d1, d0
	bcs.s prefixReturn
	move.w d1, d3
	beq.s prefixReturn
	subq.w #1, d3
prefixLoop
	move.b (a1)+, d4
	cmp.b (a2)+, d4
	bne.s prefixReturn
	dbf d3, prefixLoop
	moveq #1, d2
prefixReturn
	move.l d2, d0
	movem.l (sp)+, d2-d4/a1-a2
	tst.b d0
	rts
	.bend  ; tkpkgSemanticPrefixMatchesV2

; Parse `N.itemM[.qualifierQ].classC` or `N.itemM` using the same unsigned
; indices and exact structural separators as Rust semantic_plan_inputs.
; Inputs: A1/D0 = suffix.
; Outputs: D3=operand, D4=item, D5=class or $FFFF when absent;
;          A2/D6=qualifier span (zero length when absent); D1=0/1.
tkpkgParseTupleItemClassSpecV2	.block
	movem.l d2/d7/a0-a1/a3-a6, -(sp)
	moveq #0, d6
	moveq #-1, d5
	moveq #0, d2
	movea.l d2, a2
	movea.l a1, a3
	move.w d0, d7
	tst.w d7
	beq.w tupleSpecFail

tupleSpecOperandScan
	tst.w d7
	beq.w tupleSpecFail
	cmpi.b #'.', (a3)
	beq.s tupleSpecOperandReady
	addq.l #1, a3
	addq.w #1, d2
	subq.w #1, d7
	bra.s tupleSpecOperandScan
tupleSpecOperandReady
	move.w d2, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w tupleSpecFail
	movea.l d3, a4
	cmpi.w #5, d7
	bcs.w tupleSpecFail
	cmpi.b #'.', (a3)
	bne.w tupleSpecFail
	cmpi.b #'i', 1(a3)
	bne.w tupleSpecFail
	cmpi.b #'t', 2(a3)
	bne.w tupleSpecFail
	cmpi.b #'e', 3(a3)
	bne.w tupleSpecFail
	cmpi.b #'m', 4(a3)
	bne.w tupleSpecFail
	lea 5(a3), a3
	subi.w #5, d7
	movea.l a3, a1
	moveq #0, d2

tupleSpecItemScan
	tst.w d7
	beq.s tupleSpecItemReady
	cmpi.b #'.', (a3)
	beq.s tupleSpecItemReady
	addq.l #1, a3
	addq.w #1, d2
	subq.w #1, d7
	bra.s tupleSpecItemScan
tupleSpecItemReady
	move.w d2, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w tupleSpecFail
	movea.l d3, a5
	tst.w d7
	beq.w tupleSpecOk
	cmpi.w #6, d7
	bcs.w tupleSpecFail
	cmpi.b #'.', (a3)
	bne.w tupleSpecFail
	cmpi.b #'c', 1(a3)
	beq.w tupleSpecClass
	cmpi.w #10, d7
	bcs.w tupleSpecFail
	cmpi.b #'q', 1(a3)
	bne.w tupleSpecFail
	cmpi.b #'u', 2(a3)
	bne.w tupleSpecFail
	cmpi.b #'a', 3(a3)
	bne.w tupleSpecFail
	cmpi.b #'l', 4(a3)
	bne.w tupleSpecFail
	cmpi.b #'i', 5(a3)
	bne.w tupleSpecFail
	cmpi.b #'f', 6(a3)
	bne.w tupleSpecFail
	cmpi.b #'i', 7(a3)
	bne.w tupleSpecFail
	cmpi.b #'e', 8(a3)
	bne.w tupleSpecFail
	cmpi.b #'r', 9(a3)
	bne.w tupleSpecFail
	lea 10(a3), a3
	subi.w #10, d7
	movea.l a3, a2
	moveq #0, d6
tupleSpecQualifierScan
	tst.w d7
	beq.w tupleSpecFail
	cmpi.b #'.', (a3)
	beq.s tupleSpecQualifierReady
	addq.l #1, a3
	addq.w #1, d6
	subq.w #1, d7
	bra.s tupleSpecQualifierScan
tupleSpecQualifierReady
	tst.w d6
	beq.w tupleSpecFail

tupleSpecClass
	cmpi.w #6, d7
	bcs.w tupleSpecFail
	cmpi.b #'.', (a3)
	bne.w tupleSpecFail
	cmpi.b #'c', 1(a3)
	bne.w tupleSpecFail
	cmpi.b #'l', 2(a3)
	bne.w tupleSpecFail
	cmpi.b #'a', 3(a3)
	bne.w tupleSpecFail
	cmpi.b #'s', 4(a3)
	bne.w tupleSpecFail
	cmpi.b #'s', 5(a3)
	bne.w tupleSpecFail
	lea 6(a3), a1
	subi.w #6, d7
	move.w d7, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.s tupleSpecFail
	movea.l d3, a6

tupleSpecOk
	move.l a4, d3
	move.l a5, d4
	tst.w d7
	beq.s tupleSpecNoClass
	move.l a6, d5
	bra.s tupleSpecReady
tupleSpecNoClass
	moveq #-1, d5
tupleSpecReady
	moveq #0, d1
	bra.s tupleSpecReturn
tupleSpecFail
	moveq #1, d1
tupleSpecReturn
	movem.l (sp)+, d2/d7/a0-a1/a3-a6
	tst.l d1
	rts
	.bend  ; tkpkgParseTupleItemClassSpecV2

; Parse `N.valueK` for the neutral indirect-tuple arity projection.
; Inputs: A1/D0 = suffix. Outputs: D3=operand, D4=expected arity, D1=0/1.
tkpkgParseTupleAritySpecV2	.block
	movem.l d2/d5-d7/a0-a1/a3-a5, -(sp)
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d2
	tst.w d7
	beq.s tupleArityFail
tupleArityOperandScan
	tst.w d7
	beq.s tupleArityFail
	cmpi.b #'.', (a3)
	beq.s tupleArityOperandReady
	addq.l #1, a3
	addq.w #1, d2
	subq.w #1, d7
	bra.s tupleArityOperandScan
tupleArityOperandReady
	move.w d2, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.s tupleArityFail
	movea.l d3, a4
	cmpi.w #6, d7
	bcs.s tupleArityFail
	cmpi.b #'.', (a3)
	bne.s tupleArityFail
	cmpi.b #'v', 1(a3)
	bne.s tupleArityFail
	cmpi.b #'a', 2(a3)
	bne.s tupleArityFail
	cmpi.b #'l', 3(a3)
	bne.s tupleArityFail
	cmpi.b #'u', 4(a3)
	bne.s tupleArityFail
	cmpi.b #'e', 5(a3)
	bne.s tupleArityFail
	lea 6(a3), a1
	subi.w #6, d7
	move.w d7, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.s tupleArityFail
	movea.l d3, a5
	move.l a4, d3
	move.l a5, d4
	moveq #0, d1
	bra.s tupleArityReturn
tupleArityFail
	moveq #1, d1
tupleArityReturn
	movem.l (sp)+, d2/d5-d7/a0-a1/a3-a5
	tst.l d1
	rts
	.bend  ; tkpkgParseTupleAritySpecV2

; Remove and validate a case-insensitive `.qualifier` suffix from one tuple
; item. Inputs: A0/D0=item span; A1/D1=expected qualifier. Outputs: A0/D0=base.
tkpkgMselStripExpectedQualifierV2	.block
	movea.l a0, a4
	move.l d0, d5
	movea.l a1, a3
	move.w d1, d7
	movea.l a0, a2
	adda.l d0, a2
	move.w d0, d6
tupleQualifierScan
	tst.w d6
	beq.s tupleQualifierFail
	subq.l #1, a2
	subq.w #1, d6
	cmpi.b #'.', (a2)
	bne.s tupleQualifierScan
	movea.l a2, a1
	addq.l #1, a1
	move.w d5, d0
	sub.w d6, d0
	subq.w #1, d0
	movea.l a3, a2
	move.w d7, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	tst.b d0
	beq.s tupleQualifierFail
	movea.l a4, a0
	move.w d6, d0
	moveq #0, d1
	rts
tupleQualifierFail
	moveq #1, d1
	rts
	.bend  ; tkpkgMselStripExpectedQualifierV2

; Parse an unsigned decimal constrained to u16.
; Inputs: A1/D0.W = text. Outputs: D3.L = value; D1 = 0/1.
tkpkgParseU16DecimalV2	.block
	moveq #0, d3
	moveq #1, d1
	tst.w d0
	beq.s parseU16Return
	move.w d0, d2
parseU16Loop
	moveq #0, d4
	move.b (a1)+, d4
	cmpi.b #'0', d4
	bcs.s parseU16Return
	cmpi.b #'9', d4
	bhi.s parseU16Return
	subi.b #'0', d4
	cmpi.l #$00001999, d3
	bhi.s parseU16Return
	bne.s parseU16Accumulate
	cmpi.b #5, d4
	bhi.s parseU16Return
parseU16Accumulate
	move.l d3, d5
	lsl.l #3, d3
	add.l d5, d5
	add.l d5, d3
	add.l d4, d3
	subq.w #1, d2
	bne.s parseU16Loop
	moveq #0, d1
parseU16Return
	tst.l d1
	rts
	.bend  ; tkpkgParseU16DecimalV2

; Parse an unsigned decimal constrained to the native u32 scalar transport.
; Inputs: A1/D0.W = text. Outputs: D3.L = value; D1 = 0/1.
tkpkgParseU32DecimalV2	.block
	moveq #0, d3
	moveq #1, d1
	tst.w d0
	beq.s parseU32Return
	move.w d0, d2
parseU32Loop
	moveq #0, d4
	move.b (a1)+, d4
	cmpi.b #'0', d4
	bcs.s parseU32Return
	cmpi.b #'9', d4
	bhi.s parseU32Return
	subi.b #'0', d4
	cmpi.l #$19999999, d3
	bhi.s parseU32Return
	bne.s parseU32Accumulate
	cmpi.b #5, d4
	bhi.s parseU32Return
parseU32Accumulate
	move.l d3, d5
	lsl.l #3, d3
	add.l d5, d5
	add.l d5, d3
	add.l d4, d3
	subq.w #1, d2
	bne.s parseU32Loop
	moveq #0, d1
parseU32Return
	tst.l d1
	rts
	.bend  ; tkpkgParseU32DecimalV2

; Parse a signed decimal into the native 32-bit scalar transport.  A negative
; magnitude is retained modulo 2^32, matching Rust's cast at the encoding VM
; boundary after the package-owned signed range check.
; Inputs: A1/D7.W = text. Outputs: D3.L = value; D1 = 0/1.
tkpkgParseSignedDecimalV2	.block
	moveq #0, d6
	tst.w d7
	beq.s parseSignedFail
	cmpi.b #'-', (a1)
	bne.s parseSignedDigits
	moveq #1, d6
	addq.l #1, a1
	subq.w #1, d7
	beq.s parseSignedFail
parseSignedDigits
	move.w d7, d0
	bsr.w tkpkgParseU32DecimalV2
	bne.s parseSignedFail
	tst.b d6
	beq.s parseSignedOk
	neg.l d3
parseSignedOk
	moveq #0, d1
	rts
parseSignedFail
	moveq #1, d1
	rts
	.bend  ; tkpkgParseSignedDecimalV2

; Resolve one source spelling through the package-owned RENC v1 mapping.
; Inputs: A0/D0 = register token; D1.W = required neutral class.
; Outputs: D0 = 0 success or 1 no match/malformed; D3.L = neutral index.
tkpkgFindScopedRegisterEncodingV1	.block
	movem.l d2/d4-d7/a0-a6, -(sp)
	lea -18(sp), sp
	move.w d1, (sp)
	clr.w 2(sp)
	clr.w 4(sp)
	clr.w 6(sp)
	move.w d0, 8(sp)
	move.l a0, 10(sp)
	lea buffers.RencChunkOffsetLo, a3
	bsr.w tkpkgServiceChunkPtrFromLocatorV1
	bne.w rencFail
	bsr.w tkpkgServiceReadU16LeV1
	bne.w rencFail
	cmpi.w #1, d0
	bne.w rencFail
	bsr.w tkpkgServiceReadU32LeLow16V1
	bne.w rencFail
	addq.l #4, a2
	tst.w d0
	beq.w rencFail
	move.w d0, d7
	subq.w #1, d7

rencLoop
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w rencFail
	moveq #0, d6
	move.b (a2)+, d6
	cmpi.b #SCOPED_OWNER_DIALECT, d6
	bhi.w rencFail
	move.w d6, 16(sp)
	bsr.w tkpkgServiceLocateStringV1
	bne.w rencFail
	move.l a2, -(sp)
	bsr.w tkpkgSelectedMselOwnerMatchesV1
	movea.l (sp)+, a2
	move.w d0, 14(sp)
	bsr.w tkpkgServiceLocateStringV1
	bne.w rencFail
	move.l a2, -(sp)
	move.w 12(sp), d1
	movea.l 14(sp), a2
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	move.w d0, d5
	bsr.w tkpkgServiceReadU16LeV1
	bne.w rencFail
	move.w d0, d4
	bsr.w tkpkgServiceReadU16LeV1
	bne.w rencFail
	tst.w 14(sp)
	beq.s rencNext
	tst.w d5
	beq.s rencNext
	move.w 16(sp), d5
	addq.w #1, d5
	cmp.w 2(sp), d5
	bls.s rencNext
	move.w d5, 2(sp)
	move.w d0, 4(sp)
	move.w d4, 6(sp)
rencNext
	dbf d7, rencLoop
	cmpa.l a6, a2
	bne.s rencFail
	tst.w 2(sp)
	beq.s rencNoMatch
	move.w 6(sp), d0
	cmpi.w #$FFFF, (sp)
	beq.s rencMatch
	cmp.w (sp), d0
	bne.s rencNoMatch
rencMatch
	moveq #0, d3
	move.w 4(sp), d3
	moveq #0, d0
	bra.s rencReturn
rencNoMatch
rencFail
	moveq #1, d0
rencReturn
	lea 18(sp), sp
	movem.l (sp)+, d2/d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; tkpkgFindScopedRegisterEncodingV1

; Apply a package-owned required or optional value program to a scalar
; projection.  Optional constraint violations reject the selector candidate;
; malformed projections and value programs remain runtime errors.
; Inputs: A1/D0 = full required_value_program/value_program source.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3 = projected value.
tkpkgProjectRequiredValueV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -14(sp), sp
	move.w #1, (sp)
	clr.l 2(sp)
	clr.w 6(sp)
	clr.l 8(sp)
	clr.w 12(sp)
	movea.l a1, a5
	move.w d0, d7
	cmpi.w #29, d7
	bcs.s requiredCheckOptional
	lea RequiredValuePrefixText, a2
	movea.l a5, a1
	moveq #23, d0
	moveq #23, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	beq.s requiredCheckOptional
	lea 23(a5), a4
	move.l a4, 2(sp)
	move.w d7, d6
	subi.w #23, d6
	bra.s requiredPrefixReady

requiredCheckOptional
	clr.w (sp)
	cmpi.w #20, d7
	bcs.w requiredMalformed
	lea ValuePrefixText, a2
	movea.l a5, a1
	moveq #14, d0
	moveq #14, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	beq.w requiredMalformed
	lea 14(a5), a4
	move.l a4, 2(sp)
	move.w d7, d6
	subi.w #14, d6

requiredPrefixReady
	movea.l 2(sp), a2
	moveq #0, d5
requiredProgramScan
	tst.w d6
	beq.w requiredMalformed
	cmpi.b #':', (a2)
	beq.s requiredProgramReady
	addq.l #1, a2
	addq.w #1, d5
	subq.w #1, d6
	bra.s requiredProgramScan
requiredProgramReady
	tst.w d5
	beq.w requiredMalformed
	move.w d5, d7
	move.w d5, 6(sp)
	addq.l #1, a2
	subq.w #1, d6
	move.l a2, 8(sp)
	move.w d6, 12(sp)
	cmpi.w #5, d6
	bcs.w requiredMalformed
	cmpi.b #'e', (a2)
	bne.s requiredCheckTupleValue
	cmpi.b #'x', 1(a2)
	bne.s requiredCheckTupleValue
	cmpi.b #'p', 2(a2)
	bne.s requiredCheckTupleValue
	cmpi.b #'r', 3(a2)
	bne.s requiredCheckTupleValue
	movea.l 8(sp), a1
	bra.s requiredProjectSource

requiredCheckTupleValue
	movea.l 8(sp), a1
	move.w 12(sp), d0
	lea IndirectTupleValuePrefixText, a2
	moveq #20, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.w requiredMalformed
	movea.l 8(sp), a1
	adda.w #20, a1
	move.w 12(sp), d0
	subi.w #20, d0
	bsr.w tkpkgParseTupleItemClassSpecV2
	bne.w requiredMalformed
	cmpi.w #$FFFF, d5
	bne.w requiredMalformed
	move.w d3, d0
	move.w d4, d1
	jsr operand.tkpkgMselLocateIndirectTupleItemV2
	bne.w requiredNoMatch
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	bra.s requiredProjectionReady

requiredProjectSource
	move.w 12(sp), d0
	bsr.w tkpkgProjectCompactSemanticInputV2
requiredProjectionReady
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w requiredReturn
	movea.l 2(sp), a1
	move.w 6(sp), d0
	bsr.w tkpkgExecuteScopedValueProgramV1
	tst.l d0
	beq.s requiredOk
	cmpi.l #2, d0
	bne.s requiredMalformed
	tst.w (sp)
	beq.s requiredNoMatch
	bra.s requiredMalformed
requiredOk
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s requiredReturn
requiredNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s requiredReturn
requiredMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
requiredReturn
	lea 14(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectRequiredValueV1

; Find the most-specific scoped VALP row and execute the v1 scalar program.
; Inputs: A1/D0 = opaque program id; D3 = input value.
; Outputs: D0 = 0 success, 1 malformed/missing, or 2 constraint violation;
;          D3 = program result.
tkpkgExecuteScopedValueProgramV1	.block
	movem.l d2/d4-d7/a0-a6, -(sp)
	lea -24(sp), sp
	move.w d0, (sp)
	move.l a1, 2(sp)
	move.l d3, 6(sp)
	clr.w 10(sp)
	clr.w 12(sp)
	clr.l 14(sp)
	clr.w 18(sp)
	lea buffers.ValpChunkOffsetLo, a3
	bsr.w tkpkgServiceChunkPtrFromLocatorV1
	bne.w valpFail
	bsr.w tkpkgServiceReadU32LeLow16V1
	bne.w valpFail
	addq.l #4, a2
	tst.w d0
	beq.w valpFail
	move.w d0, d7
	subq.w #1, d7
valpLoop
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w valpFail
	moveq #0, d6
	move.b (a2)+, d6
	cmpi.b #SCOPED_OWNER_DIALECT, d6
	bhi.w valpFail
	move.w d6, 20(sp)
	bsr.w tkpkgServiceLocateStringV1
	bne.w valpFail
	move.l a2, -(sp)
	bsr.w tkpkgSelectedMselOwnerMatchesV1
	movea.l (sp)+, a2
	move.w d0, 22(sp)
	bsr.w tkpkgServiceLocateStringV1
	bne.w valpFail
	move.l a2, -(sp)
	move.w 4(sp), d1
	movea.l 6(sp), a2
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	move.w d0, d5
	bsr.w tkpkgServiceReadU16LeV1
	bne.w valpFail
	move.w d0, d4
	bsr.w tkpkgServiceReadU32LeLow16V1
	bne.w valpFail
	addq.l #4, a2
	move.w d0, d1
	movea.l a2, a0
	moveq #0, d0
	move.w d1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w valpFail
	move.w d0, d1
	adda.w d0, a2
	tst.w 22(sp)
	beq.s valpNext
	tst.w d5
	beq.s valpNext
	cmpi.w #1, d4
	bne.s valpNext
	move.w 20(sp), d5
	addq.w #1, d5
	cmp.w 10(sp), d5
	bls.s valpNext
	move.w d5, 10(sp)
	move.w d4, 12(sp)
	move.l a0, 14(sp)
	move.w d1, 18(sp)
valpNext
	dbf d7, valpLoop
	cmpa.l a6, a2
	bne.s valpFail
	tst.w 10(sp)
	beq.s valpFail
	movea.l 14(sp), a1
	move.w 18(sp), d1
	move.l 6(sp), d3
	bsr.w tkpkgExecuteValueProgramBytesV1
	bra.s valpReturn
valpFail
	moveq #1, d0
valpReturn
	lea 24(sp), sp
	movem.l (sp)+, d2/d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; tkpkgExecuteScopedValueProgramV1

; Direct Rust VALUE_VM v1 port for the operations required through Item 17.
; Inputs: A1/D1 = program bytes; D3 = input zero. Outputs: D0 status; D3 value.
tkpkgExecuteValueProgramBytesV1	.block
	movea.l a1, a0
	move.w d1, d7
	moveq #0, d6
valueLoop
	tst.w d7
	beq.w valueFail
	moveq #0, d0
	move.b (a0)+, d0
	subq.w #1, d7
	cmpi.b #$FF, d0
	beq.w valueEnd
	cmpi.b #$02, d0
	beq.s valuePushInput
	cmpi.b #$03, d0
	beq.s valueNormalize
	cmpi.b #$06, d0
	beq.w valueRequireRange
	bra.w valueFail
valuePushInput
	tst.w d7
	beq.w valueFail
	tst.b (a0)+
	bne.w valueFail
	subq.w #1, d7
	moveq #1, d6
	bra.s valueLoop
valueNormalize
	tst.b d6
	beq.w valueFail
	tst.w d7
	beq.w valueFail
	moveq #0, d2
	move.b (a0)+, d2
	subq.w #1, d7
	tst.b d2
	beq.w valueFail
	cmpi.b #32, d2
	beq.s valueLoop
	bhi.w valueFail
	moveq #-1, d4
	moveq #32, d5
	sub.b d2, d5
	lsr.l d5, d4
	and.l d4, d3
	moveq #1, d4
	subq.b #1, d2
	lsl.l d2, d4
	move.l d3, d5
	and.l d4, d5
	beq.s valueLoop
	moveq #-1, d4
	addq.b #1, d2
	lsl.l d2, d4
	or.l d4, d3
	bra.s valueLoop

valueRequireRange
	tst.b d6
	beq.w valueFail
	cmpi.w #16, d7
	bcs.w valueFail
	bsr.w tkpkgValueReadI64LeV1
	move.l d0, d4
	move.l d1, d5
	bsr.w tkpkgValueReadI64LeV1
	subi.w #16, d7
	; Reject a malformed minimum greater than the maximum, matching Rust's
	; inclusive-range program validation/execution contract.
	cmp.l d4, d0
	blt.w valueFail
	bgt.s valueRangeBoundsReady
	cmp.l d5, d1
	bcs.w valueFail

valueRangeBoundsReady
	; The native expression transport is one signed 32-bit scalar.  Compare its
	; sign-extended high/low pair against the package's signed i64 bounds.
	moveq #0, d2
	tst.l d3
	bpl.s valueRangeHighReady
	moveq #-1, d2

valueRangeHighReady
	cmp.l d4, d2
	blt.w valueConstraintFail
	bgt.s valueRangeMinOk
	cmp.l d5, d3
	bcs.w valueConstraintFail

valueRangeMinOk
	cmp.l d0, d2
	bgt.w valueConstraintFail
	blt.w valueLoop
	cmp.l d1, d3
	bhi.w valueConstraintFail
	bra.w valueLoop

valueEnd
	tst.w d7
	bne.w valueFail
	tst.b d6
	beq.w valueFail
	moveq #0, d0
	rts
valueConstraintFail
	moveq #2, d0
	rts
valueFail
	moveq #1, d0
	rts
	.bend  ; tkpkgExecuteValueProgramBytesV1

; Read one package i64 stored little-endian.  The split result preserves all
; bits for signed high-word plus unsigned low-word comparisons on 68020.
; Inputs: A0 = eight-byte value. Outputs: D0 = high 32, D1 = low 32, A0 += 8.
; Clobbers: CCR.
tkpkgValueReadI64LeV1	.block
	move.l (a0)+, d1
	ror.w #8, d1
	swap d1
	ror.w #8, d1
	move.l (a0)+, d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	rts
	.bend  ; tkpkgValueReadI64LeV1

	.pub
; Resolve one prefix-compressed CMSE string into a bounded destination.
; Inputs: D0.W = string index; A0 = destination.
; Outputs: D0.W = length and D1 = 0, or D1 = 1 on malformed input.
resolveCompactSelectorStringV1	.block
	movem.l d2-d7/a1-a6, -(sp)
	move.w d0, d7
	cmp.w buffers.CompactSelectorStringCount, d7
	bhs.s compactStringResolveFail
	movea.l a0, a5
	movea.l buffers.CompactSelectorStringsPtr, a2
	movea.l buffers.CompactSelectorChunkEndPtr, a6
	moveq #0, d5
	moveq #0, d4
compactStringResolveLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.s compactStringResolveFail
	move.w d0, d6
	cmp.w d5, d6
	bhi.s compactStringResolveFail
	bsr.w tkpkgServiceLocateStringV1
	bne.s compactStringResolveFail
	move.w d0, d3
	add.w d6, d0
	bcs.s compactStringResolveFail
	cmpi.w #buffers.COMPACT_STRING_SCRATCH_CAPACITY, d0
	bhi.s compactStringResolveFail
	move.w d0, d5
	lea buffers.CompactStringScratchBuffer, a3
	adda.w d6, a3
	tst.w d3
	beq.s compactStringResolveReady
	subq.w #1, d3
compactStringResolveSuffixCopy
	move.b (a1)+, (a3)+
	dbf d3, compactStringResolveSuffixCopy
compactStringResolveReady
	cmp.w d7, d4
	beq.s compactStringResolveCopy
	addq.w #1, d4
	bra.s compactStringResolveLoop
compactStringResolveCopy
	cmpi.w #buffers.COMPACT_SELECTOR_TEXT_CAPACITY, d5
	bhi.s compactStringResolveFail
	lea buffers.CompactStringScratchBuffer, a1
	movea.l a5, a3
	move.w d5, d3
	beq.s compactStringResolveOk
	subq.w #1, d3
compactStringResolveCopyLoop
	move.b (a1)+, (a3)+
	dbf d3, compactStringResolveCopyLoop
compactStringResolveOk
	move.w d5, d0
	moveq #0, d1
	bra.s compactStringResolveReturn
compactStringResolveFail
	moveq #0, d0
	moveq #1, d1
compactStringResolveReturn
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d1
	rts
	.bend  ; resolveCompactSelectorStringV1

; Skip one non-raw CMSE v7 plan body. Inputs: D0.B = plan kind; A2/A6 cursor.
; Output: D0 = 0 success, 1 malformed.
skipCompactSelectorPlanBodyV7	.block
	cmpi.b #1, d0
	beq.w compactPlanInputs
	cmpi.b #2, d0
	beq.w compactPlanScalar
	cmpi.b #3, d0
	beq.w compactPlanInputs
	cmpi.b #4, d0
	beq.w compactPlanSequence
	cmpi.b #5, d0
	beq.w compactPlanState
	cmpi.b #6, d0
	beq.w compactPlanReject
	bra.w compactPlanFail
compactPlanInputs
	bsr.w tkpkgServiceReadU16LeV1
	bne.w compactPlanFail
	bsr.w skipCompactSelectorInputsV7
	bne.w compactPlanFail
	bsr.w tkpkgServiceReadU16LeV1
	bne.w compactPlanFail
	bra.w compactPlanOk
compactPlanScalar
	moveq #4, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w compactPlanFail
	addq.l #4, a2
	bra.w compactPlanOk
compactPlanSequence
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w compactPlanFail
	moveq #0, d1
	move.b (a2)+, d1
	beq.w compactPlanFail
	move.w d1, -(sp)
compactPlanSequenceLoop
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s compactPlanSequenceStackFail
	move.b (a2)+, d0
	cmpi.b #2, d0
	bhi.s compactPlanSequenceStackFail
	bsr.w tkpkgServiceReadU16LeV1
	bne.s compactPlanSequenceStackFail
	bsr.w skipCompactSelectorInputsV7
	bne.s compactPlanSequenceStackFail
	subq.w #1, (sp)
	bne.s compactPlanSequenceLoop
	addq.l #2, sp
	bsr.w tkpkgServiceReadU16LeV1
	bne.s compactPlanFail
	bra.s compactPlanOk
compactPlanSequenceStackFail
	addq.l #2, sp
	bra.s compactPlanFail
compactPlanState
	bsr.w tkpkgServiceReadU16LeV1
	bne.s compactPlanFail
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s compactPlanFail
	moveq #0, d0
	move.b (a2)+, d0
	beq.s compactPlanStateRaw
	bsr.w skipCompactSelectorPlanBodyV7
	bra.s compactPlanReturn
compactPlanStateRaw
	bsr.w tkpkgServiceReadU16LeV1
	bne.s compactPlanFail
	bra.s compactPlanOk
compactPlanReject
	bsr.w tkpkgServiceReadU16LeV1
	bne.s compactPlanFail
	bsr.w skipCompactSelectorInputsV7
	bne.s compactPlanFail
compactPlanOk
	moveq #0, d0
compactPlanReturn
	rts
compactPlanFail
	moveq #1, d0
	rts
	.bend  ; skipCompactSelectorPlanBodyV7

skipCompactSelectorInputsV7	.block
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s compactInputsFail
	moveq #0, d0
	move.b (a2)+, d0
	lsl.w #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s compactInputsFail
	adda.w d0, a2
	moveq #0, d0
	rts
compactInputsFail
	moveq #1, d0
	rts
	.bend  ; skipCompactSelectorInputsV7

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

; - state.EncodeSelectedOperandStatus updated on failure.
;
; Clobbers:
; - D0-D2/D4-D7/A0-A2/A6/CCR
;
; CCR:
; - Reflects D0 on return.

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
