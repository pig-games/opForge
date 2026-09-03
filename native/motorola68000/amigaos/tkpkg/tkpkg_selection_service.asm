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
	.use tkpkg.amigaos.state_service as state_service
	.use opcore.amigaos.expr_bridge
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	.use debug.amigaos.runtime_profile as runtime_profile
.endif

TKPKG_EVAL_EXPR_REQUEST_FIXED_SIZE = 9
TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE = 16
TKPKG_SELECTED_EXTENSION_INPUT_SIZE = 24
TKPKG_SELECTED_EXTENSION_PASS_INPUT_SIZE = 28
TKPKG_SELECTED_EXTENSION_RESOLVER_INPUT_SIZE = 32
TKPKG_SELECTED_EXTENSION_DEFER_INPUT_SIZE = 36
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

IndirectTupleIdentityScalePrefixText
	.byte "indirect_tuple_identity_scale"

IndirectTupleNonidentityScalePrefixText
	.byte "indirect_tuple_nonidentity_scale"

ExprPathPrefixText
	.byte "xp1:"

CallArgIndirectRegisterPrefixText
	.byte "call_arg_indirect_register"

CallArgIndirectTupleRegisterPrefixText
	.byte "call_arg_indirect_tuple_register"

CallArgRegisterPrefixText
	.byte "call_arg_register"

CallArgRegisterSequencePrefixText
	.byte "call_arg_register_sequence"

CallArgValuePrefixText
	.byte "call_arg_value"

CallArgMemberPrefixText
	.byte "call_arg_member"

UnaryPlusIndirectRegisterPrefixText
	.byte "unary_plus_indirect_reg"

UnaryMinusIndirectRegisterPrefixText
	.byte "unary_minus_indirect_reg"

MemberShapePrefixText
	.byte "member_shape"

MemberPrefixText
	.byte "member"

NamedRegisterPrefixText
	.byte "named_register"

RegisterOrNamedRangePrefixText
	.byte "register_or_named_range"

RegisterSequencePrefixText
	.byte "register_sequence"

RegisterIndexXorPrefixText
	.byte "register_index_xor"

BoundedRegisterPrefixText
	.byte "bounded_reg"

SemanticArgMarkerText
	.byte ".arg"

SemanticClassMarkerText
	.byte ".class"

SemanticMinMarkerText
	.byte ".min"

SemanticMaxMarkerText
	.byte ".max"

SemanticOutsideMarkerText
	.byte ".outside"

SemanticCountMarkerText
	.byte ".count"

SemanticAlignMarkerText
	.byte ".align"

SemanticViolationMarkerText
	.byte ".violation-"

SemanticAlignmentViolationText
	.byte "alignment"

SemanticSequenceViolationText
	.byte "sequence"

TargetPrefixText
	.byte "target:"

AutomaticBranchCandidateText
	.byte "auto"

RejectMnemonicPlaceholderText
	.byte "{mnemonic}"

RejectFormPlaceholderText
	.byte "{form}"

RejectRegisterPlaceholderText
	.byte "{register}"

RejectValuePlaceholderText
	.byte "{value}"

RejectValueMinI32Text
	.byte "-2147483648"

RejectValueDecimalPowers
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

ImmediateRegisterShapeText
	.byte "immediate_register"

RegisterMaskPrefixText
	.byte "register_mask"

DuplicateRegisterPrefixText
	.byte "duplicate_register"

OutOfRangePrefixText
	.byte "out_of_range"

DistinctRegisterPrefixText
	.byte "distinct_register"

ImmediateDirectShapeText
	.byte "immediate_direct"

ImmediateShapeText
	.byte "immediate"

ImpliedShapeText
	.byte "implied"

RegisterShapeText
	.byte "register"

DirectShapeText
	.byte "direct"

RegisterRegisterShapeText
	.byte "register_register"

RegisterRegisterRegisterShapeText
	.byte "register_register_register"

RegisterImmediateShapeText
	.byte "register_immediate"

DirectRegisterShapeText
	.byte "direct_register"

RegisterDirectShapeText
	.byte "register_direct"

DirectDirectShapeText
	.byte "direct_direct"

	.endsection

	.section code, kind=code
	.pub

selectInstructionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_SERVICE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	movem.l (sp)+, d0-d1
.endif
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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	jsr runtime_profile.opforgeRuntimeProfileLeaveServiceV1
.endif
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
	clr.b state.EncodeSelectedDeferUnstableBranchTarget
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
	cmpi.w #TKPKG_SELECTED_EXTENSION_DEFER_INPUT_SIZE, d5
	bcs.s resolveVersions
	move.l (a5)+, d0
	tst.l d0
	sne state.EncodeSelectedDeferUnstableBranchTarget
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
	clr.b state.EncodeSelectedDeferUnstableBranchTarget

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
	lea -2(sp), sp
	clr.w (sp)
	clr.w state.EncodeSelectedMselMatchFlags
	clr.w state.EncodeSelectedMselFallbackLen
	movea.l a0, a5
	move.w d0, d2
	move.w d2, state.EncodeSelectedMselMnemonicLen
	tst.w d2
	beq.w noOutput
	tst.w state.EncodeSelectedMselShapeLen
	bne.s frontendShapePresent
	tst.l state.EncodeSelectedMselShapePtr
	beq.s inferPackageShape
frontendShapePresent
	move.w #2, (sp)
inferPackageShape
	bsr.w tkpkgInferSelectedPackageShapeV1
mselChunk
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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
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
	beq.w noFallback
	moveq #0, d1
	move.w state.EncodeSelectedMselFallbackLen, d1
	moveq #0, d2
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.w return

noFallback
	moveq #0, d1
	btst #2, state.EncodeSelectedMselMatchFlags
	beq.s unknownMnemonic
	; Rust first uses the family resolver (for example, its package-declared
	; two-direct-operand shape) and only then falls back to package_shape_input.
	; Native has no host family handler. If its neutral inferred shape found no
	; MSEL row, retry the same package rows exactly once without a shape filter
	; and let the package-owned operand plan validate the candidate.
	tst.w (sp)
	bne.s unsupportedAddress
	tst.w state.EncodeSelectedMselShapeLen
	beq.s unsupportedAddress
	clr.l state.EncodeSelectedMselShapePtr
	clr.w state.EncodeSelectedMselShapeLen
	clr.w state.EncodeSelectedMselMatchFlags
	clr.w state.EncodeSelectedMselFallbackLen
	move.w #1, (sp)
	bra.w mselChunk

unsupportedAddress
	moveq #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0
	bra.w return

unknownMnemonic
	moveq #TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC, d0
	bra.w return

unsupported
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0
	bra.w return

compactSelector
	; The frontend still supplies legacy family-resolver shapes for established
	; packages.  Preserve that first choice.  If the package has
	; no matching selector, retry exactly once with Rust's package_shape_input
	; classification; register-aware package families classify every lone
	; non-register expression (including `(register)`) as direct.
	move.b (sp), d7
	move.l state.EncodeSelectedMselShapePtr, d3
	moveq #0, d6
	move.w state.EncodeSelectedMselShapeLen, d6
	movea.l a5, a0
	moveq #0, d0
	move.w state.EncodeSelectedMselMnemonicLen, d0
	bsr.w tkpkgBuildSelectedEnvelopeFromCmseV7
	cmpi.l #TKPKG_SELECTED_STATUS_UNSUPPORTED_ADDRESS, d0
	bne.w return
	tst.w d6
	beq.w return
	; Native has no host family resolver. After exact per-row alternative matching
	; fails for a neutral package shape, let the package operand plans provide the
	; compatibility fallback. Exact shapes still win before this path, so an
	; immediate row cannot be displaced by an unrelated direct row.
	tst.b d7
	bne.s retryNeutralPackageShape
	clr.l state.EncodeSelectedMselShapePtr
	clr.w state.EncodeSelectedMselShapeLen
	movea.l a5, a0
	moveq #0, d0
	move.w state.EncodeSelectedMselMnemonicLen, d0
	bsr.w tkpkgBuildSelectedEnvelopeFromCmseV7
	bra.w return

retryNeutralPackageShape
	clr.l state.EncodeSelectedMselShapePtr
	clr.w state.EncodeSelectedMselShapeLen
	bsr.w tkpkgInferSelectedPackageShapeV1
	tst.w state.EncodeSelectedMselShapeLen
	beq.s restoreLegacyShape
	move.l state.EncodeSelectedMselShapePtr, d4
	cmp.l d3, d4
	bne.s retryCompactSelector
	cmp.w state.EncodeSelectedMselShapeLen, d6
	beq.s restoreLegacyShape

retryCompactSelector
	movea.l a5, a0
	moveq #0, d0
	move.w state.EncodeSelectedMselMnemonicLen, d0
	bsr.w tkpkgBuildSelectedEnvelopeFromCmseV7
	bra.w return

restoreLegacyShape
	move.l d3, state.EncodeSelectedMselShapePtr
	move.w d6, state.EncodeSelectedMselShapeLen

return
	addq.l #2, sp
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildSelectedEnvelopeFromMselV1

; Infer Rust's package shape at the package boundary when the frontend supplied
; no legacy selected-shape metadata. Source spans stand in for Expr nodes; the
; package register map distinguishes registers, while every other single
; operand is direct. The bounded native surface classifies up to three
; top-level operands, which covers package-owned three-register rows.
;
; Inputs: state.EncodeSelectedMselExprPtr/Len and resolved package context.
; Outputs: state.EncodeSelectedMselShapePtr/Len when inference applies.
; Clobbers: D0-D7/A0-A3/CCR.
tkpkgInferSelectedPackageShapeV1	.block
	movem.l d2-d7/a2-a4, -(sp)
	tst.w state.EncodeSelectedMselShapeLen
	bne.w return
	tst.l state.EncodeSelectedMselShapePtr
	bne.w return
	tst.w state.EncodeSelectedMselExprLen
	bne.s locateFirst
	lea ImpliedShapeText, a0
	moveq #7, d0
	bra.w setShape

locateFirst
	moveq #0, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w return
	movea.l a0, a2
	move.l d0, d2
	moveq #1, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w classifySingle
	movea.l a0, a3
	move.l d0, d4
	moveq #2, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w classifyPair
	movea.l a0, a4
	move.l d0, d5
	moveq #3, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	beq.w return

	movea.l a4, a0
	move.l d5, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w return
	movea.l a3, a0
	move.l d4, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w return
	movea.l a2, a0
	move.l d2, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w return
	lea RegisterRegisterRegisterShapeText, a0
	moveq #26, d0
	bra.w setShape

classifyPair

	cmpi.b #'#', (a2)
	beq.s classifyImmediatePair
	movea.l a3, a0
	move.l d4, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	sne d7
	movea.l a2, a0
	move.l d2, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	sne d6
	tst.b d6
	bne.s firstDirect
	cmpi.b #'#', (a3)
	beq.s registerImmediate
	tst.b d7
	bne.s registerDirect
	lea RegisterRegisterShapeText, a0
	moveq #17, d0
	bra.s setShape

registerImmediate
	lea RegisterImmediateShapeText, a0
	moveq #18, d0
	bra.s setShape

firstDirect
	tst.b d7
	bne.s directDirect
	lea DirectRegisterShapeText, a0
	moveq #15, d0
	bra.s setShape

registerDirect
	lea RegisterDirectShapeText, a0
	moveq #15, d0
	bra.s setShape

directDirect
	lea DirectDirectShapeText, a0
	moveq #13, d0
	bra.s setShape

classifyImmediatePair
	movea.l a3, a0
	move.l d4, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.s immediateDirect
	lea ImmediateRegisterShapeText, a0
	moveq #18, d0
	bra.s setShape

immediateDirect
	lea ImmediateDirectShapeText, a0
	moveq #16, d0
	bra.s setShape

classifySingle
	cmpi.b #'#', (a2)
	beq.s singleImmediate
	movea.l a2, a0
	move.l d2, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.s singleDirect
	lea RegisterShapeText, a0
	moveq #8, d0
	bra.s setShape

singleImmediate
	lea ImmediateShapeText, a0
	moveq #9, d0
	bra.s setShape

singleDirect
	lea DirectShapeText, a0
	moveq #6, d0

setShape
	move.l a0, state.EncodeSelectedMselShapePtr
	move.w d0, state.EncodeSelectedMselShapeLen

return
	movem.l (sp)+, d2-d7/a2-a4
	rts
	.bend  ; tkpkgInferSelectedPackageShapeV1

; Decode the Rust CMSE v7 wire format and recover raw or v2 scalar-input plans.
; Both paths preserve package ownership: this module only transports opaque
; program ids and neutral scalar projections into the candidate envelope.
; Inputs: A0/D0 = selected mnemonic text/length.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D1 = envelope length on success.
tkpkgBuildSelectedEnvelopeFromCmseV7	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea -44(sp), sp
	clr.w state.EncodeSelectedMselMatchFlags
	clr.w state.EncodeSelectedMselFallbackLen
	clr.w 24(sp)
	clr.w 26(sp)
	clr.w 28(sp)
	clr.w 30(sp)
	clr.w 32(sp)
	; Rust scans active selector owners in dialect, CPU, family order and keeps
	; successful candidates ahead of every less-specific owner.  CMSE stores
	; rows in package order, so retain the first success at the greatest active
	; owner rank while the remaining rows are scanned.
	clr.w 34(sp)  ; retained success owner rank: 0 none, 1 family, 2 CPU, 3 dialect
	clr.w 36(sp)  ; retained success envelope length
	clr.w 38(sp)  ; retained plan kind / unstable flags
	clr.w 40(sp)  ; retained symbol-reference flag
	move.w #$FFFF, 42(sp)  ; retained semantic diagnostic index
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
	bsr.w tkpkgServiceShapeAlternativeMatchesV1
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
	move.w #$FFFF, state.EncodeSelectedSemanticDiagnosticIndex
	clr.l state.EncodeSelectedSemanticValue
	clr.b state.EncodeSelectedSemanticValueValid
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
	move.w d0, 24(sp)
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
	move.w d4, d0
	lea buffers.CompactSelectorModeText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w cmseMalformed
	move.l a2, -(sp)
	lea buffers.CompactSelectorModeText, a1
	movea.l state.EncodeSelectedMselShapePtr, a2
	move.w state.EncodeSelectedMselShapeLen, d1
	bsr.w tkpkgServiceShapeAlternativeMatchesV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w cmseSelectorNext
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
	tst.w state.EncodeSelectedMselShapeLen
	bne.s cmseSaveRequestedShape
	moveq #0, d0
	move.w 14(sp), d0
	lea buffers.CompactSelectorShapeText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w cmseMalformed

cmseSaveRequestedShape
	move.l state.EncodeSelectedMselShapePtr, -(sp)
	move.w state.EncodeSelectedMselShapeLen, -(sp)
	tst.w (sp)
	bne.s cmseCandidateShapeReady
	lea buffers.CompactSelectorShapeText, a1
	move.l a1, state.EncodeSelectedMselShapePtr
	move.w d0, state.EncodeSelectedMselShapeLen
cmseCandidateShapeReady
	move.l state.EncodeSelectedMselShapePtr, d0
	move.l d0, state.EncodeSelectedCurrentShapePtr
	move.w state.EncodeSelectedMselShapeLen, d0
	move.w d0, state.EncodeSelectedCurrentShapeLen
	bset #0, state.EncodeSelectedMselMatchFlags
	move.l a2, -(sp)
	move.w d7, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
	jsr operand.tkpkgMselTryBuildCandidateV1
	move.w (sp)+, d7
	movea.l (sp)+, a2
	move.l d0, d5
	move.l d1, d6
	move.w (sp)+, d3
	move.w d3, state.EncodeSelectedMselShapeLen
	move.l (sp)+, d3
	move.l d3, state.EncodeSelectedMselShapePtr
	move.l d6, d1
	move.l d5, d0
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w cmseCandidateReady
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
	cmpi.b #3, d0
	beq.s cmseBuildBranch
	cmpi.b #4, d0
	beq.s cmseBuildSequence
	cmpi.b #5, d0
	beq.s cmseBuildState
	cmpi.b #6, d0
	bne.w cmseSelectorNext
	movea.l state.EncodeSelectedSemanticPlanPtr, a1
	move.l a2, -(sp)
	move.w d7, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
	bsr.w tkpkgBuildCompactSemanticRejectCandidateV2
	move.w (sp)+, d7
	movea.l (sp)+, a2
	bra.w cmseCandidateReady
cmseBuildBranch
	movea.l state.EncodeSelectedSemanticPlanPtr, a1
	move.l a2, -(sp)
	move.w d7, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
	bsr.w tkpkgBuildCompactSemanticBranchCandidateV2
	move.w (sp)+, d7
	movea.l (sp)+, a2
	bra.w cmseCandidateReady
cmseBuildSequence
	movea.l state.EncodeSelectedSemanticPlanPtr, a1
	move.l a2, -(sp)
	move.w d7, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
	bsr.w tkpkgBuildCompactSemanticSequenceCandidateV2
	move.w (sp)+, d7
	movea.l (sp)+, a2
	bra.w cmseCandidateReady
cmseBuildState
	movea.l state.EncodeSelectedSemanticPlanPtr, a1
	move.l a2, -(sp)
	move.w d7, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
	bsr.w tkpkgBuildCompactStateCandidateV2
	move.w (sp)+, d7
	movea.l (sp)+, a2
	bra.w cmseCandidateReady
cmseBuildStructured
	movea.l state.EncodeSelectedSemanticPlanPtr, a1
	move.l a2, -(sp)
	move.w d7, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
	bsr.w tkpkgBuildCompactSemanticCandidateV2
	move.w (sp)+, d7
	movea.l (sp)+, a2

cmseCandidateReady
	cmpi.l #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	beq.s cmseCandidateError
	cmpi.l #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	bne.s cmseCandidateCheckOk

cmseCandidateError
	; Rust retains a selector error from the most-specific active owner
	; (dialect, then CPU, then family) and the highest priority within that
	; owner. Preserve both semantic rejects and operand errors while later rows
	; are evaluated, because a later candidate may still encode successfully.
	; A valid candidate wins over every deferred selector error in Rust.  The
	; shared bounded retention buffer already owns that candidate envelope.
	tst.w 36(sp)
	bne.w cmseSelectorNext
	moveq #1, d2
	move.w 10(sp), d3
	cmp.w 18(sp), d3
	beq.s cmseRejectCpuRank
	cmp.w 20(sp), d3
	bne.s cmseRejectRankReady
	moveq #3, d2
	bra.s cmseRejectRankReady
cmseRejectCpuRank
	moveq #2, d2
cmseRejectRankReady
	cmp.w 26(sp), d2
	bcs.w cmseSelectorNext
	bhi.s cmseRejectSave
	move.w 24(sp), d3
	cmp.w 28(sp), d3
	bls.w cmseSelectorNext
cmseRejectSave
	move.w d0, 32(sp)
	tst.w d1
	beq.w cmseMalformed
	cmpi.w #buffers.TOKEN_SCRATCH_CAPACITY, d1
	bhi.w cmseMalformed
	lea buffers.TokenScratchBuffer, a0
	lea buffers.DeferredSemanticRejectBuffer, a1
	move.w d1, d5
	subq.w #1, d5
cmseRejectCopy
	move.b (a0)+, (a1)+
	dbf d5, cmseRejectCopy
	move.w d2, 26(sp)
	move.w 24(sp), 28(sp)
	move.w d1, 30(sp)
	bra.w cmseSelectorNext

cmseCandidateCheckOk
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w cmseCandidateNotOk
	moveq #1, d2
	move.w 10(sp), d3
	cmp.w 18(sp), d3
	beq.s cmseSuccessCpuRank
	cmp.w 20(sp), d3
	bne.s cmseSuccessRankReady
	moveq #3, d2
	bra.s cmseSuccessRankReady
cmseSuccessCpuRank
	moveq #2, d2
cmseSuccessRankReady
	cmp.w 34(sp), d2
	bls.w cmseSelectorNext
	tst.w d1
	beq.w cmseMalformed
	cmpi.w #buffers.TOKEN_SCRATCH_CAPACITY, d1
	bhi.w cmseMalformed
	lea buffers.TokenScratchBuffer, a0
	lea buffers.DeferredSemanticRejectBuffer, a1
	move.w d1, d5
	subq.w #1, d5
cmseSuccessCopy
	move.b (a0)+, (a1)+
	dbf d5, cmseSuccessCopy
	move.w d2, 34(sp)
	move.w d1, 36(sp)
	moveq #0, d3
	move.b state.EncodeSelectedSemanticPlanKind, d3
	moveq #0, d5
	move.b state.EncodeSelectedMselUnstable, d5
	lsl.w #8, d5
	or.w d5, d3
	move.w d3, 38(sp)
	moveq #0, d3
	move.b state.EncodeSelectedMselHasSymbolReference, d3
	move.w d3, 40(sp)
	move.w state.EncodeSelectedSemanticDiagnosticIndex, 42(sp)
	bset #1, state.EncodeSelectedMselMatchFlags
	move.w d1, state.EncodeSelectedMselFallbackLen
	bra.w cmseSelectorNext

cmseCandidateNotOk
	bra.w cmseSelectorNext

cmseSelectorNext
	dbf d7, cmseSelectorLoop
	move.w 36(sp), d1
	beq.s cmseNoRetainedSuccess
	lea buffers.DeferredSemanticRejectBuffer, a0
	lea buffers.TokenScratchBuffer, a1
	move.w d1, d5
	subq.w #1, d5
cmseSuccessRestore
	move.b (a0)+, (a1)+
	dbf d5, cmseSuccessRestore
	move.w 38(sp), d3
	move.b d3, state.EncodeSelectedSemanticPlanKind
	lsr.w #8, d3
	move.b d3, state.EncodeSelectedMselUnstable
	move.w 40(sp), d3
	move.b d3, state.EncodeSelectedMselHasSymbolReference
	move.w 42(sp), state.EncodeSelectedSemanticDiagnosticIndex
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s cmseReturn
cmseNoRetainedSuccess
	btst #1, state.EncodeSelectedMselMatchFlags
	beq.s cmseNoFallback
	moveq #0, d1
	move.w state.EncodeSelectedMselFallbackLen, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s cmseReturn
cmseNoFallback
	move.w 30(sp), d1
	beq.s cmseNoDeferredReject
	lea buffers.DeferredSemanticRejectBuffer, a0
	lea buffers.TokenScratchBuffer, a1
	move.w d1, d5
	subq.w #1, d5
cmseDeferredRejectCopy
	move.b (a0)+, (a1)+
	dbf d5, cmseDeferredRejectCopy
	moveq #0, d0
	move.w 32(sp), d0
	bra.s cmseReturn
cmseNoDeferredReject
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
	lea 44(sp), sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; tkpkgBuildSelectedEnvelopeFromCmseV7

	.priv
; Evaluate one package-owned state guard and dispatch its nested compact plan.
; Inputs: A1 = state plan body; A5/D2 = selected mnemonic; A6 = CMSE end.
; Outputs: D0/D1 use TKPKG_SELECTED_STATUS_* candidate convention.
tkpkgBuildCompactStateCandidateV2	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a1, a2
	bsr.w tkpkgServiceReadU16LeV1
	bne.w stateCandidateMalformed
	move.w d0, d7
	movea.l a2, a3
	lea buffers.CompactSelectorPlanText, a0
	move.w d7, d0
	bsr.w resolveCompactSelectorStringV1
	bne.w stateCandidateMalformed
	lea buffers.CompactSelectorPlanText, a0
	jsr state_service.requirementAllowsV1
	cmpi.w #2, d0
	beq.w stateCandidateMalformed
	cmpi.w #1, d0
	bne.s stateCandidateAllowed
	tst.w d1
	beq.s stateCandidateMiss
	move.w d1, d0
	move.w state.EncodeSelectedMselMnemonicLen, d2
	movea.l state.EncodeSelectedMnemonicPtr, a5
	moveq #1, d3
	suba.l a0, a0
	moveq #0, d4
	bsr.w tkpkgRenderRejectMessageCodeV1
	tst.l d0
	bne.w stateCandidateMalformed
	moveq #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	bra.w stateCandidateReturn

stateCandidateMiss
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_UNKNOWN_MNEMONIC, d0
	bra.w stateCandidateReturn

stateCandidateAllowed
	movea.l a3, a2
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.s stateCandidateMalformed
	moveq #0, d0
	move.b (a2)+, d0
	movea.l a2, a1
	move.b d0, state.EncodeSelectedSemanticPlanKind
	cmpi.b #1, d0
	beq.s stateCandidateStructured
	cmpi.b #2, d0
	beq.s stateCandidateStructured
	cmpi.b #3, d0
	beq.s stateCandidateBranch
	cmpi.b #4, d0
	beq.s stateCandidateSequence
	cmpi.b #5, d0
	beq.s stateCandidateNested
	cmpi.b #6, d0
	beq.s stateCandidateReject
	bra.s stateCandidateMalformed

stateCandidateStructured
	bsr.w tkpkgBuildCompactSemanticCandidateV2
	bra.s stateCandidateReturn
stateCandidateBranch
	bsr.w tkpkgBuildCompactSemanticBranchCandidateV2
	bra.s stateCandidateReturn
stateCandidateSequence
	bsr.w tkpkgBuildCompactSemanticSequenceCandidateV2
	bra.s stateCandidateReturn
stateCandidateNested
	bsr.w tkpkgBuildCompactStateCandidateV2
	bra.s stateCandidateReturn
stateCandidateReject
	bsr.w tkpkgBuildCompactSemanticRejectCandidateV2
	bra.s stateCandidateReturn
stateCandidateMalformed
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
stateCandidateReturn
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; tkpkgBuildCompactStateCandidateV2

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
	bne.w semanticProjectionFailed
	tst.b state.EncodeSelectedSemanticValueValid
	bne.s semanticInputValueCaptured
	move.l d3, state.EncodeSelectedSemanticValue
	move.b #1, state.EncodeSelectedSemanticValueValid
semanticInputValueCaptured
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
	bne.s semanticProjectionFailed
	move.l d3, state.EncodeSelectedSemanticValue
	move.b #1, state.EncodeSelectedSemanticValueValid
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
semanticProjectionFailed
	cmpi.l #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	beq.s semanticReturn
	bsr.w tkpkgRenderSelectedSemanticRejectV1
	tst.l d0
	bne.s semanticMalformed
	moveq #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	bra.s semanticReturn
semanticMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	moveq #0, d1
semanticReturn
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildCompactSemanticCandidateV2

; Build the neutral four-input envelope for Rust's CMSE v7 semantic-branch
; plan.  The package supplies opcode, target source, requested candidate (or
; `auto`), and automatic class; native transports those values unchanged to
; the SEMV v5 branch interpreter.
; Inputs: A1 = kind-3 plan body; A5/D2 = mnemonic; A6 = CMSE chunk end.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D1 = envelope length on success.
tkpkgBuildCompactSemanticBranchCandidateV2	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a1, a2
	bsr.w tkpkgServiceReadU16LeV1
	bne.w branchMalformed
	lea buffers.CompactSelectorPlanText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w branchMalformed
	tst.w d0
	beq.w branchMalformed
	cmpi.w #buffers.COMPACT_SELECTOR_TEXT_CAPACITY, d0
	bhi.w branchMalformed
	lea buffers.CompactSelectorPlanText, a1
	move.l a1, state.EncodeSelectedMselModePtr
	move.w d0, state.EncodeSelectedMselModeLen
	move.l state.EncodeSelectedMselShapePtr, d1
	move.l d1, state.EncodeSelectedCurrentShapePtr
	move.w state.EncodeSelectedMselShapeLen, d1
	move.w d1, state.EncodeSelectedCurrentShapeLen
	bset #0, state.EncodeSelectedMselMatchFlags

	lea buffers.TokenScratchBuffer, a4
	move.w d2, d0
	cmpi.w #255, d0
	bhi.w branchMalformed
	move.b d0, (a4)+
	movea.l a5, a0
	jsr operand.tkpkgMselCopyBytesV1
	move.b #1, (a4)+
	move.w state.EncodeSelectedMselModeLen, d0
	cmpi.w #255, d0
	bhi.w branchMalformed
	move.b d0, (a4)+
	movea.l state.EncodeSelectedMselModePtr, a0
	jsr operand.tkpkgMselCopyBytesV1

	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w branchMalformed
	cmpi.b #4, (a2)+
	bne.w branchMalformed
	move.b #4, (a4)+
	moveq #0, d6

branchInputLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.w branchMalformed
	lea buffers.CompactSelectorInputText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w branchMalformed
	tst.w d0
	beq.w branchMalformed
	lea buffers.CompactSelectorInputText, a1
	cmpi.w #1, d6
	beq.s branchProjectTarget
	cmpi.w #2, d6
	bne.s branchParseUnsigned
	cmpi.w #4, d0
	bne.s branchParseUnsigned
	move.l a2, -(sp)
	lea AutomaticBranchCandidateText, a2
	moveq #4, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.s branchReloadUnsigned
	moveq #-1, d3
	bra.s branchInputReady

branchReloadUnsigned
	lea buffers.CompactSelectorInputText, a1
	moveq #4, d0

branchParseUnsigned
	bsr.w tkpkgParseU16DecimalV2
	bne.w branchMalformed
	cmpi.w #0, d6
	bne.s branchUnsignedRangeReady
	cmpi.l #255, d3
	bhi.w branchMalformed
branchUnsignedRangeReady
	cmpi.w #2, d6
	bne.s branchCheckClass
	cmpi.l #255, d3
	bhi.w branchMalformed
	bra.s branchInputReady
branchCheckClass
	cmpi.w #3, d6
	bne.s branchInputReady
	cmpi.l #7, d3
	bhi.w branchMalformed
	bra.s branchInputReady

branchProjectTarget
	bsr.w tkpkgProjectCompactSemanticInputV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s branchProjectionFailed

branchInputReady
	move.b #4, (a4)+
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	lsr.l #8, d3
	move.b d3, (a4)+
	addq.w #1, d6
	cmpi.w #4, d6
	blo.w branchInputLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.w branchMalformed
	move.l a4, d1
	lea buffers.TokenScratchBuffer, a0
	sub.l a0, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s branchReturn

branchProjectionFailed
	cmpi.l #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	beq.s branchReturn
	bsr.w tkpkgRenderSelectedSemanticRejectV1
	tst.l d0
	bne.s branchMalformed
	moveq #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	bra.s branchReturn

branchMalformed
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
branchReturn
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildCompactSemanticBranchCandidateV2

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
	beq.w sequenceNoTargetPrefix
	move.l (sp)+, d0
	lea 7(a1), a1
	subi.w #7, d0
	beq.w sequenceInputCursorMalformed
	moveq #1, d2
	tst.b d5
	bne.s sequenceProjectInput
	bsr.w tkpkgProjectDirectSemanticTargetV2
	bra.s sequenceInputProjected
sequenceNoTargetPrefix
	move.l (sp)+, d0
sequenceProjectInput
	cmpi.b #2, d5
	bne.s sequenceProjectOrdinaryInput
	btst #0, d2
	beq.s sequenceProjectOrdinaryInput
	; Rust's forced-fixup path resolves an Expr::Member relocation through its
	; base expression before scalar member evaluation.  The native boundary
	; retains source spans, so retry only this package-declared `target:` input
	; through the same neutral member-base projection when whole-span evaluation
	; cannot resolve it.
	bsr.w tkpkgProjectForcedFixupTargetV2
	bra.s sequenceInputProjected
sequenceProjectOrdinaryInput
	bsr.w tkpkgProjectCompactSemanticInputV2
sequenceInputProjected
	movea.l (sp)+, a2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w sequenceProjectionFailed
	cmpi.b #2, d5
	bne.s sequenceTargetReferenceReady
	; Fixup input flags mirror Rust PortableFixupInput: bit zero is target
	; reference and bit one is unresolved.  The package-owned unresolved
	; policy must see bit one during pass one instead of projecting the
	; placeholder as though it were a resolved address.
	tst.b state.EncodeSelectedMselUnstable
	beq.s sequenceTargetReferenceReady
	ori.b #2, d2
sequenceTargetReferenceReady
	cmpi.b #2, d5
	bne.s sequenceInputFlagsReady
	tst.b state.EncodeSelectedSemanticTargetReference
	beq.s sequenceInputFlagsReady
	ori.b #1, d2
sequenceInputFlagsReady
	tst.b d5
	beq.s sequenceInputNext
	moveq #-1, d1
	cmpi.b #2, d5
	bne.s sequenceTargetIndexReady
	btst #0, d2
	beq.s sequenceTargetIndexReady
	jsr context.getLastResolvedSymbolIndexV1
	move.w d0, d1
sequenceTargetIndexReady
	moveq #5, d0
	cmpi.b #2, d5
	bne.s sequenceRequireInputRecord
	addq.w #3, d0
sequenceRequireInputRecord
	; The capacity helper returns its status in D1.  Preserve the opaque
	; resolved-symbol index across that bounded-write check so the emitted
	; fixup record retains the same target identity as Rust.
	move.w d1, -(sp)
	bsr.w tkpkgSequenceRequireCandidateBytesV2
	move.w d1, d0
	move.w (sp)+, d1
	tst.l d0
	bne.w sequenceMalformed
	moveq #4, d0
	cmpi.b #2, d5
	bne.s sequenceWriteInputLength
	addq.w #3, d0
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
	cmpi.b #2, d5
	bne.s sequenceInputNext
	move.w d1, d0
	lsr.w #8, d0
	move.b d0, (a4)+
	move.b d1, (a4)+

sequenceInputNext
	subq.w #1, d4
	bne.w sequenceInputLoop
	tst.b d5
	beq.s sequenceStepNext
	addq.w #1, d6
	cmpi.w #255, d6
	bhi.w sequenceMalformed
	bra.s sequenceStepNext

sequenceProjectionFailed
	cmpi.l #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	beq.w sequenceReturn
	bsr.w tkpkgRenderSelectedSemanticRejectV1
	tst.l d0
	bne.w sequenceMalformed
	moveq #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	bra.w sequenceReturn

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
; table owns the message template; this runtime substitutes Rust's standard
; selector captures (`mnemonic`, `form`, `register`, and projected `value`).
; Inputs: A1 = kind-6 plan body; A5/D2 = mnemonic; A6 = CMSE chunk end.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D1 = diagnostic length on rejection.
tkpkgBuildCompactSemanticRejectCandidateV2	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea -6(sp), sp
	clr.l (sp)
	clr.w 4(sp)
	movea.l a1, a2
	bsr.w tkpkgServiceReadU16LeV1
	bne.w rejectMalformed
	lea buffers.CompactSelectorPlanText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w rejectMalformed
	tst.w d0
	beq.w rejectMalformed
	move.w d0, d6
	moveq #1, d0
	bsr.w tkpkgServiceRequireBytesV1
	bne.w rejectMalformed
	moveq #0, d7
	move.b (a2)+, d7
	tst.w d7
	beq.w rejectMalformed

rejectInputLoop
	bsr.w tkpkgServiceReadU16LeV1
	bne.w rejectMalformed
	lea buffers.CompactSelectorInputText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.w rejectMalformed
	tst.w d0
	beq.w rejectMalformed
	move.w d0, d5
	move.l a2, -(sp)
	lea buffers.CompactSelectorInputText, a1
	lea OutOfRangePrefixText, a2
	moveq #12, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	movea.l (sp)+, a2
	tst.b d0
	beq.s rejectCheckDuplicate
	lea buffers.CompactSelectorInputText+12, a1
	move.w d5, d0
	subi.w #12, d0
	bsr.w tkpkgProjectOutOfRangeV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w rejectReturn
	move.l d3, state.EncodeSelectedSemanticValue
	move.b #1, state.EncodeSelectedSemanticValueValid
	bra.s rejectInputMatched

rejectCheckDuplicate
	move.l a2, -(sp)
	lea buffers.CompactSelectorInputText, a1
	lea DuplicateRegisterPrefixText, a2
	move.w d5, d0
	moveq #18, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	movea.l (sp)+, a2
	tst.b d0
	beq.s rejectOrdinaryInput
	lea buffers.CompactSelectorInputText+18, a1
	move.w d5, d0
	subi.w #18, d0
	bsr.w tkpkgProjectDuplicateRegisterV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w rejectReturn
	move.l a0, (sp)
	move.w d1, 4(sp)
	bra.s rejectInputMatched

rejectOrdinaryInput
	lea buffers.CompactSelectorInputText, a1
	move.w d5, d0
	bsr.w tkpkgProjectCompactSemanticInputV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w rejectReturn
rejectInputMatched
	subq.w #1, d7
	bne.w rejectInputLoop

	lea buffers.CompactSelectorPlanText, a1
	move.w d6, d0
	move.w state.EncodeSelectedMselMnemonicLen, d2
	moveq #0, d3
	movea.l (sp), a0
	moveq #0, d4
	move.w 4(sp), d4
	bsr.w tkpkgRenderRejectMessageCodeV1
	tst.l d0
	bne.s rejectMalformed
	moveq #TKPKG_SELECTED_STATUS_SEMANTIC_REJECT, d0
	bra.s rejectReturn

rejectMalformed
	moveq #0, d1
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
rejectReturn
	lea 6(sp), sp
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgBuildCompactSemanticRejectCandidateV2

; Resolve one package-owned diagnostic code and render its template into the
; bounded candidate scratch buffer.  DIAG is the Rust simple-schema sequence:
; u32 count followed by code/template length-prefixed UTF-8 string pairs.
; Inputs: A1/D0 = diagnostic code; A5/D2 = selected full mnemonic;
;         D3.B = nonzero when `{mnemonic}` uses the full form;
;         A0/D4 = optional package-neutral `{register}` capture;
;         state carries the optional first projected `{value}` scalar.
; Outputs: D0 = 0 success, 1 malformed/missing; D1 = rendered byte length.
tkpkgRenderRejectMessageCodeV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea -12(sp), sp
	move.w d2, (sp)
	move.w d0, 2(sp)
	move.w d3, 4(sp)
	move.l a0, 6(sp)
	move.w d4, 10(sp)
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
	move.w 4(sp), d3
	movea.l 6(sp), a0
	moveq #0, d4
	move.w 10(sp), d4
	bsr.w tkpkgRenderRejectMessageTemplateV1
	bra.s rejectMessageReturn

rejectMessageFail
	moveq #0, d1
	moveq #1, d0
rejectMessageReturn
	lea 12(sp), sp
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgRenderRejectMessageCodeV1

	.pub
; Resolve and render the optional CMSE v7 diagnostic attached to the selected
; semantic plan. The diagnostic code and template remain package-owned.
; Outputs: D0 = 0 with A1/D1 diagnostic text, or 1 when absent/malformed.
tkpkgRenderSelectedSemanticRejectV1	.block
	moveq #0, d0
	move.w state.EncodeSelectedSemanticDiagnosticIndex, d0
	cmpi.w #$FFFF, d0
	beq.s selectedDiagnosticFail
	lea buffers.CompactSelectorPlanText, a0
	bsr.w resolveCompactSelectorStringV1
	bne.s selectedDiagnosticFail
	move.w state.EncodeSelectedMselMnemonicLen, d2
	movea.l state.EncodeSelectedMnemonicPtr, a5
	lea buffers.CompactSelectorPlanText, a1
	moveq #1, d3
	suba.l a0, a0
	moveq #0, d4
	bsr.w tkpkgRenderRejectMessageCodeV1
	tst.l d0
	bne.s selectedDiagnosticReturn
	lea buffers.TokenScratchBuffer, a1
selectedDiagnosticReturn
	rts
selectedDiagnosticFail
	moveq #1, d0
	rts
	.bend  ; tkpkgRenderSelectedSemanticRejectV1

; Render Rust's standard selector diagnostic captures.  Literal template bytes
; remain package-owned; replacement mnemonics are ASCII-uppercased exactly as
; Rust's selector boundary does before supplying `mnemonic` and `form`.
; Inputs: A1/D0 = template; A5/D2 = selected full mnemonic;
;         D3.B = nonzero when `{mnemonic}` uses the full form;
;         A0/D4 = optional package-neutral `{register}` capture.
; Outputs: D0 = 0 success, 1 overflow; D1 = rendered length.
tkpkgRenderRejectMessageTemplateV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea -8(sp), sp
	move.w d3, (sp)
	move.l a0, 2(sp)
	move.w d4, 6(sp)
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
	tst.w (sp)
	beq.s rejectMessageMnemonicReady
	move.w d2, d4
rejectMessageMnemonicReady
	lea 10(a3), a3
	subi.w #10, d7
	bra.w rejectMessageCopyReplacement

rejectMessageCheckForm
	cmpi.w #6, d7
	bcs.s rejectMessageCheckRegister
	movea.l a3, a1
	move.w d7, d0
	lea RejectFormPlaceholderText, a2
	moveq #6, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s rejectMessageCheckRegister
	movea.l a5, a0
	move.w d2, d4
	lea 6(a3), a3
	subq.w #6, d7
	bra.w rejectMessageCopyReplacement

rejectMessageCheckRegister
	cmpi.w #10, d7
	bcs.s rejectMessageCheckValue
	movea.l a3, a1
	move.w d7, d0
	lea RejectRegisterPlaceholderText, a2
	moveq #10, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s rejectMessageCheckValue
	movea.l 2(sp), a0
	move.w 6(sp), d4
	beq.w rejectMessageTemplateFail
	lea 10(a3), a3
	subi.w #10, d7
	bra.w rejectMessageCopyReplacement

rejectMessageCheckValue
	cmpi.w #7, d7
	bcs.w rejectMessageCopyLiteral
	movea.l a3, a1
	move.w d7, d0
	lea RejectValuePlaceholderText, a2
	moveq #7, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.w rejectMessageCopyLiteral
	tst.b state.EncodeSelectedSemanticValueValid
	beq.w rejectMessageTemplateFail
	move.l state.EncodeSelectedSemanticValue, d0
	bsr.w tkpkgFormatSignedSemanticValueV1
	lea 7(a3), a3
	subi.w #7, d7

rejectMessageCopyReplacement
	move.w d6, d0
	add.w d4, d0
	bcs.w rejectMessageTemplateFail
	cmpi.w #buffers.TOKEN_SCRATCH_CAPACITY, d0
	bhi.w rejectMessageTemplateFail
	tst.w d4
	beq.w rejectMessageTemplateLoop
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
	bhs.w rejectMessageTemplateFail
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
	lea 8(sp), sp
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgRenderRejectMessageTemplateV1

; Format one native signed-32 semantic scalar exactly as Rust's `{value}`
; diagnostic capture.  Decimal powers avoid target-specific division behavior.
; Inputs: D0.L = value. Outputs: A0/D4 = bounded decimal text span.
tkpkgFormatSignedSemanticValueV1	.block
	movem.l d1-d3/d5-d7/a1-a2, -(sp)
	lea buffers.CompactStringScratchBuffer, a0
	movea.l a0, a2
	moveq #0, d4
	move.l d0, d5
	bpl.s semanticValueUnsigned
	cmpi.l #$80000000, d5
	bne.s semanticValueNegative
	lea RejectValueMinI32Text, a1
	moveq #10, d6
semanticValueMinCopy
	move.b (a1)+, (a2)+
	dbf d6, semanticValueMinCopy
	moveq #11, d4
	bra.s semanticValueDone

semanticValueNegative
	move.b #'-', (a2)+
	addq.w #1, d4
	neg.l d5

semanticValueUnsigned
	lea RejectValueDecimalPowers, a1
	moveq #9, d6
	moveq #0, d3
semanticValuePowerLoop
	move.l (a1)+, d1
	moveq #0, d2
semanticValueDigitLoop
	cmp.l d1, d5
	bcs.s semanticValueDigitReady
	sub.l d1, d5
	addq.b #1, d2
	bra.s semanticValueDigitLoop
semanticValueDigitReady
	tst.b d3
	bne.s semanticValueEmitDigit
	tst.b d2
	bne.s semanticValueStartDigits
	tst.w d6
	bne.s semanticValueNextPower
semanticValueStartDigits
	moveq #1, d3
semanticValueEmitDigit
	addi.b #'0', d2
	move.b d2, (a2)+
	addq.w #1, d4
semanticValueNextPower
	dbf d6, semanticValuePowerLoop

semanticValueDone
	movem.l (sp)+, d1-d3/d5-d7/a1-a2
	rts
	.bend  ; tkpkgFormatSignedSemanticValueV1

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
	clr.b state.EncodeSelectedSemanticTargetReference
	movea.l a1, a5
	move.w d0, d7
	movea.l a5, a1
	move.w d7, d0
	lea ExprPathPrefixText, a2
	moveq #4, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckCallArgIndirectRegister
	lea 4(a5), a1
	move.w d7, d0
	subq.w #4, d0
	bsr.w tkpkgProjectExprPathV1
	bra.w semanticProjectReturn

semanticCheckCallArgIndirectRegister
	movea.l a5, a1
	move.w d7, d0
	lea CallArgIndirectTupleRegisterPrefixText, a2
	moveq #32, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckCallArgIndirectRegisterOnly
	lea 32(a5), a1
	move.w d7, d0
	subi.w #32, d0
	bsr.w tkpkgProjectCallArgIndirectTupleRegisterV1
	bra.w semanticProjectReturn

semanticCheckCallArgIndirectRegisterOnly
	movea.l a5, a1
	move.w d7, d0
	lea CallArgIndirectRegisterPrefixText, a2
	moveq #26, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckCallArgRegister
	lea 26(a5), a1
	move.w d7, d0
	subi.w #26, d0
	moveq #1, d2
	bsr.w tkpkgProjectCallArgRegisterV1
	bra.w semanticProjectReturn

semanticCheckCallArgRegister
	movea.l a5, a1
	move.w d7, d0
	lea CallArgRegisterSequencePrefixText, a2
	moveq #26, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckCallArgRegisterOnly
	lea 26(a5), a1
	move.w d7, d0
	subi.w #26, d0
	bsr.w tkpkgProjectCallArgRegisterSequenceV1
	bra.w semanticProjectReturn

semanticCheckCallArgRegisterOnly
	movea.l a5, a1
	move.w d7, d0
	lea CallArgRegisterPrefixText, a2
	moveq #17, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckCallArgValue
	lea 17(a5), a1
	move.w d7, d0
	subi.w #17, d0
	moveq #0, d2
	bsr.w tkpkgProjectCallArgRegisterV1
	bra.w semanticProjectReturn

semanticCheckCallArgValue
	movea.l a5, a1
	move.w d7, d0
	lea CallArgValuePrefixText, a2
	moveq #14, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckCallArgMember
	lea 14(a5), a1
	move.w d7, d0
	subi.w #14, d0
	bsr.w tkpkgProjectCallArgValueV1
	bra.w semanticProjectReturn

semanticCheckCallArgMember
	movea.l a5, a1
	move.w d7, d0
	lea CallArgMemberPrefixText, a2
	moveq #15, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckMemberShape
	lea 15(a5), a1
	move.w d7, d0
	subi.w #15, d0
	bsr.w tkpkgProjectCallArgMemberV1
	bra.w semanticProjectReturn

semanticCheckMemberShape
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
	beq.s semanticCheckRegisterOrNamedRange
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

semanticCheckRegisterOrNamedRange
	movea.l a5, a1
	move.w d7, d0
	lea RegisterOrNamedRangePrefixText, a2
	moveq #23, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckNamedRegister
	lea 23(a5), a1
	move.w d7, d0
	subi.w #23, d0
	bsr.w tkpkgProjectRegisterOrNamedRangeV1
	bra.w semanticProjectReturn

semanticCheckNamedRegister
	movea.l a5, a1
	move.w d7, d0
	lea NamedRegisterPrefixText, a2
	moveq #14, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckLiteral
	lea 14(a5), a1
	move.w d7, d0
	subi.w #14, d0
	bsr.w tkpkgProjectNamedRegisterV1
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
	beq.s semanticCheckTupleIdentityScale
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
	bsr.w tkpkgMselUnwrapIdentityScaleV1
	movea.l a2, a1
	move.w d6, d1
	bsr.w tkpkgMselStripExpectedQualifierV2
	bne.w semanticProjectNoMatch
	move.l a6, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w semanticProjectNoMatch
	bra.w semanticProjectOk

semanticCheckTupleIdentityScale
	movea.l a5, a1
	move.w d7, d0
	lea IndirectTupleIdentityScalePrefixText, a2
	moveq #29, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckTupleNonidentityScale
	lea 29(a5), a1
	move.w d7, d0
	subi.w #29, d0
	bsr.w tkpkgParseTupleItemClassSpecV2
	bne.w semanticProjectMalformed
	tst.w d6
	bne.w semanticProjectMalformed
	cmpi.w #$FFFF, d5
	bne.w semanticProjectMalformed
	move.w d3, d0
	move.w d4, d1
	jsr operand.tkpkgMselLocateIndirectTupleItemV2
	bne.w semanticProjectNoMatch
	bsr.w tkpkgMselUnwrapIdentityScaleV1
	bne.w semanticProjectNoMatch
	moveq #1, d3
	bra.w semanticProjectOk

semanticCheckTupleNonidentityScale
	movea.l a5, a1
	move.w d7, d0
	lea IndirectTupleNonidentityScalePrefixText, a2
	moveq #32, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckTupleValue
	lea 32(a5), a1
	move.w d7, d0
	subi.w #32, d0
	bsr.w tkpkgProjectNonidentityScaleV1
	bra.w semanticProjectReturn

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
	move.l a0, -(sp)
	move.l d0, -(sp)
	jsr context.isSymbolTargetReferenceV1
	move.b d0, state.EncodeSelectedSemanticTargetReference
	move.l (sp)+, d0
	movea.l (sp)+, a0
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
	bra.w semanticRegisterSpec

semanticCheckRegister
	movea.l a5, a1
	move.w d7, d0
	lea RegisterSequencePrefixText, a2
	moveq #17, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckRegisterMask
	lea 17(a5), a1
	move.w d7, d0
	subi.w #17, d0
	bsr.w tkpkgProjectRegisterSequenceV1
	bra.w semanticProjectReturn

semanticCheckRegisterMask
	movea.l a5, a1
	move.w d7, d0
	lea RegisterMaskPrefixText, a2
	moveq #13, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckRegisterIndexXor
	lea 13(a5), a1
	move.w d7, d0
	subi.w #13, d0
	bsr.w tkpkgProjectRegisterMaskV1
	bra.w semanticProjectReturn

semanticCheckRegisterIndexXor
	movea.l a5, a1
	move.w d7, d0
	lea RegisterIndexXorPrefixText, a2
	moveq #18, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckRegisterSource
	lea 18(a5), a1
	move.w d7, d0
	subi.w #18, d0
	bsr.w tkpkgProjectRegisterIndexXorV1
	bra.w semanticProjectReturn

semanticCheckRegisterSource
	cmpi.w #10, d7
	bcs.w semanticCheckDistinctRegister
	cmpi.b #'r', (a5)
	bne.w semanticCheckDistinctRegister
	cmpi.b #'e', 1(a5)
	bne.w semanticCheckDistinctRegister
	cmpi.b #'g', 2(a5)
	bne.w semanticCheckDistinctRegister
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
	bsr.w tkpkgParseRegisterClassProjectionV1
	bne.w semanticProjectMalformed
	move.w d3, d6
	move.w d7, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w semanticProjectNoMatch
	move.l a6, d1
	beq.s semanticRegisterLookup
	cmpi.l #1, d1
	beq.s semanticRegisterStripParens
	cmpi.l #2, d1
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
	tst.w d2
	beq.s semanticRegisterProjected
	cmpi.w #1, d2
	beq.s semanticRegisterShift
	cmpi.w #2, d2
	beq.s semanticRegisterMask
	lsr.l d4, d3
	bra.s semanticRegisterMask
semanticRegisterShift
	lsr.l d4, d3
	bra.s semanticRegisterProjected
semanticRegisterMask
	and.l d5, d3
semanticRegisterProjected
	bra.s semanticProjectOk

semanticCheckDistinctRegister
	movea.l a5, a1
	move.w d7, d0
	lea BoundedRegisterPrefixText, a2
	moveq #11, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckDistinctRegisterOnly
	lea 11(a5), a1
	move.w d7, d0
	subi.w #11, d0
	bsr.w tkpkgProjectBoundedRegisterV1
	bra.s semanticProjectReturn

semanticCheckDistinctRegisterOnly
	movea.l a5, a1
	move.w d7, d0
	lea DistinctRegisterPrefixText, a2
	moveq #17, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s semanticCheckRequiredValue
	lea 17(a5), a1
	move.w d7, d0
	subi.w #17, d0
	bsr.w tkpkgProjectDistinctRegisterV1
	bra.s semanticProjectReturn

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

; Project Rust's neutral `bounded_regN.classC.minM.maxX[.outside]` source.
; Package data owns every index/class/range choice; native resolves the named
; register through RENC and applies Rust's inclusive/inverted match rule.
; Inputs: A1/D0.W = source text after `bounded_reg`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = register index on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-motorola68000-reference-matrix-v1.toml
; @opforge-role: facade
tkpkgProjectBoundedRegisterV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -12(sp), sp
	movea.l a1, a3
	move.w d0, d7
	clr.w 8(sp)

	lea SemanticClassMarkerText, a2
	moveq #6, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w boundedRegisterMalformed
	tst.w d0
	beq.w boundedRegisterMalformed
	move.w d0, d6
	movea.l a3, a1
	bsr.w tkpkgParseU16DecimalV2
	bne.w boundedRegisterMalformed
	move.w d3, (sp)
	lea 6(a3, d6.w), a4
	move.w d7, d5
	sub.w d6, d5
	subq.w #6, d5
	beq.w boundedRegisterMalformed

	movea.l a4, a1
	move.w d5, d0
	lea SemanticMinMarkerText, a2
	moveq #4, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w boundedRegisterMalformed
	tst.w d0
	beq.w boundedRegisterMalformed
	move.w d0, d6
	move.w d5, 10(sp)
	movea.l a4, a1
	bsr.w tkpkgParseU16DecimalV2
	bne.w boundedRegisterMalformed
	move.w d3, 2(sp)
	move.w 10(sp), d5
	lea 4(a4, d6.w), a4
	sub.w d6, d5
	subq.w #4, d5
	beq.w boundedRegisterMalformed

	movea.l a4, a1
	move.w d5, d0
	lea SemanticMaxMarkerText, a2
	moveq #4, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w boundedRegisterMalformed
	tst.w d0
	beq.w boundedRegisterMalformed
	move.w d0, d6
	move.w d5, 10(sp)
	movea.l a4, a1
	bsr.w tkpkgParseU16DecimalV2
	bne.w boundedRegisterMalformed
	move.w d3, 4(sp)
	move.w 10(sp), d5
	lea 4(a4, d6.w), a4
	sub.w d6, d5
	subq.w #4, d5
	beq.w boundedRegisterMalformed

	movea.l a4, a1
	move.w d5, d0
	lea SemanticOutsideMarkerText, a2
	moveq #8, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.s boundedRegisterPlainMax
	tst.w d0
	beq.w boundedRegisterMalformed
	move.w d0, d6
	addq.w #8, d0
	cmp.w d5, d0
	bne.w boundedRegisterMalformed
	move.w #1, 8(sp)
	bra.s boundedRegisterParseMax
boundedRegisterPlainMax
	move.w d5, d6
boundedRegisterParseMax
	movea.l a4, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w boundedRegisterMalformed
	move.w d3, 6(sp)
	move.w 4(sp), d4
	cmp.w 6(sp), d4
	bhi.w boundedRegisterMalformed

	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s boundedRegisterNoMatch
	move.w 2(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.s boundedRegisterNoMatch
	moveq #0, d4
	move.w 4(sp), d4
	cmp.l d4, d3
	bcs.s boundedRegisterOutsideRange
	moveq #0, d4
	move.w 6(sp), d4
	cmp.l d4, d3
	bhi.s boundedRegisterOutsideRange
	tst.w 8(sp)
	bne.s boundedRegisterNoMatch
	bra.s boundedRegisterOk
boundedRegisterOutsideRange
	tst.w 8(sp)
	beq.s boundedRegisterNoMatch
boundedRegisterOk
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s boundedRegisterReturn
boundedRegisterNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s boundedRegisterReturn
boundedRegisterMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
boundedRegisterReturn
	lea 12(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectBoundedRegisterV1

; Project Rust's neutral
; `register_index_xorN.classC.withM.classD[.shrS][.andK]` source.  Both
; registers must resolve through package RENC data with the requested classes;
; native XORs their opaque indices and applies the package-selected projection.
; Inputs: A1/D0.W = source text after `register_index_xor`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = projected XOR on success.
tkpkgProjectRegisterIndexXorV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -20(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6

registerIndexXorLeftClassScan
	move.w d7, d5
	sub.w d6, d5
	cmpi.w #6, d5
	bcs.w registerIndexXorMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s registerIndexXorLeftClassNext
	cmpi.b #'c', 1(a3, d6.w)
	bne.s registerIndexXorLeftClassNext
	cmpi.b #'l', 2(a3, d6.w)
	bne.s registerIndexXorLeftClassNext
	cmpi.b #'a', 3(a3, d6.w)
	bne.s registerIndexXorLeftClassNext
	cmpi.b #'s', 4(a3, d6.w)
	bne.s registerIndexXorLeftClassNext
	cmpi.b #'s', 5(a3, d6.w)
	beq.s registerIndexXorLeftClassReady
registerIndexXorLeftClassNext
	addq.w #1, d6
	bra.s registerIndexXorLeftClassScan

registerIndexXorLeftClassReady
	tst.w d6
	beq.w registerIndexXorMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerIndexXorMalformed
	move.w d3, (sp)
	lea 6(a3, d6.w), a2
	move.w d6, d4
	addq.w #6, d4
	move.w d4, d6

registerIndexXorWithScan
	move.w d7, d5
	sub.w d6, d5
	cmpi.w #5, d5
	bcs.w registerIndexXorMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s registerIndexXorWithNext
	cmpi.b #'w', 1(a3, d6.w)
	bne.s registerIndexXorWithNext
	cmpi.b #'i', 2(a3, d6.w)
	bne.s registerIndexXorWithNext
	cmpi.b #'t', 3(a3, d6.w)
	bne.s registerIndexXorWithNext
	cmpi.b #'h', 4(a3, d6.w)
	beq.s registerIndexXorWithReady
registerIndexXorWithNext
	addq.w #1, d6
	bra.s registerIndexXorWithScan

registerIndexXorWithReady
	move.w d6, d0
	sub.w d4, d0
	beq.w registerIndexXorMalformed
	movea.l a2, a1
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerIndexXorMalformed
	move.w d3, 2(sp)
	lea 5(a3, d6.w), a2
	move.w d7, d5
	sub.w d6, d5
	subq.w #5, d5
	moveq #0, d6

registerIndexXorRightClassScan
	move.w d5, d4
	sub.w d6, d4
	cmpi.w #6, d4
	bcs.w registerIndexXorMalformed
	cmpi.b #'.', 0(a2, d6.w)
	bne.s registerIndexXorRightClassNext
	cmpi.b #'c', 1(a2, d6.w)
	bne.s registerIndexXorRightClassNext
	cmpi.b #'l', 2(a2, d6.w)
	bne.s registerIndexXorRightClassNext
	cmpi.b #'a', 3(a2, d6.w)
	bne.s registerIndexXorRightClassNext
	cmpi.b #'s', 4(a2, d6.w)
	bne.s registerIndexXorRightClassNext
	cmpi.b #'s', 5(a2, d6.w)
	beq.s registerIndexXorRightClassReady
registerIndexXorRightClassNext
	addq.w #1, d6
	bra.s registerIndexXorRightClassScan

registerIndexXorRightClassReady
	tst.w d6
	beq.w registerIndexXorMalformed
	move.w d5, 14(sp)
	movea.l a2, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerIndexXorMalformed
	move.w d3, 4(sp)
	lea 6(a2, d6.w), a1
	move.w 14(sp), d0
	sub.w d6, d0
	subq.w #6, d0
	beq.w registerIndexXorMalformed
	bsr.w tkpkgParseRegisterClassProjectionV1
	bne.w registerIndexXorMalformed
	move.w d3, 6(sp)
	move.w d2, 8(sp)
	move.w d4, 10(sp)
	move.w d5, 12(sp)

	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w registerIndexXorNoMatch
	move.w 2(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w registerIndexXorNoMatch
	move.l d3, 16(sp)

	move.w 4(sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w registerIndexXorNoMatch
	move.w 6(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w registerIndexXorNoMatch
	move.l 16(sp), d2
	eor.l d2, d3
	move.w 8(sp), d2
	beq.s registerIndexXorOk
	move.w 10(sp), d4
	move.w 12(sp), d5
	cmpi.w #1, d2
	beq.s registerIndexXorShift
	cmpi.w #2, d2
	beq.s registerIndexXorMask
	lsr.l d4, d3
	bra.s registerIndexXorMask
registerIndexXorShift
	lsr.l d4, d3
	bra.s registerIndexXorOk
registerIndexXorMask
	and.l d5, d3

registerIndexXorOk
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s registerIndexXorReturn
registerIndexXorNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s registerIndexXorReturn
registerIndexXorMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
registerIndexXorReturn
	lea 20(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectRegisterIndexXorV1

; Match Rust's neutral `out_of_rangeN.minX.maxY` rejection projection.  The
; package owns the operand index and signed bounds; native only evaluates the
; selected operand and reports a match when its scalar lies outside them.
; Inputs: A1/D0.W = `N.minX.maxY`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = rejected scalar on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-later-integer-group-b-v1.toml
; @opforge-role: facade
tkpkgProjectOutOfRangeV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -12(sp), sp
	movea.l a1, a3
	move.w d0, d7
	cmpi.w #11, d7
	bcs.w outOfRangeMalformed
	move.w d7, 2(sp)
	moveq #0, d6

outOfRangeMinScan
	move.w d7, d5
	subq.w #4, d5
	cmp.w d5, d6
	bhi.w outOfRangeMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s outOfRangeMinNext
	cmpi.b #'m', 1(a3, d6.w)
	bne.s outOfRangeMinNext
	cmpi.b #'i', 2(a3, d6.w)
	bne.s outOfRangeMinNext
	cmpi.b #'n', 3(a3, d6.w)
	beq.s outOfRangeMinReady
outOfRangeMinNext
	addq.w #1, d6
	bra.s outOfRangeMinScan

outOfRangeMinReady
	tst.w d6
	beq.w outOfRangeMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w outOfRangeMalformed
	move.w d3, (sp)
	addq.w #4, d6
	move.w d6, d4

outOfRangeMaxScan
	move.w d7, d5
	subq.w #4, d5
	cmp.w d5, d4
	bhi.w outOfRangeMalformed
	cmpi.b #'.', 0(a3, d4.w)
	bne.s outOfRangeMaxNext
	cmpi.b #'m', 1(a3, d4.w)
	bne.s outOfRangeMaxNext
	cmpi.b #'a', 2(a3, d4.w)
	bne.s outOfRangeMaxNext
	cmpi.b #'x', 3(a3, d4.w)
	beq.s outOfRangeMaxReady
outOfRangeMaxNext
	addq.w #1, d4
	bra.s outOfRangeMaxScan

outOfRangeMaxReady
	move.w d4, 10(sp)
	move.w d4, d5
	sub.w d6, d5
	beq.w outOfRangeMalformed
	movea.l a3, a1
	adda.w d6, a1
	move.w d5, d7
	bsr.w tkpkgParseSignedDecimalV2
	bne.w outOfRangeMalformed
	move.l d3, 4(sp)
	move.w 10(sp), d4
	addq.w #4, d4
	move.w 2(sp), d5
	sub.w d4, d5
	beq.w outOfRangeMalformed
	movea.l a3, a1
	adda.w d4, a1
	move.w d5, d7
	bsr.w tkpkgParseSignedDecimalV2
	bne.w outOfRangeMalformed
	move.l d3, 8(sp)
	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w outOfRangeNoMatch
	moveq #0, d1
	cmpi.b #'#', (a0)
	bne.s outOfRangeEvaluate
	moveq #1, d1
outOfRangeEvaluate
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s outOfRangeReturn
	cmp.l 4(sp), d3
	blt.s outOfRangeMatched
	cmp.l 8(sp), d3
	bgt.s outOfRangeMatched
outOfRangeNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s outOfRangeReturn
outOfRangeMatched
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s outOfRangeReturn
outOfRangeMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
outOfRangeReturn
	lea 12(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectOutOfRangeV1

; Project Rust's neutral `call_arg_register_sequenceN.argA.argB.classC.alignK`
; source, including optional register-index projection and explicit alignment
; or sequence violation selection. Source spans substitute for Rust Expr nodes;
; all class, alignment, relation, projection, and diagnostic choices are package
; data.
; Inputs: A1/D0.W = text after `call_arg_register_sequence`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = projected first index.
tkpkgProjectCallArgRegisterSequenceV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -40(sp), sp
	move.l a1, (sp)
	move.w d0, 4(sp)
	clr.w 6(sp)
	clr.w 8(sp)
	clr.w 10(sp)
	clr.w 12(sp)
	clr.w 14(sp)
	clr.w 16(sp)
	clr.l 18(sp)
	clr.w 22(sp)
	clr.w 24(sp)
	clr.w 26(sp)
	clr.w 28(sp)
	clr.l 30(sp)
	clr.w 34(sp)

	lea SemanticArgMarkerText, a2
	moveq #4, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w callArgSequenceMalformed
	move.w d0, d6
	beq.w callArgSequenceMalformed
	movea.l (sp), a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgSequenceMalformed
	move.w d3, 6(sp)
	movea.l (sp), a3
	lea 4(a3, d6.w), a3
	move.w 4(sp), d7
	sub.w d6, d7
	subq.w #4, d7

	movea.l a3, a1
	move.w d7, d0
	lea SemanticArgMarkerText, a2
	moveq #4, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w callArgSequenceMalformed
	move.w d0, d6
	beq.w callArgSequenceMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgSequenceMalformed
	move.w d3, 8(sp)
	lea 4(a3, d6.w), a3
	sub.w d6, d7
	subq.w #4, d7

	movea.l a3, a1
	move.w d7, d0
	lea SemanticClassMarkerText, a2
	moveq #6, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w callArgSequenceMalformed
	move.w d0, d6
	beq.w callArgSequenceMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgSequenceMalformed
	move.w d3, 10(sp)
	lea 6(a3, d6.w), a3
	sub.w d6, d7
	subq.w #6, d7

	movea.l a3, a1
	move.w d7, d0
	lea SemanticAlignMarkerText, a2
	moveq #6, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w callArgSequenceMalformed
	move.w d0, d6
	beq.w callArgSequenceMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseRegisterClassProjectionV1
	bne.w callArgSequenceMalformed
	move.w d3, 12(sp)
	move.w d2, 14(sp)
	move.w d4, 16(sp)
	move.l d5, 18(sp)
	lea 6(a3, d6.w), a1
	move.w d7, d0
	sub.w d6, d0
	subq.w #6, d0
	bsr.w tkpkgParseSequenceTailV1
	bne.w callArgSequenceMalformed
	move.w d3, 22(sp)
	move.w d4, 24(sp)

	move.w 6(sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w callArgSequenceNoMatch
	move.l a0, 30(sp)
	move.w d0, 34(sp)
	move.w 8(sp), d1
	bsr.w tkpkgSelectCallArgumentV1
	bne.w callArgSequenceNoMatch
	move.w 12(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w callArgSequenceNoMatch
	move.w d3, 26(sp)

	movea.l 30(sp), a0
	moveq #0, d0
	move.w 34(sp), d0
	move.w 10(sp), d1
	bsr.w tkpkgSelectCallArgumentV1
	bne.w callArgSequenceNoMatch
	move.w 12(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w callArgSequenceNoMatch
	move.w d3, 28(sp)

	moveq #0, d0
	move.w 26(sp), d0
	moveq #0, d1
	move.w 28(sp), d1
	moveq #2, d2
	moveq #0, d3
	move.w 22(sp), d3
	moveq #0, d4
	move.w 24(sp), d4
	bsr.w tkpkgRegisterSequenceRelationMatchesV1
	tst.l d0
	beq.s callArgSequenceNoMatch
	moveq #0, d3
	move.w 26(sp), d3
	move.w 14(sp), d2
	move.w 16(sp), d4
	move.l 18(sp), d5
	bsr.w tkpkgApplyRegisterIndexProjectionV1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s callArgSequenceReturn

callArgSequenceMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s callArgSequenceReturn
callArgSequenceNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
callArgSequenceReturn
	lea 40(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectCallArgRegisterSequenceV1

; Project Rust's neutral call-argument register sources used by frozen package
; rows. The package selects operand, argument, and required register class;
; source spelling is resolved exclusively through package RENC records.
; Inputs: A1/D0.W = `N.argM.classC`; D2.B = unwrap-indirect flag.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = register index on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-later-integer-group-a-v1.toml
; @opforge-role: facade
tkpkgProjectCallArgRegisterV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -20(sp), sp
	move.w d2, 6(sp)
	clr.w 8(sp)
	clr.w 10(sp)
	clr.l 12(sp)
	clr.w 16(sp)
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6

callArgSpecArgScan
	move.w d7, d5
	subq.w #4, d5
	cmp.w d5, d6
	bhi.w callArgSpecMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s callArgSpecArgNext
	cmpi.b #'a', 1(a3, d6.w)
	bne.s callArgSpecArgNext
	cmpi.b #'r', 2(a3, d6.w)
	bne.s callArgSpecArgNext
	cmpi.b #'g', 3(a3, d6.w)
	beq.s callArgSpecArgReady
callArgSpecArgNext
	addq.w #1, d6
	bra.s callArgSpecArgScan

callArgSpecArgReady
	tst.w d6
	beq.w callArgSpecMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgSpecMalformed
	move.w d3, (sp)
	lea 4(a3, d6.w), a4
	move.w d7, d5
	sub.w d6, d5
	subq.w #4, d5
	moveq #0, d6

callArgSpecClassScan
	move.w d5, d4
	subq.w #6, d4
	cmp.w d4, d6
	bhi.w callArgSpecMalformed
	cmpi.b #'.', 0(a4, d6.w)
	bne.s callArgSpecClassNext
	cmpi.b #'c', 1(a4, d6.w)
	bne.s callArgSpecClassNext
	cmpi.b #'l', 2(a4, d6.w)
	bne.s callArgSpecClassNext
	cmpi.b #'a', 3(a4, d6.w)
	bne.s callArgSpecClassNext
	cmpi.b #'s', 4(a4, d6.w)
	bne.s callArgSpecClassNext
	cmpi.b #'s', 5(a4, d6.w)
	beq.s callArgSpecClassReady
callArgSpecClassNext
	addq.w #1, d6
	bra.s callArgSpecClassScan

callArgSpecClassReady
	tst.w d6
	beq.w callArgSpecMalformed
	move.w d5, 16(sp)
	movea.l a4, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgSpecMalformed
	move.w d3, 2(sp)
	lea 6(a4, d6.w), a1
	move.w 16(sp), d0
	sub.w d6, d0
	subq.w #6, d0
	beq.w callArgSpecMalformed
	bsr.w tkpkgParseRegisterClassProjectionV1
	bne.w callArgSpecMalformed
	move.w d3, 4(sp)
	move.w d2, 8(sp)
	move.w d4, 10(sp)
	move.l d5, 12(sp)

	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s callArgSpecNoMatch
	move.w 2(sp), d1
	bsr.w tkpkgSelectCallArgumentV1
	bne.s callArgSpecNoMatch
	tst.w 6(sp)
	beq.s callArgSpecLookup
	jsr operand.tkpkgMselStripOuterParensV1
	bne.s callArgSpecNoMatch
callArgSpecLookup
	move.w 4(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.s callArgSpecNoMatch
	move.w 8(sp), d2
	move.w 10(sp), d4
	move.l 12(sp), d5
	bsr.w tkpkgApplyRegisterIndexProjectionV1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s callArgSpecReturn

callArgSpecMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s callArgSpecReturn
callArgSpecNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
callArgSpecReturn
	lea 20(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectCallArgRegisterV1

; Split one neutral top-level register sequence represented as subtraction or
; a range. Inputs: A0/D0 = source span. Outputs: A0/D0 left, A1/D1 right,
; D2=0/1. This is the bounded source-span equivalent of Rust's Binary::Subtract
; or Range endpoint match.
tkpkgSplitRegisterSequenceSpanV1	.block
	movem.l d3-d7/a2-a5, -(sp)
	movea.l a0, a2
	movea.l a0, a3
	adda.l d0, a3
	moveq #0, d6
	moveq #0, d7
registerSequenceSplitScan
	cmpa.l a3, a2
	bhs.w registerSequenceSplitFail
	moveq #0, d5
	move.b (a2), d5
	cmpi.b #'(', d5
	beq.w registerSequenceSplitOpenParen
	cmpi.b #')', d5
	beq.w registerSequenceSplitCloseParen
	cmpi.b #'[', d5
	beq.w registerSequenceSplitOpenBracket
	cmpi.b #']', d5
	beq.w registerSequenceSplitCloseBracket
	tst.w d6
	bne.w registerSequenceSplitNext
	tst.w d7
	bne.w registerSequenceSplitNext
	cmpi.b #'-', d5
	beq.w registerSequenceSplitDelimiterOne
	cmpi.b #'.', d5
	bne.w registerSequenceSplitNext
	lea 1(a2), a4
	cmpa.l a3, a4
	bhs.w registerSequenceSplitFail
	cmpi.b #'.', (a4)
	bne.w registerSequenceSplitNext
	moveq #2, d4
	bra.w registerSequenceSplitDelimiter
registerSequenceSplitDelimiterOne
	moveq #1, d4
registerSequenceSplitDelimiter
	movea.l a2, a4
registerSequenceSplitTrimLeft
	cmpa.l a0, a4
	bls.w registerSequenceSplitFail
	cmpi.b #' ', -1(a4)
	beq.s registerSequenceSplitTrimLeftOne
	cmpi.b #9, -1(a4)
	bne.w registerSequenceSplitRightStart
registerSequenceSplitTrimLeftOne
	subq.l #1, a4
	bra.w registerSequenceSplitTrimLeft
registerSequenceSplitRightStart
	movea.l a2, a5
	adda.w d4, a5
registerSequenceSplitTrimRightStart
	cmpa.l a3, a5
	bhs.w registerSequenceSplitFail
	cmpi.b #' ', (a5)
	beq.s registerSequenceSplitTrimRightStartOne
	cmpi.b #9, (a5)
	bne.w registerSequenceSplitTrimRightEnd
registerSequenceSplitTrimRightStartOne
	addq.l #1, a5
	bra.w registerSequenceSplitTrimRightStart
registerSequenceSplitTrimRightEnd
	cmpa.l a5, a3
	bls.w registerSequenceSplitFail
	cmpi.b #' ', -1(a3)
	beq.s registerSequenceSplitTrimRightEndOne
	cmpi.b #9, -1(a3)
	bne.w registerSequenceSplitReady
registerSequenceSplitTrimRightEndOne
	subq.l #1, a3
	bra.w registerSequenceSplitTrimRightEnd
registerSequenceSplitReady
	move.l a4, d0
	sub.l a0, d0
	movea.l a5, a1
	move.l a3, d1
	sub.l a1, d1
	moveq #0, d2
	bra.w registerSequenceSplitReturn
registerSequenceSplitOpenParen
	addq.w #1, d6
	bra.w registerSequenceSplitNext
registerSequenceSplitCloseParen
	tst.w d6
	beq.w registerSequenceSplitFail
	subq.w #1, d6
	bra.w registerSequenceSplitNext
registerSequenceSplitOpenBracket
	addq.w #1, d7
	bra.w registerSequenceSplitNext
registerSequenceSplitCloseBracket
	tst.w d7
	beq.w registerSequenceSplitFail
	subq.w #1, d7
registerSequenceSplitNext
	addq.l #1, a2
	bra.w registerSequenceSplitScan
registerSequenceSplitFail
	moveq #0, d0
	moveq #0, d1
	moveq #1, d2
	suba.l a0, a0
	suba.l a1, a1
registerSequenceSplitReturn
	movem.l (sp)+, d3-d7/a2-a5
	tst.l d2
	rts
	.bend  ; tkpkgSplitRegisterSequenceSpanV1

; Project Rust's neutral `register_sequenceN.classC.countK.alignA` source,
; including index projections and explicit relation-violation selectors.
; Inputs: A1/D0.W = text after `register_sequence`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = projected start index.
tkpkgProjectRegisterSequenceV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -36(sp), sp
	move.l a1, (sp)
	move.w d0, 4(sp)
	clr.w 6(sp)
	clr.w 8(sp)
	clr.w 10(sp)
	clr.w 12(sp)
	clr.l 14(sp)
	clr.w 18(sp)
	clr.w 20(sp)
	clr.w 22(sp)
	clr.w 24(sp)
	clr.w 26(sp)

	lea SemanticClassMarkerText, a2
	moveq #6, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w registerSequenceMalformed
	move.w d0, d6
	beq.w registerSequenceMalformed
	movea.l (sp), a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerSequenceMalformed
	move.w d3, 6(sp)
	movea.l (sp), a3
	lea 6(a3, d6.w), a3
	move.w 4(sp), d7
	sub.w d6, d7
	subq.w #6, d7

	movea.l a3, a1
	move.w d7, d0
	lea SemanticCountMarkerText, a2
	moveq #6, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w registerSequenceMalformed
	move.w d0, d6
	beq.w registerSequenceMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseRegisterClassProjectionV1
	bne.w registerSequenceMalformed
	move.w d3, 8(sp)
	move.w d2, 10(sp)
	move.w d4, 12(sp)
	move.l d5, 14(sp)
	lea 6(a3, d6.w), a3
	sub.w d6, d7
	subq.w #6, d7

	movea.l a3, a1
	move.w d7, d0
	lea SemanticAlignMarkerText, a2
	moveq #6, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.w registerSequenceMalformed
	move.w d0, d6
	beq.w registerSequenceMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerSequenceMalformed
	tst.w d3
	beq.w registerSequenceMalformed
	move.w d3, 18(sp)
	lea 6(a3, d6.w), a1
	move.w d7, d0
	sub.w d6, d0
	subq.w #6, d0
	bsr.w tkpkgParseSequenceTailV1
	bne.w registerSequenceMalformed
	move.w d3, 20(sp)
	move.w d4, 22(sp)

	move.w 6(sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w registerSequenceNoMatch
	bsr.w tkpkgSplitRegisterSequenceSpanV1
	bne.w registerSequenceNoMatch
	move.l a1, 28(sp)
	move.w d1, 32(sp)
	move.w 8(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w registerSequenceNoMatch
	move.w d3, 24(sp)
	movea.l 28(sp), a0
	moveq #0, d0
	move.w 32(sp), d0
	move.w 8(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w registerSequenceNoMatch
	move.w d3, 26(sp)

	moveq #0, d0
	move.w 24(sp), d0
	moveq #0, d1
	move.w 26(sp), d1
	moveq #0, d2
	move.w 18(sp), d2
	moveq #0, d3
	move.w 20(sp), d3
	moveq #0, d4
	move.w 22(sp), d4
	bsr.w tkpkgRegisterSequenceRelationMatchesV1
	tst.l d0
	beq.s registerSequenceNoMatch
	moveq #0, d3
	move.w 24(sp), d3
	move.w 10(sp), d2
	move.w 12(sp), d4
	move.l 14(sp), d5
	bsr.w tkpkgApplyRegisterIndexProjectionV1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s registerSequenceReturn

registerSequenceMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s registerSequenceReturn
registerSequenceNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
registerSequenceReturn
	lea 36(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectRegisterSequenceV1

; Project Rust's neutral call-argument scalar source. The package selects the
; operand and argument; the selected span is evaluated by the shared expression
; bridge exactly like an ordinary `exprN` semantic input.
; Inputs: A1/D0.W = `N.argM`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = evaluated scalar on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-later-integer-group-b-v1.toml
; @opforge-role: facade
tkpkgProjectCallArgValueV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -4(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6

callArgValueArgScan
	move.w d7, d5
	subq.w #4, d5
	cmp.w d5, d6
	bhi.s callArgValueMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s callArgValueArgNext
	cmpi.b #'a', 1(a3, d6.w)
	bne.s callArgValueArgNext
	cmpi.b #'r', 2(a3, d6.w)
	bne.s callArgValueArgNext
	cmpi.b #'g', 3(a3, d6.w)
	beq.s callArgValueArgReady
callArgValueArgNext
	addq.w #1, d6
	bra.s callArgValueArgScan

callArgValueArgReady
	tst.w d6
	beq.s callArgValueMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.s callArgValueMalformed
	move.w d3, (sp)
	lea 4(a3, d6.w), a1
	move.w d7, d0
	sub.w d6, d0
	subq.w #4, d0
	beq.s callArgValueMalformed
	bsr.w tkpkgParseU16DecimalV2
	bne.s callArgValueMalformed
	move.w d3, 2(sp)

	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s callArgValueNoMatch
	move.w 2(sp), d1
	bsr.w tkpkgSelectCallArgumentV1
	bne.s callArgValueNoMatch
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	bra.s callArgValueReturn

callArgValueMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s callArgValueReturn
callArgValueNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
callArgValueReturn
	lea 4(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectCallArgValueV1

; Project Rust's neutral call-argument member source. The package selects the
; operand, argument, and case-insensitive member field; the member base is
; evaluated by the shared expression bridge.
; Inputs: A1/D0.W = `N.argM.fieldF`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = evaluated member base.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-later-integer-group-b-v1.toml
; @opforge-role: facade
tkpkgProjectCallArgMemberV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -12(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6

callArgMemberArgScan
	move.w d7, d5
	subq.w #4, d5
	cmp.w d5, d6
	bhi.w callArgMemberMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s callArgMemberArgNext
	cmpi.b #'a', 1(a3, d6.w)
	bne.s callArgMemberArgNext
	cmpi.b #'r', 2(a3, d6.w)
	bne.s callArgMemberArgNext
	cmpi.b #'g', 3(a3, d6.w)
	beq.s callArgMemberArgReady
callArgMemberArgNext
	addq.w #1, d6
	bra.s callArgMemberArgScan

callArgMemberArgReady
	tst.w d6
	beq.w callArgMemberMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgMemberMalformed
	move.w d3, (sp)
	lea 4(a3, d6.w), a4
	move.w d7, d5
	sub.w d6, d5
	subq.w #4, d5
	moveq #0, d6

callArgMemberFieldScan
	move.w d5, d4
	subq.w #6, d4
	cmp.w d4, d6
	bhi.w callArgMemberMalformed
	cmpi.b #'.', 0(a4, d6.w)
	bne.s callArgMemberFieldNext
	cmpi.b #'f', 1(a4, d6.w)
	bne.s callArgMemberFieldNext
	cmpi.b #'i', 2(a4, d6.w)
	bne.s callArgMemberFieldNext
	cmpi.b #'e', 3(a4, d6.w)
	bne.s callArgMemberFieldNext
	cmpi.b #'l', 4(a4, d6.w)
	bne.s callArgMemberFieldNext
	cmpi.b #'d', 5(a4, d6.w)
	beq.s callArgMemberFieldReady
callArgMemberFieldNext
	addq.w #1, d6
	bra.s callArgMemberFieldScan

callArgMemberFieldReady
	tst.w d6
	beq.w callArgMemberMalformed
	move.w d5, 6(sp)
	movea.l a4, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgMemberMalformed
	move.w d3, 2(sp)
	lea 6(a4, d6.w), a1
	move.l a1, 8(sp)
	move.w 6(sp), d0
	sub.w d6, d0
	subq.w #6, d0
	beq.w callArgMemberMalformed
	move.w d0, 4(sp)

	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s callArgMemberNoMatch
	move.w 2(sp), d1
	bsr.w tkpkgSelectCallArgumentV1
	bne.s callArgMemberNoMatch
	movea.l 8(sp), a1
	move.w 4(sp), d1
	bsr.w tkpkgMselStripExpectedQualifierV2
	bne.s callArgMemberNoMatch
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	bra.s callArgMemberReturn

callArgMemberMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s callArgMemberReturn
callArgMemberNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
callArgMemberReturn
	lea 12(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectCallArgMemberV1

; Project Rust's neutral register inside an indirect tuple call argument. The
; package supplies operand, argument, tuple-item, and register-class indices;
; register spelling and opaque encoding remain package RENC data.
; Inputs: A1/D0.W = `N.argA.itemI.classC`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = register index on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-later-integer-group-b-v1.toml
; @opforge-role: facade
tkpkgProjectCallArgIndirectTupleRegisterV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -8(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6

callArgTupleSpecArgScan
	move.w d7, d5
	subq.w #4, d5
	cmp.w d5, d6
	bhi.w callArgTupleSpecMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s callArgTupleSpecArgNext
	cmpi.b #'a', 1(a3, d6.w)
	bne.s callArgTupleSpecArgNext
	cmpi.b #'r', 2(a3, d6.w)
	bne.s callArgTupleSpecArgNext
	cmpi.b #'g', 3(a3, d6.w)
	beq.s callArgTupleSpecArgReady
callArgTupleSpecArgNext
	addq.w #1, d6
	bra.s callArgTupleSpecArgScan

callArgTupleSpecArgReady
	tst.w d6
	beq.w callArgTupleSpecMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgTupleSpecMalformed
	move.w d3, (sp)
	lea 4(a3, d6.w), a1
	move.w d7, d0
	sub.w d6, d0
	subq.w #4, d0
	bsr.w tkpkgParseTupleItemClassSpecV2
	bne.w callArgTupleSpecMalformed
	tst.w d6
	bne.w callArgTupleSpecMalformed
	cmpi.w #$FFFF, d5
	beq.w callArgTupleSpecMalformed
	move.w d3, 2(sp)
	move.w d4, 4(sp)
	move.w d5, 6(sp)

	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s callArgTupleSpecNoMatch
	move.w 2(sp), d1
	bsr.w tkpkgSelectCallArgumentV1
	bne.s callArgTupleSpecNoMatch
	jsr operand.tkpkgMselStripOuterParensV1
	bne.s callArgTupleSpecNoMatch
	move.w 4(sp), d1
	moveq #0, d2
	bsr.w tkpkgExprPathSelectTupleItemV1
	bne.s callArgTupleSpecNoMatch
	move.w 6(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.s callArgTupleSpecNoMatch
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s callArgTupleSpecReturn

callArgTupleSpecMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s callArgTupleSpecReturn
callArgTupleSpecNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
callArgTupleSpecReturn
	lea 8(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectCallArgIndirectTupleRegisterV1

; Select one zero-based argument from a bounded neutral call span. Rust's
; parser represents named `.name(arg0,arg1)`, `arg0:arg1`, and postfix
; `arg0{arg1:arg2}` spellings as calls; preserve nested parenthesized and
; bracketed expressions in every form.
; Inputs: A0/D0 = call span; D1.W = argument index.
; Outputs: A0/D0 = trimmed argument; D1 = 0/1.
; Clobbers: D1/CCR. Other working registers are restored.
; CCR: reflects D1 on return.
tkpkgSelectCallArgumentV1	.block
	movem.l d2-d7/a1-a3, -(sp)
	move.w d1, d7
	movea.l a0, a1
	movea.l a0, a2
	adda.l d0, a2
	movea.l a1, a3
	moveq #0, d6
	moveq #0, d4
	moveq #0, d5
	moveq #':', d3

	cmpa.l a2, a1
	bhs.w callArgSelectFail
	cmpi.b #'.', (a1)
	bne.s callArgSelectBraceScan
	movea.l a1, a3
	addq.l #1, a3
	moveq #0, d6

callArgSelectNamedOpenScan
	cmpa.l a2, a3
	bhs.w callArgSelectFail
	cmpi.b #'(', (a3)
	beq.s callArgSelectNamedOpenFound
	addq.l #1, a3
	addq.w #1, d6
	bra.s callArgSelectNamedOpenScan

callArgSelectNamedOpenFound
	tst.w d6
	beq.w callArgSelectFail
	movea.l a2, a1
callArgSelectNamedTrimEnd
	cmpa.l a3, a1
	bls.w callArgSelectFail
	subq.l #1, a1
	cmpi.b #' ', (a1)
	beq.s callArgSelectNamedTrimEnd
	cmpi.b #9, (a1)
	beq.s callArgSelectNamedTrimEnd
	cmpi.b #')', (a1)
	bne.w callArgSelectFail
	movea.l a1, a2
	lea 1(a3), a1
	movea.l a1, a3
	moveq #0, d6
	moveq #0, d4
	moveq #0, d5
	moveq #',', d3
	bra.w callArgSelectScan

callArgSelectBraceScan
	cmpa.l a2, a3
	bhs.s callArgSelectNoBrace
	moveq #0, d2
	move.b (a3), d2
	cmpi.b #'(', d2
	beq.s callArgSelectBraceOpenParen
	cmpi.b #')', d2
	beq.s callArgSelectBraceCloseParen
	cmpi.b #'[', d2
	beq.s callArgSelectBraceOpenBracket
	cmpi.b #']', d2
	beq.s callArgSelectBraceCloseBracket
	cmpi.b #'{', d2
	bne.s callArgSelectBraceNext
	tst.w d4
	bne.s callArgSelectBraceNext
	tst.w d5
	bne.s callArgSelectBraceNext
	bra.s callArgSelectBraceFound

callArgSelectBraceOpenParen
	addq.w #1, d4
	bra.s callArgSelectBraceNext
callArgSelectBraceCloseParen
	tst.w d4
	beq.w callArgSelectFail
	subq.w #1, d4
	bra.s callArgSelectBraceNext
callArgSelectBraceOpenBracket
	addq.w #1, d5
	bra.s callArgSelectBraceNext
callArgSelectBraceCloseBracket
	tst.w d5
	beq.w callArgSelectFail
	subq.w #1, d5
callArgSelectBraceNext
	addq.l #1, a3
	bra.s callArgSelectBraceScan

callArgSelectNoBrace
	movea.l a0, a1
	movea.l a1, a3
	moveq #0, d4
	moveq #0, d5
	moveq #':', d3
	bra.s callArgSelectScan

callArgSelectBraceFound
	tst.w d7
	bne.s callArgSelectBraceArguments
	movea.l a3, a1
	movea.l a0, a3
	moveq #0, d4
	moveq #0, d5
	bra.w callArgSelectFound

callArgSelectBraceArguments
	movea.l a2, a1
callArgSelectBraceTrimEnd
	cmpa.l a3, a1
	bls.w callArgSelectFail
	subq.l #1, a1
	cmpi.b #' ', (a1)
	beq.s callArgSelectBraceTrimEnd
	cmpi.b #9, (a1)
	beq.s callArgSelectBraceTrimEnd
	cmpi.b #'}', (a1)
	bne.w callArgSelectFail
	movea.l a1, a2
	lea 1(a3), a1
	movea.l a1, a3
	moveq #1, d6
	moveq #0, d4
	moveq #0, d5
	moveq #':', d3

callArgSelectScan
	cmpa.l a2, a1
	bhs.s callArgSelectAtEnd
	moveq #0, d2
	move.b (a1), d2
	cmpi.b #'(', d2
	beq.s callArgSelectOpenParen
	cmpi.b #')', d2
	beq.s callArgSelectCloseParen
	cmpi.b #'[', d2
	beq.s callArgSelectOpenBracket
	cmpi.b #']', d2
	beq.s callArgSelectCloseBracket
	cmp.b d3, d2
	bne.s callArgSelectNext
	tst.w d4
	bne.s callArgSelectNext
	tst.w d5
	bne.s callArgSelectNext
	cmp.w d7, d6
	beq.s callArgSelectFound
	addq.w #1, d6
	addq.l #1, a1
	movea.l a1, a3
	bra.s callArgSelectScan

callArgSelectOpenParen
	addq.w #1, d4
	bra.s callArgSelectNext
callArgSelectCloseParen
	tst.w d4
	beq.s callArgSelectFail
	subq.w #1, d4
	bra.s callArgSelectNext
callArgSelectOpenBracket
	addq.w #1, d5
	bra.s callArgSelectNext
callArgSelectCloseBracket
	tst.w d5
	beq.s callArgSelectFail
	subq.w #1, d5
callArgSelectNext
	addq.l #1, a1
	bra.s callArgSelectScan

callArgSelectAtEnd
	tst.w d4
	bne.s callArgSelectFail
	tst.w d5
	bne.s callArgSelectFail
	cmp.w d7, d6
	bne.s callArgSelectFail
callArgSelectFound
	movea.l a3, a0
callArgSelectTrimStart
	cmpa.l a1, a0
	bhs.s callArgSelectFail
	cmpi.b #' ', (a0)
	beq.s callArgSelectTrimStartOne
	cmpi.b #9, (a0)
	bne.s callArgSelectTrimEnd
callArgSelectTrimStartOne
	addq.l #1, a0
	bra.s callArgSelectTrimStart
callArgSelectTrimEnd
	cmpa.l a0, a1
	bls.s callArgSelectFail
	cmpi.b #' ', -1(a1)
	beq.s callArgSelectTrimEndOne
	cmpi.b #9, -1(a1)
	bne.s callArgSelectReady
callArgSelectTrimEndOne
	subq.l #1, a1
	bra.s callArgSelectTrimEnd
callArgSelectReady
	move.l a1, d0
	sub.l a0, d0
	moveq #0, d1
	bra.s callArgSelectReturn
callArgSelectFail
	moveq #0, d0
	suba.l a0, a0
	moveq #1, d1
callArgSelectReturn
	movem.l (sp)+, d2-d7/a1-a3
	tst.l d1
	rts
	.bend  ; tkpkgSelectCallArgumentV1

; Project Rust's neutral `distinct_registerN` predicate by inverting the
; duplicate-register visitor shared with semantic rejection. The package owns
; the operand selection and any diagnostic; this projection emits only zero
; when the bounded register container has no duplicate token.
; Inputs: A1/D0.W = operand index text after `distinct_register`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = zero on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-later-integer-group-a-v1.toml
; @opforge-role: delegation
tkpkgProjectDistinctRegisterV1	.block
	bsr.w tkpkgProjectDuplicateRegisterV1
	cmpi.l #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	beq.s distinctRegisterOk
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s distinctRegisterReturn
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s distinctRegisterReturn
distinctRegisterOk
	moveq #0, d3
	moveq #TKPKG_SELECTED_STATUS_OK, d0
distinctRegisterReturn
	rts
	.bend  ; tkpkgProjectDistinctRegisterV1

; Project the Rust `xp1:` expression-path subset used by frozen complex-address
; package rows.  Package text owns every operand, tuple, field, qualifier, and
; register-class choice; this helper only walks neutral source-expression
; containers and projects one scalar.
; Inputs: A1/D0.W = path text after `xp1:`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = projected scalar on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-full-extension-addressing-v1.toml
; @opforge-role: facade
tkpkgProjectExprPathV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -30(sp), sp
	clr.w 28(sp)
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6

exprPathOperandScan
	cmp.w d7, d6
	bhs.w exprPathMalformed
	cmpi.b #'/', 0(a3, d6.w)
	beq.s exprPathOperandReady
	addq.w #1, d6
	bra.s exprPathOperandScan

exprPathOperandReady
	tst.w d6
	beq.w exprPathMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w exprPathMalformed
	move.w d3, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w exprPathNoMatch
	move.l a0, (sp)
	move.w d0, 4(sp)
	lea 1(a3, d6.w), a4
	move.l a4, 6(sp)
	move.w d7, d5
	sub.w d6, d5
	subq.w #1, d5
	beq.w exprPathMalformed
	move.w d5, 10(sp)

exprPathStepLoop
	movea.l 6(sp), a4
	move.w 10(sp), d7
	moveq #0, d6
exprPathStepScan
	cmp.w d7, d6
	bhs.w exprPathTerminal
	cmpi.b #'/', 0(a4, d6.w)
	beq.s exprPathContainer
	addq.w #1, d6
	bra.s exprPathStepScan

exprPathContainer
	tst.w d6
	beq.w exprPathMalformed
	cmpi.w #1, d6
	bne.s exprPathCheckTuple
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	moveq #'(', d1
	moveq #')', d2
	cmpi.b #'i', (a4)
	beq.s exprPathStripContainer
	moveq #'[', d1
	moveq #']', d2
	cmpi.b #'b', (a4)
	beq.s exprPathStripContainer
	cmpi.b #'l', (a4)
	beq.s exprPathSelectBinaryLeft
	cmpi.b #'r', (a4)
	beq.s exprPathSelectBinaryRight
	bra.s exprPathCheckTuple
exprPathStripContainer
	bsr.w tkpkgExprPathStripWrapperV1
	bne.w exprPathNoMatch
	move.w d2, 28(sp)
	bra.s exprPathContainerReady

exprPathSelectBinaryLeft
	bsr.w tkpkgExprPathSplitMultiplyV1
	tst.l d2
	bne.w exprPathNoMatch
	clr.w 28(sp)
	bra.s exprPathContainerReady

exprPathSelectBinaryRight
	bsr.w tkpkgExprPathSplitMultiplyV1
	tst.l d2
	bne.w exprPathNoMatch
	movea.l a1, a0
	move.l d1, d0
	clr.w 28(sp)
	bra.s exprPathContainerReady

exprPathCheckTuple
	cmpi.b #'t', (a4)
	bne.w exprPathNoMatch
	cmpi.w #2, d6
	bcs.w exprPathMalformed
	lea 1(a4), a1
	move.w d6, d0
	subq.w #1, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w exprPathMalformed
	move.w d3, d1
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	moveq #0, d2
	move.w 28(sp), d2
	bsr.w tkpkgExprPathSelectTupleItemV1
	bne.w exprPathNoMatch
	clr.w 28(sp)

exprPathContainerReady
	move.l a0, (sp)
	move.w d0, 4(sp)
	lea 1(a4, d6.w), a4
	move.l a4, 6(sp)
	move.w d7, d5
	sub.w d6, d5
	subq.w #1, d5
	beq.w exprPathMalformed
	move.w d5, 10(sp)
	bra.w exprPathStepLoop

exprPathTerminal
	tst.w d7
	beq.w exprPathMalformed
	move.l a4, 12(sp)
	move.w d7, 16(sp)
	cmpi.b #'r', (a4)
	beq.w exprPathRegisterTerminal
	cmpi.b #'q', (a4)
	beq.w exprPathQualifiedTerminal
	cmpi.b #'s', (a4)
	beq.w exprPathScaleTerminal
	cmpi.b #'m', (a4)
	beq.w exprPathMemberTerminal
	cmpi.b #'n', (a4)
	beq.w exprPathNamedRegisterTerminal
	bra.w exprPathMalformed

exprPathRegisterTerminal
	cmpi.w #2, d7
	bcs.w exprPathMalformed
	lea 1(a4), a1
	move.w d7, d0
	subq.w #1, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w exprPathMalformed
	move.w d3, d1
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w exprPathNoMatch
	bra.w exprPathOk

exprPathNamedRegisterTerminal
	cmpi.w #2, d7
	bcs.w exprPathMalformed
	movea.l (sp), a1
	lea 1(a4), a2
	move.w 4(sp), d0
	move.w d7, d1
	subq.w #1, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	beq.w exprPathNoMatch
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w exprPathNoMatch
	bra.w exprPathOk

exprPathQualifiedTerminal
	cmpi.w #5, d7
	bcs.w exprPathMalformed
	moveq #1, d6
exprPathQualifiedClassScan
	move.w d7, d5
	subq.w #2, d5
	cmp.w d5, d6
	bhi.w exprPathMalformed
	cmpi.b #'.', 0(a4, d6.w)
	bne.s exprPathQualifiedClassNext
	cmpi.b #'c', 1(a4, d6.w)
	beq.s exprPathQualifiedClassReady
exprPathQualifiedClassNext
	addq.w #1, d6
	bra.s exprPathQualifiedClassScan
exprPathQualifiedClassReady
	cmpi.w #1, d6
	beq.w exprPathMalformed
	lea 2(a4, d6.w), a1
	move.w d7, d0
	sub.w d6, d0
	subq.w #2, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w exprPathMalformed
	move.w d3, 26(sp)
	move.w d6, 24(sp)
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	bsr.w tkpkgExprPathSplitMultiplyV1
	tst.l d2
	bne.s exprPathQualifiedSpanReady
	move.l a1, 18(sp)
	move.w d1, 22(sp)
	move.l a0, (sp)
	move.w d0, 4(sp)
	movea.l 18(sp), a0
	moveq #0, d0
	move.w 22(sp), d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	tst.l d0
	beq.s exprPathQualifiedUseLeft
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	tst.l d0
	bne.w exprPathNoMatch
	movea.l 18(sp), a0
	moveq #0, d0
	move.w 22(sp), d0
	bra.s exprPathQualifiedSpanReady
exprPathQualifiedUseLeft
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
exprPathQualifiedSpanReady
	movea.l 12(sp), a4
	move.w 24(sp), d6
	lea 1(a4), a1
	move.w d6, d1
	subq.w #1, d1
	bsr.w tkpkgMselStripExpectedQualifierV2
	bne.w exprPathNoMatch
	move.w 26(sp), d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w exprPathNoMatch
	bra.w exprPathOk

exprPathScaleTerminal
	cmpi.w #1, d7
	bne.w exprPathMalformed
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	bsr.w tkpkgExprPathSplitMultiplyV1
	tst.l d2
	bne.w exprPathNoMatch
	move.l a0, (sp)
	move.w d0, 4(sp)
	move.l a1, 18(sp)
	move.w d1, 22(sp)
	movea.l a1, a0
	move.l d1, d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	tst.l d0
	beq.s exprPathScaleValidate
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	tst.l d0
	bne.w exprPathNoMatch
exprPathScaleValidate
	cmpi.l #1, d3
	beq.s exprPathScaleZero
	cmpi.l #2, d3
	beq.s exprPathScaleOne
	cmpi.l #4, d3
	beq.s exprPathScaleTwo
	cmpi.l #8, d3
	bne.w exprPathNoMatch
	moveq #3, d3
	bra.s exprPathOk
exprPathScaleTwo
	moveq #2, d3
	bra.s exprPathOk
exprPathScaleOne
	moveq #1, d3
	bra.s exprPathOk
exprPathScaleZero
	moveq #0, d3
	bra.s exprPathOk

exprPathMemberTerminal
	cmpi.w #2, d7
	bcs.w exprPathMalformed
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	lea 1(a4), a1
	move.w d7, d1
	subq.w #1, d1
	bsr.w tkpkgMselStripExpectedQualifierV2
	bne.w exprPathNoMatch
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	bra.s exprPathReturn

exprPathMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s exprPathReturn
exprPathNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s exprPathReturn
exprPathOk
	moveq #TKPKG_SELECTED_STATUS_OK, d0
exprPathReturn
	lea 30(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectExprPathV1

; Strip one neutral wrapper from a bounded expression span.  Rust normalizes
; both `(item0,item1,...)` and `item0(item1,...)` as indirect tuples, so the
; latter preserves its prefix and reports the alias separator to the tuple
; selector.
; Inputs: A0/D0 = span; D1.B/D2.B = opening/closing delimiters.
; Outputs: A0/D0 = logical inner span; D1 = 0/1; D2 = prefix-alias flag.
tkpkgExprPathStripWrapperV1	.block
	movem.l d3-d4/a1, -(sp)
	cmpi.l #2, d0
	bcs.s exprPathWrapperFailSaved
	move.b d2, d4
	cmp.b (a0), d1
	bne.s exprPathWrapperAlias
	cmp.b -1(a0, d0.l), d4
	bne.s exprPathWrapperFailSaved
	addq.l #1, a0
	subq.l #2, d0
	beq.s exprPathWrapperFailSaved
	moveq #0, d2
	moveq #0, d1
	bra.s exprPathWrapperReturn

exprPathWrapperAlias
	cmp.b -1(a0, d0.l), d4
	bne.s exprPathWrapperFailSaved
	movea.l a0, a1
	addq.l #1, a1
	move.l d0, d3
	subq.l #1, d3
exprPathWrapperAliasScan
	cmpi.l #1, d3
	bls.s exprPathWrapperFailSaved
	cmp.b (a1), d1
	beq.s exprPathWrapperAliasReady
	addq.l #1, a1
	subq.l #1, d3
	bra.s exprPathWrapperAliasScan
exprPathWrapperAliasReady
	subq.l #1, d0
	moveq #1, d2
	moveq #0, d1
	bra.s exprPathWrapperReturn

exprPathWrapperFailSaved
	moveq #1, d1
	moveq #0, d2
exprPathWrapperReturn
	movem.l (sp)+, d3-d4/a1
	tst.l d1
	rts
	.bend  ; tkpkgExprPathStripWrapperV1

; Select one zero-based top-level tuple/list item from a bounded span.  Empty
; items still advance the neutral index, matching Rust placeholder nodes.
; Inputs: A0/D0 = container contents; D1.W = item index; D2.B = prefix-alias flag.
; Outputs: A0/D0 = trimmed item; D1 = 0/1.
tkpkgExprPathSelectTupleItemV1	.block
	movem.l d2-d7/a1-a3, -(sp)
	move.w d1, d7
	move.b d2, d3
	movea.l a0, a1
	movea.l a0, a2
	adda.l d0, a2
	movea.l a1, a3
	moveq #0, d6
	moveq #0, d4
	moveq #0, d5
exprPathTupleScan
	cmpa.l a2, a1
	bhs.s exprPathTupleAtEnd
	moveq #0, d2
	move.b (a1), d2
	cmpi.b #'(', d2
	beq.s exprPathTupleOpenParen
	cmpi.b #')', d2
	beq.s exprPathTupleCloseParen
	cmpi.b #'[', d2
	beq.s exprPathTupleOpenBracket
	cmpi.b #']', d2
	beq.s exprPathTupleCloseBracket
	cmpi.b #',', d2
	bne.s exprPathTupleNext
	tst.w d4
	bne.s exprPathTupleNext
	tst.w d5
	bne.s exprPathTupleNext
	cmp.w d7, d6
	beq.s exprPathTupleFound
	addq.w #1, d6
	addq.l #1, a1
	movea.l a1, a3
	bra.s exprPathTupleScan
exprPathTupleOpenParen
	tst.b d3
	beq.s exprPathTupleNestedParen
	tst.w d4
	bne.s exprPathTupleNestedParen
	tst.w d5
	bne.s exprPathTupleNestedParen
	cmp.w d7, d6
	beq.s exprPathTupleFound
	addq.w #1, d6
	addq.l #1, a1
	movea.l a1, a3
	moveq #0, d3
	bra.s exprPathTupleScan
exprPathTupleNestedParen
	addq.w #1, d4
	bra.s exprPathTupleNext
exprPathTupleCloseParen
	tst.w d4
	beq.s exprPathTupleFail
	subq.w #1, d4
	bra.s exprPathTupleNext
exprPathTupleOpenBracket
	addq.w #1, d5
	bra.s exprPathTupleNext
exprPathTupleCloseBracket
	tst.w d5
	beq.s exprPathTupleFail
	subq.w #1, d5
exprPathTupleNext
	addq.l #1, a1
	bra.s exprPathTupleScan
exprPathTupleAtEnd
	tst.w d4
	bne.s exprPathTupleFail
	tst.w d5
	bne.s exprPathTupleFail
	cmp.w d7, d6
	bne.s exprPathTupleFail
exprPathTupleFound
	movea.l a3, a0
exprPathTupleTrimStart
	cmpa.l a1, a0
	bhs.s exprPathTupleFail
	cmpi.b #' ', (a0)
	beq.s exprPathTupleTrimStartOne
	cmpi.b #9, (a0)
	bne.s exprPathTupleTrimEnd
exprPathTupleTrimStartOne
	addq.l #1, a0
	bra.s exprPathTupleTrimStart
exprPathTupleTrimEnd
	cmpa.l a0, a1
	bls.s exprPathTupleFail
	cmpi.b #' ', -1(a1)
	beq.s exprPathTupleTrimEndOne
	cmpi.b #9, -1(a1)
	bne.s exprPathTupleReady
exprPathTupleTrimEndOne
	subq.l #1, a1
	bra.s exprPathTupleTrimEnd
exprPathTupleReady
	move.l a1, d0
	sub.l a0, d0
	moveq #0, d1
	bra.s exprPathTupleReturn
exprPathTupleFail
	moveq #0, d0
	suba.l a0, a0
	moveq #1, d1
exprPathTupleReturn
	movem.l (sp)+, d2-d7/a1-a3
	tst.l d1
	rts
	.bend  ; tkpkgExprPathSelectTupleItemV1

; Split one neutral top-level multiply expression into trimmed halves.
; Inputs: A0/D0 = expression span.
; Outputs: A0/D0 = left; A1/D1 = right; D2 = 0/1.
tkpkgExprPathSplitMultiplyV1	.block
	movem.l d3-d7/a2-a4, -(sp)
	movea.l a0, a2
	movea.l a0, a3
	adda.l d0, a3
	moveq #0, d6
	moveq #0, d7
exprPathMultiplyScan
	cmpa.l a3, a2
	bhs.w exprPathMultiplyFail
	moveq #0, d5
	move.b (a2), d5
	cmpi.b #'(', d5
	beq.s exprPathMultiplyOpenParen
	cmpi.b #')', d5
	beq.s exprPathMultiplyCloseParen
	cmpi.b #'[', d5
	beq.s exprPathMultiplyOpenBracket
	cmpi.b #']', d5
	beq.s exprPathMultiplyCloseBracket
	cmpi.b #'*', d5
	bne.s exprPathMultiplyNext
	tst.w d6
	bne.s exprPathMultiplyNext
	tst.w d7
	bne.s exprPathMultiplyNext
	movea.l a2, a4
exprPathMultiplyTrimLeft
	cmpa.l a0, a4
	bls.s exprPathMultiplyFail
	cmpi.b #' ', -1(a4)
	beq.s exprPathMultiplyTrimLeftOne
	cmpi.b #9, -1(a4)
	bne.s exprPathMultiplyRightStart
exprPathMultiplyTrimLeftOne
	subq.l #1, a4
	bra.s exprPathMultiplyTrimLeft
exprPathMultiplyRightStart
	lea 1(a2), a1
exprPathMultiplyTrimRightStart
	cmpa.l a3, a1
	bhs.s exprPathMultiplyFail
	cmpi.b #' ', (a1)
	beq.s exprPathMultiplyTrimRightStartOne
	cmpi.b #9, (a1)
	bne.s exprPathMultiplyTrimRightEnd
exprPathMultiplyTrimRightStartOne
	addq.l #1, a1
	bra.s exprPathMultiplyTrimRightStart
exprPathMultiplyTrimRightEnd
	cmpa.l a1, a3
	bls.s exprPathMultiplyFail
	cmpi.b #' ', -1(a3)
	beq.s exprPathMultiplyTrimRightEndOne
	cmpi.b #9, -1(a3)
	bne.s exprPathMultiplyReady
exprPathMultiplyTrimRightEndOne
	subq.l #1, a3
	bra.s exprPathMultiplyTrimRightEnd
exprPathMultiplyReady
	move.l a4, d0
	sub.l a0, d0
	move.l a3, d1
	sub.l a1, d1
	moveq #0, d2
	bra.s exprPathMultiplyReturn
exprPathMultiplyOpenParen
	addq.w #1, d6
	bra.s exprPathMultiplyNext
exprPathMultiplyCloseParen
	tst.w d6
	beq.s exprPathMultiplyFail
	subq.w #1, d6
	bra.s exprPathMultiplyNext
exprPathMultiplyOpenBracket
	addq.w #1, d7
	bra.s exprPathMultiplyNext
exprPathMultiplyCloseBracket
	tst.w d7
	beq.s exprPathMultiplyFail
	subq.w #1, d7
exprPathMultiplyNext
	addq.l #1, a2
	bra.w exprPathMultiplyScan
exprPathMultiplyFail
	moveq #1, d2
exprPathMultiplyReturn
	movem.l (sp)+, d3-d7/a2-a4
	tst.l d2
	rts
	.bend  ; tkpkgExprPathSplitMultiplyV1

; Project Rust's neutral `indirect_tuple_nonidentity_scaleN.itemM` source.
; The package chooses the operand and either one tuple item or `any`; this
; helper preserves Rust's right-before-left scalar evaluation order and only
; accepts a successfully evaluated multiplication side whose value is not one.
; Inputs: A1/D0.W = text after the source prefix.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = scale on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-full-extension-addressing-v1.toml
; @opforge-role: facade
tkpkgProjectNonidentityScaleV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -8(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6
nonidentityItemSeparatorScan
	move.w d7, d5
	subq.w #5, d5
	cmp.w d5, d6
	bhi.w nonidentityMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s nonidentityItemSeparatorNext
	cmpi.b #'i', 1(a3, d6.w)
	bne.s nonidentityItemSeparatorNext
	cmpi.b #'t', 2(a3, d6.w)
	bne.s nonidentityItemSeparatorNext
	cmpi.b #'e', 3(a3, d6.w)
	bne.s nonidentityItemSeparatorNext
	cmpi.b #'m', 4(a3, d6.w)
	beq.s nonidentityItemSeparatorReady
nonidentityItemSeparatorNext
	addq.w #1, d6
	bra.s nonidentityItemSeparatorScan

nonidentityItemSeparatorReady
	tst.w d6
	beq.w nonidentityMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w nonidentityMalformed
	move.w d3, (sp)
	lea 5(a3, d6.w), a4
	move.w d7, d5
	sub.w d6, d5
	subq.w #5, d5
	beq.w nonidentityMalformed
	move.w d5, 2(sp)
	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w nonidentityNoMatch
	move.l a0, 4(sp)
	cmpi.w #3, d5
	bne.s nonidentityNumericItem
	cmpi.b #'a', (a4)
	bne.s nonidentityNumericItem
	cmpi.b #'n', 1(a4)
	bne.s nonidentityNumericItem
	cmpi.b #'y', 2(a4)
	bne.s nonidentityNumericItem
	bra.s nonidentitySpanReady

nonidentityNumericItem
	movea.l a4, a1
	move.w d5, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w nonidentityMalformed
	move.w d3, d1
	move.w (sp), d0
	jsr operand.tkpkgMselLocateIndirectTupleItemV2
	bne.w nonidentityNoMatch

nonidentitySpanReady
	bsr.w tkpkgFindNonidentityScaleSpanV1
	bne.s nonidentityNoMatch
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s nonidentityReturn
nonidentityMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s nonidentityReturn
nonidentityNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
nonidentityReturn
	lea 8(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectNonidentityScaleV1

; Find one multiplication with a non-identity evaluated side inside a bounded
; neutral expression span. Every `*` node is visited in source order; its right
; side is evaluated before its left side, matching Rust's recursive projector.
; Inputs: A0/D0 = expression span. Outputs: D0 = 0 found or 1 none; D3 = value.
tkpkgFindNonidentityScaleSpanV1	.block
	movem.l d2/d4-d7/a1-a6, -(sp)
	lea -22(sp), sp
	move.l a0, (sp)
	move.l d0, 4(sp)
	clr.w 8(sp)

nonidentityScaleScan
	moveq #0, d6
	move.w 8(sp), d6
	cmp.l 4(sp), d6
	bhs.w nonidentityScaleNone
	movea.l (sp), a2
	adda.l d6, a2
	cmpi.b #'*', (a2)
	beq.s nonidentityScaleOperator
	addq.w #1, 8(sp)
	bra.s nonidentityScaleScan

nonidentityScaleOperator
	movea.l a2, a3
nonidentityScaleLeftBound
	cmpa.l (sp), a3
	bls.w nonidentityScaleNext
	moveq #0, d5
	move.b -1(a3), d5
	cmpi.b #',', d5
	beq.s nonidentityScaleLeftReady
	cmpi.b #'(', d5
	beq.s nonidentityScaleLeftReady
	cmpi.b #'[', d5
	beq.s nonidentityScaleLeftReady
	subq.l #1, a3
	bra.s nonidentityScaleLeftBound
nonidentityScaleLeftReady
	movea.l a2, a4
nonidentityScaleTrimLeftEnd
	cmpa.l a3, a4
	bls.w nonidentityScaleNext
	cmpi.b #' ', -1(a4)
	beq.s nonidentityScaleTrimLeftOne
	cmpi.b #9, -1(a4)
	bne.s nonidentityScaleRightStart
nonidentityScaleTrimLeftOne
	subq.l #1, a4
	bra.s nonidentityScaleTrimLeftEnd
nonidentityScaleRightStart
	lea 1(a2), a5
	movea.l (sp), a6
	adda.l 4(sp), a6
nonidentityScaleTrimRightStart
	cmpa.l a6, a5
	bhs.w nonidentityScaleNext
	cmpi.b #' ', (a5)
	beq.s nonidentityScaleTrimRightStartOne
	cmpi.b #9, (a5)
	bne.s nonidentityScaleRightBound
nonidentityScaleTrimRightStartOne
	addq.l #1, a5
	bra.s nonidentityScaleTrimRightStart
nonidentityScaleRightBound
	movea.l a5, a1
nonidentityScaleRightScan
	cmpa.l a6, a1
	bhs.s nonidentityScaleSpansReady
	moveq #0, d5
	move.b (a1), d5
	cmpi.b #',', d5
	beq.s nonidentityScaleSpansReady
	cmpi.b #')', d5
	beq.s nonidentityScaleSpansReady
	cmpi.b #']', d5
	beq.s nonidentityScaleSpansReady
	addq.l #1, a1
	bra.s nonidentityScaleRightScan
nonidentityScaleSpansReady
	move.l a3, 10(sp)
	move.l a4, d5
	sub.l a3, d5
	move.w d5, 14(sp)
	move.l a5, 16(sp)
	move.l a1, d5
	sub.l a5, d5
	move.w d5, 20(sp)
	movea.l 16(sp), a0
	moveq #0, d0
	move.w 20(sp), d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	tst.l d0
	bne.s nonidentityScaleTryLeft
	cmpi.l #1, d3
	bne.s nonidentityScaleFound
	bra.s nonidentityScaleNext
nonidentityScaleTryLeft
	movea.l 10(sp), a0
	moveq #0, d0
	move.w 14(sp), d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	tst.l d0
	bne.s nonidentityScaleNext
	cmpi.l #1, d3
	bne.s nonidentityScaleFound
nonidentityScaleNext
	addq.w #1, 8(sp)
	bra.w nonidentityScaleScan
nonidentityScaleFound
	moveq #0, d0
	bra.s nonidentityScaleReturn
nonidentityScaleNone
	moveq #1, d0
nonidentityScaleReturn
	lea 22(sp), sp
	movem.l (sp)+, d2/d4-d7/a1-a6
	tst.l d0
	rts
	.bend  ; tkpkgFindNonidentityScaleSpanV1

; Project Rust's neutral `named_registerN=NAME` semantic input.  The expected
; spelling is package data; this runtime only performs the bounded operand
; lookup and the same ASCII case-insensitive equality used by Rust.
; Inputs: A1/D0.W = text after `named_register`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = zero on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68010-delta-v1.toml
; @opforge-role: facade
tkpkgProjectNamedRegisterV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -8(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d2

namedRegisterSeparatorScan
	cmp.w d7, d2
	bhs.s namedRegisterMalformed
	cmpi.b #'=', 0(a3, d2.w)
	beq.s namedRegisterSeparatorReady
	addq.w #1, d2
	bra.s namedRegisterSeparatorScan

namedRegisterSeparatorReady
	tst.w d2
	beq.s namedRegisterMalformed
	move.w d7, d6
	sub.w d2, d6
	subq.w #1, d6
	beq.s namedRegisterMalformed
	movea.l a3, a1
	move.w d2, d0
	movem.l d2/d6, -(sp)
	bsr.w tkpkgParseU16DecimalV2
	movem.l (sp)+, d2/d6
	tst.l d1
	bne.s namedRegisterMalformed
	move.w d3, 6(sp)
	lea 1(a3, d2.w), a4
	move.l a4, (sp)
	move.w d6, 4(sp)
	move.w 6(sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s namedRegisterNoMatch
	movea.l a0, a1
	movea.l (sp), a2
	move.w 4(sp), d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	beq.s namedRegisterNoMatch
	moveq #0, d3
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s namedRegisterReturn

namedRegisterMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s namedRegisterReturn
namedRegisterNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
namedRegisterReturn
	lea 8(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectNamedRegisterV1

; Project Rust's neutral `duplicate_registerN` rejection capture from the
; bounded source form of a divide-composed register list.  Subtraction ranges
; are opaque nodes for duplicate detection, matching Rust's recursive visitor.
; Inputs: A1/D0.W = operand index text after `duplicate_register`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; A0/D1.W = duplicate token on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68000-remaining-base-v1.toml
; @opforge-role: delegation
tkpkgProjectDuplicateRegisterV1	.block
	movem.l d2-d7/a1-a6, -(sp)
	lea -8(sp), sp
	bsr.w tkpkgParseU16DecimalV2
	bne.w duplicateRegisterMalformed
	move.w d3, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w duplicateRegisterNoMatch
	movea.l a0, a5
	movea.l a0, a2
	move.l d0, d6

duplicateRegisterTokenLoop
	tst.l d6
	beq.w duplicateRegisterNoMatch
duplicateRegisterTrimStart
	move.b (a2), d0
	cmpi.b #' ', d0
	beq.s duplicateRegisterTrimStartOne
	cmpi.b #9, d0
	bne.s duplicateRegisterTokenStart
duplicateRegisterTrimStartOne
	addq.l #1, a2
	subq.l #1, d6
	bne.s duplicateRegisterTrimStart
	bra.w duplicateRegisterNoMatch

duplicateRegisterTokenStart
	movea.l a2, a3
	moveq #0, d5
duplicateRegisterTokenScan
	cmp.l d6, d5
	bhs.s duplicateRegisterTokenEnd
	cmpi.b #'/', 0(a3, d5.l)
	beq.s duplicateRegisterTokenEnd
	cmpi.b #':', 0(a3, d5.l)
	beq.s duplicateRegisterTokenEnd
	addq.l #1, d5
	bra.s duplicateRegisterTokenScan

duplicateRegisterTokenEnd
	move.l d5, d4
duplicateRegisterTrimEnd
	tst.l d4
	beq.w duplicateRegisterNoMatch
	move.b -1(a3, d4.l), d0
	cmpi.b #' ', d0
	beq.s duplicateRegisterTrimEndOne
	cmpi.b #9, d0
	bne.s duplicateRegisterTokenReady
duplicateRegisterTrimEndOne
	subq.l #1, d4
	bra.s duplicateRegisterTrimEnd

duplicateRegisterTokenReady
	move.l a3, (sp)
	move.w d4, 4(sp)
	move.w d5, 6(sp)
	moveq #0, d0
duplicateRegisterCurrentShape
	cmp.w d4, d0
	bhs.s duplicateRegisterPreviousInit
	cmpi.b #'-', 0(a3, d0.w)
	beq.w duplicateRegisterAdvance
	addq.w #1, d0
	bra.s duplicateRegisterCurrentShape

duplicateRegisterPreviousInit
	movea.l a5, a4
	move.l a3, d2
	sub.l a5, d2

duplicateRegisterPreviousLoop
	tst.l d2
	beq.w duplicateRegisterAdvance
duplicateRegisterPreviousTrimStart
	move.b (a4), d0
	cmpi.b #' ', d0
	beq.s duplicateRegisterPreviousTrimOne
	cmpi.b #9, d0
	bne.s duplicateRegisterPreviousStart
duplicateRegisterPreviousTrimOne
	addq.l #1, a4
	subq.l #1, d2
	bne.s duplicateRegisterPreviousTrimStart
	bra.w duplicateRegisterAdvance

duplicateRegisterPreviousStart
	moveq #0, d3
	suba.l a6, a6
duplicateRegisterPreviousScan
	cmp.l d2, d3
	bhs.w duplicateRegisterAdvance
	move.b 0(a4, d3.l), d0
	cmpi.b #'/', d0
	beq.s duplicateRegisterPreviousEnd
	cmpi.b #':', d0
	beq.s duplicateRegisterPreviousEnd
	cmpi.b #'-', d0
	bne.s duplicateRegisterPreviousNext
	movea.l #1, a6
duplicateRegisterPreviousNext
	addq.l #1, d3
	bra.s duplicateRegisterPreviousScan

duplicateRegisterPreviousEnd
	move.l d3, d7
	move.l d3, d1
duplicateRegisterPreviousTrimEnd
	tst.l d1
	beq.s duplicateRegisterPreviousAdvance
	move.b -1(a4, d1.l), d0
	cmpi.b #' ', d0
	beq.s duplicateRegisterPreviousTrimEndOne
	cmpi.b #9, d0
	bne.s duplicateRegisterPreviousCompare
duplicateRegisterPreviousTrimEndOne
	subq.l #1, d1
	bra.s duplicateRegisterPreviousTrimEnd

duplicateRegisterPreviousCompare
	move.l a6, d0
	bne.s duplicateRegisterPreviousAdvance
	cmp.w 4(sp), d1
	bne.s duplicateRegisterPreviousAdvance
	moveq #0, d0
duplicateRegisterCompareLoop
	cmp.w d1, d0
	bhs.s duplicateRegisterFound
	moveq #0, d3
	move.b 0(a4, d0.w), d3
	cmpi.b #'a', d3
	bcs.s duplicateRegisterPreviousFolded
	cmpi.b #'z', d3
	bhi.s duplicateRegisterPreviousFolded
	subi.b #$20, d3
duplicateRegisterPreviousFolded
	movea.l (sp), a6
	moveq #0, d5
	move.b 0(a6, d0.w), d5
	cmpi.b #'a', d5
	bcs.s duplicateRegisterCurrentFolded
	cmpi.b #'z', d5
	bhi.s duplicateRegisterCurrentFolded
	subi.b #$20, d5
duplicateRegisterCurrentFolded
	cmp.b d5, d3
	bne.s duplicateRegisterPreviousAdvance
	addq.w #1, d0
	bra.s duplicateRegisterCompareLoop

duplicateRegisterPreviousAdvance
	movea.l a4, a0
	adda.l d7, a0
	addq.l #1, a0
	movea.l a0, a4
	sub.l d7, d2
	subq.l #1, d2
	bra.w duplicateRegisterPreviousLoop

duplicateRegisterFound
	movea.l (sp), a0
	moveq #0, d1
	move.w 4(sp), d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s duplicateRegisterReturn

duplicateRegisterAdvance
	moveq #0, d5
	move.w 6(sp), d5
	cmp.l d6, d5
	bhs.s duplicateRegisterNoMatch
	movea.l (sp), a2
	adda.l d5, a2
	addq.l #1, a2
	sub.l d5, d6
	subq.l #1, d6
	bra.w duplicateRegisterTokenLoop

duplicateRegisterMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s duplicateRegisterFail
duplicateRegisterNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
duplicateRegisterFail
	suba.l a0, a0
	moveq #0, d1
duplicateRegisterReturn
	lea 8(sp), sp
	movem.l (sp)+, d2-d7/a1-a6
	rts
	.bend  ; tkpkgProjectDuplicateRegisterV1

; Project Rust's architecture-neutral `register_maskN.mapC=O[+...][.reverse16]`
; semantic input from a bounded source span.  Package RENC records own every
; register spelling, class, index, and class-to-bit offset used here.
; Inputs: A1/D0.W = text after `register_mask`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = u16 mask on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68000-remaining-base-v1.toml
; @opforge-role: facade
tkpkgProjectRegisterMaskV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -20(sp), sp
	movea.l a1, a5
	move.w d0, d7
	moveq #0, d4

registerMaskFindMap
	cmp.w d7, d4
	bhs.w registerMaskMalformed
	cmpi.b #'.', 0(a5, d4.w)
	bne.s registerMaskFindMapNext
	move.w d7, d6
	sub.w d4, d6
	cmpi.w #4, d6
	bcs.w registerMaskMalformed
	cmpi.b #'m', 1(a5, d4.w)
	bne.s registerMaskFindMapNext
	cmpi.b #'a', 2(a5, d4.w)
	bne.s registerMaskFindMapNext
	cmpi.b #'p', 3(a5, d4.w)
	bne.s registerMaskFindMapNext
	bra.s registerMaskMapFound

registerMaskFindMapNext
	addq.w #1, d4
	bra.s registerMaskFindMap

registerMaskMapFound
	tst.w d4
	beq.w registerMaskMalformed
	movea.l a5, a1
	move.w d4, d0
	move.w d4, -(sp)
	bsr.w tkpkgParseU16DecimalV2
	move.w (sp)+, d4
	tst.l d1
	bne.w registerMaskMalformed
	move.w d3, (sp)
	lea 4(a5, d4.w), a2
	move.w d7, d6
	sub.w d4, d6
	subi.w #4, d6
	beq.w registerMaskMalformed
	clr.w 8(sp)
	cmpi.w #10, d6
	bcs.s registerMaskPlanReady
	move.w d6, d5
	subi.w #10, d5
	cmpi.b #'.', 0(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'r', 1(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'e', 2(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'v', 3(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'e', 4(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'r', 5(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'s', 6(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'e', 7(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'1', 8(a2, d5.w)
	bne.s registerMaskPlanReady
	cmpi.b #'6', 9(a2, d5.w)
	bne.s registerMaskPlanReady
	moveq #1, d0
	move.w d0, 8(sp)
	move.w d5, d6
	beq.w registerMaskMalformed

registerMaskPlanReady
	move.l a2, 2(sp)
	move.w d6, 6(sp)
	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w registerMaskNoMatch
	move.l a0, 10(sp)
	move.w d0, 14(sp)
	clr.w 16(sp)
	clr.w 18(sp)
	movea.l a0, a2
	move.l d0, d6
	moveq #0, d7
	moveq #0, d5

registerMaskTokenLoop
	tst.l d6
	beq.w registerMaskNoMatch
registerMaskTrimStart
	move.b (a2), d0
	cmpi.b #' ', d0
	beq.s registerMaskTrimStartOne
	cmpi.b #9, d0
	bne.s registerMaskTokenStart
registerMaskTrimStartOne
	addq.l #1, a2
	subq.l #1, d6
	bne.s registerMaskTrimStart
	bra.w registerMaskNoMatch

registerMaskTokenStart
	movea.l a2, a3
	moveq #0, d4
registerMaskTokenScan
	cmp.l d6, d4
	bhs.s registerMaskTokenEnd
	move.b 0(a3, d4.l), d0
	cmpi.b #'/', d0
	beq.s registerMaskTokenEnd
	cmpi.b #'-', d0
	beq.s registerMaskTokenEnd
	addq.l #1, d4
	bra.s registerMaskTokenScan

registerMaskTokenEnd
	move.w d4, 14(sp)
	move.l d4, d2
registerMaskTrimTokenEnd
	tst.l d2
	beq.w registerMaskNoMatch
	move.b -1(a3, d2.l), d0
	cmpi.b #' ', d0
	beq.s registerMaskTrimTokenEndOne
	cmpi.b #9, d0
	bne.s registerMaskResolveToken
registerMaskTrimTokenEndOne
	subq.l #1, d2
	bra.s registerMaskTrimTokenEnd

registerMaskResolveToken
	movea.l a3, a0
	move.l d2, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingClassV1
	bne.w registerMaskNoMatch
	movea.l 2(sp), a1
	moveq #0, d0
	move.w 6(sp), d0
	bsr.w tkpkgRegisterMaskMappedBitV1
	cmpi.w #2, d0
	beq.w registerMaskMalformed
	tst.w d0
	bne.w registerMaskNoMatch

	tst.w 16(sp)
	beq.s registerMaskFirstBit
	cmpi.w #'/', d5
	beq.s registerMaskAddBit
	cmpi.w #'-', d5
	bne.w registerMaskNoMatch
	cmp.w 18(sp), d4
	bne.w registerMaskNoMatch
	move.w 16(sp), d2
	subq.w #1, d2
	cmp.w d2, d3
	blo.w registerMaskNoMatch
registerMaskFillRange
	bset d2, d7
	cmp.w d3, d2
	beq.s registerMaskRememberBit
	addq.w #1, d2
	bra.s registerMaskFillRange

registerMaskFirstBit
	moveq #1, d0
	move.w d0, 16(sp)
registerMaskAddBit
	bset d3, d7
registerMaskRememberBit
	move.w d3, 16(sp)
	addq.w #1, 16(sp)
	move.w d4, 18(sp)
	moveq #0, d2
	move.w 14(sp), d2
	cmp.l d6, d2
	bhs.s registerMaskTokensComplete
	moveq #0, d5
	move.b 0(a3, d2.l), d5
	lea 1(a3, d2.l), a2
	move.l d6, d0
	sub.l d2, d0
	subq.l #1, d0
	move.l d0, d6
	beq.w registerMaskNoMatch
	bra.w registerMaskTokenLoop

registerMaskTokensComplete
	tst.w 8(sp)
	beq.s registerMaskOk
	moveq #0, d3
	moveq #0, d4
	moveq #15, d5
registerMaskReverseLoop
	btst d5, d7
	beq.s registerMaskReverseNext
	bset d4, d3
registerMaskReverseNext
	addq.w #1, d4
	subq.w #1, d5
	bpl.s registerMaskReverseLoop
	move.w d3, d7

registerMaskOk
	moveq #0, d3
	move.w d7, d3
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s registerMaskReturn
registerMaskMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s registerMaskReturn
registerMaskNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
registerMaskReturn
	lea 20(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectRegisterMaskV1

; Validate one package class mapping and return the mapped bit for a resolved
; class/index.  All mapping entries are parsed before success, matching Rust's
; fail-closed collection of the mapping vector.
; Inputs: A1/D0.W = mapping text; D4.W = class; D3.W = register index.
; Outputs: D0 = 0 success, 1 no mapping/range, 2 malformed; D3.W = bit.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68000-remaining-base-v1.toml
; @opforge-role: delegation
tkpkgRegisterMaskMappedBitV1	.block
	movem.l d2/d4-d7/a0-a6, -(sp)
	lea -6(sp), sp
	move.w d3, d7
	move.w d4, d6
	movea.l a1, a5
	move.w d0, d5
	clr.w (sp)
	clr.w 2(sp)

registerMaskMapEntry
	tst.w d5
	beq.w registerMaskMapDone
	movea.l a5, a4
	moveq #0, d2
registerMaskMapFindEquals
	cmp.w d5, d2
	bhs.w registerMaskMapMalformed
	move.b 0(a4, d2.w), d0
	cmpi.b #'=', d0
	beq.s registerMaskMapEquals
	cmpi.b #'+', d0
	beq.w registerMaskMapMalformed
	addq.w #1, d2
	bra.s registerMaskMapFindEquals

registerMaskMapEquals
	tst.w d2
	beq.w registerMaskMapMalformed
	movea.l a4, a1
	move.w d2, d0
	movem.l d2/d5, -(sp)
	bsr.w tkpkgParseU16DecimalV2
	movem.l (sp)+, d2/d5
	tst.l d1
	bne.w registerMaskMapMalformed
	move.w d3, 4(sp)
	lea 1(a4, d2.w), a3
	move.w d5, d4
	sub.w d2, d4
	subq.w #1, d4
	beq.w registerMaskMapMalformed
	moveq #0, d2
registerMaskMapFindPlus
	cmp.w d4, d2
	bhs.s registerMaskMapOffsetReady
	cmpi.b #'+', 0(a3, d2.w)
	beq.s registerMaskMapOffsetReady
	addq.w #1, d2
	bra.s registerMaskMapFindPlus

registerMaskMapOffsetReady
	tst.w d2
	beq.w registerMaskMapMalformed
	movea.l a3, a1
	move.w d2, d0
	movem.l d2/d4, -(sp)
	bsr.w tkpkgParseU16DecimalV2
	movem.l (sp)+, d2/d4
	tst.l d1
	bne.w registerMaskMapMalformed
	cmp.w 4(sp), d6
	bne.s registerMaskMapAdvance
	tst.w (sp)
	bne.s registerMaskMapAdvance
	add.w d7, d3
	bcs.s registerMaskMapOutOfRange
	cmpi.w #16, d3
	blo.s registerMaskMapRemember
registerMaskMapOutOfRange
	moveq #2, d0
	move.w d0, (sp)
	bra.s registerMaskMapAdvance
registerMaskMapRemember
	moveq #1, d0
	move.w d0, (sp)
	move.w d3, 2(sp)

registerMaskMapAdvance
	movea.l a3, a5
	adda.w d2, a5
	move.w d4, d5
	sub.w d2, d5
	beq.s registerMaskMapDone
	addq.l #1, a5
	subq.w #1, d5
	beq.w registerMaskMapMalformed
	bra.w registerMaskMapEntry

registerMaskMapDone
	cmpi.w #1, (sp)
	bne.s registerMaskMapNoMatch
	moveq #0, d3
	move.w 2(sp), d3
	moveq #0, d0
	bra.s registerMaskMapReturn
registerMaskMapMalformed
	moveq #2, d0
	bra.s registerMaskMapReturn
registerMaskMapNoMatch
	moveq #1, d0
registerMaskMapReturn
	lea 6(sp), sp
	movem.l (sp)+, d2/d4-d7/a0-a6
	rts
	.bend  ; tkpkgRegisterMaskMappedBitV1

; Project Rust's `target:exprN` match predicate.  Rust accepts an expression as
; a relocation target when any recursively nested term is an Identifier.  The
; native selector boundary retains the source span rather than Rust Expr nodes,
; so the neutral lexical scan below applies the same identifier/register split
; while ignoring numeric and quoted literal text.
; Inputs: A1/D0 = `exprN` suffix. Outputs: D0 selected status; D3=0 on match.
tkpkgProjectDirectSemanticTargetV2	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	movea.l a1, a5
	move.w d0, d7
	movea.l a5, a1
	move.w d7, d0
	lea CallArgValuePrefixText, a2
	moveq #14, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s targetCheckCallArgMember
	lea 14(a5), a1
	move.w d7, d0
	subi.w #14, d0
	bsr.w tkpkgProjectCallArgValueTargetV1
	bra.w targetReturn

targetCheckCallArgMember
	movea.l a5, a1
	move.w d7, d0
	lea CallArgMemberPrefixText, a2
	moveq #15, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.s targetCheckExpr
	lea 15(a5), a1
	move.w d7, d0
	subi.w #15, d0
	bsr.w tkpkgProjectCallArgMemberTargetV1
	bra.w targetReturn

targetCheckExpr
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
	move.w d3, d6
	; Rust's expression_can_be_relocation_target recurses through Indirect,
	; but deliberately has no Tuple arm.  Address-displacement syntax such as
	; `symbol(a3)` is parsed as Indirect(Tuple(symbol, a3)) and therefore must
	; not select a direct absolute-target plan merely because the tuple contains
	; an identifier.  Probe item one first: success proves a multi-item indirect
	; tuple, while a simple parenthesized identifier still falls through.
	move.w d6, d0
	moveq #1, d1
	jsr operand.tkpkgMselLocateIndirectTupleItemV2
	beq.w targetNoMatch
	move.w d6, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w targetNoMatch
	bsr.w tkpkgValidateRelocationTargetSpanV2
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

; Project one package-declared forced fixup target.  Rust first attempts the
; complete expression, then resolves an Expr::Member relocation through its
; base expression.  Native has the original bounded span rather than an Expr
; tree, so a failed whole-span scalar projection retries the longest dotted
; prefix.  No field spelling is interpreted here; the package's `target:`
; declaration owns the relocation meaning.
; Inputs: A1/D0 = semantic source after `target:` (currently `exprN`).
; Outputs: D0 selected status; D3 projected scalar value on success.
tkpkgProjectForcedFixupTargetV2	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	movea.l a1, a5
	move.w d0, d7
	bsr.w tkpkgProjectCompactSemanticInputV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	beq.w forcedTargetReturn
	cmpi.l #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	bne.w forcedTargetReturn
	cmpi.w #5, d7
	bcs.w forcedTargetReturn
	cmpi.b #'e', (a5)
	bne.w forcedTargetReturn
	cmpi.b #'x', 1(a5)
	bne.w forcedTargetReturn
	cmpi.b #'p', 2(a5)
	bne.w forcedTargetReturn
	cmpi.b #'r', 3(a5)
	bne.w forcedTargetReturn
	lea 4(a5), a1
	move.w d7, d0
	subi.w #4, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w forcedTargetReturn
	move.w d3, d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w forcedTargetReturn
	movea.l a0, a3
	move.l d0, d6

forcedTargetMemberScan
	tst.l d6
	beq.w forcedTargetReturn
	subq.l #1, d6
	cmpi.b #'.', 0(a3, d6.l)
	bne.s forcedTargetMemberScan
	tst.l d6
	beq.w forcedTargetReturn
	movea.l a3, a0
	move.l d6, d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	beq.s forcedTargetReturn
	cmpi.l #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	beq.s forcedTargetMemberScan

forcedTargetReturn
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectForcedFixupTargetV2

; Match Rust `expression_can_be_relocation_target` against a native source
; span.  Inputs: A0/D0 = one parsed expression span. Outputs: selected status
; in D0 and D3=0 on match.  Any non-register identifier term is sufficient;
; numeric and quoted literal contents do not manufacture identifiers.
tkpkgValidateRelocationTargetSpanV2	.block
	movem.l d2/d4-d7/a0-a6, -(sp)
	movea.l a0, a3
	move.l d0, d7

targetScan
	tst.l d7
	beq.w targetScanNoMatch
	moveq #0, d4
	move.b (a3), d4
	cmpi.b #'"', d4
	beq.w targetScanQuoted
	cmpi.b #39, d4
	beq.w targetScanQuoted
	cmpi.b #'$', d4
	beq.w targetScanDollar
	cmpi.b #'%', d4
	beq.w targetScanPercent
	cmpi.b #'0', d4
	bcs.s targetScanIdentifierStart
	cmpi.b #'9', d4
	bls.w targetScanNumber

targetScanIdentifierStart
	cmpi.b #'_', d4
	beq.s targetScanIdentifier
	cmpi.b #'A', d4
	bcs.w targetScanAdvance
	cmpi.b #'Z', d4
	bls.s targetScanIdentifier
	cmpi.b #'a', d4
	bcs.w targetScanAdvance
	cmpi.b #'z', d4
	bhi.w targetScanAdvance

targetScanIdentifier
	movea.l a3, a4
	moveq #1, d6

targetScanIdentifierRest
	cmp.l d7, d6
	bhs.s targetScanIdentifierReady
	moveq #0, d4
	move.b 0(a3, d6.l), d4
	cmpi.b #'_', d4
	beq.s targetScanIdentifierContinue
	cmpi.b #'.', d4
	beq.s targetScanIdentifierContinue
	cmpi.b #'$', d4
	beq.s targetScanIdentifierContinue
	cmpi.b #'0', d4
	bcs.s targetScanIdentifierLetter
	cmpi.b #'9', d4
	bls.s targetScanIdentifierContinue
targetScanIdentifierLetter
	cmpi.b #'A', d4
	bcs.s targetScanIdentifierReady
	cmpi.b #'Z', d4
	bls.s targetScanIdentifierContinue
	cmpi.b #'a', d4
	bcs.s targetScanIdentifierReady
	cmpi.b #'z', d4
	bhi.s targetScanIdentifierReady
targetScanIdentifierContinue
	addq.l #1, d6
	bra.s targetScanIdentifierRest

targetScanIdentifierReady
	movea.l a4, a0
	move.l d6, d0
	move.w #$FFFF, d1
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.w targetScanMatch
	adda.l d6, a3
	sub.l d6, d7
	bra.w targetScan

targetScanQuoted
	move.b d4, d5
	addq.l #1, a3
	subq.l #1, d7
targetScanQuotedLoop
	tst.l d7
	beq.w targetScanNoMatch
	moveq #0, d4
	move.b (a3)+, d4
	subq.l #1, d7
	cmp.b d5, d4
	beq.w targetScan
	cmpi.b #'\\', d4
	bne.s targetScanQuotedLoop
	tst.l d7
	beq.w targetScanNoMatch
	addq.l #1, a3
	subq.l #1, d7
	bra.s targetScanQuotedLoop

targetScanDollar
	addq.l #1, a3
	subq.l #1, d7
targetScanHexLoop
	tst.l d7
	beq.w targetScanNoMatch
	moveq #0, d4
	move.b (a3), d4
	cmpi.b #'_', d4
	beq.s targetScanHexContinue
	cmpi.b #'0', d4
	bcs.s targetScanHexUpper
	cmpi.b #'9', d4
	bls.s targetScanHexContinue
targetScanHexUpper
	cmpi.b #'A', d4
	bcs.s targetScanHexLower
	cmpi.b #'F', d4
	bls.s targetScanHexContinue
targetScanHexLower
	cmpi.b #'a', d4
	bcs.w targetScan
	cmpi.b #'f', d4
	bhi.w targetScan
targetScanHexContinue
	addq.l #1, a3
	subq.l #1, d7
	bra.s targetScanHexLoop

targetScanPercent
	addq.l #1, a3
	subq.l #1, d7
targetScanBinaryLoop
	tst.l d7
	beq.w targetScanNoMatch
	moveq #0, d4
	move.b (a3), d4
	cmpi.b #'_', d4
	beq.s targetScanBinaryContinue
	cmpi.b #'0', d4
	beq.s targetScanBinaryContinue
	cmpi.b #'1', d4
	bne.w targetScan
targetScanBinaryContinue
	addq.l #1, a3
	subq.l #1, d7
	bra.s targetScanBinaryLoop

targetScanNumber
	addq.l #1, a3
	subq.l #1, d7
targetScanNumberLoop
	tst.l d7
	beq.w targetScanNoMatch
	moveq #0, d4
	move.b (a3), d4
	cmpi.b #'_', d4
	beq.s targetScanNumberContinue
	cmpi.b #'0', d4
	bcs.s targetScanNumberUpper
	cmpi.b #'9', d4
	bls.s targetScanNumberContinue
targetScanNumberUpper
	cmpi.b #'A', d4
	bcs.s targetScanNumberLower
	cmpi.b #'Z', d4
	bls.s targetScanNumberContinue
targetScanNumberLower
	cmpi.b #'a', d4
	bcs.w targetScan
	cmpi.b #'z', d4
	bhi.w targetScan
targetScanNumberContinue
	addq.l #1, a3
	subq.l #1, d7
	bra.s targetScanNumberLoop

targetScanAdvance
	addq.l #1, a3
	subq.l #1, d7
	bra.w targetScan

targetScanMatch
	moveq #0, d3
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s targetScanReturn
targetScanNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
targetScanReturn
	movem.l (sp)+, d2/d4-d7/a0-a6
	rts
	.bend  ; tkpkgValidateRelocationTargetSpanV2

; Project Rust's `target:call_arg_valueN.argM` predicate for the direct
; identifier subset supported by native target projection. Selection is purely
; structural; the fixup step evaluates the same selected argument later.
; Inputs: A1/D0.W = `N.argM`.
; Outputs: D0 selected status; D3=0 on match.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-later-integer-group-b-v1.toml
; @opforge-role: facade
tkpkgProjectCallArgValueTargetV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -4(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6

callArgValueTargetArgScan
	move.w d7, d5
	subq.w #4, d5
	cmp.w d5, d6
	bhi.w callArgValueTargetMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s callArgValueTargetArgNext
	cmpi.b #'a', 1(a3, d6.w)
	bne.s callArgValueTargetArgNext
	cmpi.b #'r', 2(a3, d6.w)
	bne.s callArgValueTargetArgNext
	cmpi.b #'g', 3(a3, d6.w)
	beq.s callArgValueTargetArgReady
callArgValueTargetArgNext
	addq.w #1, d6
	bra.s callArgValueTargetArgScan

callArgValueTargetArgReady
	tst.w d6
	beq.s callArgValueTargetMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.s callArgValueTargetMalformed
	move.w d3, (sp)
	lea 4(a3, d6.w), a1
	move.w d7, d0
	sub.w d6, d0
	subq.w #4, d0
	beq.s callArgValueTargetMalformed
	bsr.w tkpkgParseU16DecimalV2
	bne.s callArgValueTargetMalformed
	move.w d3, 2(sp)

	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s callArgValueTargetNoMatch
	move.w 2(sp), d1
	bsr.w tkpkgSelectCallArgumentV1
	bne.s callArgValueTargetNoMatch
	bsr.w tkpkgValidateRelocationTargetSpanV2
	bra.s callArgValueTargetReturn

callArgValueTargetMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s callArgValueTargetReturn
callArgValueTargetNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
callArgValueTargetReturn
	lea 4(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectCallArgValueTargetV1

; Project Rust's `target:call_arg_memberN.argM.fieldF` predicate for the direct
; identifier subset supported by native target projection. The selected call
; argument must have the package-declared member suffix; its base is validated
; structurally and is evaluated only by the later fixup step.
; Inputs: A1/D0.W = `N.argM.fieldF`.
; Outputs: D0 selected status; D3=0 on match.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68020-later-integer-group-b-v1.toml
; @opforge-role: facade
tkpkgProjectCallArgMemberTargetV1	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -12(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d6

callArgMemberTargetArgScan
	move.w d7, d5
	subq.w #4, d5
	cmp.w d5, d6
	bhi.w callArgMemberTargetMalformed
	cmpi.b #'.', 0(a3, d6.w)
	bne.s callArgMemberTargetArgNext
	cmpi.b #'a', 1(a3, d6.w)
	bne.s callArgMemberTargetArgNext
	cmpi.b #'r', 2(a3, d6.w)
	bne.s callArgMemberTargetArgNext
	cmpi.b #'g', 3(a3, d6.w)
	beq.s callArgMemberTargetArgReady
callArgMemberTargetArgNext
	addq.w #1, d6
	bra.s callArgMemberTargetArgScan

callArgMemberTargetArgReady
	tst.w d6
	beq.w callArgMemberTargetMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgMemberTargetMalformed
	move.w d3, (sp)
	lea 4(a3, d6.w), a4
	move.w d7, d5
	sub.w d6, d5
	subq.w #4, d5
	moveq #0, d6

callArgMemberTargetFieldScan
	move.w d5, d4
	subq.w #6, d4
	cmp.w d4, d6
	bhi.w callArgMemberTargetMalformed
	cmpi.b #'.', 0(a4, d6.w)
	bne.s callArgMemberTargetFieldNext
	cmpi.b #'f', 1(a4, d6.w)
	bne.s callArgMemberTargetFieldNext
	cmpi.b #'i', 2(a4, d6.w)
	bne.s callArgMemberTargetFieldNext
	cmpi.b #'e', 3(a4, d6.w)
	bne.s callArgMemberTargetFieldNext
	cmpi.b #'l', 4(a4, d6.w)
	bne.s callArgMemberTargetFieldNext
	cmpi.b #'d', 5(a4, d6.w)
	beq.s callArgMemberTargetFieldReady
callArgMemberTargetFieldNext
	addq.w #1, d6
	bra.s callArgMemberTargetFieldScan

callArgMemberTargetFieldReady
	tst.w d6
	beq.w callArgMemberTargetMalformed
	move.w d5, 6(sp)
	movea.l a4, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w callArgMemberTargetMalformed
	move.w d3, 2(sp)
	lea 6(a4, d6.w), a1
	move.l a1, 8(sp)
	move.w 6(sp), d0
	sub.w d6, d0
	subq.w #6, d0
	beq.w callArgMemberTargetMalformed
	move.w d0, 4(sp)

	move.w (sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.s callArgMemberTargetNoMatch
	move.w 2(sp), d1
	bsr.w tkpkgSelectCallArgumentV1
	bne.s callArgMemberTargetNoMatch
	movea.l 8(sp), a1
	move.w 4(sp), d1
	bsr.w tkpkgMselStripExpectedQualifierV2
	bne.s callArgMemberTargetNoMatch
	jsr operand.tkpkgMselStripOuterParensV1
	bne.s callArgMemberTargetNoMatch
	bsr.w tkpkgValidateRelocationTargetSpanV2
	bra.s callArgMemberTargetReturn

callArgMemberTargetMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
	bra.s callArgMemberTargetReturn
callArgMemberTargetNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
callArgMemberTargetReturn
	lea 12(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgProjectCallArgMemberTargetV1
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

; Find one exact package grammar marker in a bounded semantic source.
; Inputs: A1/D0.W = source; A2/D1.W = marker.
; Outputs: D0.W = marker offset; D1 = 0 found, 1 absent.
tkpkgFindSemanticMarkerV1	.block
	movem.l d2-d7/a0-a2, -(sp)
	move.w d0, d7
	move.w d1, d6
	moveq #0, d5
semanticMarkerCandidate
	move.w d7, d4
	sub.w d5, d4
	cmp.w d6, d4
	bcs.s semanticMarkerAbsent
	lea 0(a1, d5.w), a0
	moveq #0, d3
semanticMarkerCompare
	cmp.w d6, d3
	beq.s semanticMarkerFound
	moveq #0, d2
	move.b 0(a0, d3.w), d2
	cmp.b 0(a2, d3.w), d2
	bne.s semanticMarkerNext
	addq.w #1, d3
	bra.s semanticMarkerCompare
semanticMarkerNext
	addq.w #1, d5
	bra.s semanticMarkerCandidate
semanticMarkerFound
	move.w d5, d0
	moveq #0, d1
	bra.s semanticMarkerReturn
semanticMarkerAbsent
	moveq #0, d0
	moveq #1, d1
semanticMarkerReturn
	movem.l (sp)+, d2-d7/a0-a2
	tst.l d1
	rts
	.bend  ; tkpkgFindSemanticMarkerV1

; Parse `alignment[.violation-alignment|.violation-sequence]` exactly as
; Rust's two neutral register-sequence projectors.
; Inputs: A1/D0.W = tail. Outputs: D3.W = nonzero alignment;
; D4.W = 0 ordinary, 1 alignment violation, 2 sequence violation; D1=0/1.
tkpkgParseSequenceTailV1	.block
	movem.l d2/d5-d7/a0-a6, -(sp)
	lea -8(sp), sp
	move.l a1, (sp)
	move.w d0, 4(sp)
	clr.w 6(sp)
	lea SemanticViolationMarkerText, a2
	moveq #11, d1
	bsr.w tkpkgFindSemanticMarkerV1
	bne.s sequenceTailPlain
	move.w d0, d6
	beq.s sequenceTailMalformed
	movea.l (sp), a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.s sequenceTailMalformed
	tst.w d3
	beq.s sequenceTailMalformed
	move.w d3, 6(sp)
	movea.l (sp), a1
	lea 11(a1, d6.w), a1
	move.w 4(sp), d0
	sub.w d6, d0
	subi.w #11, d0
	cmpi.w #9, d0
	bne.s sequenceTailCheckSequence
	lea SemanticAlignmentViolationText, a2
	moveq #9, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	beq.s sequenceTailMalformed
	moveq #1, d4
	bra.s sequenceTailOk
sequenceTailCheckSequence
	cmpi.w #8, d0
	bne.s sequenceTailMalformed
	lea SemanticSequenceViolationText, a2
	moveq #8, d1
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	beq.s sequenceTailMalformed
	moveq #2, d4
	bra.s sequenceTailOk

sequenceTailPlain
	movea.l (sp), a1
	move.w 4(sp), d0
	bsr.w tkpkgParseU16DecimalV2
	bne.s sequenceTailMalformed
	tst.w d3
	beq.s sequenceTailMalformed
	move.w d3, 6(sp)
	moveq #0, d4
sequenceTailOk
	move.w 6(sp), d3
	moveq #0, d1
	bra.s sequenceTailReturn
sequenceTailMalformed
	moveq #0, d3
	moveq #0, d4
	moveq #1, d1
sequenceTailReturn
	lea 8(sp), sp
	movem.l (sp)+, d2/d5-d7/a0-a6
	tst.l d1
	rts
	.bend  ; tkpkgParseSequenceTailV1

; Apply the package's parsed opaque register-index projection.
; Inputs: D3.L index; D2.W kind; D4.W shift; D5.L mask. Output: D3.L.
tkpkgApplyRegisterIndexProjectionV1	.block
	tst.w d2
	beq.s registerIndexProjectionReturn
	cmpi.w #1, d2
	beq.s registerIndexProjectionShift
	cmpi.w #2, d2
	beq.s registerIndexProjectionMask
	lsr.l d4, d3
	and.l d5, d3
	bra.s registerIndexProjectionReturn
registerIndexProjectionShift
	lsr.l d4, d3
	bra.s registerIndexProjectionReturn
registerIndexProjectionMask
	and.l d5, d3
registerIndexProjectionReturn
	rts
	.bend  ; tkpkgApplyRegisterIndexProjectionV1

; Match Rust's aligned/consecutive relation, including its two explicit
; violation selectors. Inputs: D0.W first, D1.W last, D2.W count,
; D3.W alignment, D4.W violation. Output: D0=1 match, 0 no match.
tkpkgRegisterSequenceRelationMatchesV1	.block
	movem.l d1-d7, -(sp)
	move.w d0, d5
	move.w d1, d6
	move.w d2, d7
	moveq #0, d1
	move.w d5, d0
	divu.w d3, d0
	swap d0
	tst.w d0
	seq d1
	andi.w #1, d1
	move.w d5, d2
	move.w d7, d0
	subq.w #1, d0
	add.w d0, d2
	bcc.s registerRelationExpectedReady
	move.w #$FFFF, d2
registerRelationExpectedReady
	moveq #0, d3
	cmp.w d2, d6
	seq d3
	andi.w #1, d3
	tst.w d4
	beq.s registerRelationOrdinary
	cmpi.w #1, d4
	beq.s registerRelationAlignment
	tst.w d1
	beq.s registerRelationNo
	tst.w d3
	beq.s registerRelationYes
	bra.s registerRelationNo
registerRelationAlignment
	tst.w d1
	bne.s registerRelationNo
	tst.w d3
	bne.s registerRelationYes
	bra.s registerRelationNo
registerRelationOrdinary
	tst.w d1
	beq.s registerRelationNo
	tst.w d3
	beq.s registerRelationNo
registerRelationYes
	moveq #1, d0
	bra.s registerRelationReturn
registerRelationNo
	moveq #0, d0
registerRelationReturn
	movem.l (sp)+, d1-d7
	tst.l d0
	rts
	.bend  ; tkpkgRegisterSequenceRelationMatchesV1

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

; Unwrap one neutral tuple-item multiplication when either evaluated side is
; exactly the identity value.  The right side is tried first, matching Rust's
; BinaryOp::Multiply projection; the other text span is returned unchanged.
; Inputs: A0/D0 = tuple-item span. Outputs: A0/D0 = non-identity span;
;         D1 = 0 match or 1 no match (with the original span restored).
tkpkgMselUnwrapIdentityScaleV1	.block
	movem.l d2-d7/a1-a6, -(sp)
	lea -24(sp), sp
	move.l a0, (sp)
	move.w d0, 4(sp)
	movea.l a0, a2
	adda.l d0, a2
	move.w d0, d7
	moveq #0, d6

identityScaleFindOperator
	tst.w d7
	beq.w identityScaleNoMatch
	subq.l #1, a2
	subq.w #1, d7
	moveq #0, d2
	move.b (a2), d2
	cmpi.b #')', d2
	beq.s identityScaleNestedClose
	cmpi.b #'(', d2
	beq.s identityScaleNestedOpen
	cmpi.b #'*', d2
	bne.s identityScaleFindOperator
	tst.w d6
	bne.s identityScaleFindOperator
	bra.s identityScaleHaveOperator

identityScaleNestedClose
	addq.w #1, d6
	bra.s identityScaleFindOperator

identityScaleNestedOpen
	tst.w d6
	beq.w identityScaleNoMatch
	subq.w #1, d6
	bra.s identityScaleFindOperator

identityScaleHaveOperator
	movea.l (sp), a4
	movea.l a2, a3
identityScaleTrimLeftEnd
	cmpa.l a4, a3
	bls.w identityScaleNoMatch
	cmpi.b #' ', -1(a3)
	beq.s identityScaleTrimLeftOne
	cmpi.b #9, -1(a3)
	bne.s identityScaleLeftReady
identityScaleTrimLeftOne
	subq.l #1, a3
	bra.s identityScaleTrimLeftEnd

identityScaleLeftReady
	lea 1(a2), a5
	movea.l (sp), a6
	moveq #0, d5
	move.w 4(sp), d5
	adda.l d5, a6
identityScaleTrimRightStart
	cmpa.l a6, a5
	bhs.w identityScaleNoMatch
	cmpi.b #' ', (a5)
	beq.s identityScaleTrimRightStartOne
	cmpi.b #9, (a5)
	bne.s identityScaleTrimRightEnd
identityScaleTrimRightStartOne
	addq.l #1, a5
	bra.s identityScaleTrimRightStart

identityScaleTrimRightEnd
	cmpa.l a5, a6
	bls.w identityScaleNoMatch
	cmpi.b #' ', -1(a6)
	beq.s identityScaleTrimRightEndOne
	cmpi.b #9, -1(a6)
	bne.s identityScaleSpansReady
identityScaleTrimRightEndOne
	subq.l #1, a6
	bra.s identityScaleTrimRightEnd

identityScaleSpansReady
	move.l a4, 8(sp)
	move.l a3, d4
	sub.l a4, d4
	move.w d4, 12(sp)
	move.l a5, 16(sp)
	move.l a6, d5
	sub.l a5, d5
	move.w d5, 20(sp)

	movea.l 16(sp), a0
	moveq #0, d0
	move.w 20(sp), d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s identityScaleTryLeft
	cmpi.l #1, d3
	beq.s identityScaleReturnLeft

identityScaleTryLeft
	movea.l 8(sp), a0
	moveq #0, d0
	move.w 12(sp), d0
	moveq #0, d1
	jsr operand.tkpkgMselEvaluateSemanticSpanV2
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.s identityScaleNoMatch
	cmpi.l #1, d3
	bne.s identityScaleNoMatch
	movea.l 16(sp), a0
	moveq #0, d0
	move.w 20(sp), d0
	bra.s identityScaleMatched

identityScaleReturnLeft
	movea.l 8(sp), a0
	moveq #0, d0
	move.w 12(sp), d0

identityScaleMatched
	moveq #0, d1
	bra.s identityScaleReturn

identityScaleNoMatch
	movea.l (sp), a0
	moveq #0, d0
	move.w 4(sp), d0
	moveq #1, d1

identityScaleReturn
	lea 24(sp), sp
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d1
	rts
	.bend  ; tkpkgMselUnwrapIdentityScaleV1

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

; Parse Rust's neutral register-index projection grammar.  The package may
; retain the resolved index, shift it right, mask it, or shift and then mask.
; Inputs: A1/D0.W = `class`, `class.shrN`, `class.andM`, or
;         `class.shrN.andM` text after the `.class` marker.
; Outputs: D3.W = class; D2.W = 0 identity, 1 shift, 2 mask, 3 shift+mask;
;          D4.W = shift; D5.W = mask; D1 = 0/1.
; Clobbers: D0-D5/A1-A3/CCR.
tkpkgParseRegisterClassProjectionV1	.block
	movem.l d6-d7/a2-a3, -(sp)
	lea -2(sp), sp
	movea.l a1, a3
	move.w d0, d7
	moveq #0, d2
	moveq #0, d4
	moveq #0, d5
	clr.w (sp)
	tst.w d7
	beq.w registerProjectionFail
	moveq #0, d6

registerProjectionClassScan
	cmp.w d7, d6
	bhs.s registerProjectionIdentity
	cmpi.b #'.', 0(a3, d6.w)
	beq.s registerProjectionSuffix
	addq.w #1, d6
	bra.s registerProjectionClassScan

registerProjectionIdentity
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerProjectionFail
	move.w d3, (sp)
	moveq #0, d2
	moveq #0, d4
	moveq #0, d5
	bra.w registerProjectionOk

registerProjectionSuffix
	tst.w d6
	beq.w registerProjectionFail
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerProjectionFail
	move.w d3, (sp)
	lea 0(a3, d6.w), a2
	move.w d7, d0
	sub.w d6, d0
	cmpi.w #5, d0
	bcs.w registerProjectionFail
	cmpi.b #'.', (a2)
	bne.w registerProjectionFail
	cmpi.b #'s', 1(a2)
	bne.w registerProjectionCheckMask
	cmpi.b #'h', 2(a2)
	bne.w registerProjectionFail
	cmpi.b #'r', 3(a2)
	bne.w registerProjectionFail
	lea 4(a2), a1
	subq.w #4, d0
	moveq #0, d6

registerProjectionShiftScan
	cmp.w d0, d6
	bhs.s registerProjectionShiftOnly
	move.w d0, d7
	sub.w d6, d7
	cmpi.w #4, d7
	bcs.s registerProjectionShiftNext
	cmpi.b #'.', 0(a1, d6.w)
	bne.s registerProjectionShiftNext
	cmpi.b #'a', 1(a1, d6.w)
	bne.s registerProjectionShiftNext
	cmpi.b #'n', 2(a1, d6.w)
	bne.s registerProjectionShiftNext
	cmpi.b #'d', 3(a1, d6.w)
	beq.s registerProjectionShiftMask
registerProjectionShiftNext
	addq.w #1, d6
	bra.s registerProjectionShiftScan

registerProjectionShiftOnly
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerProjectionFail
	cmpi.l #15, d3
	bhi.w registerProjectionFail
	move.w d3, d4
	moveq #1, d2
	bra.s registerProjectionOk

registerProjectionShiftMask
	tst.w d6
	beq.w registerProjectionFail
	move.w d0, d7
	move.w d6, d0
	movea.l a1, a2
	bsr.w tkpkgParseU16DecimalV2
	bne.w registerProjectionFail
	cmpi.l #15, d3
	bhi.w registerProjectionFail
	move.w d3, d4
	lea 4(a2, d6.w), a1
	move.w d7, d0
	sub.w d6, d0
	subq.w #4, d0
	beq.w registerProjectionFail
	move.l d4, -(sp)
	bsr.w tkpkgParseU16DecimalV2
	move.l (sp)+, d4
	tst.l d1
	bne.w registerProjectionFail
	move.w d3, d5
	moveq #3, d2
	bra.s registerProjectionOk

registerProjectionCheckMask
	cmpi.b #'a', 1(a2)
	bne.w registerProjectionFail
	cmpi.b #'n', 2(a2)
	bne.w registerProjectionFail
	cmpi.b #'d', 3(a2)
	bne.w registerProjectionFail
	lea 4(a2), a1
	subq.w #4, d0
	beq.s registerProjectionFail
	bsr.w tkpkgParseU16DecimalV2
	bne.s registerProjectionFail
	move.w d3, d5
	moveq #2, d2

registerProjectionOk
	moveq #0, d3
	move.w (sp), d3
	moveq #0, d1
	bra.s registerProjectionReturn
registerProjectionFail
	moveq #1, d1
registerProjectionReturn
	lea 2(sp), sp
	movem.l (sp)+, d6-d7/a2-a3
	tst.l d1
	rts
	.bend  ; tkpkgParseRegisterClassProjectionV1

; Project Rust's neutral `register_or_named_rangeN.classesC[+C]
; .prefixP.minX.maxY` source. Active package RENC classes take precedence;
; otherwise the raw identifier may match the package-declared prefix/range.
; Inputs: A1/D0.W = text after `register_or_named_range`.
; Outputs: D0 = TKPKG_SELECTED_STATUS_*; D3.L = register/range index.
tkpkgProjectRegisterOrNamedRangeV1	.block
	movem.l d2/d4-d7/a0-a6, -(sp)
	lea -40(sp), sp
	move.l a1, (sp)
	move.w d0, 4(sp)
	clr.w 6(sp)
	clr.l 8(sp)
	clr.w 12(sp)
	clr.l 14(sp)
	clr.w 18(sp)
	clr.l 20(sp)
	clr.l 24(sp)
	clr.l 28(sp)
	clr.w 32(sp)
	clr.l 34(sp)
	clr.w 38(sp)

	movea.l a1, a2
	move.w d0, d7
	moveq #0, d6
rangeFindClasses
	tst.w d7
	beq.w rangeMalformed
	cmpi.b #'.', (a2)
	beq.s rangeClassesMarker
	addq.l #1, a2
	addq.w #1, d6
	subq.w #1, d7
	bra.s rangeFindClasses

rangeClassesMarker
	tst.w d6
	beq.w rangeMalformed
	movea.l (sp), a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w rangeMalformed
	move.w d3, 6(sp)
	cmpi.w #8, d7
	bcs.w rangeMalformed
	cmpi.b #'.', (a2)
	bne.w rangeMalformed
	cmpi.b #'c', 1(a2)
	bne.w rangeMalformed
	cmpi.b #'l', 2(a2)
	bne.w rangeMalformed
	cmpi.b #'a', 3(a2)
	bne.w rangeMalformed
	cmpi.b #'s', 4(a2)
	bne.w rangeMalformed
	cmpi.b #'s', 5(a2)
	bne.w rangeMalformed
	cmpi.b #'e', 6(a2)
	bne.w rangeMalformed
	cmpi.b #'s', 7(a2)
	bne.w rangeMalformed
	lea 8(a2), a3
	subi.w #8, d7
	move.l a3, 8(sp)
	moveq #0, d6

rangeFindPrefix
	cmpi.w #7, d7
	bcs.w rangeMalformed
	cmpi.b #'.', (a3)
	bne.s rangePrefixNext
	cmpi.b #'p', 1(a3)
	bne.s rangePrefixNext
	cmpi.b #'r', 2(a3)
	bne.s rangePrefixNext
	cmpi.b #'e', 3(a3)
	bne.s rangePrefixNext
	cmpi.b #'f', 4(a3)
	bne.s rangePrefixNext
	cmpi.b #'i', 5(a3)
	bne.s rangePrefixNext
	cmpi.b #'x', 6(a3)
	beq.s rangePrefixMarker
rangePrefixNext
	addq.l #1, a3
	addq.w #1, d6
	subq.w #1, d7
	bra.s rangeFindPrefix

rangePrefixMarker
	tst.w d6
	beq.w rangeMalformed
	move.w d6, 12(sp)
	lea 7(a3), a4
	subi.w #7, d7
	move.l a4, 14(sp)
	moveq #0, d6

rangeFindMin
	cmpi.w #4, d7
	bcs.w rangeMalformed
	cmpi.b #'.', (a4)
	bne.s rangeMinNext
	cmpi.b #'m', 1(a4)
	bne.s rangeMinNext
	cmpi.b #'i', 2(a4)
	bne.s rangeMinNext
	cmpi.b #'n', 3(a4)
	beq.s rangeMinMarker
rangeMinNext
	addq.l #1, a4
	addq.w #1, d6
	subq.w #1, d7
	bra.s rangeFindMin

rangeMinMarker
	tst.w d6
	beq.w rangeMalformed
	move.w d6, 18(sp)
	lea 4(a4), a5
	subq.w #4, d7
	moveq #0, d6

rangeFindMax
	cmpi.w #4, d7
	bcs.w rangeMalformed
	cmpi.b #'.', 0(a5, d6.w)
	bne.s rangeMaxNext
	cmpi.b #'m', 1(a5, d6.w)
	bne.s rangeMaxNext
	cmpi.b #'a', 2(a5, d6.w)
	bne.s rangeMaxNext
	cmpi.b #'x', 3(a5, d6.w)
	beq.s rangeMaxMarker
rangeMaxNext
	addq.w #1, d6
	subq.w #1, d7
	bra.s rangeFindMax

rangeMaxMarker
	tst.w d6
	beq.w rangeMalformed
	lea 0(a5, d6.w), a2
	lea 4(a2), a3
	subq.w #4, d7
	beq.w rangeMalformed
	movea.l a5, a1
	move.w d6, d0
	bsr.w tkpkgParseU32DecimalV2
	bne.w rangeMalformed
	move.l d3, 20(sp)
	movea.l a3, a1
	move.w d7, d0
	bsr.w tkpkgParseU32DecimalV2
	bne.w rangeMalformed
	move.l d3, 24(sp)
	move.l 20(sp), d4
	cmp.l 24(sp), d4
	bhi.w rangeMalformed

	move.w 6(sp), d0
	jsr operand.tkpkgMselLocateSemanticOperandV2
	bne.w rangeNoMatch
	move.l a0, 28(sp)
	move.w d0, 32(sp)
	movea.l 8(sp), a3
	move.w 12(sp), d7

rangeClassLoop
	moveq #0, d6
rangeClassScan
	cmp.w d7, d6
	beq.s rangeClassReady
	cmpi.b #'+', 0(a3, d6.w)
	beq.s rangeClassReady
	addq.w #1, d6
	bra.s rangeClassScan
rangeClassReady
	tst.w d6
	beq.w rangeMalformed
	movea.l a3, a1
	move.w d6, d0
	bsr.w tkpkgParseU16DecimalV2
	bne.w rangeMalformed
	move.w d3, d1
	movea.l 28(sp), a0
	moveq #0, d0
	move.w 32(sp), d0
	bsr.w tkpkgFindScopedRegisterEncodingV1
	bne.s rangeClassAdvance
	tst.w 38(sp)
	bne.s rangeClassAdvance
	move.l d3, 34(sp)
	move.w #1, 38(sp)
rangeClassAdvance
	cmp.w d7, d6
	beq.s rangeClassesDone
	addq.w #1, d6
	adda.w d6, a3
	sub.w d6, d7
	beq.w rangeMalformed
	bra.s rangeClassLoop

rangeClassesDone
	tst.w 38(sp)
	beq.s rangeTryNamed
	move.l 34(sp), d3
	bra.s rangeOk

rangeTryNamed
	move.w 32(sp), d0
	move.w 18(sp), d1
	cmp.w d1, d0
	bls.s rangeNoMatch
	movea.l 28(sp), a1
	movea.l 14(sp), a2
	move.w d1, d0
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	beq.s rangeNoMatch
	movea.l 28(sp), a1
	moveq #0, d0
	move.w 18(sp), d0
	adda.l d0, a1
	moveq #0, d0
	move.w 32(sp), d0
	sub.w 18(sp), d0
	bsr.w tkpkgParseU32DecimalV2
	bne.s rangeNoMatch
	cmp.l 20(sp), d3
	bcs.s rangeNoMatch
	cmp.l 24(sp), d3
	bhi.s rangeNoMatch

rangeOk
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.s rangeReturn
rangeNoMatch
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.s rangeReturn
rangeMalformed
	moveq #TKPKG_SELECTED_STATUS_RUNTIME_ERROR, d0
rangeReturn
	lea 40(sp), sp
	movem.l (sp)+, d2/d4-d7/a0-a6
	rts
	.bend  ; tkpkgProjectRegisterOrNamedRangeV1

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
	move.l d4, -(sp)
	bsr.w tkpkgFindScopedRegisterEncodingClassV1
	move.l (sp)+, d4
	tst.l d0
	rts
	.bend  ; tkpkgFindScopedRegisterEncodingV1

; Resolve one spelling while retaining its package-owned neutral class.
; Inputs: A0/D0 = register token; D1.W = required class or $FFFF.
; Outputs: D0 = 0/1; D3.L = index; D4.W = class on success.
; @opforge-owner: tkpkg.amigaos.selection_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68000-remaining-base-v1.toml
; @opforge-role: delegation
tkpkgFindScopedRegisterEncodingClassV1	.block
	movem.l d2/d5-d7/a0-a6, -(sp)
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
	moveq #0, d4
	move.w 6(sp), d4
	moveq #0, d0
	bra.s rencReturn
rencNoMatch
rencFail
	moveq #1, d0
rencReturn
	lea 18(sp), sp
	movem.l (sp)+, d2/d5-d7/a0-a6
	tst.l d0
	rts
	.bend  ; tkpkgFindScopedRegisterEncodingClassV1

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
requiredProjectSource
	; Rust's frozen v1 value-source grammar permits exactly expr, member, or
	; indirect-tuple value projections. Reject every other neutral input source
	; before reusing the common projector.
	movea.l 8(sp), a1
	move.w 12(sp), d0
	cmpi.w #5, d0
	bcs.s requiredCheckMemberSource
	cmpi.b #'e', (a1)
	bne.s requiredCheckMemberSource
	cmpi.b #'x', 1(a1)
	bne.s requiredCheckMemberSource
	cmpi.b #'p', 2(a1)
	bne.s requiredCheckMemberSource
	cmpi.b #'r', 3(a1)
	beq.s requiredProjectValidatedSource
requiredCheckMemberSource
	movea.l 8(sp), a1
	move.w 12(sp), d0
	lea MemberPrefixText, a2
	moveq #6, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	bne.s requiredProjectValidatedSourceReload
	movea.l 8(sp), a1
	move.w 12(sp), d0
	lea IndirectTupleValuePrefixText, a2
	moveq #20, d1
	bsr.w tkpkgSemanticPrefixMatchesV2
	tst.b d0
	beq.w requiredMalformed
requiredProjectValidatedSourceReload
	movea.l 8(sp), a1
	move.w 12(sp), d0
requiredProjectValidatedSource
	bsr.w tkpkgProjectCompactSemanticInputV2
requiredProjectionReady
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w requiredReturn
	movea.l 2(sp), a1
	move.w 6(sp), d0
	bsr.w tkpkgExecuteScopedValueProgramV2
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

; Find the most-specific scoped VALP row and execute its v1/v2 scalar program.
; Inputs: A1/D0 = opaque program id; D3 = input value.
; Outputs: D0 = 0 success, 1 malformed/missing, or 2 constraint violation;
;          D3 = program result.
tkpkgExecuteScopedValueProgramV2	.block
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
	beq.s valpVersionReady
	cmpi.w #2, d4
	bne.s valpNext
valpVersionReady
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
	move.w 12(sp), d0
	bsr.w tkpkgExecuteValueProgramBytesV2
	bra.s valpReturn
valpFail
	moveq #1, d0
valpReturn
	lea 24(sp), sp
	movem.l (sp)+, d2/d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; tkpkgExecuteScopedValueProgramV2

; Direct Rust VALUE_VM v1/v2 port over the native signed-32 scalar transport.
; Inputs: D0 = opcode version; A1/D1 = program bytes; D3 = input zero.
; Outputs: D0 = 0 success, 1 malformed, or 2 constraint violation; D3 = value.
tkpkgExecuteValueProgramBytesV2	.block
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_SERVICE_VALUE, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	movem.l (sp)+, d0-d1
.endif
	movea.l a1, a0
	move.w d1, d7
	movea.l d0, a3
	cmpi.w #1, d0
	beq.s valueVersionReady
	cmpi.w #2, d0
	bne.w valueFail
valueVersionReady
	moveq #0, d6
valueLoop
	tst.w d7
	beq.w valueFail
	moveq #0, d0
	move.b (a0)+, d0
	subq.w #1, d7
	cmpi.b #$FF, d0
	beq.w valueEnd
	cmpi.b #$01, d0
	beq.s valuePushLiteral
	cmpi.b #$02, d0
	beq.s valuePushInput
	cmpi.b #$03, d0
	beq.s valueNormalize
	cmpi.b #$04, d0
	beq.w valueRequireSignedBits
	cmpi.b #$05, d0
	beq.w valueRequireUnsignedBits
	cmpi.b #$06, d0
	beq.w valueRequireRange
	cmpi.b #$07, d0
	beq.w valueEncodeUpperBoundAsZero
	bra.w valueFail

valuePushLiteral
	tst.b d6
	bne.w valueFail
	cmpi.w #8, d7
	bcs.w valueFail
	bsr.w tkpkgValueReadI64LeV1
	subi.w #8, d7
	moveq #0, d2
	tst.l d1
	bpl.s valueLiteralHighReady
	moveq #-1, d2
valueLiteralHighReady
	cmp.l d2, d0
	bne.w valueFail
	move.l d1, d3
	moveq #1, d6
	bra.w valueLoop

valuePushInput
	tst.b d6
	bne.w valueFail
	tst.w d7
	beq.w valueFail
	tst.b (a0)+
	bne.w valueFail
	subq.w #1, d7
	moveq #1, d6
	bra.w valueLoop
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
	cmpi.b #64, d2
	bhi.w valueFail
	cmpi.b #32, d2
	bhs.w valueLoop
	moveq #1, d4
	subq.b #1, d2
	lsl.l d2, d4
	move.l d4, d5
	subq.l #1, d5
	cmp.l d5, d3
	ble.w valueLoop
	addq.b #1, d2
	add.l d4, d4
	cmpi.b #31, d2
	beq.s valueNormalizeApply
	cmp.l d4, d3
	bge.w valueLoop
valueNormalizeApply
	sub.l d4, d3
	bra.w valueLoop

valueRequireSignedBits
	tst.b d6
	beq.w valueFail
	tst.w d7
	beq.w valueFail
	moveq #0, d2
	move.b (a0)+, d2
	subq.w #1, d7
	tst.b d2
	beq.w valueFail
	cmpi.b #64, d2
	bhi.w valueFail
	cmpi.b #32, d2
	bhs.w valueLoop
	moveq #1, d4
	subq.b #1, d2
	lsl.l d2, d4
	move.l d4, d0
	neg.l d4
	subq.l #1, d0
	cmp.l d4, d3
	blt.w valueConstraintFail
	cmp.l d0, d3
	bgt.w valueConstraintFail
	bra.w valueLoop

valueRequireUnsignedBits
	tst.b d6
	beq.w valueFail
	tst.w d7
	beq.w valueFail
	moveq #0, d2
	move.b (a0)+, d2
	subq.w #1, d7
	tst.b d2
	beq.w valueFail
	cmpi.b #64, d2
	bhi.w valueFail
	tst.l d3
	bmi.w valueConstraintFail
	cmpi.b #31, d2
	bhs.w valueLoop
	moveq #1, d4
	lsl.l d2, d4
	subq.l #1, d4
	cmp.l d4, d3
	bhi.w valueConstraintFail
	bra.w valueLoop

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

valueEncodeUpperBoundAsZero
	move.l a3, d1
	cmpi.w #2, d1
	bne.w valueFail
	tst.b d6
	beq.w valueFail
	tst.w d7
	beq.w valueFail
	moveq #0, d2
	move.b (a0)+, d2
	subq.w #1, d7
	tst.b d2
	beq.w valueFail
	cmpi.b #62, d2
	bhi.w valueFail
	tst.l d3
	ble.w valueConstraintFail
	cmpi.b #31, d2
	bhs.w valueLoop
	moveq #1, d4
	lsl.l d2, d4
	cmp.l d4, d3
	bhi.w valueConstraintFail
	bne.w valueLoop
	moveq #0, d3
	bra.w valueLoop

valueEnd
	tst.w d7
	bne.w valueFail
	tst.b d6
	beq.w valueFail
	moveq #0, d0
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	bra.s valueReturn
.else
	rts
.endif
valueConstraintFail
	moveq #2, d0
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	bra.s valueReturn
.else
	rts
.endif
valueFail
	moveq #1, d0
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
valueReturn
	jsr runtime_profile.opforgeRuntimeProfileLeaveServiceV1
.endif
	rts
	.bend  ; tkpkgExecuteValueProgramBytesV2

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
	move.w d0, state.EncodeSelectedSemanticDiagnosticIndex
	bra.w compactPlanOk
compactPlanScalar
	bsr.w tkpkgServiceReadU16LeV1
	bne.w compactPlanFail
	bsr.w tkpkgServiceReadU16LeV1
	bne.w compactPlanFail
	move.w d0, state.EncodeSelectedSemanticDiagnosticIndex
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
	move.w d0, state.EncodeSelectedSemanticDiagnosticIndex
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

; Match Rust runtime_model_core's selector-shape expansion. The package string
; is split on `|`; surrounding ASCII space/tab is trimmed and empty alternatives
; are ignored before case-insensitive comparison with the requested shape.
; Inputs: A1/D0 = package shape string; A2/D1 = requested shape.
; Outputs: D0 = 1 match, 0 no match.
tkpkgServiceShapeAlternativeMatchesV1	.block
	movem.l d2-d7/a1-a6, -(sp)
	movea.l a1, a3
	move.w d0, d7
	movea.l a2, a4
	move.w d1, d6

shapeAlternativeNext
	tst.w d7
	beq.s shapeAlternativeNoMatch
shapeAlternativeTrimStart
	tst.w d7
	beq.s shapeAlternativeNoMatch
	cmpi.b #' ', (a3)
	beq.s shapeAlternativeSkipStart
	cmpi.b #9, (a3)
	bne.s shapeAlternativeStartReady
shapeAlternativeSkipStart
	addq.l #1, a3
	subq.w #1, d7
	bra.s shapeAlternativeTrimStart

shapeAlternativeStartReady
	cmpi.b #'|', (a3)
	bne.s shapeAlternativeScan
	addq.l #1, a3
	subq.w #1, d7
	bra.s shapeAlternativeNext

shapeAlternativeScan
	movea.l a3, a5
	move.w d7, d5
	moveq #0, d4
shapeAlternativeScanLoop
	cmp.w d5, d4
	beq.s shapeAlternativeEndReady
	cmpi.b #'|', 0(a5, d4.w)
	beq.s shapeAlternativeEndReady
	addq.w #1, d4
	bra.s shapeAlternativeScanLoop

shapeAlternativeEndReady
	move.w d4, d2
shapeAlternativeTrimEnd
	tst.w d2
	beq.s shapeAlternativeAdvance
	cmpi.b #' ', -1(a3, d2.w)
	beq.s shapeAlternativeTrimOne
	cmpi.b #9, -1(a3, d2.w)
	bne.s shapeAlternativeCompare
shapeAlternativeTrimOne
	subq.w #1, d2
	bra.s shapeAlternativeTrimEnd

shapeAlternativeCompare
	cmp.w d6, d2
	bne.s shapeAlternativeAdvance
	movea.l a3, a1
	movea.l a4, a2
	move.w d2, d0
	move.w d6, d1
	move.w d4, -(sp)
	bsr.w tkpkgServiceStringEqAsciiCasefoldV1
	move.w (sp)+, d4
	tst.b d0
	bne.s shapeAlternativeMatch

shapeAlternativeAdvance
	cmp.w d7, d4
	beq.s shapeAlternativeNoMatch
	addq.w #1, d4
	adda.w d4, a3
	sub.w d4, d7
	bra.s shapeAlternativeNext

shapeAlternativeMatch
	moveq #1, d0
	bra.s shapeAlternativeReturn
shapeAlternativeNoMatch
	moveq #0, d0
shapeAlternativeReturn
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; tkpkgServiceShapeAlternativeMatchesV1

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
