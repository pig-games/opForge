; Expression request preparation and execution for the tkpkg service facade.

	.module tkpkg.amigaos.expression_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.runtime_context as context
	.use opcore.amigaos.expr_bridge

TKPKG_EVAL_EXPR_REQUEST_FIXED_SIZE = 9
TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE = 16
TKPKG_EVAL_EXPR_EXTENSION_RESOLVER_INPUT_SIZE = 32
TKPKG_EVAL_EXPR_EXTENSION_RESULT_OFF = 16
EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN = 45
EVAL_EXPR_FAILED_TEXT_LEN = 36
EVAL_EXPR_VALUE_PREFIX_LEN = 6
EVAL_EXPR_MIN_I32_TEXT_LEN = 11
EVAL_EXPR_ZERO_OUTPUT_TEXT_LEN = 39
EVAL_EXPR_BRIDGE_CODE1_TEXT_LEN = 41
EVAL_EXPR_BRIDGE_CODE3_TEXT_LEN = 50
EVAL_EXPR_BRIDGE_CODE4_TEXT_LEN = 51
EVAL_EXPR_BRIDGE_CODE5_TEXT_LEN = 49
EVAL_EXPR_BRIDGE_CODE33_TEXT_LEN = 48
EVAL_EXPR_BRIDGE_CODE34_TEXT_LEN = 47
EVAL_EXPR_NO_LABEL_CONTEXT_TEXT_LEN = 49
	.section data, kind=data
	.priv

NeedsPipelineText
	.byte "OTR001: evaluate_expression requires pipeline", 0
FailedText
	.byte "OTR901: expression evaluation failed", 0
ZeroOutputText
	.byte "OTR904: evaluate expression returned no output", 0
BridgeCode1Text
	.byte "OTR920: expression bridge returned code 1", 0
BridgeCode3Text
	.byte "OTR923: expression bridge reported compile failure", 0
BridgeCode4Text
	.byte "OTR924: expression bridge reported finalize failure", 0
BridgeCode5Text
	.byte "OTR925: expression bridge reported exprvm failure", 0
BridgeCode33Text
	.byte "OTR921: expression bridge reported trailing text", 0
BridgeCode34Text
	.byte "OTR922: expression bridge reported missing term", 0
NoLabelContextText
	.byte "OTR930: evaluate expression had no label context", 0
ValuePrefixText
	.byte "VALUE ", 0
MinI32Text
	.byte "-2147483648", 0
DecimalPowers
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

PreparedOperandPtr
	.res long, 1
PreparedOperandLen
	.res long, 1
PreparedLabelNamePtr
	.res long, 1
PreparedLabelValuePtr
	.res long, 1
PreparedLabelCount
	.res long, 1
PreparedCurrentPc
	.res long, 1
PreparedExtensionPtr
	.res long, 1
PreparedSymbolResolverPtr
	.res long, 1
PreparedFlags
	.res word, 1
DecimalBuffer
	.res byte, 16

	.endsection

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Validate and retain one expression request envelope for later execution.
;
; Inputs:
; - A0: validated service control block.
;
; Outputs:
; - D0: 0 on prepared success, ABI status on failure.
; - D1/A1: diagnostic length/text on runtime failure.
;
; Clobbers:
; - D0-D7/A1-A5/CCR.
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
prepareV1	.block
	btst #1, buffers.PackageStateFlags
	bne.s havePipeline
	lea NeedsPipelineText, a1
	moveq #EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

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
	movea.l a4, a1
	adda.w d0, a1
	move.l a1, PreparedOperandPtr
	move.l d4, d1
	sub.l d2, d1
	move.l d1, PreparedOperandLen
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
	clr.w PreparedFlags
	clr.l PreparedLabelNamePtr
	clr.l PreparedLabelValuePtr
	clr.l PreparedLabelCount
	clr.l PreparedCurrentPc
	clr.l PreparedExtensionPtr
	clr.l PreparedSymbolResolverPtr
	cmpi.w #TKPKG_EVAL_EXPR_EXTENSION_INPUT_SIZE, d1
	bcs.s prepared
	lea 0(a0, d0.W), a5
	move.l a5, PreparedExtensionPtr
	movea.l (a5)+, a1
	movea.l (a5)+, a2
	move.l (a5)+, d2
	move.l (a5)+, d3
	move.l a1, PreparedLabelNamePtr
	move.l a2, PreparedLabelValuePtr
	move.l d2, PreparedLabelCount
	move.l d3, PreparedCurrentPc
	bset #0, PreparedFlags
	cmpi.w #TKPKG_EVAL_EXPR_EXTENSION_RESOLVER_INPUT_SIZE, d1
	bcs.s prepared
	move.l 12(a5), PreparedSymbolResolverPtr

prepared
	moveq #0, d0
	moveq #0, d1
	rts

badPayload
	moveq #abi.STATUS_BAD_REQUEST_V1, d0
	moveq #0, d1
	tst.b d0
	rts
	.bend  ; prepareV1

; ---------------------------------------------------------------------------
; Execute the expression request prepared by `prepareV1`.
;
; Inputs:
; - the most recent successful `prepareV1` state.
;
; Outputs:
; - D0: 0 on success, ABI status on failure.
; - D1/A1: output length or diagnostic length/text.
;
; Clobbers:
; - D0-D7/A0-A6/CCR.
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
executePreparedV1	.block
	btst #0, PreparedFlags
	bne.s haveLabelContext
	lea NoLabelContextText, a1
	moveq #EVAL_EXPR_NO_LABEL_CONTEXT_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

haveLabelContext
	jsr context.getPassV1
	move.l d0, d6
	move.l PreparedLabelCount, d0
	jsr context.getSymbolStabilityTableV1
	tst.b d0
	bne.s missingContext
	movea.l a0, a6
	movea.l PreparedOperandPtr, a0
	move.l PreparedOperandLen, d0
	movea.l PreparedLabelNamePtr, a1
	movea.l PreparedLabelValuePtr, a2
	move.l PreparedLabelCount, d1
	move.l PreparedCurrentPc, d2
	tst.l PreparedSymbolResolverPtr
	beq.s evaluateSnapshotOnly
	movea.l PreparedSymbolResolverPtr, a5
	jsr expr_bridge.opcoreExvmEvalOperandWithResolverV1
	bra.s evaluateDone
evaluateSnapshotOnly
	jsr expr_bridge.opcoreExvmEvalOperandV1
evaluateDone
	tst.b d0
	bne.s bridgeFail
	movea.l PreparedExtensionPtr, a5
	move.l d3, TKPKG_EVAL_EXPR_EXTENSION_RESULT_OFF(a5)
	bsr.w writeValueOutputV1
	tst.w d1
	bne.s ok
	lea ZeroOutputText, a1
	moveq #EVAL_EXPR_ZERO_OUTPUT_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

ok
	moveq #0, d0
	rts

missingContext
	lea NoLabelContextText, a1
	moveq #EVAL_EXPR_NO_LABEL_CONTEXT_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

bridgeFail
	cmpi.b #5, d0
	beq.s bridgeFail5
	cmpi.b #4, d0
	beq.s bridgeFail4
	cmpi.b #3, d0
	beq.s bridgeFail3
	cmpi.b #33, d0
	beq.s bridgeFail33
	cmpi.b #34, d0
	beq.s bridgeFail34
	cmpi.b #1, d0
	beq.s bridgeFail1
	lea FailedText, a1
	moveq #EVAL_EXPR_FAILED_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

bridgeFail1
	lea BridgeCode1Text, a1
	moveq #EVAL_EXPR_BRIDGE_CODE1_TEXT_LEN, d1
	bra.s runtimeError
bridgeFail3
	lea BridgeCode3Text, a1
	moveq #EVAL_EXPR_BRIDGE_CODE3_TEXT_LEN, d1
	bra.s runtimeError
bridgeFail4
	lea BridgeCode4Text, a1
	moveq #EVAL_EXPR_BRIDGE_CODE4_TEXT_LEN, d1
	bra.s runtimeError
bridgeFail5
	lea BridgeCode5Text, a1
	moveq #EVAL_EXPR_BRIDGE_CODE5_TEXT_LEN, d1
	bra.s runtimeError
bridgeFail33
	lea BridgeCode33Text, a1
	moveq #EVAL_EXPR_BRIDGE_CODE33_TEXT_LEN, d1
	bra.s runtimeError
bridgeFail34
	lea BridgeCode34Text, a1
	moveq #EVAL_EXPR_BRIDGE_CODE34_TEXT_LEN, d1

runtimeError
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts
	.bend  ; executePreparedV1

	.priv

writeValueOutputV1	.block
	movem.l d0/d2-d7/a0-a2, -(sp)
	lea buffers.LastErrorBuffer, a2
	lea ValuePrefixText, a1
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
	lea MinI32Text, a1
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
	lea DecimalBuffer, a1
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
	.bend  ; writeValueOutputV1

appendUnsignedDecimalV1	.block
	lea DecimalPowers, a1
	lea DecimalBuffer, a0
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

	.endsection
	.endmodule
