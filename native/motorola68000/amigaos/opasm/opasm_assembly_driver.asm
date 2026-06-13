; Native opasm assembly-session driver.

	.module opasm.amigaos.assembly_driver
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.engine as eng
	.use opasm.amigaos.events
	.use opasm.amigaos.tkpkg_bridge as tkpkg

	.section code, kind=code
	.pub

; Run one native opasm assembly session.
;
; Inputs:
; - A0: abi.OPASM_ASSEMBLE_REQ_* frame.
;
; Outputs:
; - D0: current opasm engine status.
; - A0: original request frame pointer.
assembleSessionV1	.block
	movem.l a1-a2/a4, -(sp)
	movea.l a0, a1
	move.l a1, OpasmActiveAssembleReqPtr
	tst.l abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a1)
	beq.s buildContext
	movea.l abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a1), a0
	clr.w (a0)

buildContext
	suba.l #eng.OPASM_ENGINE_CALLBACK_REQ_BYTES, sp
	movea.l sp, a0
	move.l abi.OPASM_ASSEMBLE_REQ_BIN_REQUESTED_PTR(a1), eng.OPASM_ENGINE_CALLBACK_REQ_BIN_REQUESTED_PTR(a0)
	move.l #opasmDriverPassOneBegin, eng.OPASM_ENGINE_CALLBACK_REQ_PASS1_BEGIN_CB(a0)
	move.l #opasmDriverPassTwoBegin, eng.OPASM_ENGINE_CALLBACK_REQ_PASS2_BEGIN_CB(a0)
	move.l #opasmDriverPassOneOk, eng.OPASM_ENGINE_CALLBACK_REQ_PASS1_OK_CB(a0)
	move.l #opasmDriverPassTwoOk, eng.OPASM_ENGINE_CALLBACK_REQ_PASS2_OK_CB(a0)
	move.l #opasmDriverRecordLabel, eng.OPASM_ENGINE_CALLBACK_REQ_RECORD_LABEL_CB(a0)
	move.l #opasmDriverAdvancePc, eng.OPASM_ENGINE_CALLBACK_REQ_ADVANCE_PC_CB(a0)
	move.l #opasmDriverEmitImageBytes, eng.OPASM_ENGINE_CALLBACK_REQ_EMIT_IMAGE_CB(a0)
	jsr eng.opasmEngineBuildCallbackContextV1
	adda.l #eng.OPASM_ENGINE_CALLBACK_REQ_BYTES, sp
	jsr eng.opasmEngineRunTwoPassV1
	movea.l a1, a0
	movem.l (sp)+, a1-a2/a4
	rts
	.bend  ; assembleSessionV1

	.priv

opasmDriverPassOneBegin	.block
	moveq #abi.OPASM_EVENT_PASS_BEGIN, d0
	moveq #1, d1
	bsr.w appendPassEvent
	jsr eng.opasmEngineBeginPassOneV1
	rts
	.bend  ; opasmDriverPassOneBegin

opasmDriverPassOneOk	.block
	moveq #abi.OPASM_EVENT_PASS_OK, d0
	moveq #1, d1
	bsr.w appendPassEvent
	moveq #0, d0
	rts
	.bend  ; opasmDriverPassOneOk

opasmDriverPassTwoBegin	.block
	moveq #abi.OPASM_EVENT_PASS_BEGIN, d0
	moveq #2, d1
	bsr.w appendPassEvent
	jsr eng.opasmEngineBeginPassTwoV1
	rts
	.bend  ; opasmDriverPassTwoBegin

opasmDriverPassTwoOk	.block
	moveq #abi.OPASM_EVENT_PASS_OK, d0
	moveq #2, d1
	bsr.w appendPassEvent
	moveq #0, d0
	rts
	.bend  ; opasmDriverPassTwoOk

opasmDriverRecordLabel	.block
	movem.l d1-d5/a0, -(sp)
	jsr eng.opasmEngineRecordStatementLabelV1
	move.l a0, d4
	move.l d2, d5
	cmpi.w #eng.OPASM_ENGINE_LABEL_EVENT_STORED, d1
	beq.s stored
	cmpi.w #eng.OPASM_ENGINE_LABEL_EVENT_DUPLICATE, d1
	beq.s duplicate
	bra.s return

stored
	movea.l d4, a0
	bsr.w tokenLen
	move.w d0, d1
	movea.l d4, a0
	move.l d5, d2
	moveq #abi.OPASM_EVENT_LABEL_STORED, d0
	bsr.w appendTextValueEvent
	moveq #0, d0
	bra.s return

duplicate
	movea.l d4, a0
	bsr.w tokenLen
	move.w d0, d1
	movea.l d4, a0
	moveq #abi.OPASM_EVENT_LABEL_DUPLICATE, d0
	bsr.w appendTextEvent
	moveq #1, d0

return
	movem.l (sp)+, d1-d5/a0
	rts
	.bend  ; opasmDriverRecordLabel

opasmDriverEmitImageBytes	.block
	movem.l d1-d6/a0-a4, -(sp)
	move.w d0, d6
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d6, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w ok
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), a1
	move.l a1, d5
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a0), d4
	beq.w ok
	moveq #0, d0
	move.w d6, d0
	move.w d4, d1
	movea.l d5, a0
	jsr eng.opasmEngineStatementMnemonicDuplicatesLabelV1
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea OrgMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea CpuMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea EndMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea RegionMnemonicText, a1
	moveq #6, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea SectionMnemonicText, a1
	moveq #7, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea EndsectionMnemonicText, a1
	moveq #10, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea PlaceMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea AlignMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w emitAlign
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea DsMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	bne.w emitDs
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea ResMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea FillMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w emitFill
	bsr.w prepareEncodeSelectedRequestForStatement
	bne.w return
	tst.w OpasmDriverEvalRequestLen
	beq.w ok
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEncodeSelectedV1
	move.w d2, d4
	tst.b d0
	bne.w serviceFail
	tst.w d1
	beq.w noOutput
	move.w d1, d6
	bsr.w serviceFramePtr
	jsr tkpkg.readOutputPtrV1
	moveq #0, d0
	move.w d6, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w fail
	moveq #abi.OPASM_EVENT_SELECTOR_STATUS_OK, d0
	bsr.w appendKindEvent

ok
	moveq #0, d0
	bra.w return

fail
	moveq #abi.OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED, d0
	suba.l a0, a0
	moveq #0, d1
	move.l d6, d2
	bsr.w appendTextValueEvent
	moveq #1, d0
	bra.w return

serviceFail
	moveq #0, d0
	move.w d6, d0
	jsr eng.opasmEngineStatementLooksBareColumnOneV1
	bne.w ok
	tst.w d4
	beq.s serviceFailReturn
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	clr.b 0(a0, d4.W)
	bsr.w emitSelectorDiagnostic
	bne.s serviceFailReturn
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	move.w d4, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextEvent

serviceFailReturn
	moveq #1, d0
	bra.s return

noOutput
	moveq #abi.OPASM_EVENT_UNSUPPORTED_ADDRESSING, d0
	bsr.w appendKindEvent
	moveq #1, d0
	bra.s return

emitAlign
	move.w d6, d7
	bsr.w readAlignPadForStatement
	bne.s emitLayoutFail
	move.l d3, d0
	moveq #0, d1
	bsr.w appendRepeatedByte
	bne.s emitLayoutFail
	moveq #0, d0
	bra.s return

emitDs
	move.w d6, d7
	moveq #2, d5
	bsr.w readOperandValueForStatement
	bne.s emitLayoutFail
	move.l d3, d0
	moveq #0, d1
	bsr.w appendRepeatedByte
	bne.s emitLayoutFail
	moveq #0, d0
	bra.s return

emitFill
	move.w d6, d7
	moveq #2, d6
	bsr.w readCommaOperandValueForStatement
	bne.s emitLayoutFail
	move.l d3, d5
	move.w d7, d6
	moveq #3, d6
	bsr.w readCommaOperandValueForStatement
	bne.s emitLayoutFail
	move.l d5, d0
	move.b d3, d1
	bsr.w appendRepeatedByte
	bne.s emitLayoutFail
	moveq #0, d0
	bra.s return

emitLayoutFail
	moveq #abi.OPASM_EVENT_UNRESOLVED_LABEL, d0
	bsr.w appendKindEvent
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d6/a0-a4
	rts
	.bend  ; opasmDriverEmitImageBytes

; Advance the current PC by the size inferred for the selected statement text.
; Inputs: D0 = statement index.
; Outputs: D0 = 0 on success, nonzero on selector or evaluation failure.
; Clobbers: D1-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
opasmDriverAdvancePc	.block
	movem.l d0-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	move.l d0, d7
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w done
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), a1
	move.l a1, d5
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a0), d6
	moveq #0, d4
	move.w d7, d4
	add.w d4, d4
	tst.w d6
	beq.w done
	moveq #0, d0
	move.w d7, d0
	move.w d6, d1
	movea.l d5, a0
	jsr eng.opasmEngineStatementMnemonicDuplicatesLabelV1
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea OrgMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w org
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea CpuMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea EndMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea RegionMnemonicText, a1
	moveq #6, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea SectionMnemonicText, a1
	moveq #7, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea EndsectionMnemonicText, a1
	moveq #10, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea PlaceMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea AlignMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w align
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea DsMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	bne.w ds
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea ResMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w res
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea FillMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w fill
	moveq #0, d0
	move.w d7, d0
	bsr.w trySelectedEncodeSizeForStatement
	beq.s selectedSizeOk
	move.w d7, d0
	jsr eng.opasmEngineStatementLooksBareColumnOneV1
	bne.w done
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

selectedSizeOk
	cmpi.w #1, d1
	beq.w advanceOne
	cmpi.w #2, d1
	beq.w advanceTwo
	cmpi.w #3, d1
	beq.w advanceThree
	bra.w done

org
	moveq #2, d5
	bsr.w readOperandValueForStatement
	beq.s orgOk

orgBad
	moveq #abi.OPASM_EVENT_BAD_ORG, d0
	bsr.w appendKindEvent
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

orgOk
	move.l d3, d0
	jsr eng.opasmEngineSetOriginV1
	bra.w done

align
	bsr.w readAlignPadForStatement
	beq.s advanceLayoutD3
	bra.s orgBad

ds
	moveq #2, d5
	bsr.w readOperandValueForStatement
	beq.s advanceLayoutD3
	bra.s orgBad

res
	moveq #2, d6
	bsr.w readCommaOperandValueForStatement
	beq.s advanceLayoutD3
	bra.s orgBad

fill
	moveq #2, d6
	bsr.w readCommaOperandValueForStatement
	bne.s orgBad

advanceLayoutD3
	move.l d3, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

advanceOne
	moveq #1, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

advanceTwo
	moveq #2, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

advanceThree
	moveq #3, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

done
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #0, d0
	rts
	.bend  ; opasmDriverAdvancePc

trySelectedEncodeSizeForStatement	.block
	movem.l d2-d7/a0-a2, -(sp)
	move.w d0, d6
	clr.w d4
	bsr.w prepareEncodeSelectedRequestForStatement
	bne.w prepareFail
	tst.w OpasmDriverEvalRequestLen
	beq.w empty
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEncodeSelectedV1
	move.w d2, d4
	tst.b d0
	bne.w fail
	moveq #0, d0
	bra.s return

empty
	moveq #0, d1
	moveq #0, d0
	bra.s return

prepareFail

fail
	tst.w d4
	beq.s failReturn
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	clr.b 0(a0, d4.W)
	bsr.w emitSelectorDiagnostic
	bne.s failReturn
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	move.w d4, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextEvent

failReturn
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0-a2
	rts
	.bend  ; trySelectedEncodeSizeForStatement

; Inputs:
;   D7.W = statement index
;   D5.B = operand width policy used for range checks
; Outputs:
;   D0.L = 0 on success, 1 when the operand cannot be resolved or fails width checks
;   D3.L = resolved operand value on success
; Clobbers:
;   D0-D7/A0-A2/CCR
; CCR:
;   Reflects D0.L on return
readOperandValueForStatement	.block
	movem.l d1-d2/d4-d7/a0-a2, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	jsr eng.statementHasExprMetadataV1
	clr.w d6
	bra.w storedText

loadExprSlice
	suba.l #eng.OPASM_ENGINE_EXPR_META_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementExprMetadataV1
	tst.l d0
	beq.s exprSliceFail
	move.l eng.OPASM_ENGINE_EXPR_META_SPAN_START(a0), d1
	move.l eng.OPASM_ENGINE_EXPR_META_SPAN_END(a0), d2
	adda.l #eng.OPASM_ENGINE_EXPR_META_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementExprTextSliceV1
	tst.l d0
	bne.s haveText
	bra.w fail

exprSliceFail
	adda.l #eng.OPASM_ENGINE_EXPR_META_BYTES, sp
	bra.w fail

storedText
	clr.l d3
	moveq #0, d0
	move.w d7, d0
	jsr eng.getStatementSourceLineTextV1
	tst.l d0
	beq.w fail
	bsr.w skipLineWhitespace
	bsr.w skipSourceHeadToken
	bsr.w skipLineWhitespace
	tst.l d0
	bne.s haveText
	bra.w fail

haveText
	tst.w d6
	bne.s prepareRequest
	bsr.w skipLineWhitespace
	bne.s prepareRequest
	bra.w fail

prepareRequest
	move.l a0, OpasmDriverEvalFallbackPtr
	move.l d0, OpasmDriverEvalFallbackLen
	bsr.w parseDirectiveLiteralValue
	beq.s checkWidth
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w prepareEvaluateExpressionRequest
	beq.s prepareExtension
	bra.s evalFallback

prepareExtension
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEvaluateExpressionV1
	beq.s readValue
	bra.s evalFallback

readValue
	bsr.w readEvaluateExpressionValue
	tst.l d3
	bne.s checkWidth
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w parseDirectiveLiteralValue
	bne.s fail

checkWidth
	cmpi.b #1, d5
	bne.s ok
	cmpi.l #$000000FF, d3
	bls.s ok

fail
	moveq #abi.OPASM_EVENT_UNRESOLVED_LABEL, d0
	bsr.w appendKindEvent
	moveq #1, d0
	bra.s return

ok
	moveq #0, d0
	bra.s return

evalFallback
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w parseDirectiveLiteralValue
	beq.s ok

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	rts
	.bend  ; readOperandValueForStatement

; Evaluate a comma-separated directive operand part.
; Inputs: D7.W = statement index; D6.W = one-based operand part number.
; Outputs: D0.L = 0 on success, 1 on parse/evaluation failure; D3.L = value.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
readCommaOperandValueForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	moveq #0, d0
	move.w d7, d0
	jsr eng.getStatementSourceLineTextV1
	tst.l d0
	beq.w fail
	bsr.w skipLineWhitespace
	bsr.w skipSourceHeadToken
	bsr.w skipLineWhitespace
	tst.l d0
	beq.w fail
	movea.l a0, a2
	move.l d0, d2

	moveq #1, d4

partStart
	bsr.w skipPartWhitespace
	movea.l a2, a3
	moveq #0, d5

partScan
	tst.l d2
	beq.s partEnd
	move.b (a2), d0
	cmpi.b #',', d0
	beq.s partEnd
	addq.l #1, a2
	subq.l #1, d2
	addq.l #1, d5
	bra.s partScan

partEnd
	cmp.w d6, d4
	beq.s evaluatePart
	tst.l d2
	beq.s fail
	addq.l #1, a2
	subq.l #1, d2
	addq.w #1, d4
	bra.s partStart

evaluatePart
	movea.l a3, a0
	move.l d5, d0
	bsr.w trimPartTrailing
	move.l a0, OpasmDriverEvalFallbackPtr
	move.l d0, OpasmDriverEvalFallbackLen
	beq.s fail
	bsr.w parseDirectiveLiteralValue
	beq.s evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w prepareEvaluateExpressionRequest
	bne.s evalPartFallback
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEvaluateExpressionV1
	bne.s evalPartFallback
	bsr.w readEvaluateExpressionValue
	tst.l d3
	bne.s evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w parseDirectiveLiteralValue
	bne.s fail

evalPartOk
	moveq #0, d0
	bra.s return

evalPartFallback
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w parseDirectiveLiteralValue
	beq.s return

fail
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; readCommaOperandValueForStatement

; Compute Rust-compatible `.align` padding for the current native PC.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on invalid expression/boundary; D3.L = pad.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0.L on return.
readAlignPadForStatement	.block
	movem.l d1-d2/d4-d7/a0-a2, -(sp)
	moveq #2, d5
	bsr.w readOperandValueForStatement
	bne.s fail
	move.l d3, d4
	beq.s fail
	move.l d4, d0
	subq.l #1, d0
	move.l d0, d5
	and.l d4, d0
	bne.s fail
	jsr eng.opasmEngineGetSessionCurrentPcV1
	and.l d5, d0
	beq.s aligned
	move.l d4, d3
	sub.l d0, d3
	bra.s ok

aligned
	clr.l d3

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	rts
	.bend  ; readAlignPadForStatement

skipPartWhitespace	.block
loop
	tst.l d2
	beq.s done
	move.b (a2), d0
	cmpi.b #' ', d0
	beq.s skip
	cmpi.b #9, d0
	beq.s skip
	bra.s done

skip
	addq.l #1, a2
	subq.l #1, d2
	bra.s loop

done
	rts
	.bend  ; skipPartWhitespace

skipSourceHeadToken	.block
loop
	tst.l d0
	beq.s done
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s done
	cmpi.b #9, d1
	beq.s done
	addq.l #1, a0
	subq.l #1, d0
	bra.s loop

done
	rts
	.bend  ; skipSourceHeadToken

trimPartTrailing	.block
	tst.l d0
	beq.s done
	movea.l a0, a1
	adda.l d0, a1

loop
	tst.l d0
	beq.s done
	move.b -(a1), d1
	cmpi.b #' ', d1
	beq.s trim
	cmpi.b #9, d1
	beq.s trim
	bra.s done

trim
	subq.l #1, d0
	bra.s loop

done
	rts
	.bend  ; trimPartTrailing

; Parse a simple directive literal used as a fallback after package eval.
; Inputs: A0/D0 = text slice (`$hex` or decimal).
; Outputs: D0.L = 0 on success, 1 on parse failure; D3.L = value.
; Clobbers: D0-D4/A0-A1/CCR.
; CCR: reflects D0.L on return.
parseDirectiveLiteralValue	.block
	movem.l d1-d2/d4/a0-a1, -(sp)
	bsr.w skipLineWhitespace
	bsr.w trimLiteralFallbackTrailing
	tst.l d0
	beq.s fail
	clr.l d3
	move.l d0, d4
	move.b (a0), d1
	cmpi.b #'$', d1
	beq.s hexPrefix
	bra.s decimalLoop

hexPrefix
	addq.l #1, a0
	subq.l #1, d4
	beq.s fail

hexLoop
	tst.l d4
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w hexNibbleValue
	bmi.s fail
	lsl.l #4, d3
	or.l d1, d3
	subq.l #1, d4
	bra.s hexLoop

decimalLoop
	tst.l d4
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #'0', d1
	blo.s fail
	cmpi.b #'9', d1
	bhi.s fail
	subi.b #'0', d1
	move.l d3, d2
	lsl.l #3, d3
	add.l d2, d3
	add.l d2, d3
	add.l d1, d3
	subq.l #1, d4
	bra.s decimalLoop

ok
	movem.l (sp)+, d1-d2/d4/a0-a1
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d1-d2/d4/a0-a1
	moveq #1, d0
	rts
	.bend  ; parseDirectiveLiteralValue

trimLiteralFallbackTrailing	.block
	tst.l d0
	beq.s done
	movea.l a0, a1
	adda.l d0, a1

loop
	tst.l d0
	beq.s done
	move.b -(a1), d1
	cmpi.b #' ', d1
	beq.s trim
	cmpi.b #9, d1
	beq.s trim
	bra.s done

trim
	subq.l #1, d0
	bra.s loop

done
	rts
	.bend  ; trimLiteralFallbackTrailing

hexNibbleValue	.block
	cmpi.b #'0', d1
	blo.s fail
	cmpi.b #'9', d1
	bls.s decimal
	cmpi.b #'A', d1
	blo.s lower
	cmpi.b #'F', d1
	bls.s upper

lower
	cmpi.b #'a', d1
	blo.s fail
	cmpi.b #'f', d1
	bhi.s fail
	subi.b #'a' - 10, d1
	moveq #0, d0
	rts

upper
	subi.b #'A' - 10, d1
	moveq #0, d0
	rts

decimal
	subi.b #'0', d1
	moveq #0, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; hexNibbleValue

; Append one byte value repeatedly to the current image stream.
; Inputs: D0.L = count; D1.B = byte value.
; Outputs: D0.L = 0 on success, 1 on image capacity failure.
; Clobbers: D0-D3/A0/CCR.
; CCR: reflects D0.L on return.
appendRepeatedByte	.block
	movem.l d1-d3/a0, -(sp)
	move.l d0, d2
	move.b d1, d3
	subq.l #2, sp
	move.b d3, (sp)

loop
	tst.l d2
	beq.s ok
	movea.l sp, a0
	moveq #1, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.s fail
	subq.l #1, d2
	bra.s loop

ok
	addq.l #2, sp
	movem.l (sp)+, d1-d3/a0
	moveq #0, d0
	rts

fail
	addq.l #2, sp
	movem.l (sp)+, d1-d3/a0
	moveq #1, d0
	rts
	.bend  ; appendRepeatedByte

; Inputs:
;   D6.W = statement index
; Outputs:
;   D0.L = 0 on success, 1 when selector request preparation fails
;   OpasmDriverEvalRequestLen updated from D1 on success
; Clobbers:
;   D0-D1/D6/A0-A1/CCR
; CCR:
;   Reflects D0.L on return
prepareEncodeSelectedRequestForStatement	.block
	movem.l d1/d6/a1, -(sp)
	clr.w OpasmDriverEvalRequestLen
	moveq #0, d0
	move.w d6, d0
	bsr.w serviceIoBufferPtr
	movea.l a0, a1
	jsr eng.prepareSelectedEvaluateRequestV1
	bne.s return
	move.w d1, OpasmDriverEvalRequestLen
	tst.l d0

return
	movem.l (sp)+, d1/d6/a1
	rts
	.bend  ; prepareEncodeSelectedRequestForStatement

; Inputs:
;   A0 = expression text pointer
;   D0.L = expression text length
;   D7.W = statement index
; Outputs:
;   D0.L = 0 on success, 1 when expression request preparation fails
;   OpasmDriverEvalRequestLen updated from D1 on success
; Clobbers:
;   D0-D1/D7/A0-A2/CCR
; CCR:
;   Reflects D0.L on return
prepareEvaluateExpressionRequest	.block
	movem.l d1/a0-a2, -(sp)
	movea.l a0, a2
	clr.w OpasmDriverEvalRequestLen
	bsr.w serviceIoBufferPtr
	movea.l a0, a1
	movea.l a2, a0
	move.w d7, d1
	jsr eng.prepareEvaluateExpressionRequestV1
	bne.s return
	move.w d1, OpasmDriverEvalRequestLen
	tst.l d0

return
	movem.l (sp)+, d1/a0-a2
	rts
	.bend  ; prepareEvaluateExpressionRequest

prepareEvaluateExpressionExtension	.block
	movem.l d0-d1/a0-a2, -(sp)
	bsr.w serviceIoBufferPtr
	moveq #0, d0
	move.w OpasmDriverEvalRequestLen, d0
	movea.l a0, a2
	bsr.w serviceEvalExtensionPtr
	movea.l a0, a1
	movea.l a2, a0
	jsr eng.prepareEvaluateExpressionExtensionV1
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; prepareEvaluateExpressionExtension

readEvaluateExpressionValue	.block
	movem.l a0, -(sp)
	bsr.w serviceEvalExtensionPtr
	move.l 16(a0), d3
	movem.l (sp)+, a0
	rts
	.bend  ; readEvaluateExpressionValue

serviceFramePtr	.block
	movea.l OpasmActiveAssembleReqPtr, a0
	movea.l abi.OPASM_ASSEMBLE_REQ_SERVICE_FRAME_PTR(a0), a0
	rts
	.bend  ; serviceFramePtr

serviceIoBufferPtr	.block
	bsr.w serviceFramePtr
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a0
	rts
	.bend  ; serviceIoBufferPtr

serviceEvalExtensionPtr	.block
	bsr.w serviceFramePtr
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a0
	rts
	.bend  ; serviceEvalExtensionPtr

emitSelectorDiagnostic	.block
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	lea DriverSelectorUnknownRawText, a1
	bsr.w tokenEquals
	bne.s unknownMnemonic
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	lea DriverSelectorUnsupportedRawText, a1
	bsr.w tokenEquals
	bne.s unsupportedAddressing
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	lea DriverSelectorOperandRawText, a1
	bsr.w tokenEquals
	bne.s operandError
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	lea DriverSelectedOperandCompileRawText, a1
	bsr.w tokenEquals
	bne.s operandError
	moveq #0, d0
	rts

unknownMnemonic
	moveq #abi.OPASM_EVENT_UNKNOWN_MNEMONIC, d0
	bsr.w appendKindEvent
	moveq #1, d0
	rts

unsupportedAddressing
	moveq #abi.OPASM_EVENT_UNSUPPORTED_ADDRESSING, d0
	bsr.w appendKindEvent
	moveq #1, d0
	rts

operandError
	moveq #abi.OPASM_EVENT_UNRESOLVED_LABEL, d0
	bsr.w appendKindEvent
	moveq #1, d0
	rts
	.bend  ; emitSelectorDiagnostic

appendKindEvent	.block
	movem.l d1/a0-a2, -(sp)
	suba.l #abi.OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w clearEventFrame
	move.w d0, abi.OPASM_EVENT_KIND(a0)
	movea.l a0, a2
	bsr.w appendEventFrame
	adda.l #abi.OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d1/a0-a2
	rts
	.bend  ; appendKindEvent

appendPassEvent	.block
	movem.l d2/a0-a2, -(sp)
	suba.l #abi.OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w clearEventFrame
	move.w d0, abi.OPASM_EVENT_KIND(a0)
	move.w d1, abi.OPASM_EVENT_PASS(a0)
	movea.l a0, a2
	bsr.w appendEventFrame
	adda.l #abi.OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d2/a0-a2
	rts
	.bend  ; appendPassEvent

appendTextEvent	.block
	movem.l d2/a0-a2, -(sp)
	movea.l a0, a1
	suba.l #abi.OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w clearEventFrame
	move.w d0, abi.OPASM_EVENT_KIND(a0)
	move.l a1, abi.OPASM_EVENT_TEXT_PTR(a0)
	move.w d1, abi.OPASM_EVENT_TEXT_LEN(a0)
	movea.l a0, a2
	bsr.w appendEventFrame
	adda.l #abi.OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d2/a0-a2
	rts
	.bend  ; appendTextEvent

appendTextValueEvent	.block
	movem.l d3/a0-a2, -(sp)
	movea.l a0, a1
	suba.l #abi.OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w clearEventFrame
	move.w d0, abi.OPASM_EVENT_KIND(a0)
	move.l a1, abi.OPASM_EVENT_TEXT_PTR(a0)
	move.w d1, abi.OPASM_EVENT_TEXT_LEN(a0)
	move.l d2, abi.OPASM_EVENT_VALUE(a0)
	movea.l a0, a2
	bsr.w appendEventFrame
	adda.l #abi.OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d3/a0-a2
	rts
	.bend  ; appendTextValueEvent

appendEventFrame	.block
	movem.l d0-d1/a0-a1/a3, -(sp)
	movea.l OpasmActiveAssembleReqPtr, a3
	move.l a3, d0
	beq.s ok
	tst.l abi.OPASM_ASSEMBLE_REQ_EVENT_BUFFER_PTR(a3)
	beq.s ok
	tst.l abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a3)
	beq.s ok
	move.w abi.OPASM_ASSEMBLE_REQ_EVENT_CAPACITY(a3), d0
	beq.s ok
	movea.l abi.OPASM_ASSEMBLE_REQ_EVENT_BUFFER_PTR(a3), a0
	movea.l abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a3), a1
	jsr events.appendV1

ok
	movem.l (sp)+, d0-d1/a0-a1/a3
	rts
	.bend  ; appendEventFrame

clearEventFrame	.block
	movem.l d0-d1/a0, -(sp)
	moveq #abi.OPASM_EVENT_BYTES - 1, d1

loop
	clr.b (a0)+
	dbf d1, loop
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; clearEventFrame

tokenLen	.block
	movem.l d1/a0, -(sp)
	moveq #0, d0

loop
	move.b (a0)+, d1
	beq.s done
	addq.w #1, d0
	bra.s loop

done
	movem.l (sp)+, d1/a0
	rts
	.bend  ; tokenLen

tokenEquals	.block
	movem.l d1-d2/a0-a1, -(sp)

loop
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmp.b d2, d1
	bne.s no
	tst.b d1
	beq.s yes
	bra.s loop

yes
	movem.l (sp)+, d1-d2/a0-a1
	moveq #1, d0
	rts

no
	movem.l (sp)+, d1-d2/a0-a1
	moveq #0, d0
	rts
	.bend  ; tokenEquals

lineStartsWith	.block
	movem.l d2-d4/a0-a3, -(sp)
	tst.l d0
	beq.s no
	cmpi.b #'.', (a0)
	bne.s compareStart
	addq.l #1, a0
	subq.l #1, d0

compareStart
	cmp.l d1, d0
	bcs.s no
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d2
	beq.s boundary
	subq.l #1, d2

loop
	move.b (a2)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	addi.b #32, d3

compare
	cmp.b d4, d3
	bne.s no
	dbra d2, loop

boundary
	cmp.l d1, d0
	beq.s yes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.s yes
	cmpi.b #9, d3
	beq.s yes
	cmpi.b #';', d3
	beq.s yes
	moveq #0, d0
	movem.l (sp)+, d2-d4/a0-a3
	rts

yes
	movem.l (sp)+, d2-d4/a0-a3
	moveq #1, d0
	rts

no
	movem.l (sp)+, d2-d4/a0-a3
	moveq #0, d0
	rts
	.bend  ; lineStartsWith

; Inputs:
; - A0/D0: source-line pointer and remaining length.
;
; Outputs:
; - A0/D0: advanced pointer and remaining length after skipping leading spaces/tabs.
;
; Clobbers:
; - D0-D1/A0/CCR
;
; CCR:
; - Reflects D0 on return.
skipLineWhitespace	.block
loop
	tst.l d0
	beq.s done
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s skip
	cmpi.b #9, d1
	beq.s skip
	bra.s done

skip
	addq.l #1, a0
	subq.l #1, d0
	bra.s loop

done
	tst.l d0
	rts
	.bend  ; skipLineWhitespace

	.endsection

	.section data, kind=data

OrgMnemonicText
	.byte "org", 0

CpuMnemonicText
	.byte "cpu", 0

EndMnemonicText
	.byte "end", 0

RegionMnemonicText
	.byte "region", 0

SectionMnemonicText
	.byte "section", 0

EndsectionMnemonicText
	.byte "endsection", 0

PlaceMnemonicText
	.byte "place", 0

AlignMnemonicText
	.byte "align", 0

DsMnemonicText
	.byte "ds", 0

ResMnemonicText
	.byte "res", 0

FillMnemonicText
	.byte "fill", 0

DriverSelectorUnknownRawText
	.byte "OTR901: selector unknown mnemonic", 0

DriverSelectorUnsupportedRawText
	.byte "OTR901: selector unsupported address", 0

DriverSelectorOperandRawText
	.byte "OTR901: selector operand error", 0

DriverSelectedOperandCompileRawText
	.byte "OTR901: selected operand compile failed", 0

	.endsection

	.section bss, kind=bss

OpasmActiveAssembleReqPtr
	.res long, 1

OpasmDriverEvalRequestLen
	.res word, 1

OpasmDriverEvalFallbackPtr
	.res long, 1

OpasmDriverEvalFallbackLen
	.res long, 1

	.endsection
	.endmodule
