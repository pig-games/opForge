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
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea CpuMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea EndMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w ok
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
	beq.w ok
	move.w d1, d6
	moveq #abi.OPASM_EVENT_SELECTOR_STATUS_OK, d0
	bsr.w appendKindEvent
	bsr.w serviceIoBufferPtr
	moveq #0, d0
	move.w d6, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w fail

ok
	moveq #0, d0
	bra.s return

fail
	moveq #abi.OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED, d0
	bsr.w appendKindEvent
	moveq #1, d0
	bra.s return

serviceFail
	moveq #0, d0
	move.w d6, d0
	jsr eng.opasmEngineStatementLooksBareColumnOneV1
	bne.w ok
	tst.w d4
	beq.s serviceFailReturn
	bsr.w serviceIoBufferPtr
	clr.b 0(a0, d4.W)
	bsr.w emitSelectorDiagnostic
	bne.s serviceFailReturn
	bsr.w serviceIoBufferPtr
	move.w d4, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextEvent

serviceFailReturn
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
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w org
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea CpuMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea EndMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w done
	moveq #0, d0
	move.w d7, d0
	bsr.w trySelectedEncodeSizeForStatement
	bne.w fail
	cmpi.w #1, d1
	beq.w advanceOne
	cmpi.w #2, d1
	beq.w advanceTwo
	cmpi.w #3, d1
	beq.w advanceThree
	bra.w done

org
	move.w d4, d7
	moveq #2, d5
	bsr.w readOperandValueForStatement
	beq.s orgOk
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

fail
	move.w d7, d0
	jsr eng.opasmEngineStatementLooksBareColumnOneV1
	bne.w done
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

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
	bsr.w serviceIoBufferPtr
	clr.b 0(a0, d4.W)
	bsr.w emitSelectorDiagnostic
	bne.s failReturn
	bsr.w serviceIoBufferPtr
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
	move.w d0, d6
	bne.s loadSourceLine
	bra.w storedText

loadSourceLine
	moveq #0, d0
	move.w d7, d0
	jsr eng.getStatementSourceLineTextV1
	tst.l d0
	bne.s haveText
	bra.w fail

storedText
	clr.l d3
	movea.l sp, a0
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(a0), d1
	bne.s storedTextReady
	bra.w fail

storedTextReady
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(a0), a0
	move.l d1, d0

haveText
	tst.w d6
	bne.s prepareRequest
	bsr.w skipLineWhitespace
	tst.l d0
	bne.s prepareRequest
	bra.w fail

prepareRequest
	bsr.w prepareEvaluateExpressionRequest
	beq.s prepareExtension
	bra.w fail

prepareExtension
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEvaluateExpressionV1
	beq.s readValue
	bra.w fail

readValue
	bsr.w readEvaluateExpressionValue
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

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	rts
	.bend  ; readOperandValueForStatement

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
;   D7.W = expression text length
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
	bsr.w serviceIoBufferPtr
	lea DriverSelectorUnknownRawText, a1
	bsr.w tokenEquals
	bne.s unknownMnemonic
	bsr.w serviceIoBufferPtr
	lea DriverSelectorUnsupportedRawText, a1
	bsr.w tokenEquals
	bne.s unsupportedAddressing
	bsr.w serviceIoBufferPtr
	lea DriverSelectorOperandRawText, a1
	bsr.w tokenEquals
	bne.s operandError
	bsr.w serviceIoBufferPtr
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
	rts
	.bend  ; skipLineWhitespace

	.endsection

	.section data, kind=data

OrgMnemonicText
	.byte ".org", 0

CpuMnemonicText
	.byte ".cpu", 0

EndMnemonicText
	.byte ".end", 0

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

	.endsection
	.endmodule
