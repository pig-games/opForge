; Native AmigaOS opForge CLI two-pass opasm engine callbacks.

	.module opforge.cli.engine_callbacks
	.cpu 68020

	.use tkpkg.amigaos.abi (ENTRY_ORD_EVALUATE_EXPRESSION, ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION)
	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, LAST_ERROR_BUFFER_PTR_V1)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)

	.use opasm.amigaos.engine (opasmEngineRunTwoPassV1, opasmEngineBuildCallbackContextV1)
	.use opasm.amigaos.engine (opasmEngineBeginPassOneV1, opasmEngineBeginPassTwoV1)
	.use opasm.amigaos.engine (opasmEngineRecordStatementLabelV1, opasmEngineSetOriginV1, opasmEngineAdvancePcBySizeV1)
	.use opasm.amigaos.engine (opasmEngineAppendImageBytesV1)
	.use opasm.amigaos.engine (opasmEngineGetStatementTextMetadataV1)
	.use opasm.amigaos.engine (opasmEngineGetStatementSourceLineTextV1)
	.use opasm.amigaos.engine (opasmEngineStatementHasExprMetadataV1)
	.use opasm.amigaos.engine (opasmEngineStatementMnemonicDuplicatesLabelV1)
	.use opasm.amigaos.engine (opasmEngineStatementLooksBareColumnOneV1)
	.use opasm.amigaos.engine (OPASM_ENGINE_STMT_TEXT_MNEM_PTR, OPASM_ENGINE_STMT_TEXT_MNEM_LEN)
	.use opasm.amigaos.engine (OPASM_ENGINE_STMT_TEXT_OPERAND_PTR, OPASM_ENGINE_STMT_TEXT_OPERAND_LEN)
	.use opasm.amigaos.engine (OPASM_ENGINE_STMT_TEXT_BYTES)
	.use opasm.amigaos.engine (OPASM_ENGINE_LABEL_EVENT_STORED, OPASM_ENGINE_LABEL_EVENT_DUPLICATE)
	.use opasm.amigaos.engine (OPASM_ENGINE_CALLBACK_REQ_BIN_REQUESTED_PTR)
	.use opasm.amigaos.engine (OPASM_ENGINE_CALLBACK_REQ_PASS1_BEGIN_CB, OPASM_ENGINE_CALLBACK_REQ_PASS2_BEGIN_CB)
	.use opasm.amigaos.engine (OPASM_ENGINE_CALLBACK_REQ_PASS1_OK_CB, OPASM_ENGINE_CALLBACK_REQ_PASS2_OK_CB)
	.use opasm.amigaos.engine (OPASM_ENGINE_CALLBACK_REQ_RECORD_LABEL_CB, OPASM_ENGINE_CALLBACK_REQ_ADVANCE_PC_CB)
	.use opasm.amigaos.engine (OPASM_ENGINE_CALLBACK_REQ_EMIT_IMAGE_CB, OPASM_ENGINE_CALLBACK_REQ_BYTES)

	.use opforge.cli.constants (NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, NATIVE_EVAL_EXPR_EXTENSION_BYTES)
	.use opforge.cli.state (NativeCliBinRequested)
	.use opforge.cli.state (NativeCliEvalRequestLen)
	.use opforge.cli.strings (NativePassOneText, NativePassTwoText, NativePassOneOkText, NativePassTwoOkText)
	.use opforge.cli.strings (NativeLabelText, NativeDuplicateLabelText, NativeImageCapacityText)
	.use opforge.cli.strings (NativeSelectorStatusOkText, NativeUnknownMnemonicText, NativeUnsupportedAddressingText)
	.use opforge.cli.strings (NativeUnresolvedLabelText, NativeBadOrgText, NewlineText)
	.use opforge.cli.strings (OrgMnemonicText, CpuMnemonicText, EndMnemonicText)
	.use opforge.cli.strings (NativeSelectorUnknownRawText, NativeSelectorUnsupportedRawText)
	.use opforge.cli.strings (NativeSelectorOperandRawText, NativeSelectedOperandCompileRawText)
	.use opforge.cli.dos (opforgeNativeCliPutStr)
	.use opforge.cli.token_util (opforgeNativeCliTokenEquals)
	.use opforge.cli.line_text (opforgeNativeCliSkipLineWhitespace, opforgeNativeCliLineStartsWith)
	.use opforge.cli.text_output (opforgeNativeCliPutSpace, opforgeNativeCliPutHexU32)
	.use opforge.cli.tkpkg_control_block (opforgeNativeCliWriteInputWindow, opforgeNativeCliWriteExtensionWindow)
	.use opforge.cli.tkpkg_control_block (opforgeNativeCliReadStatus, opforgeNativeCliReadOutputLen, opforgeNativeCliReadLastErrorLen)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEncodeSelectedRequestForStatement)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEvaluateExpressionExtension)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEvaluateExpressionRequest)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliReadEvaluateExpressionValue)

	.section code, kind=code
	.pub

opforgeNativeCliRunTwoPassEngine	.block
	bsr.w opforgeNativeCliBuildOpasmEngineContext
	jsr opasmEngineRunTwoPassV1
	rts
	.bend  ; opforgeNativeCliRunTwoPassEngine

	.priv

opforgeNativeCliBuildOpasmEngineContext	.block
	suba.l #OPASM_ENGINE_CALLBACK_REQ_BYTES, sp
	movea.l sp, a0
	move.l #NativeCliBinRequested, OPASM_ENGINE_CALLBACK_REQ_BIN_REQUESTED_PTR(a0)
	move.l #opforgeNativeCliOpasmPassOneBegin, OPASM_ENGINE_CALLBACK_REQ_PASS1_BEGIN_CB(a0)
	move.l #opforgeNativeCliOpasmPassTwoBegin, OPASM_ENGINE_CALLBACK_REQ_PASS2_BEGIN_CB(a0)
	move.l #opforgeNativeCliOpasmPassOneOk, OPASM_ENGINE_CALLBACK_REQ_PASS1_OK_CB(a0)
	move.l #opforgeNativeCliOpasmPassTwoOk, OPASM_ENGINE_CALLBACK_REQ_PASS2_OK_CB(a0)
	move.l #opforgeNativeCliPassOneRecordLabel, OPASM_ENGINE_CALLBACK_REQ_RECORD_LABEL_CB(a0)
	move.l #opforgeNativeCliPassAdvancePc, OPASM_ENGINE_CALLBACK_REQ_ADVANCE_PC_CB(a0)
	move.l #opforgeNativeCliPassTwoEmitImageBytes, OPASM_ENGINE_CALLBACK_REQ_EMIT_IMAGE_CB(a0)
	jsr opasmEngineBuildCallbackContextV1
	adda.l #OPASM_ENGINE_CALLBACK_REQ_BYTES, sp
	rts
	.bend  ; opforgeNativeCliBuildOpasmEngineContext

opforgeNativeCliOpasmPassOneBegin	.block
	movem.l d1, -(sp)
	move.l #NativePassOneText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineBeginPassOneV1
	movem.l (sp)+, d1
	rts
	.bend  ; opforgeNativeCliOpasmPassOneBegin

opforgeNativeCliOpasmPassOneOk	.block
	movem.l d1, -(sp)
	move.l #NativePassOneOkText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d1
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliOpasmPassOneOk

opforgeNativeCliOpasmPassTwoBegin	.block
	movem.l d1, -(sp)
	move.l #NativePassTwoText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineBeginPassTwoV1
	movem.l (sp)+, d1
	rts
	.bend  ; opforgeNativeCliOpasmPassTwoBegin

opforgeNativeCliOpasmPassTwoOk	.block
	movem.l d1, -(sp)
	move.l #NativePassTwoOkText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d1
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliOpasmPassTwoOk

opforgeNativeCliPassOneRecordLabel	.block
	movem.l d1-d5/a0, -(sp)
	jsr opasmEngineRecordStatementLabelV1
	move.l a0, d4
	move.l d2, d5
	cmpi.w #OPASM_ENGINE_LABEL_EVENT_STORED, d1
	beq.s stored
	cmpi.w #OPASM_ENGINE_LABEL_EVENT_DUPLICATE, d1
	beq.s duplicate
	bra.s return

stored
	move.l #NativeLabelText, d1
	jsr opforgeNativeCliPutStr
	move.l d4, d1
	jsr opforgeNativeCliPutStr
	jsr opforgeNativeCliPutSpace
	move.l d5, d0
	jsr opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	bra.s return

duplicate
	move.l #NativeDuplicateLabelText, d1
	jsr opforgeNativeCliPutStr
	move.l d4, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0

return
	movem.l (sp)+, d1-d5/a0
	rts
	.bend  ; opforgeNativeCliPassOneRecordLabel

opforgeNativeCliPassTwoEmitImageBytes	.block
	movem.l d1-d6/a0-a4, -(sp)
	move.w d0, d6
	suba.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d6, d0
	jsr opasmEngineGetStatementTextMetadataV1
	tst.l d0
	bne.w ok
	movea.l OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), a1
	move.l a1, d5
	move.l OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a0), d4
	beq.w ok
	move.w d6, d0
	move.w d4, d1
	movea.l d5, a0
	bsr.w opforgeNativeCliStatementMnemDuplicatesLabel
	tst.l d0
	bne.w ok
	movea.l d5, a0
	move.w d4, d0
	lea OrgMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w ok
	movea.l d5, a0
	move.w d4, d0
	lea CpuMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w ok
	movea.l d5, a0
	move.w d4, d0
	lea EndMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w ok
	jsr opforgeNativeCliPrepareEncodeSelectedRequestForStatement
	tst.l d0
	bne.w return
	tst.w NativeCliEvalRequestLen.l
	beq.w ok
	jsr opforgeNativeCliPrepareEvaluateExpressionExtension
	lea ControlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliEvalRequestLen.l, d1
	jsr opforgeNativeCliWriteInputWindow
	move.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, d0
	move.w #NATIVE_EVAL_EXPR_EXTENSION_BYTES, d1
	jsr opforgeNativeCliWriteExtensionWindow
	moveq #ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0
	jsr tkpkgServiceDispatchV1
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadStatus
	tst.b d0
	bne.w serviceFail
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadOutputLen
	tst.w d0
	beq.w ok
	move.w d0, d6
	move.l #NativeSelectorStatusOkText, d1
	jsr opforgeNativeCliPutStr
	lea lastErrorBuffer, a1
	movea.l a1, a0
	move.w d6, d0
	jsr opasmEngineAppendImageBytesV1
	tst.l d0
	bne.w fail

ok
	moveq #0, d0
	bra.s return

fail
	move.l #NativeImageCapacityText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	bra.s return

serviceFail
	move.w d6, d0
	bsr.w opforgeNativeCliStatementLooksBareColumnOne
	tst.l d0
	bne.w ok
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadLastErrorLen
	tst.w d0
	beq.s serviceFailReturn
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	bsr.w opforgeNativeCliPassTwoEmitSelectorDiagnostic
	tst.l d0
	bne.s serviceFailReturn
	move.l #lastErrorBuffer, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr

serviceFailReturn
	moveq #1, d0

return
	adda.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d6/a0-a4
	rts
	.bend  ; opforgeNativeCliPassTwoEmitImageBytes

opforgeNativeCliPassTwoEmitSelectorDiagnostic	.block
	lea lastErrorBuffer, a0
	lea NativeSelectorUnknownRawText, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.s unknownMnemonic
	lea lastErrorBuffer, a0
	lea NativeSelectorUnsupportedRawText, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.s unsupportedAddressing
	lea lastErrorBuffer, a0
	lea NativeSelectorOperandRawText, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.s operandError
	lea lastErrorBuffer, a0
	lea NativeSelectedOperandCompileRawText, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.s operandError
	moveq #0, d0
	rts

unknownMnemonic
	move.l #NativeUnknownMnemonicText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts

unsupportedAddressing
	move.l #NativeUnsupportedAddressingText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts

operandError
	move.l #NativeUnresolvedLabelText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliPassTwoEmitSelectorDiagnostic

opforgeNativeCliReadOperandValueForStatement	.block
	movem.l d1-d2/d4-d7/a0-a2, -(sp)
	suba.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineStatementHasExprMetadataV1
	move.w d0, d6
	tst.w d6
	bne.s loadSourceLine
	bra.w storedText

loadSourceLine
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementSourceLineTextV1
	tst.l d0
	bne.s haveText
	bra.w fail

storedText
	clr.l d3
	movea.l sp, a0
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementTextMetadataV1
	tst.l d0
	bne.w fail
	move.l OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(a0), d1
	bne.s storedTextReady
	bra.w fail

storedTextReady
	movea.l OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(a0), a0
	move.l d1, d0

haveText
	tst.w d6
	bne.s prepareRequest
	jsr opforgeNativeCliSkipLineWhitespace
	tst.l d0
	bne.s prepareRequest
	bra.w fail

prepareRequest
	jsr opforgeNativeCliPrepareEvaluateExpressionRequest
	tst.l d0
	beq.s prepareExtension
	bra.w fail

prepareExtension
	jsr opforgeNativeCliPrepareEvaluateExpressionExtension
	lea ControlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliEvalRequestLen, d1
	jsr opforgeNativeCliWriteInputWindow
	move.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, d0
	move.w #NATIVE_EVAL_EXPR_EXTENSION_BYTES, d1
	jsr opforgeNativeCliWriteExtensionWindow
	moveq #ENTRY_ORD_EVALUATE_EXPRESSION, d0
	jsr tkpkgServiceDispatchV1
	jsr opforgeNativeCliReadStatus
	beq.s readValue
	bra.w fail

readValue
	jsr opforgeNativeCliReadEvaluateExpressionValue
	cmpi.b #1, d5
	bne.s ok
	cmpi.l #$000000FF, d3
	bls.s ok

fail
	move.l #NativeUnresolvedLabelText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	bra.s return

ok
	moveq #0, d0

return
	adda.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliReadOperandValueForStatement

opforgeNativeCliPassAdvancePc	.block
	movem.l d0-d7/a0-a3, -(sp)
	suba.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	move.l d0, d7
	movea.l sp, a0
	jsr opasmEngineGetStatementTextMetadataV1
	tst.l d0
	bne.w done
	movea.l OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), a1
	move.l a1, d5
	move.l OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a0), d6
	moveq #0, d4
	move.w d7, d4
	add.w d4, d4
	tst.w d6
	beq.w done
	move.w d7, d0
	move.w d6, d1
	movea.l d5, a0
	bsr.w opforgeNativeCliStatementMnemDuplicatesLabel
	tst.l d0
	bne.w done
	lea OrgMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w org
	movea.l d5, a0
	move.w d6, d0
	lea CpuMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w done
	movea.l d5, a0
	move.w d6, d0
	lea EndMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w done
	moveq #0, d0
	move.w d7, d0
	bsr.w opforgeNativeCliTrySelectedEncodeSizeForStatement
	tst.l d0
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
	bsr.w opforgeNativeCliReadOperandValueForStatement
	tst.l d0
	beq.s orgOk
	move.l #NativeBadOrgText, d1
	jsr opforgeNativeCliPutStr
	adda.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

orgOk
	move.l d3, d0
	jsr opasmEngineSetOriginV1
	bra.w done

advanceOne
	moveq #1, d0
	jsr opasmEngineAdvancePcBySizeV1
	bra.w done

advanceTwo
	moveq #2, d0
	jsr opasmEngineAdvancePcBySizeV1
	bra.w done

advanceThree
	moveq #3, d0
	jsr opasmEngineAdvancePcBySizeV1
	bra.w done

fail
	move.w d7, d0
	bsr.w opforgeNativeCliStatementLooksBareColumnOne
	tst.l d0
	bne.w done
	adda.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

done
	adda.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliPassAdvancePc

opforgeNativeCliTrySelectedEncodeSizeForStatement	.block
	movem.l d2-d7/a0-a2, -(sp)
	move.w d0, d6
	jsr opforgeNativeCliPrepareEncodeSelectedRequestForStatement
	tst.l d0
	bne.w prepareFail
	tst.w NativeCliEvalRequestLen.l
	beq.w empty
	jsr opforgeNativeCliPrepareEvaluateExpressionExtension
	lea ControlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliEvalRequestLen.l, d1
	jsr opforgeNativeCliWriteInputWindow
	move.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, d0
	move.w #NATIVE_EVAL_EXPR_EXTENSION_BYTES, d1
	jsr opforgeNativeCliWriteExtensionWindow
	moveq #ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0
	jsr tkpkgServiceDispatchV1
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadStatus
	tst.b d0
	bne.w fail
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadOutputLen
	move.w d0, d1
	moveq #0, d0
	bra.s return

empty
	moveq #0, d1
	moveq #0, d0
	bra.s return

prepareFail

fail
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadLastErrorLen
	tst.w d0
	beq.s failReturn
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	bsr.w opforgeNativeCliPassTwoEmitSelectorDiagnostic
	tst.l d0
	bne.s failReturn
	move.l #lastErrorBuffer, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr

failReturn
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliTrySelectedEncodeSizeForStatement

opforgeNativeCliStatementMnemDuplicatesLabel	.block
	jsr opasmEngineStatementMnemonicDuplicatesLabelV1
	rts
	.bend  ; opforgeNativeCliStatementMnemDuplicatesLabel

opforgeNativeCliStatementLooksBareColumnOne	.block
	jsr opasmEngineStatementLooksBareColumnOneV1
	rts
	.bend  ; opforgeNativeCliStatementLooksBareColumnOne

	.endsection
	.endmodule
