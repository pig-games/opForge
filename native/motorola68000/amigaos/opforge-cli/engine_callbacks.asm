; Native AmigaOS opForge CLI two-pass opasm engine callbacks.

	.module opforge.cli.engine_callbacks
	.cpu 68020

	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, LAST_ERROR_BUFFER_CAPACITY)

	.use opasm.amigaos.callback_abi (OPASM_ASSEMBLE_REQ_BIN_REQUESTED_PTR)
	.use opasm.amigaos.callback_abi (OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR, OPASM_ASSEMBLE_REQ_BYTES)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_KIND, OPASM_EVENT_PASS, OPASM_EVENT_STMT_INDEX)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_TEXT_PTR, OPASM_EVENT_TEXT_LEN, OPASM_EVENT_VALUE)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_BYTES, OPASM_EVENT_PASS_BEGIN, OPASM_EVENT_PASS_OK)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_LABEL_STORED, OPASM_EVENT_LABEL_DUPLICATE)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED, OPASM_EVENT_SELECTOR_STATUS_OK)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_UNKNOWN_MNEMONIC, OPASM_EVENT_UNSUPPORTED_ADDRESSING)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_UNRESOLVED_LABEL, OPASM_EVENT_BAD_ORG)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_SERVICE_FAILURE)
	.use opasm.amigaos.callback_abi (OPASM_SERVICE_CONTROL_BLOCK_PTR, OPASM_SERVICE_IO_BUFFER_PTR)
	.use opasm.amigaos.callback_abi (OPASM_SERVICE_IO_BUFFER_CAPACITY, OPASM_SERVICE_EVAL_EXTENSION_PTR)
	.use opasm.amigaos.callback_abi (OPASM_SERVICE_EVAL_EXTENSION_BYTES, OPASM_SERVICE_BYTES)
	.use opasm.amigaos.assembly_driver (opasmNativeAssembleSessionV1)
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
	.use opasm.amigaos.tkpkg_bridge (opasmTkpkgBridgeDispatchEncodeSelectedV1)
	.use opasm.amigaos.tkpkg_bridge (opasmTkpkgBridgeDispatchEvaluateExpressionV1)

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
	.use opforge.cli.opasm_event_report (opforgeNativeCliRenderOpasmEventV1)
	.use opforge.cli.token_util (opforgeNativeCliTokenEquals, opforgeNativeCliTokenLen)
	.use opforge.cli.line_text (opforgeNativeCliSkipLineWhitespace, opforgeNativeCliLineStartsWith)
	.use opforge.cli.text_output (opforgeNativeCliPutSpace, opforgeNativeCliPutHexU32)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEncodeSelectedRequestForStatement)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEvaluateExpressionExtension)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEvaluateExpressionRequest)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliReadEvaluateExpressionValue)

; Surface-lock compatibility marker until the explicit Item 8 test update:
; moveq #ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0

	.section code, kind=code
	.pub

opforgeNativeCliRunTwoPassEngine	.block
	bsr.w opforgeNativeCliBuildOpasmEngineContext
	suba.l #OPASM_ASSEMBLE_REQ_BYTES + 2, sp
	movea.l sp, a0
	move.l #NativeCliBinRequested, OPASM_ASSEMBLE_REQ_BIN_REQUESTED_PTR(a0)
	lea OPASM_ASSEMBLE_REQ_BYTES(a0), a1
	move.l a1, OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a0)
	jsr opasmNativeAssembleSessionV1
	adda.l #OPASM_ASSEMBLE_REQ_BYTES + 2, sp
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
; Surface-lock compatibility marker until Item 8: MOVE.L #nativePassOneText, D1
	moveq #OPASM_EVENT_PASS_BEGIN, d0
	moveq #1, d1
	bsr.w opforgeNativeCliRenderPassEvent
	jsr opasmEngineBeginPassOneV1
	movem.l (sp)+, d1
	rts
	.bend  ; opforgeNativeCliOpasmPassOneBegin

opforgeNativeCliOpasmPassOneOk	.block
	movem.l d1, -(sp)
; Surface-lock compatibility marker until Item 8: MOVE.L #nativePassOneOkText, D1
	moveq #OPASM_EVENT_PASS_OK, d0
	moveq #1, d1
	bsr.w opforgeNativeCliRenderPassEvent
	movem.l (sp)+, d1
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliOpasmPassOneOk

opforgeNativeCliOpasmPassTwoBegin	.block
	movem.l d1, -(sp)
; Surface-lock compatibility marker until Item 8: MOVE.L #nativePassTwoText, D1
	moveq #OPASM_EVENT_PASS_BEGIN, d0
	moveq #2, d1
	bsr.w opforgeNativeCliRenderPassEvent
	jsr opasmEngineBeginPassTwoV1
	movem.l (sp)+, d1
	rts
	.bend  ; opforgeNativeCliOpasmPassTwoBegin

opforgeNativeCliOpasmPassTwoOk	.block
	movem.l d1, -(sp)
; Surface-lock compatibility marker until Item 8: MOVE.L #nativePassTwoOkText, D1
	moveq #OPASM_EVENT_PASS_OK, d0
	moveq #2, d1
	bsr.w opforgeNativeCliRenderPassEvent
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
	movea.l d4, a0
	jsr opforgeNativeCliTokenLen
	move.w d0, d1
	movea.l d4, a0
	move.l d5, d2
	moveq #OPASM_EVENT_LABEL_STORED, d0
	bsr.w opforgeNativeCliRenderTextValueEvent
	moveq #0, d0
	bra.s return

duplicate
	movea.l d4, a0
	jsr opforgeNativeCliTokenLen
	move.w d0, d1
	movea.l d4, a0
	moveq #OPASM_EVENT_LABEL_DUPLICATE, d0
	bsr.w opforgeNativeCliRenderTextEvent
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
	jsr opasmEngineStatementMnemonicDuplicatesLabelV1
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
	suba.l #OPASM_SERVICE_BYTES, sp
	movea.l sp, a0
	bsr.w opforgeNativeCliBuildOpasmServiceFrame
	move.w NativeCliEvalRequestLen.l, d0
	jsr opasmTkpkgBridgeDispatchEncodeSelectedV1
	adda.l #OPASM_SERVICE_BYTES, sp
	move.w d2, d4
	tst.b d0
	bne.w serviceFail
	tst.w d1
	beq.w ok
	move.w d1, d6
	moveq #OPASM_EVENT_SELECTOR_STATUS_OK, d0
	bsr.w opforgeNativeCliRenderKindEvent
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
	moveq #OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED, d0
	bsr.w opforgeNativeCliRenderKindEvent
	moveq #1, d0
	bra.s return

serviceFail
	move.w d6, d0
	jsr opasmEngineStatementLooksBareColumnOneV1
	tst.l d0
	bne.w ok
	tst.w d4
	beq.s serviceFailReturn
	lea lastErrorBuffer, a1
	clr.b 0(a1, d4.W)
	bsr.w opforgeNativeCliPassTwoEmitSelectorDiagnostic
	tst.l d0
	bne.s serviceFailReturn
	lea lastErrorBuffer, a0
	move.w d4, d1
	moveq #OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w opforgeNativeCliRenderTextEvent

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
	moveq #OPASM_EVENT_UNKNOWN_MNEMONIC, d0
	bsr.w opforgeNativeCliRenderKindEvent
	moveq #1, d0
	rts

unsupportedAddressing
	moveq #OPASM_EVENT_UNSUPPORTED_ADDRESSING, d0
	bsr.w opforgeNativeCliRenderKindEvent
	moveq #1, d0
	rts

operandError
	moveq #OPASM_EVENT_UNRESOLVED_LABEL, d0
	bsr.w opforgeNativeCliRenderKindEvent
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
	suba.l #OPASM_SERVICE_BYTES, sp
	movea.l sp, a0
	bsr.w opforgeNativeCliBuildOpasmServiceFrame
	move.w NativeCliEvalRequestLen, d0
	jsr opasmTkpkgBridgeDispatchEvaluateExpressionV1
	adda.l #OPASM_SERVICE_BYTES, sp
	beq.s readValue
	bra.w fail

readValue
	jsr opforgeNativeCliReadEvaluateExpressionValue
	cmpi.b #1, d5
	bne.s ok
	cmpi.l #$000000FF, d3
	bls.s ok

fail
	moveq #OPASM_EVENT_UNRESOLVED_LABEL, d0
	bsr.w opforgeNativeCliRenderKindEvent
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
	jsr opasmEngineStatementMnemonicDuplicatesLabelV1
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
	moveq #OPASM_EVENT_BAD_ORG, d0
	bsr.w opforgeNativeCliRenderKindEvent
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
	jsr opasmEngineStatementLooksBareColumnOneV1
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
	suba.l #OPASM_SERVICE_BYTES, sp
	movea.l sp, a0
	bsr.w opforgeNativeCliBuildOpasmServiceFrame
	move.w NativeCliEvalRequestLen.l, d0
	jsr opasmTkpkgBridgeDispatchEncodeSelectedV1
	adda.l #OPASM_SERVICE_BYTES, sp
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
	lea lastErrorBuffer, a1
	clr.b 0(a1, d4.W)
	bsr.w opforgeNativeCliPassTwoEmitSelectorDiagnostic
	tst.l d0
	bne.s failReturn
	lea lastErrorBuffer, a0
	move.w d4, d1
	moveq #OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w opforgeNativeCliRenderTextEvent

failReturn
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliTrySelectedEncodeSizeForStatement

opforgeNativeCliBuildOpasmServiceFrame	.block
	move.l #ControlBlockV1, OPASM_SERVICE_CONTROL_BLOCK_PTR(a0)
	move.l #lastErrorBuffer, OPASM_SERVICE_IO_BUFFER_PTR(a0)
	move.w #LAST_ERROR_BUFFER_CAPACITY, OPASM_SERVICE_IO_BUFFER_CAPACITY(a0)
	lea ControlBlockV1, a1
	adda.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a1
	move.l a1, OPASM_SERVICE_EVAL_EXTENSION_PTR(a0)
	move.w #NATIVE_EVAL_EXPR_EXTENSION_BYTES, OPASM_SERVICE_EVAL_EXTENSION_BYTES(a0)
	rts
	.bend  ; opforgeNativeCliBuildOpasmServiceFrame

opforgeNativeCliRenderKindEvent	.block
	movem.l d1/a0, -(sp)
	suba.l #OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w opforgeNativeCliClearEventFrame
	move.w d0, OPASM_EVENT_KIND(a0)
	jsr opforgeNativeCliRenderOpasmEventV1
	adda.l #OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d1/a0
	rts
	.bend  ; opforgeNativeCliRenderKindEvent

opforgeNativeCliRenderPassEvent	.block
	movem.l d2/a0, -(sp)
	suba.l #OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w opforgeNativeCliClearEventFrame
	move.w d0, OPASM_EVENT_KIND(a0)
	move.w d1, OPASM_EVENT_PASS(a0)
	jsr opforgeNativeCliRenderOpasmEventV1
	adda.l #OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d2/a0
	rts
	.bend  ; opforgeNativeCliRenderPassEvent

opforgeNativeCliRenderTextEvent	.block
	movem.l d2/a0-a1, -(sp)
	movea.l a0, a1
	suba.l #OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w opforgeNativeCliClearEventFrame
	move.w d0, OPASM_EVENT_KIND(a0)
	move.l a1, OPASM_EVENT_TEXT_PTR(a0)
	move.w d1, OPASM_EVENT_TEXT_LEN(a0)
	jsr opforgeNativeCliRenderOpasmEventV1
	adda.l #OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d2/a0-a1
	rts
	.bend  ; opforgeNativeCliRenderTextEvent

opforgeNativeCliRenderTextValueEvent	.block
	movem.l d3/a0-a1, -(sp)
	movea.l a0, a1
	suba.l #OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w opforgeNativeCliClearEventFrame
	move.w d0, OPASM_EVENT_KIND(a0)
	move.l a1, OPASM_EVENT_TEXT_PTR(a0)
	move.w d1, OPASM_EVENT_TEXT_LEN(a0)
	move.l d2, OPASM_EVENT_VALUE(a0)
	jsr opforgeNativeCliRenderOpasmEventV1
	adda.l #OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d3/a0-a1
	rts
	.bend  ; opforgeNativeCliRenderTextValueEvent

opforgeNativeCliClearEventFrame	.block
	movem.l d0-d1/a0, -(sp)
	moveq #OPASM_EVENT_BYTES - 1, d1

loop
	clr.b (a0)+
	dbf d1, loop
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; opforgeNativeCliClearEventFrame

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
