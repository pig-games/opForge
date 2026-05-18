; Native AmigaOS opForge CLI two-pass opasm engine callbacks.

	.module opforge.cli.engine_callbacks
	.cpu 68020

	.use tkpkg.amigaos.abi (ENTRY_ORD_EVALUATE_EXPRESSION, ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION)
	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, LAST_ERROR_BUFFER_PTR_V1)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)

	.use opasm.amigaos.engine (opasmEngineRunTwoPassV1)
	.use opasm.amigaos.engine (opasmEngineSessionPass, opasmEngineStmtCount)
	.use opasm.amigaos.engine (opasmEngineLabelCount, opasmEngineImageByteCount)
	.use opasm.amigaos.engine (opasmEngineSessionOrigin, opasmEngineSessionCurrentPc)
	.use opasm.amigaos.engine (opasmEngineStmtSourceLineLenTable, opasmEngineStmtSourceLineTextTable)
	.use opasm.amigaos.engine (opasmEngineStmtLabelLenTable, opasmEngineStmtMnemLenTable)
	.use opasm.amigaos.engine (opasmEngineStmtOperandLenTable, opasmEngineStmtMnemNameTable)
	.use opasm.amigaos.engine (opasmEngineStmtLabelNameTable, opasmEngineStmtOperandNameTable)
	.use opasm.amigaos.engine (opasmEngineLabelNameTable, opasmEngineLabelValueTable)
	.use opasm.amigaos.engine (opasmEngineLabelFinalizedTable, opasmEngineImageBuffer)

	.use opforge.cli.constants (NATIVE_LABEL_TABLE_CAPACITY, NATIVE_IMAGE_BUFFER_CAPACITY)
	.use opforge.cli.constants (NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, NATIVE_EVAL_EXPR_EXTENSION_BYTES)
	.use opforge.cli.state (NativeCliBinRequested, NativeCliOpasmEngineContext)
	.use opforge.cli.state (NativeCliEvalRequestLen, NativeCliStmtExprFound)
	.use opforge.cli.state (NativeCliStmtExprSpanStart, NativeCliStmtExprSpanEnd)
	.use opforge.cli.strings (NativePassOneText, NativePassTwoText, NativePassOneOkText, NativePassTwoOkText)
	.use opforge.cli.strings (NativeLabelText, NativeDuplicateLabelText, NativeImageCapacityText)
	.use opforge.cli.strings (NativeSelectorStatusOkText, NativeUnknownMnemonicText, NativeUnsupportedAddressingText)
	.use opforge.cli.strings (NativeUnresolvedLabelText, NativeBadOrgText, NewlineText)
	.use opforge.cli.strings (OrgMnemonicText, CpuMnemonicText, EndMnemonicText)
	.use opforge.cli.strings (NativeSelectorUnknownRawText, NativeSelectorUnsupportedRawText)
	.use opforge.cli.strings (NativeSelectorOperandRawText, NativeSelectedOperandCompileRawText)
	.use opforge.cli.dos (opforgeNativeCliPutStr)
	.use opforge.cli.copy (opforgeNativeCliCopyFixedString)
	.use opforge.cli.token_util (opforgeNativeCliTokenLen, opforgeNativeCliTokenEquals)
	.use opforge.cli.line_text (opforgeNativeCliSkipLineWhitespace, opforgeNativeCliLineStartsWith)
	.use opforge.cli.text_output (opforgeNativeCliPutSpace, opforgeNativeCliPutHexU32)
	.use opforge.cli.tkpkg_control_block (opforgeNativeCliWriteInputWindow, opforgeNativeCliWriteExtensionWindow)
	.use opforge.cli.tkpkg_control_block (opforgeNativeCliReadStatus, opforgeNativeCliReadOutputLen, opforgeNativeCliReadLastErrorLen)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEncodeSelectedRequestForStatement)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEvaluateExpressionExtension)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliPrepareEvaluateExpressionRequest)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliReadEvaluateExpressionValue)
	.use opforge.cli.encode_eval_bridge (opforgeNativeCliLoadStatementExprMetadata, opforgeNativeCliLoadStatementSourceLineText)

	.section code, kind=code
	.pub

opforgeNativeCliRunTwoPassEngine	.block
	bsr.w opforgeNativeCliBuildOpasmEngineContext
	jsr opasmEngineRunTwoPassV1
	rts
	.bend  ; opforgeNativeCliRunTwoPassEngine

opforgeNativeCliBuildOpasmEngineContext	.block
	lea NativeCliOpasmEngineContext.l, a4
	move.l #opasmEngineSessionPass, (a4)+
	move.l #opasmEngineStmtCount, (a4)+
	move.l #NativeCliBinRequested, (a4)+
	move.l #opforgeNativeCliOpasmPassOneBegin, (a4)+
	move.l #opforgeNativeCliOpasmPassTwoBegin, (a4)+
	move.l #opforgeNativeCliOpasmPassOneOk, (a4)+
	move.l #opforgeNativeCliOpasmPassTwoOk, (a4)+
	move.l #opforgeNativeCliPassOneRecordLabel, (a4)+
	move.l #opforgeNativeCliPassAdvancePc, (a4)+
	move.l #opforgeNativeCliPassTwoEmitImageBytes, (a4)+
	lea NativeCliOpasmEngineContext.l, a4
	rts
	.bend  ; opforgeNativeCliBuildOpasmEngineContext

opforgeNativeCliOpasmPassOneBegin	.block
	movem.l d0-d1, -(sp)
	move.l #NativePassOneText, d1
	jsr opforgeNativeCliPutStr
	clr.w opasmEngineLabelCount.l
	lea opasmEngineLabelFinalizedTable.l, a0
	moveq #NATIVE_LABEL_TABLE_CAPACITY - 1, d0

opforgeNativeCliPassOneClearLabelFinalizedLoop
	clr.b (a0)+
	dbf d0, opforgeNativeCliPassOneClearLabelFinalizedLoop
	clr.w opasmEngineImageByteCount.l
	move.l #$00000800, opasmEngineSessionOrigin.l
	move.l opasmEngineSessionOrigin.l, d0
	move.l d0, opasmEngineSessionCurrentPc.l
	movem.l (sp)+, d0-d1
	moveq #0, d0
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
	movem.l d0-d1, -(sp)
	move.l #NativePassTwoText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineLabelCount.l, d0
	subq.w #1, d0
	bmi.s opforgeNativeCliPassTwoFinalizeLabelsDone
	lea opasmEngineLabelFinalizedTable.l, a0

opforgeNativeCliPassTwoFinalizeLabelLoop
	move.b #1, (a0)+
	dbf d0, opforgeNativeCliPassTwoFinalizeLabelLoop

opforgeNativeCliPassTwoFinalizeLabelsDone
	clr.w opasmEngineImageByteCount.l
	move.l opasmEngineSessionOrigin.l, d0
	move.l d0, opasmEngineSessionCurrentPc.l
	movem.l (sp)+, d0-d1
	moveq #0, d0
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
	movem.l d1-d7/a0-a2, -(sp)
	move.l d0, d7
	lsl.l #6, d7
	lea opasmEngineStmtLabelNameTable.l, a1
	adda.l d7, a1
	tst.b (a1)
	beq.w opforgeNativeCliPassOneRecordLabelOk
	moveq #0, d0
	move.w opasmEngineLabelCount.l, d0
	cmpi.w #NATIVE_LABEL_TABLE_CAPACITY, d0
	bhs.w opforgeNativeCliPassOneRecordLabelFail
	moveq #0, d6

opforgeNativeCliPassOneDuplicateLoop
	move.w opasmEngineLabelCount.l, d0
	cmp.w d0, d6
	bhs.s opforgeNativeCliPassOneStoreLabel
	moveq #0, d5
	move.w d6, d5
	lsl.l #6, d5
	lea opasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	moveq #0, d0
	move.l d7, d5
	lsr.l #6, d5
	add.w d5, d5
	lea opasmEngineStmtLabelLenTable.l, a2
	move.w 0(a2, d5.l), d0
	bne.s opforgeNativeCliPassOneDuplicateHaveLabelLen
	move.l a0, d3
	movea.l a1, a0
	jsr opforgeNativeCliTokenLen
	movea.l d3, a0

opforgeNativeCliPassOneDuplicateHaveLabelLen
	bsr.w opforgeNativeCliLabelEquals
	tst.l d0
	bne.w opforgeNativeCliPassOneDuplicate
	addq.w #1, d6
	bra.s opforgeNativeCliPassOneDuplicateLoop

opforgeNativeCliPassOneStoreLabel
	moveq #0, d6
	move.w opasmEngineLabelCount.l, d6
	move.l d6, d5
	lsl.l #2, d5
	lea opasmEngineLabelValueTable.l, a0
	move.l opasmEngineSessionCurrentPc.l, 0(a0, d5.l)
	lea opasmEngineLabelFinalizedTable.l, a0
	clr.b 0(a0, d6.l)
	move.l d6, d5
	lsl.l #6, d5
	lea opasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	move.l a0, d2
	move.l a0, d4
	move.l a1, d3
	movea.l a1, a2
	movea.l a0, a1
	movea.l a2, a0
	moveq #0, d0
	move.l d7, d5
	lsr.l #6, d5
	add.w d5, d5
	lea opasmEngineStmtLabelLenTable.l, a2
	move.w 0(a2, d5.l), d0
	bne.s opforgeNativeCliPassOneStoreHaveLabelLen
	movea.l d3, a0
	jsr opforgeNativeCliTokenLen

opforgeNativeCliPassOneStoreHaveLabelLen
	jsr opforgeNativeCliCopyFixedString
	clr.b (a1)
	addq.w #1, opasmEngineLabelCount.l
	move.l #NativeLabelText, d1
	jsr opforgeNativeCliPutStr
	move.l d4, d1
	jsr opforgeNativeCliPutStr
	jsr opforgeNativeCliPutSpace
	move.l opasmEngineSessionCurrentPc.l, d0
	jsr opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	bra.s opforgeNativeCliPassOneRecordLabelOk

opforgeNativeCliPassOneDuplicate
	move.l #NativeDuplicateLabelText, d1
	jsr opforgeNativeCliPutStr
	move.l a1, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr

opforgeNativeCliPassOneRecordLabelFail
	moveq #1, d0
	bra.s opforgeNativeCliPassOneRecordLabelReturn

opforgeNativeCliPassOneRecordLabelOk
	moveq #0, d0

opforgeNativeCliPassOneRecordLabelReturn
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliPassOneRecordLabel

opforgeNativeCliPassTwoEmitImageBytes	.block
	movem.l d1-d6/a0-a4, -(sp)
	move.w d0, d6
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea opasmEngineStmtMnemNameTable.l, a0
	adda.l d0, a0
	move.l a0, d5
	moveq #0, d3
	move.w d6, d3
	add.w d3, d3
	lea opasmEngineStmtMnemLenTable.l, a1
	moveq #0, d0
	move.w 0(a1, d3.l), d0
	bne.s opforgeNativeCliPassTwoEmitHaveMlen
	movea.l d5, a0
	jsr opforgeNativeCliTokenLen

opforgeNativeCliPassTwoEmitHaveMlen
	move.w d0, d4
	beq.w opforgeNativeCliPassTwoEmitOk
	move.w d6, d0
	move.w d4, d1
	movea.l d5, a0
	bsr.w opforgeNativeCliStatementMnemDuplicatesLabel
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	movea.l d5, a0
	move.w d4, d0
	lea OrgMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	movea.l d5, a0
	move.w d4, d0
	lea CpuMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	movea.l d5, a0
	move.w d4, d0
	lea EndMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	jsr opforgeNativeCliPrepareEncodeSelectedRequestForStatement
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitReturn
	tst.w NativeCliEvalRequestLen.l
	beq.w opforgeNativeCliPassTwoEmitOk
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
	bne.w opforgeNativeCliPassTwoEmitServiceFail
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadOutputLen
	tst.w d0
	beq.w opforgeNativeCliPassTwoEmitOk
	move.w d0, d6
	move.l #NativeSelectorStatusOkText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	add.w d6, d0
	cmpi.w #NATIVE_IMAGE_BUFFER_CAPACITY, d0
	bhi.w opforgeNativeCliPassTwoEmitFail
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	lea opasmEngineImageBuffer.l, a0
	adda.l d0, a0
	lea lastErrorBuffer, a1
	move.w d6, d1

opforgeNativeCliPassTwoCopyEncodedLoop
	move.b (a1)+, (a0)+
	subq.w #1, d1
	bne.s opforgeNativeCliPassTwoCopyEncodedLoop
	add.w d6, opasmEngineImageByteCount.l

opforgeNativeCliPassTwoEmitOk
	moveq #0, d0
	bra.s opforgeNativeCliPassTwoEmitReturn

opforgeNativeCliPassTwoEmitFail
	move.l #NativeImageCapacityText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	bra.s opforgeNativeCliPassTwoEmitReturn

opforgeNativeCliPassTwoEmitServiceFail
	move.w d6, d0
	bsr.w opforgeNativeCliStatementLooksBareColumnOne
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadLastErrorLen
	tst.w d0
	beq.s opforgeNativeCliPassTwoEmitServiceFailReturn
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	bsr.w opforgeNativeCliPassTwoEmitSelectorDiagnostic
	tst.l d0
	bne.s opforgeNativeCliPassTwoEmitServiceFailReturn
	move.l #lastErrorBuffer, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr

opforgeNativeCliPassTwoEmitServiceFailReturn
	moveq #1, d0

opforgeNativeCliPassTwoEmitReturn
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
	jsr opforgeNativeCliLoadStatementExprMetadata
	tst.w NativeCliStmtExprFound
	bne.s loadSourceLine
	bra.w storedText

loadSourceLine
	jsr opforgeNativeCliLoadStatementSourceLineText
	tst.l d0
	bne.s haveText
	bra.w fail

storedText
	clr.l d3
	moveq #0, d0
	move.w d7, d0
	add.w d0, d0
	lea opasmEngineStmtOperandLenTable.l, a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	bne.s storedTextReady
	bra.w fail

storedTextReady
	moveq #0, d0
	move.w d7, d0
	lsl.l #6, d0
	lea opasmEngineStmtOperandNameTable.l, a0
	adda.l d0, a0
	move.l d1, d0

haveText
	tst.w NativeCliStmtExprFound
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
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliReadOperandValueForStatement

opforgeNativeCliLoadStatementExprText	.block
	moveq #0, d0
	move.w d7, d0
	add.w d0, d0
	lea opasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	beq.s fail
	moveq #0, d0
	move.w d7, d0
	lsl.l #8, d0
	add.l d0, d0
	lea opasmEngineStmtSourceLineTextTable.l, a0
	adda.l d0, a0
	move.l NativeCliStmtExprSpanStart, d2
	beq.s fail
	move.l NativeCliStmtExprSpanEnd, d0
	cmp.l d2, d0
	bls.s fail
	subq.l #1, d2
	cmp.l d1, d2
	bhs.s fail
	adda.l d2, a0
	sub.l d2, d1
	move.l NativeCliStmtExprSpanEnd, d0
	sub.l NativeCliStmtExprSpanStart, d0
	cmp.l d1, d0
	bls.s done
	move.l d1, d0

done
	rts

fail
	clr.l d0
	rts
	.bend  ; opforgeNativeCliLoadStatementExprText

opforgeNativeCliPassAdvancePc	.block
	movem.l d0-d7/a0-a3, -(sp)
	move.l d0, d7
	lsl.l #6, d0
	lea opasmEngineStmtMnemNameTable.l, a0
	adda.l d0, a0
	move.l a0, d5
	move.l d0, d4
	lsr.l #6, d4
	add.w d4, d4
	lea opasmEngineStmtMnemLenTable.l, a1
	moveq #0, d0
	move.w 0(a1, d4.l), d0
	bne.s opforgeNativeCliPassAdvanceHaveMlen
	movea.l d5, a0
	jsr opforgeNativeCliTokenLen

opforgeNativeCliPassAdvanceHaveMlen
	move.w d0, d6
	beq.w opforgeNativeCliPassAdvanceDone
	move.w d7, d0
	move.w d6, d1
	movea.l d5, a0
	bsr.w opforgeNativeCliStatementMnemDuplicatesLabel
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceDone
	lea OrgMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceOrg
	movea.l d5, a0
	move.w d6, d0
	lea CpuMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceDone
	movea.l d5, a0
	move.w d6, d0
	lea EndMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceDone
	moveq #0, d0
	move.w d7, d0
	bsr.w opforgeNativeCliTrySelectedEncodeSizeForStatement
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceFail
	cmpi.w #1, d1
	beq.w opforgeNativeCliPassAdvanceOne
	cmpi.w #2, d1
	beq.w opforgeNativeCliPassAdvanceTwo
	cmpi.w #3, d1
	beq.w opforgeNativeCliPassAdvanceThree
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceOrg
	move.w d4, d7
	moveq #2, d5
	bsr.w opforgeNativeCliReadOperandValueForStatement
	tst.l d0
	beq.s opforgeNativeCliPassAdvanceOrgOk
	move.l #NativeBadOrgText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

opforgeNativeCliPassAdvanceOrgOk
	move.l d3, opasmEngineSessionOrigin.l
	move.l opasmEngineSessionOrigin.l, d0
	move.l d0, opasmEngineSessionCurrentPc.l
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceOne
	addq.l #1, opasmEngineSessionCurrentPc.l
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceTwo
	addq.l #2, opasmEngineSessionCurrentPc.l
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceThree
	addq.l #3, opasmEngineSessionCurrentPc.l
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceFail
	move.w d7, d0
	bsr.w opforgeNativeCliStatementLooksBareColumnOne
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceDone
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

opforgeNativeCliPassAdvanceDone
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
	movem.l d1-d4/a0-a2, -(sp)
	move.l d0, d2
	add.w d2, d2
	lea opasmEngineStmtLabelLenTable.l, a2
	moveq #0, d3
	move.w 0(a2, d2.l), d3
	beq.s no
	cmp.w d1, d3
	bne.s no
	move.l d0, d4
	lsl.l #6, d4
	lea opasmEngineStmtLabelNameTable.l, a1
	adda.l d4, a1
	move.l d1, d0
	bsr.w opforgeNativeCliLabelEquals
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; opforgeNativeCliStatementMnemDuplicatesLabel

opforgeNativeCliStatementLooksBareColumnOne	.block
	movem.l d1-d4/a0, -(sp)
	move.l d0, d1
	add.w d1, d1
	lea opasmEngineStmtOperandLenTable.l, a0
	tst.w 0(a0, d1.l)
	bne.w no
	lea opasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d4
	move.w 0(a0, d1.l), d4
	beq.w no
	move.l d0, d2
	lsl.l #8, d2
	add.l d2, d2
	lea opasmEngineStmtSourceLineTextTable.l, a0
	adda.l d2, a0
	move.b (a0), d3
	tst.b d3
	beq.w no
	cmpi.b #10, d3
	beq.w no
	cmpi.b #13, d3
	beq.w no
	cmpi.b #' ', d3
	beq.w no
	cmpi.b #9, d3
	beq.w no
	cmpi.b #'.', d3
	beq.w no
	cmpi.b #';', d3
	beq.w no

tokenLoop
	tst.l d4
	beq.s yes
	move.b (a0), d3
	tst.b d3
	beq.s yes
	cmpi.b #10, d3
	beq.s yes
	cmpi.b #13, d3
	beq.s yes
	cmpi.b #';', d3
	beq.s yes
	cmpi.b #' ', d3
	beq.s trailingLoop
	cmpi.b #9, d3
	beq.s trailingLoop
	addq.l #1, a0
	subq.l #1, d4
	bra.s tokenLoop

trailingLoop
	tst.l d4
	beq.s yes
	move.b (a0), d3
	tst.b d3
	beq.s yes
	cmpi.b #10, d3
	beq.s yes
	cmpi.b #13, d3
	beq.s yes
	cmpi.b #';', d3
	beq.s yes
	cmpi.b #' ', d3
	beq.s trailingOne
	cmpi.b #9, d3
	beq.s trailingOne
	bra.s no

trailingOne
	addq.l #1, a0
	subq.l #1, d4
	bra.s trailingLoop

yes
	moveq #1, d0
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d4/a0
	rts
	.bend  ; opforgeNativeCliStatementLooksBareColumnOne

opforgeNativeCliLabelEquals	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l d0, d3
	beq.s no

loop
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmp.b d2, d1
	bne.s no
	subq.l #1, d3
	bne.s loop
	tst.b (a0)
	bne.s no
	moveq #1, d0
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; opforgeNativeCliLabelEquals

	.endsection
	.endmodule
