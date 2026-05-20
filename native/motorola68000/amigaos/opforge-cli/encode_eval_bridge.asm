; Native AmigaOS opForge CLI encode/evaluate bridge.

	.module opforge.cli.encode_eval_bridge
	.cpu 68020

	.use tkpkg.amigaos.abi (ENTRY_ORD_ENCODE_INSTRUCTION)
	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, LAST_ERROR_BUFFER_PTR_V1)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)

	.use opasm.amigaos.engine (opasmEngineGetStatementSourceLineTextV1, opasmEngineGetStatementExprMetadataV1)
	.use opasm.amigaos.engine (opasmEngineGetStatementTextMetadataV1)
	.use opasm.amigaos.engine (opasmEngineGetStatementLineNumberV1)
	.use opasm.amigaos.engine (opasmEngineWriteEvaluateExpressionExtensionBaseV1)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_OPERAND_INDEX, OPASM_ENGINE_EXPR_META_SLOT_INDEX)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_START_TOKEN, OPASM_ENGINE_EXPR_META_END_TOKEN)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_SPAN_LINE, OPASM_ENGINE_EXPR_META_SPAN_START)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_SPAN_END, OPASM_ENGINE_EXPR_META_BYTES)
	.use opasm.amigaos.engine (OPASM_ENGINE_STMT_TEXT_MNEM_PTR, OPASM_ENGINE_STMT_TEXT_MNEM_LEN)
	.use opasm.amigaos.engine (OPASM_ENGINE_STMT_TEXT_OPERAND_PTR, OPASM_ENGINE_STMT_TEXT_OPERAND_LEN)
	.use opasm.amigaos.engine (OPASM_ENGINE_STMT_TEXT_BYTES)

	.use opforge.cli.constants (NATIVE_EVAL_EXPR_EXTENSION_PTR_V1)
	.use opforge.cli.state (NativeCliEncodeRequestLen, NativeCliEvalRequestLen)
	.use opforge.cli.state (NativeCliStmtMnemStart, NativeCliStmtMnemLen)
	.use opforge.cli.state (NativeCliStmtExprFound, NativeCliStmtExprOperandIndex, NativeCliStmtExprSlotIndex)
	.use opforge.cli.state (NativeCliStmtExprStartToken, NativeCliStmtExprEndToken)
	.use opforge.cli.state (NativeCliStmtExprSpanLine, NativeCliStmtExprSpanStart, NativeCliStmtExprSpanEnd)
	.use opforge.cli.strings (NativeCliSelectedShapeAccumulatorText, NativeCliSelectedShapeImmediateText)
	.use opforge.cli.strings (NativeCliSelectedShapeDirectText, NativeCliSelectedShapeDirectXText, NativeCliSelectedShapeDirectYText)
	.use opforge.cli.strings (NativeCliSelectedShapeIndirectText, NativeCliSelectedShapeIndexedIndirectXText)
	.use opforge.cli.strings (NativeCliSelectedShapeIndirectIndexedYText)
	.use opforge.cli.copy (opforgeNativeCliCopyBytes, opforgeNativeCliCopyFixedString)
	.use opforge.cli.tkpkg_control_block (opforgeNativeCliWriteInputWindow, opforgeNativeCliReadStatus)

	.section code, kind=code
	.pub

opforgeNativeCliPrepareEncodeSelectedRequestForStatement	.block
	movem.l d1-d7/a0-a2, -(sp)
	move.w d6, d7
	suba.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementTextMetadataV1
	tst.l d0
	bne.w fail
	movea.l OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), a2
	move.l OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a0), d6
	move.l a2, NativeCliStmtMnemStart
	move.l d6, NativeCliStmtMnemLen
	move.l OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(a0), d1
	movea.l OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(a0), a0

buildRequest
	move.l a0, d3
	move.l d1, d4
	bsr.w opforgeNativeCliLoadStatementExprMetadata
	tst.w NativeCliStmtExprFound
	bne.w maybeSourceLineRequest

syntheticRequest
	bsr.w opforgeNativeCliClearStatementExprSpanForSyntheticRequest
	movea.l d3, a0
	move.l d4, d0
	bsr.w opforgeNativeCliPrepareEvaluateExpressionRequest
	bra.w return

maybeSourceLineRequest
	tst.l d4
	bne.w syntheticRequest
	move.l NativeCliStmtExprSpanStart, d2
	move.l NativeCliStmtExprSpanEnd, d3
	cmp.l d2, d3
	bls.w syntheticRequest

sourceLineRequest
	bsr.w opforgeNativeCliLoadStatementSourceLineText
	tst.l d0
	beq.w syntheticRequest
	move.l d0, d1
	move.l d2, d0
	subq.l #1, d0
	cmp.l d1, d0
	bhs.w syntheticRequest
	move.l d3, d0
	subq.l #1, d0
	cmp.l d1, d0
	bhi.w syntheticRequest
	move.l d1, d0
	bsr.w opforgeNativeCliPrepareEvaluateExpressionRequest
	bra.w return

fail
	moveq #1, d0

return
	adda.l #OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliPrepareEncodeSelectedRequestForStatement

opforgeNativeCliPrepareEvaluateExpressionRequest	.block
	movem.l d1-d7/a1-a2, -(sp)
	movea.l a0, a2
	move.l d0, d6
	lea lastErrorBuffer, a1
	move.l NativeCliStmtExprSpanLine, d2
	tst.l d2
	bne.s haveLineNum
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementLineNumberV1
	move.l d0, d2

haveLineNum
	move.l d2, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	tst.w NativeCliStmtExprFound
	beq.s syntheticSpan
	move.l NativeCliStmtExprSpanStart, d2
	move.l NativeCliStmtExprSpanEnd, d3
	bra.s writeSpan

syntheticSpan
	tst.l d6
	bne.s syntheticNonEmptySpan
	clr.l d2
	clr.l d3
	bra.s writeSpan

syntheticNonEmptySpan
	moveq #1, d2
	move.l d6, d3
	addq.l #1, d3

writeSpan
	move.w d2, d4
	move.b d4, (a1)+
	lsr.w #8, d4
	move.b d4, (a1)+
	move.w d3, d4
	move.b d4, (a1)+
	lsr.w #8, d4
	move.b d4, (a1)+
	move.l NativeCliStmtMnemLen, d5
	cmpi.l #255, d5
	bhi.w fail
	move.b d5, (a1)+
	tst.l d5
	beq.s copyOperand
	movea.l NativeCliStmtMnemStart, a0
	move.w d5, d0
	jsr opforgeNativeCliCopyFixedString

copyOperand
	movea.l a2, a0
	move.w d6, d0
	jsr opforgeNativeCliCopyFixedString
	move.w d6, d0
	add.w d5, d0
	addi.w #9, d0
	move.w d0, NativeCliEvalRequestLen
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a1-a2
	rts
	.bend  ; opforgeNativeCliPrepareEvaluateExpressionRequest

opforgeNativeCliPrepareEvaluateExpressionExtension	.block
	movem.l d1-d7/a0-a2, -(sp)
	lea ControlBlockV1, a1
	adda.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a1
	jsr opasmEngineWriteEvaluateExpressionExtensionBaseV1
	bsr.w opforgeNativeCliInferSelectedShapeForEvalRequest
	tst.w d0
	beq.s done
	lea ControlBlockV1, a1
	adda.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1 + 16, a1
	move.l a0, (a1)
	move.l d0, 4(a1)

done
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliPrepareEvaluateExpressionExtension

opforgeNativeCliReadEvaluateExpressionValue	.block
	lea ControlBlockV1, a0
	adda.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a0
	move.l 16(a0), d3
	rts
	.bend  ; opforgeNativeCliReadEvaluateExpressionValue

opforgeNativeCliLoadStatementSourceLineText	.block
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementSourceLineTextV1
	rts
	.bend  ; opforgeNativeCliLoadStatementSourceLineText

opforgeNativeCliLoadStatementExprMetadata	.block
	suba.l #OPASM_ENGINE_EXPR_META_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d7, d0
	jsr opasmEngineGetStatementExprMetadataV1
	move.l OPASM_ENGINE_EXPR_META_OPERAND_INDEX(a0), NativeCliStmtExprOperandIndex
	move.l OPASM_ENGINE_EXPR_META_SLOT_INDEX(a0), NativeCliStmtExprSlotIndex
	move.l OPASM_ENGINE_EXPR_META_START_TOKEN(a0), NativeCliStmtExprStartToken
	move.l OPASM_ENGINE_EXPR_META_END_TOKEN(a0), NativeCliStmtExprEndToken
	move.l OPASM_ENGINE_EXPR_META_SPAN_LINE(a0), NativeCliStmtExprSpanLine
	move.l OPASM_ENGINE_EXPR_META_SPAN_START(a0), NativeCliStmtExprSpanStart
	move.l OPASM_ENGINE_EXPR_META_SPAN_END(a0), NativeCliStmtExprSpanEnd
	tst.l d0
	beq.s empty
	move.w #1, NativeCliStmtExprFound
	adda.l #OPASM_ENGINE_EXPR_META_BYTES, sp
	rts

empty
	clr.w NativeCliStmtExprFound
	adda.l #OPASM_ENGINE_EXPR_META_BYTES, sp
	rts
	.bend  ; opforgeNativeCliLoadStatementExprMetadata

	.priv

opforgeNativeCliPrepareEncodeInstructionRequest	.block
	lea lastErrorBuffer, a2
	move.l NativeCliStmtMnemLen, d0
	cmpi.l #255, d0
	bhi.s fail
	move.b d0, (a2)+
	tst.l d0
	beq.s candidateCount
	movea.l NativeCliStmtMnemStart, a1
	jsr opforgeNativeCliCopyBytes

candidateCount
	clr.b (a2)+
	addq.w #2, d0
	move.w d0, NativeCliEncodeRequestLen
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliPrepareEncodeInstructionRequest

opforgeNativeCliInferSelectedShapeForEvalRequest	.block
	movem.l d1-d7/a1-a2, -(sp)
	lea lastErrorBuffer, a0
	moveq #0, d0
	move.b 8(a0), d0
	movea.l a0, a2
	bsr.w opforgeNativeCliInferSelectedShapeBranchMnemonic
	tst.l d0
	bne.w opforgeNativeCliInferSelectedShapeDirect
	movea.l a2, a0
	moveq #0, d0
	move.b 8(a0), d0
	moveq #0, d2
	move.w NativeCliEvalRequestLen.l, d2
	subi.w #9, d2
	bcs.w opforgeNativeCliInferSelectedShapeNone
	sub.w d0, d2
	bcs.w opforgeNativeCliInferSelectedShapeNone
	lea 9(a0, d0.w), a0

opforgeNativeCliInferSelectedShapeTrimLeading
	tst.w d2
	beq.w opforgeNativeCliInferSelectedShapeNone
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s opforgeNativeCliInferSelectedShapeTrimLeadingOne
	cmpi.b #9, d3
	bne.s opforgeNativeCliInferSelectedShapeTrimTrailing

opforgeNativeCliInferSelectedShapeTrimLeadingOne
	addq.l #1, a0
	subq.w #1, d2
	bra.s opforgeNativeCliInferSelectedShapeTrimLeading

opforgeNativeCliInferSelectedShapeTrimTrailing
	tst.w d2
	beq.w opforgeNativeCliInferSelectedShapeNone
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #' ', d3
	beq.s opforgeNativeCliInferSelectedShapeTrimTrailingOne
	cmpi.b #9, d3
	bne.s opforgeNativeCliInferSelectedShapeReady

opforgeNativeCliInferSelectedShapeTrimTrailingOne
	subq.w #1, d2
	bra.s opforgeNativeCliInferSelectedShapeTrimTrailing

opforgeNativeCliInferSelectedShapeReady
	cmpi.w #1, d2
	bne.s opforgeNativeCliInferSelectedShapeCheckPrefix
	move.b (a0), d3
	ori.b #$20, d3
	cmpi.b #'a', d3
	beq.w opforgeNativeCliInferSelectedShapeAccumulator

opforgeNativeCliInferSelectedShapeCheckPrefix
	move.b (a0), d3
	cmpi.b #'#', d3
	beq.w opforgeNativeCliInferSelectedShapeImmediate
	cmpi.b #'(', d3
	beq.w opforgeNativeCliInferSelectedShapeParen
	bsr.w opforgeNativeCliInferSelectedShapeSuffix
	cmpi.b #'x', d0
	beq.w opforgeNativeCliInferSelectedShapeDirectX
	cmpi.b #'y', d0
	beq.w opforgeNativeCliInferSelectedShapeDirectY
	bra.w opforgeNativeCliInferSelectedShapeDirect

opforgeNativeCliInferSelectedShapeParen
	bsr.w opforgeNativeCliInferSelectedShapeSuffix
	cmpi.b #'y', d0
	beq.w opforgeNativeCliInferSelectedShapeIndirectIndexedY
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #')', d3
	bne.w opforgeNativeCliInferSelectedShapeIndirect
	cmpi.w #4, d2
	bcs.w opforgeNativeCliInferSelectedShapeIndirect
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	ori.b #$20, d3
	cmpi.b #'x', d3
	bne.w opforgeNativeCliInferSelectedShapeIndirect
	tst.w d4
	beq.w opforgeNativeCliInferSelectedShapeIndirect
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #',', d3
	beq.w opforgeNativeCliInferSelectedShapeIndexedIndirectX
	bra.w opforgeNativeCliInferSelectedShapeIndirect

opforgeNativeCliInferSelectedShapeSuffix
	moveq #0, d0
	cmpi.w #3, d2
	bcs.s opforgeNativeCliInferSelectedShapeSuffixReturn
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	ori.b #$20, d3
	cmpi.b #'x', d3
	beq.s opforgeNativeCliInferSelectedShapeSuffixMaybe
	cmpi.b #'y', d3
	bne.s opforgeNativeCliInferSelectedShapeSuffixReturn

opforgeNativeCliInferSelectedShapeSuffixMaybe
	move.b d3, d0
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #',', d3
	beq.s opforgeNativeCliInferSelectedShapeSuffixReturn
	moveq #0, d0

opforgeNativeCliInferSelectedShapeSuffixReturn
	rts

opforgeNativeCliInferSelectedShapeBranchMnemonic
	cmpi.w #3, d0
	beq.s opforgeNativeCliInferSelectedShapeBranchLenOk
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchLenOk
	lea 9(a2), a1
	move.b (a1)+, d1
	ori.b #$20, d1
	cmpi.b #'b', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchHaveB
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchHaveB
	move.b (a1)+, d1
	move.b (a1), d2
	ori.b #$20, d1
	ori.b #$20, d2
	cmpi.b #'c', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckC
	cmpi.b #'e', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckEq
	cmpi.b #'n', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckNe
	cmpi.b #'m', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckMi
	cmpi.b #'p', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckPl
	cmpi.b #'v', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckV
	cmpi.b #'r', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckRa
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckC
	cmpi.b #'c', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	cmpi.b #'s', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckEq
	cmpi.b #'q', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckNe
	cmpi.b #'e', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckMi
	cmpi.b #'i', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckPl
	cmpi.b #'l', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckV
	cmpi.b #'c', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	cmpi.b #'s', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckRa
	cmpi.b #'a', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchYes
	moveq #1, d0
	rts

opforgeNativeCliInferSelectedShapeAccumulator
	lea NativeCliSelectedShapeAccumulatorText, a0
	moveq #11, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeImmediate
	lea NativeCliSelectedShapeImmediateText, a0
	moveq #9, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeDirect
	lea NativeCliSelectedShapeDirectText, a0
	moveq #6, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeDirectX
	lea NativeCliSelectedShapeDirectXText, a0
	moveq #8, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeDirectY
	lea NativeCliSelectedShapeDirectYText, a0
	moveq #8, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeIndirect
	lea NativeCliSelectedShapeIndirectText, a0
	moveq #8, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeIndexedIndirectX
	lea NativeCliSelectedShapeIndexedIndirectXText, a0
	moveq #18, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeIndirectIndexedY
	lea NativeCliSelectedShapeIndirectIndexedYText, a0
	moveq #18, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeNone
	moveq #0, d0

opforgeNativeCliInferSelectedShapeReturn
	movem.l (sp)+, d1-d7/a1-a2
	rts
	.bend  ; opforgeNativeCliInferSelectedShapeForEvalRequest

opforgeNativeCliClearStatementExprSpanForSyntheticRequest	.block
	clr.w NativeCliStmtExprFound
	clr.l NativeCliStmtExprSpanLine
	clr.l NativeCliStmtExprSpanStart
	clr.l NativeCliStmtExprSpanEnd
	rts
	.bend  ; opforgeNativeCliClearStatementExprSpanForSyntheticRequest

opforgeNativeCliDispatchEncodeInstructionEnvelope	.block
	bsr.w opforgeNativeCliPrepareEncodeInstructionRequest
	tst.l d0
	bne.s done
	lea ControlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliEncodeRequestLen, d1
	jsr opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_ENCODE_INSTRUCTION, d0
	jsr tkpkgServiceDispatchV1
	jsr opforgeNativeCliReadStatus

done
	rts
	.bend  ; opforgeNativeCliDispatchEncodeInstructionEnvelope

	.endsection
	.endmodule
