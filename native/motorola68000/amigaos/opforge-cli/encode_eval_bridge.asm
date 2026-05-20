; Native AmigaOS opForge CLI encode/evaluate bridge.

	.module opforge.cli.encode_eval_bridge
	.cpu 68020

	.use tkpkg.amigaos.abi (ENTRY_ORD_ENCODE_INSTRUCTION)
	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, LAST_ERROR_BUFFER_PTR_V1)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)

	.use opasm.amigaos.engine (opasmEngineGetStatementSourceLineTextV1, opasmEngineGetStatementExprMetadataV1)
	.use opasm.amigaos.engine (opasmEngineGetStatementLineNumberV1)
	.use opasm.amigaos.engine (opasmEngineWriteEvaluateExpressionExtensionBaseV1)
	.use opasm.amigaos.engine (opasmEnginePrepareSelectedEvaluateRequestV1)
	.use opasm.amigaos.engine (opasmEnginePrepareEncodeInstructionRequestV1)
	.use opasm.amigaos.engine (opasmEngineInferSelectedShapeForEvalRequestV1)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_OPERAND_INDEX, OPASM_ENGINE_EXPR_META_SLOT_INDEX)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_START_TOKEN, OPASM_ENGINE_EXPR_META_END_TOKEN)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_SPAN_LINE, OPASM_ENGINE_EXPR_META_SPAN_START)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_SPAN_END, OPASM_ENGINE_EXPR_META_BYTES)

	.use opforge.cli.constants (NATIVE_EVAL_EXPR_EXTENSION_PTR_V1)
	.use opforge.cli.state (NativeCliEncodeRequestLen, NativeCliEvalRequestLen)
	.use opforge.cli.state (NativeCliStmtMnemStart, NativeCliStmtMnemLen)
	.use opforge.cli.state (NativeCliStmtExprFound, NativeCliStmtExprOperandIndex, NativeCliStmtExprSlotIndex)
	.use opforge.cli.state (NativeCliStmtExprStartToken, NativeCliStmtExprEndToken)
	.use opforge.cli.state (NativeCliStmtExprSpanLine, NativeCliStmtExprSpanStart, NativeCliStmtExprSpanEnd)
	.use opforge.cli.copy (opforgeNativeCliCopyFixedString)
	.use opforge.cli.tkpkg_control_block (opforgeNativeCliWriteInputWindow, opforgeNativeCliReadStatus)

	.section code, kind=code
	.pub

opforgeNativeCliPrepareEncodeSelectedRequestForStatement	.block
	movem.l d1/d6/a1, -(sp)
	moveq #0, d0
	move.w d6, d0
	lea lastErrorBuffer, a1
	jsr opasmEnginePrepareSelectedEvaluateRequestV1
	tst.l d0
	bne.s return
	move.w d1, NativeCliEvalRequestLen

return
	movem.l (sp)+, d1/d6/a1
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
	lea lastErrorBuffer, a0
	moveq #0, d0
	move.w NativeCliEvalRequestLen, d0
	jsr opasmEngineInferSelectedShapeForEvalRequestV1
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
	movem.l d1/a0-a1, -(sp)
	lea lastErrorBuffer, a1
	movea.l NativeCliStmtMnemStart, a0
	move.l NativeCliStmtMnemLen, d0
	jsr opasmEnginePrepareEncodeInstructionRequestV1
	tst.l d0
	bne.s return
	move.w d1, NativeCliEncodeRequestLen

return
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; opforgeNativeCliPrepareEncodeInstructionRequest

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
