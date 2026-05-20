; Native AmigaOS opForge CLI encode/evaluate bridge.

	.module opforge.cli.encode_eval_bridge
	.cpu 68020

	.use tkpkg.amigaos.abi (ENTRY_ORD_ENCODE_INSTRUCTION)
	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, LAST_ERROR_BUFFER_PTR_V1)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)

	.use opasm.amigaos.engine (opasmEngineGetStatementSourceLineTextV1, opasmEngineGetStatementExprMetadataV1)
	.use opasm.amigaos.engine (opasmEngineWriteEvaluateExpressionExtensionBaseV1)
	.use opasm.amigaos.engine (opasmEnginePrepareEvaluateExpressionRequestV1)
	.use opasm.amigaos.engine (opasmEnginePrepareSelectedEvaluateRequestV1)
	.use opasm.amigaos.engine (opasmEnginePrepareEncodeInstructionRequestV1)
	.use opasm.amigaos.engine (opasmEngineInferSelectedShapeForEvalRequestV1)
	.use opasm.amigaos.engine (OPASM_ENGINE_EXPR_META_BYTES)

	.use opforge.cli.constants (NATIVE_EVAL_EXPR_EXTENSION_PTR_V1)
	.use opforge.cli.state (NativeCliEncodeRequestLen, NativeCliEvalRequestLen)
	.use opforge.cli.state (NativeCliStmtMnemStart, NativeCliStmtMnemLen)
	.use opforge.cli.state (NativeCliStmtExprFound)
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
	movem.l d1/a0-a1, -(sp)
	lea lastErrorBuffer, a1
	move.w d7, d1
	jsr opasmEnginePrepareEvaluateExpressionRequestV1
	tst.l d0
	bne.s return
	move.w d1, NativeCliEvalRequestLen

return
	movem.l (sp)+, d1/a0-a1
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
