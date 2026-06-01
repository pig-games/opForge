; Native AmigaOS opForge CLI encode/evaluate bridge.

	.module opforge.cli.encode_eval_bridge
	.cpu 68020

	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.service

	.use opasm.amigaos.engine

	.use opforge.cli.constants
	.use opforge.cli.state
	.use opforge.cli.tkpkg_control_block

	.section code, kind=code
	.pub

opforgeNativeCliPrepareEncodeSelectedRequestForStatement	.block
	movem.l d1/d6/a1, -(sp)
	moveq #0, d0
	move.w d6, d0
	lea buffers.lastErrorBuffer, a1
	jsr engine.prepareSelectedEvaluateRequestV1
	bne.s return
	move.w d1, state.NativeCliEvalRequestLen

return
	movem.l (sp)+, d1/d6/a1
	rts
	.bend  ; opforgeNativeCliPrepareEncodeSelectedRequestForStatement

opforgeNativeCliPrepareEvaluateExpressionRequest	.block
	movem.l d1/a0-a1, -(sp)
	lea buffers.lastErrorBuffer, a1
	move.w d7, d1
	jsr engine.prepareEvaluateExpressionRequestV1
	bne.s return
	move.w d1, state.NativeCliEvalRequestLen

return
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; opforgeNativeCliPrepareEvaluateExpressionRequest

opforgeNativeCliPrepareEvaluateExpressionExtension	.block
	movem.l d1/a0-a1, -(sp)
	lea buffers.lastErrorBuffer, a0
	moveq #0, d0
	move.w state.NativeCliEvalRequestLen, d0
	lea buffers.ControlBlockV1, a1
	adda.w #constants.NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a1
	jsr engine.prepareEvaluateExpressionExtensionV1
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; opforgeNativeCliPrepareEvaluateExpressionExtension

opforgeNativeCliReadEvaluateExpressionValue	.block
	lea buffers.ControlBlockV1, a0
	adda.w #constants.NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a0
	move.l 16(a0), d3
	rts
	.bend  ; opforgeNativeCliReadEvaluateExpressionValue

opforgeNativeCliLoadStatementSourceLineText	.block
	moveq #0, d0
	move.w d7, d0
	jsr engine.getStatementSourceLineTextV1
	rts
	.bend  ; opforgeNativeCliLoadStatementSourceLineText

opforgeNativeCliLoadStatementExprMetadata	.block
	moveq #0, d0
	move.w d7, d0
	jsr engine.statementHasExprMetadataV1
	tst.l d0
	beq.s empty
	move.w #1, state.NativeCliStmtExprFound
	rts

empty
	clr.w state.NativeCliStmtExprFound
	rts
	.bend  ; opforgeNativeCliLoadStatementExprMetadata

	.priv

; Build the encode-instruction service payload for the current statement mnemonic.
; Inputs: state.NativeCliStmtMnemStart/state.NativeCliStmtMnemLen describe the mnemonic text.
; Outputs: D0 = 0 on success; state.NativeCliEncodeRequestLen updated from D1 on success.
; Clobbers: D1/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliPrepareEncodeInstructionRequest	.block
	movem.l d1/a0-a1, -(sp)
	lea buffers.lastErrorBuffer, a1
	movea.l state.NativeCliStmtMnemStart, a0
	move.l state.NativeCliStmtMnemLen, d0
	jsr engine.prepareEncodeInstructionRequestV1
	bne.s return
	move.w d1, state.NativeCliEncodeRequestLen

return
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; opforgeNativeCliPrepareEncodeInstructionRequest

opforgeNativeCliDispatchEncodeInstructionEnvelope	.block
	bsr.w opforgeNativeCliPrepareEncodeInstructionRequest
	bne.s done
	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.w state.NativeCliEncodeRequestLen, d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_ENCODE_INSTRUCTION, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus

done
	rts
	.bend  ; opforgeNativeCliDispatchEncodeInstructionEnvelope

	.endsection
	.endmodule
