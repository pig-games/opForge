; Native opasm operand/evaluation request construction.

	.module opasm.amigaos.operand_eval
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.engine as eng

	.section code, kind=code
	.pub

; Build one selected-instruction evaluation request.
; Inputs: D0.W = statement index; A0 = OPASM_SERVICE_* frame.
; Outputs: D0 = status; D1.W = request bytes on success.
prepareSelectedRequestV1	.block
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a1
	jsr eng.prepareSelectedEvaluateRequestV1
	rts
	.bend  ; prepareSelectedRequestV1

; Build one textual expression evaluation request.
; Inputs: A0 = expression text; D0.L = text bytes; D1.W = statement index;
;         A1 = OPASM_SERVICE_* frame.
; Outputs: D0 = status; D1.W = request bytes on success.
prepareExpressionRequestV1	.block
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a1), a2
	movea.l a2, a1
	jsr eng.prepareEvaluateExpressionRequestV1
	rts
	.bend  ; prepareExpressionRequestV1

; Append the evaluation extension to a prepared request.
; Inputs: A0 = OPASM_SERVICE_* frame; D0.W = request bytes.
; Outputs: D0 = engine status.
prepareExpressionExtensionV1	.block
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a2
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a1
	movea.l a2, a0
	jsr eng.prepareEvaluateExpressionExtensionV1
	rts
	.bend  ; prepareExpressionExtensionV1

; Prepare the directive-specific evaluation extension.
; Inputs: A0 = OPASM_SERVICE_* frame.
; Outputs: D0 = engine status.
prepareDirectiveExpressionExtensionV1	.block
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a0
	jsr eng.prepareDirectiveEvaluateExpressionExtensionV1
	rts
	.bend  ; prepareDirectiveExpressionExtensionV1

	.endsection
	.endmodule
