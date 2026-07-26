; opasm-owned bridge for tkpkg encode/evaluate service dispatch.

	.module opasm.amigaos.tkpkg_bridge
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi
	.use tkpkg.amigaos.abi as tkabi
	.use tkpkg.amigaos.service

	.section code, kind=code
	.pub

; Dispatch the selected-instruction encoder service using an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
; - D0: request byte length in the service IO buffer.
;
; Outputs:
; - D0: tkpkg status byte.
; - D1: tkpkg output byte length.
; - D2: tkpkg last-error byte length.
; - Clobbers D3-D5.
adaptSelectedEncodeRequestV1	.block
	move.w d0, d1
	moveq #tkabi.ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0
	bra.w dispatchServiceV1
	.bend  ; adaptSelectedEncodeRequestV1

; Compatibility delegate retained while callers migrate to the explicit
; selector-adaptation name.
dispatchEncodeSelectedV1	.block
	bra.w adaptSelectedEncodeRequestV1
	.bend  ; dispatchEncodeSelectedV1

; Dispatch the expression evaluator service using an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
; - D0: request byte length in the service IO buffer.
;
; Outputs:
; - D0: tkpkg status byte.
; - D1: tkpkg output byte length.
; - D2: tkpkg last-error byte length.
; - Clobbers D3-D5.
dispatchEvaluateExpressionV1	.block
	move.w d0, d1
	moveq #tkabi.ENTRY_ORD_EVALUATE_EXPRESSION, d0
	bra.w dispatchServiceV1
	.bend  ; dispatchEvaluateExpressionV1

; Dispatch one tkpkg service using an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
; - D0: tkabi.ENTRY_ORD_* service ordinal.
; - D1: request byte length in the service IO buffer.
;
; Outputs:
; - D0: tkpkg status byte.
; - D1: tkpkg output byte length.
; - D2: tkpkg last-error byte length.
; - Clobbers D3-D5.
dispatchServiceV1	.block
	movem.l a0-a2, -(sp)
	movea.l a0, a2
	move.w d0, d4
	move.w d1, d5
	bsr.w writeInputWindow
	bsr.w writeExtensionWindow
	move.l a2, -(sp)
	movea.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), a0
	move.w d4, d0
	jsr service.dispatchV1
	movea.l (sp)+, a2
	movea.l a2, a0
	bsr.w readStatusV1
	move.w d0, d3
	movea.l a2, a0
	bsr.w readOutputLenV1
	move.w d0, d4
	movea.l a2, a0
	bsr.w readLastErrorLenV1
	move.w d0, d5
	move.w d3, d0
	move.w d4, d1
	move.w d5, d2
	movem.l (sp)+, a0-a2
	rts
	.bend  ; dispatchServiceV1

; Read tkpkg service status from the control block named by an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
;
; Outputs:
; - D0: tkpkg status byte.
readStatusV1	.block
	movea.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a0), a0
	moveq #0, d0
	move.b tkabi.CB_STATUS_CODE(a0), d0
	rts
	.bend  ; readStatusV1

; Read tkpkg output length from the control block named by an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
;
; Outputs:
; - D0: tkpkg output byte length.
readOutputLenV1	.block
	movea.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a0), a0
	moveq #0, d0
	move.b tkabi.CB_OUTPUT_LEN(a0), d0
	moveq #0, d1
	move.b 23(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts
	.bend  ; readOutputLenV1

; Read tkpkg output pointer from the control block named by an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
;
; Outputs:
; - A0: tkpkg output buffer pointer.
readOutputPtrV1	.block
	movea.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a0), a0
	moveq #0, d0
	move.b tkabi.CB_OUTPUT_PTR(a0), d0
	moveq #0, d1
	move.b 21(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	adda.w d0, a0
	rts
	.bend  ; readOutputPtrV1

; Read tkpkg last-error length from the control block named by an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
;
; Outputs:
; - D0: tkpkg last-error byte length.
readLastErrorLenV1	.block
	movea.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a0), a0
	moveq #0, d0
	move.b tkabi.CB_LAST_ERROR_LEN(a0), d0
	moveq #0, d1
	move.b 31(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts
	.bend  ; readLastErrorLenV1

; Read tkpkg last-error pointer from the control block named by an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
;
; Outputs:
; - A0: tkpkg last-error buffer pointer.
readLastErrorPtrV1	.block
	movea.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a0), a0
	moveq #0, d0
	move.b tkabi.CB_LAST_ERROR_PTR(a0), d0
	moveq #0, d1
	move.b 29(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	adda.w d0, a0
	rts
	.bend  ; readLastErrorPtrV1

	.priv

writeInputWindow	.block
	movea.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), a0
	move.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a2), d0
	sub.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), d0
	move.w d5, d1
	move.b d0, tkabi.CB_INPUT_PTR(a0)
	lsr.w #8, d0
	move.b d0, 17(a0)
	move.b d1, tkabi.CB_INPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 19(a0)
	rts
	.bend  ; writeInputWindow

writeExtensionWindow	.block
	movea.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), a0
	move.w abi.OPASM_SERVICE_EVAL_EXTENSION_BYTES(a2), d1
	beq.s clearExtension
	move.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a2), d0
	sub.l abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), d0
	move.b d0, tkabi.CB_EXTENSION_PTR(a0)
	lsr.w #8, d0
	move.b d0, 25(a0)
	move.b d1, tkabi.CB_EXTENSION_LEN(a0)
	lsr.w #8, d1
	move.b d1, 27(a0)
	rts

clearExtension
	clr.b tkabi.CB_EXTENSION_PTR(a0)
	clr.b 25(a0)
	clr.b tkabi.CB_EXTENSION_LEN(a0)
	clr.b 27(a0)
	rts
	.bend  ; writeExtensionWindow

	.endsection
	.endmodule
