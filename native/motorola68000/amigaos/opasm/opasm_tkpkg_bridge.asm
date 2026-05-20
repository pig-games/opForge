; opasm-owned bridge for tkpkg encode/evaluate service dispatch.

	.module opasm.amigaos.tkpkg_bridge
	.cpu 68020

	.use opasm.amigaos.callback_abi (OPASM_SERVICE_CONTROL_BLOCK_PTR, OPASM_SERVICE_IO_BUFFER_PTR)
	.use opasm.amigaos.callback_abi (OPASM_SERVICE_EVAL_EXTENSION_PTR, OPASM_SERVICE_EVAL_EXTENSION_BYTES)
	.use tkpkg.amigaos.abi (ENTRY_ORD_EVALUATE_EXPRESSION, ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION)
	.use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN, CB_OUTPUT_LEN)
	.use tkpkg.amigaos.abi (CB_EXTENSION_PTR, CB_EXTENSION_LEN, CB_STATUS_CODE, CB_LAST_ERROR_LEN)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)

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
opasmTkpkgBridgeDispatchEncodeSelectedV1	.block
	move.w d0, d1
	moveq #ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0
	bra.w opasmTkpkgBridgeDispatchServiceV1
	.bend  ; opasmTkpkgBridgeDispatchEncodeSelectedV1

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
opasmTkpkgBridgeDispatchEvaluateExpressionV1	.block
	move.w d0, d1
	moveq #ENTRY_ORD_EVALUATE_EXPRESSION, d0
	bra.w opasmTkpkgBridgeDispatchServiceV1
	.bend  ; opasmTkpkgBridgeDispatchEvaluateExpressionV1

; Dispatch one tkpkg service using an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
; - D0: ENTRY_ORD_* service ordinal.
; - D1: request byte length in the service IO buffer.
;
; Outputs:
; - D0: tkpkg status byte.
; - D1: tkpkg output byte length.
; - D2: tkpkg last-error byte length.
opasmTkpkgBridgeDispatchServiceV1	.block
	movem.l d3-d5/a0-a2, -(sp)
	movea.l a0, a2
	move.w d0, d4
	move.w d1, d5
	bsr.w writeInputWindow
	bsr.w writeExtensionWindow
	movea.l OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), a0
	move.w d4, d0
	jsr tkpkgServiceDispatchV1
	movea.l a2, a0
	bsr.w opasmTkpkgBridgeReadStatusV1
	move.w d0, d3
	movea.l a2, a0
	bsr.w opasmTkpkgBridgeReadOutputLenV1
	move.w d0, d1
	movea.l a2, a0
	bsr.w opasmTkpkgBridgeReadLastErrorLenV1
	move.w d0, d2
	move.w d3, d0
	movem.l (sp)+, d3-d5/a0-a2
	rts
	.bend  ; opasmTkpkgBridgeDispatchServiceV1

; Read tkpkg service status from the control block named by an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
;
; Outputs:
; - D0: tkpkg status byte.
opasmTkpkgBridgeReadStatusV1	.block
	movea.l OPASM_SERVICE_CONTROL_BLOCK_PTR(a0), a0
	moveq #0, d0
	move.b CB_STATUS_CODE(a0), d0
	rts
	.bend  ; opasmTkpkgBridgeReadStatusV1

; Read tkpkg output length from the control block named by an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
;
; Outputs:
; - D0: tkpkg output byte length.
opasmTkpkgBridgeReadOutputLenV1	.block
	movea.l OPASM_SERVICE_CONTROL_BLOCK_PTR(a0), a0
	moveq #0, d0
	move.b CB_OUTPUT_LEN(a0), d0
	moveq #0, d1
	move.b 23(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts
	.bend  ; opasmTkpkgBridgeReadOutputLenV1

; Read tkpkg last-error length from the control block named by an opasm service frame.
;
; Inputs:
; - A0: OPASM_SERVICE_* frame.
;
; Outputs:
; - D0: tkpkg last-error byte length.
opasmTkpkgBridgeReadLastErrorLenV1	.block
	movea.l OPASM_SERVICE_CONTROL_BLOCK_PTR(a0), a0
	moveq #0, d0
	move.b CB_LAST_ERROR_LEN(a0), d0
	moveq #0, d1
	move.b 31(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts
	.bend  ; opasmTkpkgBridgeReadLastErrorLenV1

	.priv

writeInputWindow	.block
	movea.l OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), a0
	move.l OPASM_SERVICE_IO_BUFFER_PTR(a2), d0
	sub.l OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), d0
	move.w d5, d1
	move.b d0, CB_INPUT_PTR(a0)
	lsr.w #8, d0
	move.b d0, 17(a0)
	move.b d1, CB_INPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 19(a0)
	rts
	.bend  ; writeInputWindow

writeExtensionWindow	.block
	movea.l OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), a0
	move.w OPASM_SERVICE_EVAL_EXTENSION_BYTES(a2), d1
	beq.s clearExtension
	move.l OPASM_SERVICE_EVAL_EXTENSION_PTR(a2), d0
	sub.l OPASM_SERVICE_CONTROL_BLOCK_PTR(a2), d0
	move.b d0, CB_EXTENSION_PTR(a0)
	lsr.w #8, d0
	move.b d0, 25(a0)
	move.b d1, CB_EXTENSION_LEN(a0)
	lsr.w #8, d1
	move.b d1, 27(a0)
	rts

clearExtension
	clr.b CB_EXTENSION_PTR(a0)
	clr.b 25(a0)
	clr.b CB_EXTENSION_LEN(a0)
	clr.b 27(a0)
	rts
	.bend  ; writeExtensionWindow

	.endsection
	.endmodule
