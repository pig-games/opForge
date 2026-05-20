; Native opasm assembly-session driver.

	.module opasm.amigaos.assembly_driver
	.cpu 68020

	.use opasm.amigaos.callback_abi (OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR)
	.use opasm.amigaos.engine (opasmEngineRunTwoPassV1)

	.section code, kind=code
	.pub

; Run one native opasm assembly session.
;
; Inputs:
; - A0: OPASM_ASSEMBLE_REQ_* frame.
; - A4: transitional opasm engine callback context.
;
; Outputs:
; - D0: current opasm engine status.
; - A0: original request frame pointer.
opasmNativeAssembleSessionV1	.block
	movem.l a1, -(sp)
	movea.l a0, a1
	tst.l OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a1)
	beq.s run
	movea.l OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a1), a0
	clr.w (a0)

run
	jsr opasmEngineRunTwoPassV1
	movea.l a1, a0
	movem.l (sp)+, a1
	rts
	.bend  ; opasmNativeAssembleSessionV1

	.endsection
	.endmodule
