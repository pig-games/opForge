; Native opasm statement-navigation helpers.

	.module opasm.amigaos.flow_navigation
	.cpu 68020

	.section code, kind=code
	.pub

; Initialize ordinary forward statement flow for one callback invocation.
; Inputs: D0.W = current statement index.
; Outputs: D0 = 0; D1 = 0 to process; D2.W = next statement index.
; Clobbers: D0-D2/CCR.
; CCR: reflects D0 on return.
initializeStatementFlowV1	.block
	move.w d0, d2
	addq.w #1, d2
	clr.w d1
	moveq #0, d0
	rts
	.bend  ; initializeStatementFlowV1

	.endsection
	.endmodule
