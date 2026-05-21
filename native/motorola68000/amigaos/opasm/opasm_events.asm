; Native opasm event record helpers.

	.module opasm.amigaos.events
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi

	.section code, kind=code
	.pub

; Clear an opasm event count word.
;
; Inputs:
; - A0: event count word pointer.
;
; Outputs:
; - D0: abi.OPASM_STATUS_OK.
resetCountV1	.block
	clr.w (a0)
	moveq #abi.OPASM_STATUS_OK, d0
	rts
	.bend  ; resetCountV1

; Append one fixed-size opasm event record to an event buffer.
;
; Inputs:
; - A0: event buffer base.
; - A1: event count word pointer.
; - A2: source event record using abi.OPASM_EVENT_* offsets.
; - D0: event capacity in records.
;
; Outputs:
; - D0: abi.OPASM_STATUS_OK or abi.OPASM_STATUS_EVENT_CAPACITY.
; - A0: appended event pointer on success, or zero on capacity failure.
appendV1	.block
	movem.l d1-d3/a1-a3, -(sp)
	move.w (a1), d1
	cmp.w d0, d1
	bhs.s capacity
	movea.l a0, a3
	moveq #0, d2
	move.w d1, d2
	lsl.l #5, d2
	adda.l d2, a3
	movea.l a3, a0
	moveq #abi.OPASM_EVENT_BYTES - 1, d3

copyLoop
	move.b (a2)+, (a3)+
	dbf d3, copyLoop
	addq.w #1, (a1)
	movem.l (sp)+, d1-d3/a1-a3
	moveq #abi.OPASM_STATUS_OK, d0
	rts

capacity
	movem.l (sp)+, d1-d3/a1-a3
	suba.l a0, a0
	moveq #abi.OPASM_STATUS_EVENT_CAPACITY, d0
	rts
	.bend  ; appendV1

	.endsection
	.endmodule
