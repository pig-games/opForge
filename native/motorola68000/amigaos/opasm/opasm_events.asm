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

; Append one fixed-size event while preserving the newest event when the
; caller's bounded buffer is already full. Rust retains the complete ordered
; event stream; native callers cannot grow their buffer, so they retain the
; first capacity-1 events plus the newest event. This keeps the terminal error
; that caused a failed assembly observable instead of hiding it behind earlier
; successful progress events.
;
; Inputs and outputs match appendV1. On replacement D0 remains
; abi.OPASM_STATUS_EVENT_CAPACITY and A0 addresses the replaced final record.
appendRetainingNewestV1	.block
	movem.l d1-d4/a1-a3, -(sp)
	move.w (a1), d1
	cmp.w d0, d1
	blo.s append
	tst.w d0
	beq.s capacity
	moveq #0, d2
	move.w d0, d2
	subq.w #1, d2
	lsl.l #5, d2
	adda.l d2, a0
	movea.l a0, a3
	moveq #abi.OPASM_EVENT_BYTES - 1, d4
replaceLoop
	move.b (a2)+, (a3)+
	dbf d4, replaceLoop
	movem.l (sp)+, d1-d4/a1-a3
	moveq #abi.OPASM_STATUS_EVENT_CAPACITY, d0
	rts
append
	jsr appendV1
	movem.l (sp)+, d1-d4/a1-a3
	rts
capacity
	movem.l (sp)+, d1-d4/a1-a3
	suba.l a0, a0
	moveq #abi.OPASM_STATUS_EVENT_CAPACITY, d0
	rts
	.bend  ; appendRetainingNewestV1

	.endsection
	.endmodule
