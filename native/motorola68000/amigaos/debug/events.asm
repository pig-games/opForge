; Bounded structured native debug-event storage.

	.module opforge.debug.events
	.cpu 68020
	.pub

DEBUG_EVENT_KIND = 0
DEBUG_EVENT_CONTRACT_ID = 2
DEBUG_EVENT_ROUTINE_ID = 4
DEBUG_EVENT_STATEMENT_INDEX = 6
DEBUG_EVENT_LINE_NUMBER = 8
DEBUG_EVENT_ARG0 = 12
DEBUG_EVENT_ARG1 = 16
DEBUG_EVENT_ARG2 = 20
DEBUG_EVENT_ARG3 = 24
DEBUG_EVENT_RECORD_BYTES = 28
DEBUG_EVENT_CAPACITY = 8

	.section code, kind=code

; ---------------------------------------------------------------------------
; Reset the bounded debug-event buffer.
;
; Inputs:
; - None.
;
; Outputs:
; - DebugEventCount is zero.
;
; Clobbers:
; - None.
;
; CCR:
; - Preserved exactly.
; ---------------------------------------------------------------------------
debugEventReset	.block
	move.w ccr, -(sp)
	clr.w DebugEventCount
	move.w (sp)+, ccr
	rts
	.bend  ; debugEventReset

; ---------------------------------------------------------------------------
; Append one structured event when capacity remains.
;
; Inputs:
; - D0: event kind.
; - D1: contract ID, or zero for a passive event.
; - D2: routine ID.
; - D3-D6: four unsigned event arguments.
;
; Outputs:
; - DebugEventCount and DebugEventBuffer are updated when capacity remains.
; - A full buffer is left unchanged.
;
; Clobbers:
; - None.
;
; CCR:
; - Preserved exactly.
; ---------------------------------------------------------------------------
debugEventU32x4	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea DebugEventCount, a0
	moveq #0, d7
	move.w (a0), d7
	cmpi.w #DEBUG_EVENT_CAPACITY, d7
	bhs.s done

	mulu.w #DEBUG_EVENT_RECORD_BYTES, d7
	lea DebugEventBuffer, a1
	adda.l d7, a1
	move.w d0, DEBUG_EVENT_KIND(a1)
	move.w d1, DEBUG_EVENT_CONTRACT_ID(a1)
	move.w d2, DEBUG_EVENT_ROUTINE_ID(a1)
	clr.w DEBUG_EVENT_STATEMENT_INDEX(a1)
	clr.l DEBUG_EVENT_LINE_NUMBER(a1)
	move.l d3, DEBUG_EVENT_ARG0(a1)
	move.l d4, DEBUG_EVENT_ARG1(a1)
	move.l d5, DEBUG_EVENT_ARG2(a1)
	move.l d6, DEBUG_EVENT_ARG3(a1)
	addq.w #1, (a0)

done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; debugEventU32x4

	.endsection

	.section bss, kind=bss

DebugEventCount
	.res word, 1

	.align 2
DebugEventBuffer
	.res byte, DEBUG_EVENT_RECORD_BYTES * DEBUG_EVENT_CAPACITY

	.endsection
	.endmodule
