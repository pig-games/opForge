; Fixed-size native debug-contract call-site macros.
;
; Callers import opforge.debug.assert as debug_assert and
; opforge.debug.events as debug_events before including this file.

DEBUG_ASSERT_SPAN_IN_TEXT	.macro contract_id
.ifdef OPFORGE_DEBUG_CONTRACTS
	move.w ccr, -(sp)
	move.w #.contract_id, -(sp)
	jsr debug_assert.debugAssertSpanInText
	lea 2(sp), sp
	move.w (sp)+, ccr
.endif
.endmacro

DEBUG_ASSERT_NO_BUFFER_OVERLAP	.macro contract_id
.ifdef OPFORGE_DEBUG_CONTRACTS
	move.w ccr, -(sp)
	move.w #.contract_id, -(sp)
	jsr debug_assert.debugAssertNoBufferOverlap
	lea 2(sp), sp
	move.w (sp)+, ccr
.endif
.endmacro

DEBUG_EVENT_U32X4	.macro event_id
.ifdef OPFORGE_DEBUG_CONTRACTS
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #0, d0
	move.w #.event_id, d0
	jsr debug_events.debugEventU32x4
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endmacro
