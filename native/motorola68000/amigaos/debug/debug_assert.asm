; Generic native debug-contract predicate routines.

	.module opforge.debug.assert
	.cpu 68020

	.use opforge.debug.contracts as contracts
	.use opforge.debug.events as events

DEBUG_ASSERT_ROUTINE_SPAN_IN_TEXT = 1
DEBUG_ASSERT_ROUTINE_NO_BUFFER_OVERLAP = 2
DEBUG_ASSERT_CONTRACT_STACK_OFFSET = 66

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Assert that a 1-based, end-exclusive span is inside a text buffer.
;
; Inputs:
; - D0: text length.
; - D1: 1-based span start.
; - D2: 1-based exclusive span end.
; - 4(SP): contract ID word pushed by the fixed-size call-site macro.
;
; Outputs:
; - One EVENT_ASSERT_FAIL record on failure; no event on success.
;
; Clobbers:
; - None.
;
; CCR:
; - Preserved exactly.
; ---------------------------------------------------------------------------
debugAssertSpanInText	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l d0, a0
	movea.l d1, a1
	movea.l d2, a2

	cmpi.l #1, d1
	blo.s fail
	cmp.l d1, d2
	blo.s fail
	move.l d0, d7
	addq.l #1, d7
	bcs.s fail
	cmp.l d7, d2
	bls.s done

fail
	moveq #0, d0
	move.w #contracts.EVENT_ASSERT_FAIL, d0
	moveq #0, d1
	move.w DEBUG_ASSERT_CONTRACT_STACK_OFFSET(sp), d1
	moveq #DEBUG_ASSERT_ROUTINE_SPAN_IN_TEXT, d2
	move.l a0, d3
	move.l a1, d4
	move.l a2, d5
	moveq #0, d6
	jsr events.debugEventU32x4

done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; debugAssertSpanInText

; ---------------------------------------------------------------------------
; Assert that two half-open pointer ranges do not overlap or wrap.
;
; Inputs:
; - D0: first range pointer.
; - D1: first range byte length.
; - D2: second range pointer.
; - D3: second range byte length.
; - 4(SP): contract ID word pushed by the fixed-size call-site macro.
;
; Outputs:
; - One EVENT_ASSERT_FAIL record on failure; no event on success.
;
; Clobbers:
; - None.
;
; CCR:
; - Preserved exactly.
; ---------------------------------------------------------------------------
debugAssertNoBufferOverlap	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l d0, a0
	movea.l d1, a1
	movea.l d2, a2
	movea.l d3, a3

	tst.l d1
	beq.s done
	tst.l d3
	beq.s done
	move.l d0, d4
	add.l d1, d4
	bcs.s fail
	move.l d2, d5
	add.l d3, d5
	bcs.s fail
	cmp.l d2, d4
	bls.s done
	cmp.l d0, d5
	bls.s done

fail
	moveq #0, d0
	move.w #contracts.EVENT_ASSERT_FAIL, d0
	moveq #0, d1
	move.w DEBUG_ASSERT_CONTRACT_STACK_OFFSET(sp), d1
	moveq #DEBUG_ASSERT_ROUTINE_NO_BUFFER_OVERLAP, d2
	move.l a0, d3
	move.l a1, d4
	move.l a2, d5
	move.l a3, d6
	jsr events.debugEventU32x4

done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; debugAssertNoBufferOverlap

	.endsection
	.endmodule
