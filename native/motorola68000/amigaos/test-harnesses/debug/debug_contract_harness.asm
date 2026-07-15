; Executable FS-UAE harness for native debug-contract behavior and preservation.

	.module debug.contract.harness
	.cpu 68020

	.use opforge.debug.contracts as contracts
	.use opforge.debug.assert as debug_assert
	.use opforge.debug.events as debug_events

	.include "../../debug/debug_macros.i"

HARNESS_RETURN_OK = 0
HARNESS_RETURN_FAIL = 20
HARNESS_RETURN_BEHAVIOR_FAIL = 21
HARNESS_RETURN_PRESERVATION_FAIL = 22
HARNESS_EXPECTED_FLAGS = $001f

	.section entry, kind=code
	.pub

; Run native predicate, event, capacity, register, CCR, and stack checks.
;
; Inputs:
; - None.
;
; Outputs:
; - D0: zero on success; HARNESS_RETURN_FAIL on the first mismatch.
;
; Clobbers:
; - D0-D7/A0-A6/CCR.
;
; CCR:
; - Reflects D0 on return.
start	.block
	jsr runBehaviorChecks
	tst.l d0
	bne.s fail
	jsr runPreservationChecks
	tst.l d0
	bne.s fail
.ifdef OPFORGE_FS_UAE_CONSOLE_DEBUGGER_HARNESS
	; Instrumentation point: controlled console-debugger test harness stop.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A6 by the macro; this harness then loops.
	; SR/CCR preserved: not relied on by the loop.
	; Stack delta at return: zero; the loop has no stack traffic.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: test-harness-only build gate,
	; reached only after all behavior and preservation checks have passed.
	; Removal/stabilization plan: retain as the controlled FS-UAE console target.
	moveq #0, d0
	.DEBUG_EVENT_U32X4 contracts.EVENT_CONSOLE_DEBUGGER_READY
consoleDebuggerStopLoop
	bra.s consoleDebuggerStopLoop
.endif
	moveq #HARNESS_RETURN_OK, d0
	rts

fail
	rts
	.bend  ; start

; Exercise every initial macro shape from one stable listing boundary.
;
; Inputs:
; - D0-D6: predicate/event arguments.
;
; Outputs:
; - None.
;
; Clobbers:
; - None.
;
; CCR:
; - Preserved exactly by each macro expansion.
debugContractMacroStart	.block
	.DEBUG_ASSERT_SPAN_IN_TEXT contracts.CONTRACT_EXPR_REQ_001
	.DEBUG_ASSERT_NO_BUFFER_OVERLAP contracts.CONTRACT_BUF_001
	.DEBUG_EVENT_U32X4 contracts.EVENT_EXPR_REQUEST
debugContractMacroEnd
	rts
	.bend  ; debugContractMacroStart

; Run passing, failing, boundary, record-content, and capacity checks.
; Outputs: D0 = zero on success, HARNESS_RETURN_FAIL on mismatch.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0 on return.
runBehaviorChecks	.block
	jsr debug_events.debugEventReset

	; Zero-length text accepts the canonical empty span [1, 1).
	moveq #0, d0
	moveq #1, d1
	moveq #1, d2
	move.w #contracts.CONTRACT_EXPR_REQ_001, -(sp)
	jsr debug_assert.debugAssertSpanInText
	lea 2(sp), sp
	tst.w debug_events.DebugEventCount
	bne.w behaviorFail

	; Largest representable valid end-exclusive span must not overflow.
	move.l #$fffffffe, d0
	moveq #1, d1
	move.l #$ffffffff, d2
	move.w #contracts.CONTRACT_EXPR_REQ_001, -(sp)
	jsr debug_assert.debugAssertSpanInText
	lea 2(sp), sp
	tst.w debug_events.DebugEventCount
	bne.w behaviorFail

	; Invalid start emits the expected contract ID and original arguments.
	moveq #4, d0
	moveq #0, d1
	moveq #1, d2
	move.w #contracts.CONTRACT_EXPR_REQ_001, -(sp)
	jsr debug_assert.debugAssertSpanInText
	lea 2(sp), sp
	cmpi.w #1, debug_events.DebugEventCount
	bne.w behaviorFail
	lea debug_events.DebugEventBuffer, a0
	cmpi.w #contracts.EVENT_ASSERT_FAIL, debug_events.DEBUG_EVENT_KIND(a0)
	bne.w behaviorFail
	cmpi.w #contracts.CONTRACT_EXPR_REQ_001, debug_events.DEBUG_EVENT_CONTRACT_ID(a0)
	bne.w behaviorFail
	cmpi.l #4, debug_events.DEBUG_EVENT_ARG0(a0)
	bne.w behaviorFail
	tst.l debug_events.DEBUG_EVENT_ARG1(a0)
	bne.w behaviorFail
	cmpi.l #1, debug_events.DEBUG_EVENT_ARG2(a0)
	bne.w behaviorFail

	; Adjacent ranges pass; overlapping ranges append one failure.
	move.l #$1000, d0
	moveq #8, d1
	move.l #$1008, d2
	moveq #4, d3
	move.w #contracts.CONTRACT_BUF_001, -(sp)
	jsr debug_assert.debugAssertNoBufferOverlap
	lea 2(sp), sp
	cmpi.w #1, debug_events.DebugEventCount
	bne.w behaviorFail

	move.l #$1000, d0
	moveq #9, d1
	move.l #$1008, d2
	moveq #4, d3
	move.w #contracts.CONTRACT_BUF_001, -(sp)
	jsr debug_assert.debugAssertNoBufferOverlap
	lea 2(sp), sp
	cmpi.w #2, debug_events.DebugEventCount
	bne.w behaviorFail

	; The ninth passive event must not write beyond the eight-record buffer.
	jsr debug_events.debugEventReset
	moveq #contracts.EVENT_EXPR_REQUEST & $7f, d0
	moveq #0, d1
	moveq #7, d2
	moveq #1, d3
	moveq #2, d4
	moveq #3, d5
	moveq #4, d6
	moveq #8, d7
capacityLoop
	jsr debug_events.debugEventU32x4
	dbf d7, capacityLoop
	cmpi.w #debug_events.DEBUG_EVENT_CAPACITY, debug_events.DebugEventCount
	bne.s behaviorFail
	lea debug_events.DebugEventBuffer, a0
	adda.l #debug_events.DEBUG_EVENT_RECORD_BYTES * 7, a0
	cmpi.l #4, debug_events.DEBUG_EVENT_ARG3(a0)
	bne.s behaviorFail

	moveq #HARNESS_RETURN_OK, d0
	rts

behaviorFail
	moveq #HARNESS_RETURN_BEHAVIOR_FAIL, d0
	rts
	.bend  ; runBehaviorChecks

; Execute every framework routine with seeded registers and CCR.
; Outputs: D0 = zero on success, HARNESS_RETURN_FAIL on mismatch.
; Clobbers: D0-D7/A0-A6/CCR.
; CCR: reflects D0 on return.
runPreservationChecks	.block
	lea ExpectedSpanState, a0
	move.l a0, ExpectedStateTable
	bsr.w seedSpanState
	move.l sp, ExpectedCallDepth
	move.w #contracts.CONTRACT_EXPR_REQ_001, -(sp)
	move.w #HARNESS_EXPECTED_FLAGS, ccr
	jsr debug_assert.debugAssertSpanInText
	lea 2(sp), sp
	bsr.w captureAndCompare
	tst.l d0
	bne.s preservationReturn

	lea ExpectedOverlapState, a0
	move.l a0, ExpectedStateTable
	bsr.w seedOverlapState
	move.l sp, ExpectedCallDepth
	move.w #contracts.CONTRACT_BUF_001, -(sp)
	move.w #HARNESS_EXPECTED_FLAGS, ccr
	jsr debug_assert.debugAssertNoBufferOverlap
	lea 2(sp), sp
	bsr.w captureAndCompare
	tst.l d0
	bne.s preservationReturn

	lea ExpectedEventState, a0
	move.l a0, ExpectedStateTable
	bsr.w seedEventState
	move.l sp, ExpectedCallDepth
	move.w #HARNESS_EXPECTED_FLAGS, ccr
	jsr debug_events.debugEventU32x4
	bsr.w captureAndCompare
	tst.l d0
	bne.s preservationReturn

	moveq #HARNESS_RETURN_OK, d0
	rts

preservationReturn
	rts
	.bend  ; runPreservationChecks

; Snapshot returned state before using working registers, then compare it.
; Outputs: D0 = zero on match, HARNESS_RETURN_FAIL on mismatch.
; Clobbers: D0-D2/A0-A1/CCR.
; CCR: reflects D0 on return.
captureAndCompare	.block
	movem.l d0-d7/a0-a6, ActualState
	move.w ccr, ActualFlags
	lea 4(sp), a0
	cmpa.l ExpectedCallDepth, a0
	bne.s stackMismatch
	cmpi.w #HARNESS_EXPECTED_FLAGS, ActualFlags
	bne.s flagsMismatch
	movea.l ExpectedStateTable, a0
	lea ActualState, a1
	moveq #0, d2
compareLoop
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.s registerMismatch
	addq.l #1, d2
	cmpi.l #15, d2
	blo.s compareLoop
	moveq #HARNESS_RETURN_OK, d0
	rts

stackMismatch
	moveq #30, d0
	rts

flagsMismatch
	moveq #31, d0
	rts

registerMismatch
	moveq #40, d0
	add.l d2, d0
	rts
	.bend  ; captureAndCompare

seedSpanState	.block
	moveq #0, d0
	moveq #1, d1
	moveq #1, d2
	move.l #$03030303, d3
	move.l #$04040404, d4
	move.l #$05050505, d5
	move.l #$06060606, d6
	move.l #$07070707, d7
	movea.l #$10101010, a0
	movea.l #$11111111, a1
	movea.l #$12121212, a2
	movea.l #$13131313, a3
	movea.l #$14141414, a4
	movea.l #$15151515, a5
	movea.l #$16161616, a6
	rts
	.bend  ; seedSpanState

seedOverlapState	.block
	move.l #$1000, d0
	moveq #8, d1
	move.l #$1008, d2
	moveq #4, d3
	move.l #$24242424, d4
	move.l #$25252525, d5
	move.l #$26262626, d6
	move.l #$27272727, d7
	movea.l #$20202020, a0
	movea.l #$21212121, a1
	movea.l #$22222222, a2
	movea.l #$23232323, a3
	movea.l #$24242424, a4
	movea.l #$25252525, a5
	movea.l #$26262626, a6
	rts
	.bend  ; seedOverlapState

seedEventState	.block
	move.l #contracts.EVENT_EXPR_REQUEST, d0
	moveq #0, d1
	moveq #7, d2
	moveq #1, d3
	moveq #2, d4
	moveq #3, d5
	moveq #4, d6
	move.l #$37373737, d7
	movea.l #$30303030, a0
	movea.l #$31313131, a1
	movea.l #$32323232, a2
	movea.l #$33333333, a3
	movea.l #$34343434, a4
	movea.l #$35353535, a5
	movea.l #$36363636, a6
	rts
	.bend  ; seedEventState

	.endsection

	.section data, kind=data

ExpectedSpanState
	.long 0, 1, 1, $03030303, $04040404, $05050505, $06060606, $07070707
	.long $10101010, $11111111, $12121212, $13131313, $14141414, $15151515, $16161616

ExpectedOverlapState
	.long $1000, 8, $1008, 4, $24242424, $25252525, $26262626, $27272727
	.long $20202020, $21212121, $22222222, $23232323, $24242424, $25252525, $26262626

ExpectedEventState
	.long contracts.EVENT_EXPR_REQUEST, 0, 7, 1, 2, 3, 4, $37373737
	.long $30303030, $31313131, $32323232, $33333333, $34343434, $35353535, $36363636

	.endsection

	.section bss, kind=bss

ExpectedStateTable
	.res long, 1
ExpectedCallDepth
	.res long, 1
ActualFlags
	.res word, 1
	.align 2
ActualState
	.res long, 15

	.endsection

	.output "build/debug_contract_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
