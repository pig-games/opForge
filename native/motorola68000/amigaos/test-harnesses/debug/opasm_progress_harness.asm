; Focused guest harness for the bounded native progress bridge.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module opasm.progress.harness
	.cpu 68020

	.use opasm.amigaos.progress as progress
	.use opforge.debug.contracts as contracts
	.use opforge.debug.events as events

HARNESS_FAIL = 20
HARNESS_PACKAGE_TICKS_OFFSET = progress.OPASM_PROGRESS_PHASE_TICKS_OFFSET + 4
HARNESS_PASS_ONE_TICKS_OFFSET = progress.OPASM_PROGRESS_PHASE_TICKS_OFFSET + 16

	.section entry, kind=code
	.pub

; Execute deterministic phase, heartbeat, saturation-boundary, abort, and
; incomplete-terminal checks against the real progress routines.
start	.block
	jsr events.debugEventReset
	clr.l HarnessTick
	lea nextTick, a0
	jsr progress.opasmProgressBeginRunV1

	jsr progress.opasmProgressGetRecordV1
	moveq #21, d7
	cmpi.l #progress.OPASM_PROGRESS_MAGIC, progress.OPASM_PROGRESS_MAGIC_OFFSET(a0)
	bne.w fail
	moveq #22, d7
	cmpi.w #progress.OPASM_PROGRESS_SCHEMA_VERSION, progress.OPASM_PROGRESS_SCHEMA_OFFSET(a0)
	bne.w fail
	moveq #23, d7
	cmpi.l #10, progress.OPASM_PROGRESS_RUN_ID_OFFSET(a0)
	bne.w fail

	moveq #30, d7
	moveq #progress.OPASM_PROGRESS_PHASE_PACKAGE, d0
	moveq #0, d1
	moveq #0, d2
	jsr progress.opasmProgressSetPhaseV1
	moveq #progress.OPASM_PROGRESS_PHASE_PASS_ONE, d0
	moveq #1, d1
	moveq #0, d2
	jsr progress.opasmProgressSetPhaseV1
	cmpi.l #10, HARNESS_PACKAGE_TICKS_OFFSET(a0)
	bne.w fail

	moveq #31, d7
	moveq #2, d0
	jsr progress.opasmProgressSetHeartbeatV1
	moveq #0, d0
	moveq #3, d1
	jsr progress.opasmProgressStatementBeginV1
	jsr progress.opasmProgressStatementCompleteV1
	moveq #1, d0
	moveq #3, d1
	jsr progress.opasmProgressStatementBeginV1
	jsr progress.opasmProgressStatementCompleteV1
	cmpi.w #1, events.DebugEventCount
	bne.w fail
	lea events.DebugEventBuffer, a1
	cmpi.w #contracts.EVENT_ASSEMBLY_PROGRESS, events.DEBUG_EVENT_KIND(a1)
	bne.w fail
	cmpi.l #2, events.DEBUG_EVENT_ARG3(a1)
	bne.w fail

	moveq #32, d7
	moveq #3, d0
	jsr progress.opasmProgressSetAbortVisitsV1
	moveq #2, d0
	moveq #3, d1
	jsr progress.opasmProgressStatementBeginV1
	jsr progress.opasmProgressAbortRequestedV1
	cmpi.l #1, d0
	bne.w fail

	; Exercise the two saturating counters through their production routines.
	moveq #34, d7
	moveq #0, d0
	jsr progress.opasmProgressSetHeartbeatV1
	move.l #$fffffffe, progress.OPASM_PROGRESS_VISITS_OFFSET(a0)
	moveq #3, d0
	moveq #5, d1
	jsr progress.opasmProgressStatementBeginV1
	moveq #4, d0
	moveq #5, d1
	jsr progress.opasmProgressStatementBeginV1
	cmpi.l #$ffffffff, progress.OPASM_PROGRESS_VISITS_OFFSET(a0)
	bne.w fail
	move.l progress.OPASM_PROGRESS_OVERFLOW_OFFSET(a0), d0
	andi.l #progress.OPASM_PROGRESS_OVERFLOW_VISITS, d0
	beq.w fail

	moveq #35, d7
	move.l #$fffffffe, HARNESS_PASS_ONE_TICKS_OFFSET(a0)
	moveq #progress.OPASM_PROGRESS_PHASE_LAYOUT, d0
	moveq #1, d1
	moveq #1, d2
	jsr progress.opasmProgressSetPhaseV1
	cmpi.l #$ffffffff, HARNESS_PASS_ONE_TICKS_OFFSET(a0)
	bne.w fail
	move.l progress.OPASM_PROGRESS_OVERFLOW_OFFSET(a0), d0
	andi.l #progress.OPASM_PROGRESS_OVERFLOW_PHASE_TICKS, d0
	beq.w fail

	moveq #33, d7
	moveq #HARNESS_FAIL, d0
	jsr progress.opasmProgressFinishV1
	jsr progress.opasmProgressGetRecordV1
	move.w progress.OPASM_PROGRESS_FLAGS_OFFSET(a0), d0
	andi.w #progress.OPASM_PROGRESS_FLAG_ACTIVE, d0
	bne.w fail
	move.w progress.OPASM_PROGRESS_FLAGS_OFFSET(a0), d0
	andi.w #progress.OPASM_PROGRESS_FLAG_COMPLETE, d0
	bne.w fail
	move.w progress.OPASM_PROGRESS_FLAGS_OFFSET(a0), d0
	andi.w #progress.OPASM_PROGRESS_FLAG_INCOMPLETE, d0
	beq.w fail
	cmpi.l #$ffffffff, progress.OPASM_PROGRESS_VISITS_OFFSET(a0)
	bne.w fail
	cmpi.l #1, progress.OPASM_PROGRESS_LAST_STMT_OFFSET(a0)
	bne.w fail
	cmpi.l #HARNESS_FAIL, progress.OPASM_PROGRESS_EXIT_STATUS_OFFSET(a0)
	bne.w fail

	moveq #0, d0
	rts
fail
	move.l d7, d0
	rts
	.bend  ; start

; Deterministic coarse-tick callback. Every sampled boundary advances 10 ticks.
nextTick	.block
	addq.l #5, HarnessTick
	addq.l #5, HarnessTick
	move.l HarnessTick, d0
	rts
	.bend  ; nextTick

	.endsection

	.section bss, kind=bss
	.align 4
HarnessTick
	.res long, 1
	.endsection

	.output "build/opasm_progress_harness", format=hunk, sections=entry, code, bss
	.endmodule
