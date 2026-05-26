; Native tokenizer VM control API.

	.module tkvm.amigaos.control
	.cpu 68020
	.pub
	.use tkvm.amigaos.demo_program
	.use tkvm.amigaos.state

	.section code, kind=code

	.pub

; Override the tokenizer VM step budget for the next runs; nonpositive restores default.
tkvmSetStepBudget68000	.block
	tst.l d0
	bgt.s store
	move.l #state.TKVM_DEFAULT_MAX_STEPS_PER_LINE, d0
store
	move.l d0, state.TkvmStepBudget
	rts
	.bend  ; tkvmSetStepBudget68000

; Install a package-provided state table; invalid counts fall back to demo state 0.
tkvmSetProgramStateTable68000	.block
	tst.l d0
	bgt.s store
	lea demo_program.DemoStateEntryOffsets, a0
	moveq #1, d0
	moveq #0, d1
store
	move.l a0, state.TkvmProgramStateTablePtr
	move.l d0, state.TkvmProgramStateCount
	move.w d1, state.TkvmProgramStartState
	rts
	.bend  ; tkvmSetProgramStateTable68000

; Return the last explicit VM failure kind/operand captured by tkvmRun68000.
tkvmReadLastFailure68000	.block
	moveq #0, d0
	move.w state.TkvmLastFailureKind, d0
	moveq #0, d1
	move.w state.TkvmLastFailureOperand, d1
	rts
	.bend  ; tkvmReadLastFailure68000

	.endsection
	.endmodule
