; Native tokenizer VM control API.

	.module tkvm.amigaos.control
	.cpu 68020
	.pub
	.use tkvm.amigaos.demo_program
	.use tkvm.amigaos.state

	.section code, kind=code

	.pub

; Override the tokenizer VM step budget for subsequent runs.
; Inputs: D0 = requested step budget; nonpositive restores the default budget.
; Outputs: D0 = stored budget; state.TkvmStepBudget updated.
; Clobbers: CCR.
; CCR: reflects D0 on return.
tkvmSetStepBudget68000	.block
	tst.l d0
	bgt.s store
	move.l #state.TKVM_DEFAULT_MAX_STEPS_PER_LINE, d0
store
	move.l d0, state.TkvmStepBudget
	rts
	.bend  ; tkvmSetStepBudget68000

; Install a package-provided state table for subsequent runs.
; Inputs: A0 = state table pointer; D0 = state count; D1 = start state index.
; Nonpositive D0 falls back to the demo program table and start state 0.
; Outputs: state.TkvmProgramStateTablePtr/state.TkvmProgramStateCount/state.TkvmProgramStartState
; updated from the selected table.
; Clobbers: CCR.
; CCR: reflects the final stored state-count path.
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
; Inputs: none.
; Outputs: D0 = last failure kind; D1 = last failure operand.
; Clobbers: CCR.
; CCR: reflects D1 on return.
tkvmReadLastFailure68000	.block
	moveq #0, d0
	move.w state.TkvmLastFailureKind, d0
	moveq #0, d1
	move.w state.TkvmLastFailureOperand, d1
	rts
	.bend  ; tkvmReadLastFailure68000

	.endsection
	.endmodule
