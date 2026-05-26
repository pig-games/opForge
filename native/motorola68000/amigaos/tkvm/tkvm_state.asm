; Native tokenizer VM mutable runtime state.

	.module tkvm.amigaos.state
	.cpu 68020
	.pub
	.use tkvm.amigaos.demo_program

TKVM_DEFAULT_MAX_STEPS_PER_LINE = 2048

	.section data, kind=data
	.pub

TkvmStepBudget
	.long TKVM_DEFAULT_MAX_STEPS_PER_LINE

TkvmProgramStateTablePtr
	.long demo_program.DemoStateEntryOffsets

TkvmProgramStateCount
	.long 1

TkvmProgramStartState
	.word 0

TkvmLastFailureKind
	.word 0

TkvmLastFailureOperand
	.word 0

	.endsection
	.endmodule
