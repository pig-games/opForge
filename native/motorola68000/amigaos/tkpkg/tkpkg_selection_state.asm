; Shared mutable state for selection and operand-plan runtime.
; Layout is preserved verbatim from the former selection owner.

	.module tkpkg.amigaos.selection_state
	.cpu 68020
	.pub

	.section bss, kind=bss
	.pub

EncodeSelectedSelectorContext
	.res long, 2
EncodeSelectedMnemonicPtr
	.res long, 1
EncodeSelectedSourceLinePtr
	.res long, 1
EncodeSelectedLabelNamePtr
	.res long, 1
EncodeSelectedLabelValuePtr
	.res long, 1
EncodeSelectedLabelCount
	.res long, 1
EncodeSelectedCurrentPc
	.res long, 1
EncodeSelectedSymbolResolverPtr
	.res long, 1
EncodeSelectedSessionPass
	.res word, 1
EncodeSelectedExvmOpcodeVersion
	.res word, 1
EncodeSelectedExprOpcodeVersion
	.res word, 1
EncodeSelectedOperandStatus
	.res word, 1
EncodeSelectedMselShapePtr
	.res long, 1
EncodeSelectedMselShapeLen
	.res word, 1
EncodeSelectedCurrentShapePtr
	.res long, 1
EncodeSelectedCurrentShapeLen
	.res word, 1
EncodeSelectedMselMnemonicLen
	.res word, 1
EncodeSelectedMselOwnerPtr
	.res long, 1
EncodeSelectedMselOwnerLen
	.res word, 1
EncodeSelectedMselExprPtr
	.res long, 1
EncodeSelectedMselExprLen
	.res word, 1
EncodeSelectedMselModePtr
	.res long, 1
EncodeSelectedMselModeLen
	.res word, 1
EncodeSelectedMselPlanPtr
	.res long, 1
EncodeSelectedMselPlanLen
	.res word, 1
EncodeSelectedMselValue
	.res long, 1
EncodeSelectedMselUnstable
	.res byte, 1
	.align 2
EncodeSelectedMselMatchFlags
	.res word, 1
EncodeSelectedMselFallbackLen
	.res word, 1

PairAPtr
	.res long, 1
PairALen
	.res word, 1
PairBPtr
	.res long, 1
PairBLen
	.res word, 1
PairAVal
	.res long, 1
PairBVal

	.endsection
	.endmodule
