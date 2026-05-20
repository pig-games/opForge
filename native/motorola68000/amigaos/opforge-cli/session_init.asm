; Native AmigaOS opForge CLI session initialization helpers.

	.module opforge.cli.session_init
	.cpu 68020

	.use opasm.amigaos.engine (opasmEngineInitSessionV1, opasmEngineResetStatementCollectionV1)
	.use opforge.cli.constants (NATIVE_MODULE_USE_STATE_BYTES)
	.use opforge.cli.state (NativeCliCpuName, NativeCliModuleUseStateStart)
	.use opforge.cli.strings (DefaultCpuName)
	.use opforge.cli.copy (opforgeNativeCliClearBytes)

	.section code, kind=code
	.pub

; Initialize transitional native assembly-session state for the current CLI run.
opforgeNativeCliInitAssemblySession	.block
	lea NativeCliCpuName, a0
	tst.b (a0)
	bne.s haveCpu
	lea DefaultCpuName, a0

haveCpu
	jsr opasmEngineInitSessionV1
	rts
	.bend  ; opforgeNativeCliInitAssemblySession

; Clear module/use and statement collection state before parsing input.
opforgeNativeCliInitModuleUseState	.block
	movem.l d0-d1/a0, -(sp)
	lea NativeCliModuleUseStateStart, a0
	move.l #NATIVE_MODULE_USE_STATE_BYTES, d0
	jsr opforgeNativeCliClearBytes
	jsr opasmEngineResetStatementCollectionV1
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; opforgeNativeCliInitModuleUseState

	.endsection
	.endmodule
