; Native AmigaOS opForge CLI session initialization helpers.

	.module opforge.cli.session_init
	.cpu 68020

	.use opasm.amigaos.engine
	.use opasm.amigaos.compile_values as compile_values
	.use opforge.cli.constants
	.use opforge.cli.state
	.use opforge.cli.strings
	.use opforge.cli.copy
	.use opforge.cli.preprocessor
	.use opforge.cli.statement_owners as statement_owners

	.section code, kind=code
	.pub

; Initialize transitional native assembly-session state for the current CLI run.
opforgeNativeCliInitAssemblySession	.block
	lea state.NativeCliCpuName, a0
	tst.b (a0)
	bne.s haveCpu
	lea strings.DefaultCpuName, a0

haveCpu
	jsr engine.initSessionV1
	jsr compile_values.resetV1
	rts
	.bend  ; opforgeNativeCliInitAssemblySession

; Clear module/use and statement collection state before parsing input.
opforgeNativeCliInitModuleUseState	.block
	movem.l d0-d1/a0, -(sp)
	lea state.NativeCliModuleUseStateStart, a0
	move.l #state.NATIVE_CLI_MODULE_USE_STATE_BYTES, d0
	jsr copy.clearBytes
	lea state.NativeCliArtifactRequestStateStart, a0
	move.l #state.NATIVE_CLI_ARTIFACT_REQUEST_STATE_BYTES, d0
	jsr copy.clearBytes
	clr.w state.NativeCliOutputBootstrapFromSource
	jsr preprocessor.opforgeNativeCliResetPreprocessorV1
	jsr engine.resetStatementCollectionV1
	jsr statement_owners.opforgeNativeCliResetStatementOwnersV1
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; opforgeNativeCliInitModuleUseState

	.endsection
	.endmodule
