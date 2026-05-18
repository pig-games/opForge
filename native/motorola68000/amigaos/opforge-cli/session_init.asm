; Native AmigaOS opForge CLI session initialization helpers.

	.module opforge.cli.session_init
	.cpu 68020

	.use opasm.amigaos.engine (opasmEngineAssemblySessionStart, opasmEngineStmtCount)
	.use opasm.amigaos.engine (opasmEngineSessionCpuName)
	.use opforge.cli.constants (NATIVE_ASSEMBLY_SESSION_BYTES, NATIVE_MODULE_USE_STATE_BYTES)
	.use opforge.cli.constants (TOKEN_BUFFER_CAPACITY)
	.use opforge.cli.state (NativeCliCpuName, NativeCliModuleUseStateStart)
	.use opforge.cli.strings (DefaultCpuName)
	.use opforge.cli.copy (opforgeNativeCliClearBytes)

	.section code, kind=code
	.pub

; Initialize transitional native assembly-session state for the current CLI run.
opforgeNativeCliInitAssemblySession	.block
	movem.l d0-d1/a0-a1, -(sp)
	lea opasmEngineAssemblySessionStart.l, a0
	move.l #NATIVE_ASSEMBLY_SESSION_BYTES, d0
	jsr opforgeNativeCliClearBytes
	lea NativeCliCpuName, a0
	tst.b (a0)
	bne.s haveCpu
	lea DefaultCpuName, a0

haveCpu
	lea opasmEngineSessionCpuName, a1
	bsr.w copySessionCpuName
	movem.l (sp)+, d0-d1/a0-a1
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliInitAssemblySession

; Clear module/use and statement collection state before parsing input.
opforgeNativeCliInitModuleUseState	.block
	movem.l d0-d1/a0, -(sp)
	lea NativeCliModuleUseStateStart, a0
	move.l #NATIVE_MODULE_USE_STATE_BYTES, d0
	jsr opforgeNativeCliClearBytes
	clr.w opasmEngineStmtCount.l
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; opforgeNativeCliInitModuleUseState

	.priv

copySessionCpuName	.block
	move.l #TOKEN_BUFFER_CAPACITY - 1, d0

loop
	move.b (a0)+, d1
	move.b d1, (a1)+
	beq.s done
	subq.l #1, d0
	bne.s loop
	clr.b -(a1)

done
	rts
	.bend  ; copySessionCpuName

	.endsection
	.endmodule
