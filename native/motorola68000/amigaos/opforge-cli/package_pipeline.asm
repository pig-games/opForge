; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.package_pipeline
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use tkpkg.amigaos.package_loader
	.use tkpkg.amigaos.service
	.use tkpkg.amigaos.buffers

	.use tkpkg.amigaos.abi

	.use opforge.cli.tkpkg_control_block
	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.state
	.use opforge.cli.text_output
	.use opforge.cli.prvm_bridge
.ifdef OPFORGE_DEBUG_CONTRACTS
	.use opforge.debug.contracts as debug_contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"
.endif

	.section code, kind=code
	.pub

; Inputs:
;   state.NativeCliPackagePath = optional external package path
;   state.NativeCliCpuName = optional pipeline CPU name override
; Outputs:
;   D0.L = 0 on success, 1 when package staging/loading or pipeline selection fails
;   buffers.ControlBlockV1 and package/pipeline request state updated for tkpkg
; Clobbers:
;   D0-D1/D5-D7/A0-A2/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliInitPackagePipeline	.block
	lea buffers.ControlBlockV1, a0
	moveq #abi.ENTRY_ORD_INIT, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus
	bne.w fail

	bsr.w opforgeNativeCliStagePackage
	bne.w fail

	lea buffers.ControlBlockV1, a0
	move.w #constants.PACKAGE_INPUT_PTR_V1, d0
	move.l state.NativeCliPackageLenActive, d1
	swap d1
	tst.w d1
	bne.s loadStagedPayload
	swap d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_LOAD_PACKAGE, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus
	bne.s fail
	bra.s packageLoaded

loadStagedPayload
	move.l state.NativeCliPackageLenActive, d0
	jsr package_loader.tkpkgPackageLoaderLoadStagedV1
	tst.l d0
	bne.s fail

packageLoaded
	bsr.w opforgeNativeCliApplyCurrentPipeline
	cmpi.l #2, d0
	beq.s pipelineUnavailable
	tst.l d0
	bne.s fail
	jsr prvm_bridge.opforgeNativeCliSampleActivePrvmLengthField
	move.l d0, state.NativeCliPrvmPipelineDetail
	moveq #0, d0
	rts

pipelineUnavailable
	moveq #2, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliInitPackagePipeline

; Inputs:
;   state.NativeCliCpuName = optional requested CPU name
; Outputs:
;   D0.L = 0 on success, 2 when the requested pipeline is unavailable, 1 on write/dispatch failure
; Clobbers:
;   D0-D1/A0/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliApplyCurrentPipeline	.block
	bsr.w opforgeNativeCliPreparePipelineRequest
	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.w state.NativeCliPipelineRequestLen, d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_SET_PIPELINE, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus
.ifdef OPFORGE_DEBUG_CONTRACTS
	; Instrumentation point: native package pipeline selection result.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A6.
	; SR/CCR preserved: CCR restored before the status branch.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: the saved status and CCR are
	; restored immediately before the following BEQ.
	; Removal/stabilization plan: retain as the stable source-CPU pipeline
	; selection contract while runtime pipeline switching is supported.
	move.w ccr, -(sp)
	movem.l d1/d3-d6, -(sp)
	move.l d0, d4
	moveq #2, d1
	moveq #0, d3
	move.w state.NativeCliPipelineRequestLen, d3
	moveq #0, d5
	lea state.NativeCliCpuName, a0
	move.b (a0), d5
	moveq #0, d6
	move.w state.NativeCliPackageLenActive, d6
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_PIPELINE_SELECT
	movem.l (sp)+, d1/d3-d6
	move.w (sp)+, ccr
.endif
	beq.s ok
	bsr.w opforgeNativeCliEmitPipelineLastError
	cmpi.b #abi.STATUS_RUNTIME_ERROR_V1, d0
	beq.s unavailable
	moveq #1, d0
	rts

ok
	moveq #0, d0
	rts

unavailable
	moveq #2, d0
	rts
	.bend  ; opforgeNativeCliApplyCurrentPipeline

	.priv

; Inputs:
;   state.NativeCliPackagePath = optional external package path
; Outputs:
;   D0.L = 0 on success, 1 when package staging fails
;   buffers.packageStorage/state.NativeCliPackageLenActive contain the staged package on success
; Clobbers:
;   D0-D7/A0-A2/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliStagePackage	.block
	tst.b state.NativeCliPackagePath
	bne.s externalPackage

	move.l #OPFORGE_NATIVE_CLI_PACKAGE_LEN, d0
	cmpi.l #buffers.PACKAGE_STORAGE_CAPACITY, d0
	bhi.s embeddedPackageTooLarge
	lea opforgeNativeCliPackageData.l, a1
	lea buffers.packageStorage, a2
	move.l d0, state.NativeCliPackageLenActive
	jsr copy.copyBytes
	moveq #0, d0
	rts

embeddedPackageTooLarge
	move.l #strings.PackageTooLargeText, d1
	jsr dos.putErrStr
	moveq #1, d0
	rts

externalPackage
	lea state.NativeCliPackagePath, a0
	jsr dos.openInput
	tst.l d0
	bne.s externalOpenOk
	moveq #1, d0
	rts

externalOpenOk
	move.l d0, d5
	lea buffers.packageStorage, a0
	move.l #buffers.PACKAGE_STORAGE_CAPACITY, d0
	move.l d5, d1
	jsr dos.readInput
	move.l d0, d6
	cmp.l #-1, d6
	beq.w externalReadFail
	cmpi.l #buffers.PACKAGE_STORAGE_CAPACITY, d6
	bne.s externalReadOk
	lea state.NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	jsr dos.readInput
	move.l d0, d7
	cmp.l #-1, d7
	beq.w externalReadFail
	tst.l d7
	beq.s externalReadOk
	move.l d5, d1
	jsr dos.close
	move.l #strings.PackageTooLargeText, d1
	jsr dos.putErrStr
	moveq #1, d0
	rts

externalReadOk
	move.l d5, d1
	jsr dos.close
	move.l d6, state.NativeCliPackageLenActive
	moveq #0, d0
	rts

externalReadFail
	move.l d5, d1
	jsr dos.close
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliStagePackage

; Inputs:
;   state.NativeCliCpuName = optional requested CPU name
; Outputs:
;   D0.L = 0
;   state.NativeCliPipelineRequestLen = request byte length
;   buffers.lastErrorBuffer = C-string pipeline request payload
; Clobbers:
;   D0/A0-A1/CCR
; CCR:
;   Reflects D0.L on return. This helper has no failure path with the current fixed-size request buffer.
opforgeNativeCliPreparePipelineRequest	.block
	lea state.NativeCliCpuName, a0
	tst.b (a0)
	bne.s haveCpu
	lea strings.DefaultCpuName, a0

haveCpu
	lea buffers.lastErrorBuffer, a1
	jsr copy.copyCString
	move.w d0, state.NativeCliPipelineRequestLen
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliPreparePipelineRequest

opforgeNativeCliEmitPipelineLastError	.block
	movem.l d0-d1/a0-a1, -(sp)
	lea buffers.ControlBlockV1, a0
	jsr tkpkg_control_block.opforgeNativeCliReadLastErrorLen
	tst.w d0
	beq.s done
	lea buffers.LastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	move.l a1, d1
	jsr dos.putErrStr
	move.l #strings.NewlineText, d1
	jsr dos.putErrStr

done
	movem.l (sp)+, d0-d1/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitPipelineLastError

	.endsection

	.section data, kind=data

	.align 2

OpforgeNativeCliPackageLen
	.word OPFORGE_NATIVE_CLI_PACKAGE_LEN

	.align 2
opforgeNativeCliPackageData
	.incbin "opforge_cli_package.opasm"
OPFORGE_NATIVE_CLI_PACKAGE_DATA_END

OPFORGE_NATIVE_CLI_PACKAGE_LEN = OPFORGE_NATIVE_CLI_PACKAGE_DATA_END - opforgeNativeCliPackageData

	.endsection

	.endmodule
