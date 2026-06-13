; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.package_pipeline
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use tkpkg.amigaos.service
	.use tkpkg.amigaos.buffers

	.use tkpkg.amigaos.abi

	.use opforge.cli.tkpkg_control_block
	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.state

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
	move.w state.NativeCliPackageLenActive, d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_LOAD_PACKAGE, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus
	bne.s fail

	bsr.w opforgeNativeCliPreparePipelineRequest

	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.w state.NativeCliPipelineRequestLen, d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_SET_PIPELINE, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus
	bne.s pipelineUnavailable
	moveq #0, d0
	rts

pipelineUnavailable
	moveq #2, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliInitPackagePipeline

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

	lea opforgeNativeCliPackageData, a1
	lea buffers.packageStorage, a2
	move.w OpforgeNativeCliPackageLen, d0
	move.w d0, state.NativeCliPackageLenActive
	jsr copy.copyBytes
	moveq #0, d0
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
	jsr dos.putStr
	moveq #1, d0
	rts

externalReadOk
	move.l d5, d1
	jsr dos.close
	move.w d6, state.NativeCliPackageLenActive
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
