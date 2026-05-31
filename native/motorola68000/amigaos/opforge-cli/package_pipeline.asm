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

; Initialize tkpkg, stage/load package bytes, and select the requested pipeline.
opforgeNativeCliInitPackagePipeline	.block
	lea buffers.ControlBlockV1, a0
	moveq #abi.ENTRY_ORD_INIT, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus
	bne.w fail

	bsr.w opforgeNativeCliStagePackage
	tst.l d0
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
	tst.l d0
	bne.s fail

	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.w state.NativeCliPipelineRequestLen, d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_SET_PIPELINE, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus
	bne.s fail
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliInitPackagePipeline

	.priv

; Stage either the embedded package or an external --opasm-package file.
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

; Build the tkpkg set-pipeline request payload from --cpu or the default CPU.
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

DEFAULT_FAMILY_NAME_LEN = strings.DefaultFamilyNameEnd - strings.DefaultFamilyName
MOS6502_FAMILY_NAME_LEN = strings.mos6502FamilyNameEnd - strings.Mos6502FamilyName
OPFORGE_NATIVE_CLI_PACKAGE_LEN = OPFORGE_NATIVE_CLI_PACKAGE_DATA_END - opforgeNativeCliPackageData

	.endsection

	.endmodule
