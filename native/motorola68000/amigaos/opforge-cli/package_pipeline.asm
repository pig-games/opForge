; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.package_pipeline
	.cpu 68020

	.use opforge.cli.constants (PACKAGE_INPUT_PTR_V1)
	.use opforge.cli.copy (opforgeNativeCliCopyBytes, opforgeNativeCliCopyCString)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)
	.use tkpkg.amigaos.buffers (LAST_ERROR_BUFFER_PTR_V1, ControlBlockV1, packageStorage, PACKAGE_STORAGE_CAPACITY, lastErrorBuffer)

	.use tkpkg.amigaos.abi (ENTRY_ORD_INIT, ENTRY_ORD_LOAD_PACKAGE)
	.use tkpkg.amigaos.abi (ENTRY_ORD_SET_PIPELINE)

	.use opforge.cli.tkpkg_control_block (opforgeNativeCliReadStatus, opforgeNativeCliWriteInputWindow)
	.use opforge.cli.strings (PackageTooLargeText, DefaultCpuName, DefaultFamilyName, DefaultFamilyNameEnd, Mos6502FamilyName, Mos6502FamilyNameEnd)
	.use opforge.cli.dos (opforgeNativeCliPutStr, opforgeNativeCliOpenInput, opforgeNativeCliReadInput, opforgeNativeCliClose)
	.use opforge.cli.state (NativeCliPackageLenActive, NativeCliPipelineRequestLen, NativeCliPackagePath, NativeCliCpuName, NativeCliCurrentPath, NativeCliInputChar)

	.section code, kind=code
	.pub

; Initialize tkpkg, stage/load package bytes, and select the requested pipeline.
opforgeNativeCliInitPackagePipeline	.block
	lea ControlBlockV1, a0
	moveq #ENTRY_ORD_INIT, d0
	jsr tkpkgServiceDispatchV1
	jsr opforgeNativeCliReadStatus
	tst.b d0
	bne.w fail

	bsr.w opforgeNativeCliStagePackage
	tst.l d0
	bne.w fail

	lea ControlBlockV1, a0
	move.w #PACKAGE_INPUT_PTR_V1, d0
	move.w NativeCliPackageLenActive, d1
	jsr opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_LOAD_PACKAGE, d0
	jsr tkpkgServiceDispatchV1
	jsr opforgeNativeCliReadStatus
	tst.b d0
	bne.s fail

	bsr.w opforgeNativeCliPreparePipelineRequest
	tst.l d0
	bne.s fail

	lea ControlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliPipelineRequestLen, d1
	jsr opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_SET_PIPELINE, d0
	jsr tkpkgServiceDispatchV1
	jsr opforgeNativeCliReadStatus
	tst.b d0
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
	tst.b NativeCliPackagePath
	bne.s externalPackage

	lea opforgeNativeCliPackageData, a1
	lea packageStorage, a2
	move.w OpforgeNativeCliPackageLen, d0
	move.w d0, NativeCliPackageLenActive
	jsr opforgeNativeCliCopyBytes
	moveq #0, d0
	rts

externalPackage
	lea NativeCliPackagePath, a0
	jsr opforgeNativeCliOpenInput
	tst.l d0
	bne.s externalOpenOk
	moveq #1, d0
	rts

externalOpenOk
	move.l d0, d5
	lea packageStorage, a0
	move.l #PACKAGE_STORAGE_CAPACITY, d0
	move.l d5, d1
	jsr opforgeNativeCliReadInput
	move.l d0, d6
	cmp.l #-1, d6
	beq.w externalReadFail
	cmpi.l #PACKAGE_STORAGE_CAPACITY, d6
	bne.s externalReadOk
	lea NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	jsr opforgeNativeCliReadInput
	move.l d0, d7
	cmp.l #-1, d7
	beq.w externalReadFail
	tst.l d7
	beq.s externalReadOk
	move.l d5, d1
	jsr opforgeNativeCliClose
	move.l #PackageTooLargeText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts

externalReadOk
	move.l d5, d1
	jsr opforgeNativeCliClose
	move.w d6, NativeCliPackageLenActive
	moveq #0, d0
	rts

externalReadFail
	move.l d5, d1
	jsr opforgeNativeCliClose
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliStagePackage

; Build the tkpkg set-pipeline request payload from --cpu or the default CPU.
opforgeNativeCliPreparePipelineRequest	.block
	lea NativeCliCpuName, a0
	tst.b (a0)
	bne.s haveCpu
	lea DefaultCpuName, a0

haveCpu
	lea lastErrorBuffer, a1
	jsr opforgeNativeCliCopyCString
	move.w d0, NativeCliPipelineRequestLen
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

DEFAULT_FAMILY_NAME_LEN = DefaultFamilyNameEnd - DefaultFamilyName
MOS6502_FAMILY_NAME_LEN = mos6502FamilyNameEnd - Mos6502FamilyName
OPFORGE_NATIVE_CLI_PACKAGE_LEN = OPFORGE_NATIVE_CLI_PACKAGE_DATA_END - opforgeNativeCliPackageData

	.endsection

	.endmodule
