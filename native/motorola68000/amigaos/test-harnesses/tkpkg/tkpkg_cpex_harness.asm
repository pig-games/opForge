; Focused native CPEX selection/getter proof harness.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.package_loader
	.use tkpkg.amigaos.pipeline
	.use tkpkg.amigaos.runtime_context
	.use tkpkg.amigaos.service

SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
PUT_STR = -948
RETURN_FAIL = 20
PACKAGE_INPUT_PTR_V1 = buffers.PACKAGE_STORAGE_PTR_V1

	.section entry, kind=code
	.pub
start	.block
	move.l #RETURN_FAIL, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase

	lea buffers.ControlBlockV1, a0
	move.l #abi.ENTRY_ORD_INIT, d0
	jsr service.dispatchV1
	tst.l d0
	bne.w closeDos
	lea PackageData, a1
	lea buffers.packageStorage, a2
	move.l #PACKAGE_LEN, d0
copyPackage
	move.b (a1)+, (a2)+
	subq.l #1, d0
	bne.s copyPackage
	move.l #PACKAGE_LEN, d0
	jsr package_loader.tkpkgPackageLoaderLoadStagedV1
	tst.l d0
	bne.w closeDos

.ifdef OPFORGE_TKPKG_CPEX_LEGACY
	lea CpuCanonicalA, a1
	moveq #CPU_CANONICAL_A_LEN, d0
	bsr.w selectCpu
	bne.w closeDos
	jsr runtime_context.getCpuWordSizeBytesV1
	tst.l d0
	beq.w closeDos
	jsr runtime_context.getCpuMaxProgramAddressV1
	tst.l d0
	beq.w closeDos
	move.l #MissingText, d1
	movea.l DosBase, a6
	jsr PUT_STR(a6)
.else
	lea ResultBytes, a3
	lea CpuAliasA, a1
	moveq #CPU_ALIAS_A_LEN, d0
	bsr.w selectCpu
	bne.w closeDos
	bsr.w captureProperties
	bne.w closeDos
	lea CpuCanonicalB, a1
	moveq #CPU_CANONICAL_B_LEN, d0
	bsr.w selectCpu
	bne.w closeDos
	bsr.w captureProperties
	bne.w closeDos
	lea OutputPath, a0
	bsr.w writeResult
	bne.w closeDos
	clr.l ReturnCode
.endif

closeDos
	movea.l DosBase, a1
	movea.l SYS_BASE.w, a6
	jsr CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	rts
	.bend  ; start

; A1/D0 request bytes ending at the CPU separator (no explicit dialect).
selectCpu	.block
	movem.l d1-d3/a0-a3, -(sp)
	move.w d0, d3
	lea buffers.lastErrorBuffer, a2
copyRequest
	move.b (a1)+, (a2)+
	subq.w #1, d0
	bne.s copyRequest
	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.b d0, abi.CB_INPUT_PTR(a0)
	lsr.w #8, d0
	move.b d0, abi.CB_INPUT_PTR + 1(a0)
	move.b d3, abi.CB_INPUT_LEN(a0)
	lsr.w #8, d3
	move.b d3, abi.CB_INPUT_LEN + 1(a0)
	jsr pipeline.tkpkgPipelineSetActiveV1
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; selectCpu

captureProperties	.block
	jsr runtime_context.getCpuWordSizeBytesV1
	tst.l d0
	bne.s fail
	move.l d1, (a3)+
	jsr runtime_context.getCpuMaxProgramAddressV1
	tst.l d0
	bne.s fail
	move.l d1, (a3)+
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; captureProperties

; A0 path. Write the exact 16-byte getter payload.
writeResult	.block
	movem.l d1-d4/a6, -(sp)
	move.l a0, d1
	move.l #1006, d2
	movea.l DosBase, a6
	jsr -30(a6)
	tst.l d0
	beq.s fail
	move.l d0, d4
	move.l d0, d1
	move.l #ResultBytes, d2
	moveq #16, d3
	jsr -48(a6)
	move.l d0, -(sp)
	move.l d4, d1
	jsr -36(a6)
	move.l (sp)+, d0
	cmpi.l #16, d0
	bne.s fail
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d4/a6
	tst.l d0
	rts
	.bend  ; writeResult

	.endsection

	.section data, kind=data
DosName
	.byte "dos.library", 0
OutputPath
	.byte "Work:build/cpex-values.bin", 0
MissingText
	.byte "CPEX property unavailable", 10, 0
CpuAliasA
	.byte "68000", 0
CPU_ALIAS_A_END
CPU_ALIAS_A_LEN = CPU_ALIAS_A_END - CpuAliasA
CpuCanonicalA
	.byte "m68000", 0
CPU_CANONICAL_A_END
CPU_CANONICAL_A_LEN = CPU_CANONICAL_A_END - CpuCanonicalA
CpuCanonicalB
	.byte "m68020", 0
CPU_CANONICAL_B_END
CPU_CANONICAL_B_LEN = CPU_CANONICAL_B_END - CpuCanonicalB
PackageData
	.incbin "../../tkpkg/tkpkg_debug_cli_package.opasm"
PACKAGE_END
PACKAGE_LEN = PACKAGE_END - PackageData
	.endsection

	.section bss, kind=bss
DosBase
	.res long, 1
ReturnCode
	.res long, 1
ResultBytes
	.res byte, 16
	.endsection

	.output "build/tkpkg_cpex_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
