; Direct package/selection/compact-lookup lifecycle proof with live Rust oracles.
; The binary batch and output records are a permanent boundary contract.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.service
	.use tkpkg.amigaos.package_loader as loader
	.use tkpkg.amigaos.pipeline
	.use tkpkg.amigaos.compact_table as compact

SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
OPEN = -30
CLOSE = -36
READ = -42
WRITE = -48
MODE_OLDFILE = 1005
MODE_NEWFILE = 1006
RETURN_FAIL = 20
CASE_CAPACITY = 64
INPUT_CAPACITY = 2 * buffers.PACKAGE_STORAGE_CAPACITY + 65536
INPUT_BUFFER_BYTES = INPUT_CAPACITY + 1
OUTPUT_RECORD_BYTES = 72
OUTPUT_CAPACITY = CASE_CAPACITY * OUTPUT_RECORD_BYTES
RESULT_PAYLOAD_CAPACITY = 64

	.section entry, kind=code
	.pub
; AmigaDOS entry: no arguments; D0 exit 0 only after a complete binary proof run.
; Clobbers D0-D7/A0-A6/CCR. API failures are recorded as data, not guest exits.
start	.block
	move.l #RETURN_FAIL, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	bsr.w readInput
	bne.w closeDos
	lea buffers.ControlBlockV1, a0
	moveq #abi.ENTRY_ORD_INIT, d0
	jsr service.dispatchV1
	tst.l d0
	bne.w closeDos
	bsr.w executeBatch
	bne.w closeDos
	bsr.w writeOutput
	bne.w closeDos
	clr.l ReturnCode
closeDos
	movea.l DosBase, a1
	movea.l SYS_BASE.w, a6
	jsr CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	rts
	.bend  ; start

	.priv

readInput	.block
	move.l #InputPath, d1
	move.l #MODE_OLDFILE, d2
	movea.l DosBase, a6
	jsr OPEN(a6)
	tst.l d0
	beq.s fail
	move.l d0, d4
	move.l d0, d1
	move.l #InputBuffer, d2
	move.l #INPUT_BUFFER_BYTES, d3
	jsr READ(a6)
	move.l d0, InputLength
	move.l d4, d1
	jsr CLOSE(a6)
	tst.l InputLength
	bmi.s fail
	cmpi.l #INPUT_CAPACITY, InputLength
	bhi.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; readInput

; Batch: BE u16 version=1/count, then bounded command records.
; LOAD=1: BE u32 length + package; SELECT=2: BE u16 length + request;
; LOOKUP=3: BE u16 request/mode lengths + request bytes + mode bytes.
; Every API call produces status as data, including expected failures.
executeBatch	.block
	lea InputBuffer, a3
	movea.l a3, a4
	adda.l InputLength, a4
	moveq #4, d0
	bsr.w requireInput
	bne.w fail
	cmpi.w #1, (a3)+
	bne.w fail
	moveq #0, d7
	move.w (a3)+, d7
	cmpi.w #CASE_CAPACITY, d7
	bhi.w fail
	lea ResultBuffer, a5
nextCommand
	tst.w d7
	beq.w batchDone
	moveq #OUTPUT_RECORD_BYTES / 4 - 1, d0
	movea.l a5, a2
clearRecord
	clr.l (a2)+
	dbra d0, clearRecord
	moveq #1, d0
	bsr.w requireInput
	bne.w fail
	moveq #0, d6
	move.b (a3)+, d6
	cmpi.b #1, d6
	beq.s loadPackage
	cmpi.b #2, d6
	beq.w selectPipeline
	cmpi.b #3, d6
	beq.w lookup
	bra.w fail

loadPackage
	moveq #4, d0
	bsr.w requireInput
	bne.w fail
	move.l (a3)+, d6
	cmpi.l #buffers.PACKAGE_STORAGE_CAPACITY, d6
	bhi.w fail
	move.l d6, d0
	bsr.w requireInput
	bne.w fail
	lea buffers.PackageStorage, a2
	move.l d6, d0
	beq.s packageCopied
copyPackage
	move.b (a3)+, (a2)+
	subq.l #1, d0
	bne.s copyPackage
packageCopied
	move.l d6, d0
	movem.l d2-d7/a2-a6, -(sp)
	jsr loader.tkpkgPackageLoaderLoadStagedV1
	movem.l (sp)+, d2-d7/a2-a6
	bra.w recordLoaderStatus

selectPipeline
	moveq #2, d0
	bsr.w requireInput
	bne.w fail
	moveq #0, d5
	move.w (a3)+, d5
	cmpi.w #buffers.LAST_ERROR_BUFFER_CAPACITY, d5
	bhi.w fail
	move.l d5, d0
	bsr.w requireInput
	bne.w fail
	bsr.w copyRequest
	movem.l d2-d7/a2-a6, -(sp)
	jsr pipeline.tkpkgPipelineSetActiveV1
	movem.l (sp)+, d2-d7/a2-a6
	bra.w recordStatus

lookup
	moveq #4, d0
	bsr.w requireInput
	bne.w fail
	moveq #0, d5
	move.w (a3)+, d5
	moveq #0, d6
	move.w (a3)+, d6
	cmpi.w #buffers.LAST_ERROR_BUFFER_CAPACITY, d5
	bhi.w fail
	move.l d5, d0
	add.l d6, d0
	bsr.w requireInput
	bne.w fail
	bsr.w copyRequest
	movea.l a3, a1
	adda.l d6, a3
	move.l d6, d0
	movem.l d2-d7/a2-a6, -(sp)
	jsr compact.findFixedProgramFromRequestV1
	movem.l (sp)+, d2-d7/a2-a6
recordPayload
	move.l d0, (a5)
	moveq #0, d2
	move.w d1, d2
	cmpi.w #RESULT_PAYLOAD_CAPACITY, d2
	bhi.s fail
	move.w d2, 4(a5)
	tst.w d2
	beq.s recordDone
	lea 8(a5), a2
copyResult
	move.b (a1)+, (a2)+
	subq.w #1, d2
	bne.s copyResult
	bra.s recordDone
recordLoaderStatus
	tst.l d0
	bne.s recordPayload
	bra.s recordStatusOnly
recordStatus
	cmpi.l #abi.STATUS_RUNTIME_ERROR_V1, d0
	beq.s recordPayload
recordStatusOnly
	move.l d0, (a5)
recordDone
	adda.w #OUTPUT_RECORD_BYTES, a5
	subq.w #1, d7
	bra.w nextCommand
batchDone
	cmpa.l a4, a3
	bne.s fail
	lea ResultBuffer, a0
	move.l a5, d0
	sub.l a0, d0
	move.l d0, OutputLength
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; executeBatch

; A3 cursor/A4 end, D0 required bytes. D0=0 success/1 failure; clobbers D1/CCR.
requireInput	.block
	move.l a4, d1
	sub.l a3, d1
	cmp.l d1, d0
	bhi.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; requireInput

; Copy D5 request bytes from A3 and expose the existing LE control-block window.
; Returns A0 CB; advances A3; clobbers D0/A2/CCR.
copyRequest	.block
	lea buffers.lastErrorBuffer, a2
	move.w d5, d0
	beq.s copied
copy
	move.b (a3)+, (a2)+
	subq.w #1, d0
	bne.s copy
copied
	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.b d0, abi.CB_INPUT_PTR(a0)
	lsr.w #8, d0
	move.b d0, abi.CB_INPUT_PTR + 1(a0)
	move.w d5, d0
	move.b d0, abi.CB_INPUT_LEN(a0)
	lsr.w #8, d0
	move.b d0, abi.CB_INPUT_LEN + 1(a0)
	rts
	.bend  ; copyRequest

writeOutput	.block
	move.l #OutputPath, d1
	move.l #MODE_NEWFILE, d2
	movea.l DosBase, a6
	jsr OPEN(a6)
	tst.l d0
	beq.s fail
	move.l d0, d4
	move.l d0, d1
	move.l #ResultBuffer, d2
	move.l OutputLength, d3
	jsr WRITE(a6)
	move.l d0, -(sp)
	move.l d4, d1
	jsr CLOSE(a6)
	move.l (sp)+, d0
	cmp.l OutputLength, d0
	bne.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; writeOutput

	.endsection

	.section data, kind=data
DosName
	.byte "dos.library", 0
InputPath
	.byte "Work:compact-memo-cases.bin", 0
OutputPath
	.byte "Work:build/compact-memo-results.bin", 0
	.endsection

	.section bss, kind=bss
DosBase
	.res long, 1
ReturnCode
	.res long, 1
InputLength
	.res long, 1
OutputLength
	.res long, 1
InputBuffer
	.res byte, INPUT_BUFFER_BYTES
	.align 2
ResultBuffer
	.res byte, OUTPUT_CAPACITY
	.endsection

	.output "build/tkpkg_compact_memo_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
