; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.dos
	.cpu 68020

	.use opforge.cli.state
	.use opforge.cli.constants

	.section code, kind=code
	.pub

; Write a zero-terminated string through dos.library/PutStr.
; Inputs: D1 = zero-terminated string pointer.
; Outputs: D0 = dos.library/PutStr result.
; Clobbers: D0/A6/CCR.
; CCR: unspecified on return.
putStr	.block
	movea.l state.NativeCliDosBase, a6
	jsr constants.PUT_STR(a6)
	rts
	.bend  ; putStr

; Write a zero-terminated diagnostic through the process ErrorOutput stream.
; Inputs: D1 = zero-terminated string pointer.
; Outputs: D0 = dos.library/FPuts result, or -1 when DOS/output is unavailable.
; Clobbers: D0-D2/A0-A1/A6/CCR.
; CCR: reflects D0 only on unavailable-output paths; otherwise unspecified.
putErrStr	.block
	tst.l state.NativeCliDosBase
	beq.s unavailable
	move.l d1, -(sp)
	suba.l a1, a1
	movea.l constants.SYS_BASE.W, a6
	jsr constants.FIND_TASK(a6)
	movea.l d0, a0
	move.l constants.PR_CES(a0), d1
	bne.s haveHandle
	move.l constants.PR_COS(a0), d1

haveHandle
	move.l (sp)+, d2
	tst.l d1
	beq.s noHandle
	movea.l state.NativeCliDosBase, a6
	jsr constants.FPUTS(a6)
	rts

noHandle
	moveq #-1, d0
	rts

unavailable
	moveq #-1, d0
	rts
	.bend  ; putErrStr

; Open an existing AmigaDOS input file.
openInput	.block
	move.l a0, d1
	move.l #constants.MODE_OLDFILE, d2
	movea.l state.NativeCliDosBase, a6
	jsr constants.OPEN(a6)
	rts
	.bend  ; openInput

; Close an AmigaDOS file handle in D1.
close	.block
	movea.l state.NativeCliDosBase, a6
	jsr constants.CLOSE(a6)
	rts
	.bend  ; dos.close

; Read D0 bytes from file handle D1 into buffer A0.
readInput	.block
	move.l a0, d2
	move.l d0, d3
	movea.l state.NativeCliDosBase, a6
	jsr constants.READ(a6)
	rts
	.bend  ; readInput

; Lock an existing path for shared read access.
; Inputs: A0 = NUL-terminated path. Outputs: D0 = BCPL lock or zero.
; Clobbers: D0-D2/A6/CCR. CCR: reflects the dos.library result.
lockRead	.block
	move.l a0, d1
	move.l #constants.ACCESS_READ, d2
	movea.l state.NativeCliDosBase, a6
	jsr constants.LOCK(a6)
	rts
	.bend  ; lockRead

; Release a BCPL lock.
; Inputs: D1 = lock. Outputs: none. Clobbers: D0-D1/A6/CCR.
; CCR: unspecified on return.
unlock	.block
	movea.l state.NativeCliDosBase, a6
	jsr constants.UNLOCK(a6)
	rts
	.bend  ; unlock

; Initialize a longword-aligned FileInfoBlock for a directory lock.
; Inputs: D1 = lock; A0 = FileInfoBlock. Outputs: D0 = nonzero on success.
; Clobbers: D0-D2/A6/CCR. CCR: reflects the dos.library result.
examine	.block
	move.l a0, d2
	movea.l state.NativeCliDosBase, a6
	jsr constants.EXAMINE(a6)
	rts
	.bend  ; examine

; Read the next directory entry using the same lock/FileInfoBlock pair.
; Inputs: D1 = lock; A0 = initialized FileInfoBlock.
; Outputs: D0 = nonzero on success. Clobbers: D0-D2/A6/CCR.
; CCR: reflects the dos.library result.
exNext	.block
	move.l a0, d2
	movea.l state.NativeCliDosBase, a6
	jsr constants.EX_NEXT(a6)
	rts
	.bend  ; exNext

; Return the last AmigaDOS I/O error.
; Outputs: D0 = error code. Clobbers: D0/A6/CCR.
; CCR: reflects the dos.library result.
ioErr	.block
	movea.l state.NativeCliDosBase, a6
	jsr constants.IO_ERR(a6)
	rts
	.bend  ; ioErr

; Open or create an AmigaDOS output file.
openOutput	.block
	move.l a0, d1
	move.l #constants.MODE_NEWFILE, d2
	movea.l state.NativeCliDosBase, a6
	jsr constants.OPEN(a6)
	rts
	.bend  ; openOutput

; Create one AmigaDOS directory and return its lock.
; Inputs: A0 = NUL-terminated path. Outputs: D0 = lock or zero.
; Clobbers: D0-D1/A6/CCR. CCR: reflects the dos.library result.
createDir	.block
	move.l a0, d1
	movea.l state.NativeCliDosBase, a6
	jsr constants.CREATE_DIR(a6)
	rts
	.bend  ; createDir

; Append one AmigaDOS path component using dos.library path semantics.
; Inputs: A0 = writable NUL path, A1 = NUL component, D0 = path capacity.
; Outputs: D0 = nonzero on success.
; Clobbers: D0-D3/A6/CCR. CCR: reflects the dos.library result.
addPart	.block
	move.l a0, d1
	move.l a1, d2
	move.l d0, d3
	movea.l state.NativeCliDosBase, a6
	jsr constants.ADD_PART(a6)
	rts
	.bend  ; addPart

; Write D0 bytes from buffer A0 to file handle D1.
writeOutput	.block
	move.l a0, d2
	move.l d0, d3
	movea.l state.NativeCliDosBase, a6
	jsr constants.WRITE(a6)
	rts
	.bend  ; writeOutput

	.endsection
	.endmodule
