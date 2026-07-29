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
; Outputs: D0 = dos.library/FPuts result, or -1 when no output handle exists.
; Clobbers: D0-D2/A0-A1/A6/CCR.
; CCR: reflects D0 only on the no-handle path; otherwise unspecified.
putErrStr	.block
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

; Open or create an AmigaDOS output file.
openOutput	.block
	move.l a0, d1
	move.l #constants.MODE_NEWFILE, d2
	movea.l state.NativeCliDosBase, a6
	jsr constants.OPEN(a6)
	rts
	.bend  ; openOutput

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
