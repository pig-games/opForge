; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.dos
	.cpu 68020

	.use opforge.cli.state (NativeCliDosBase)
	.use opforge.cli.constants (PUT_STR, OPEN, CLOSE, READ, WRITE, MODE_OLDFILE, MODE_NEWFILE)

	.section code, kind=code
	.pub

; Write a zero-terminated string through dos.library/PutStr.
opforgeNativeCliPutStr .block
	movea.l NativeCliDosBase, a6
	jsr PUT_STR(a6)
	rts
	.bend ; opforgeNativeCliPutStr

; Open an existing AmigaDOS input file.
opforgeNativeCliOpenInput .block
	move.l a0, d1
	move.l #MODE_OLDFILE, d2
	movea.l NativeCliDosBase, a6
	jsr OPEN(a6)
	rts
	.bend ; opforgeNativeCliOpenInput

; Close an AmigaDOS file handle in D1.
opforgeNativeCliClose .block
	movea.l NativeCliDosBase, a6
	jsr CLOSE(a6)
	rts
	.bend ; opforgeNativeCliClose

; Read D0 bytes from file handle D1 into buffer A0.
opforgeNativeCliReadInput .block
	move.l a0, d2
	move.l d0, d3
	movea.l NativeCliDosBase, a6
	jsr READ(a6)
	rts
	.bend ; opforgeNativeCliReadInput

; Open or create an AmigaDOS output file.
opforgeNativeCliOpenOutput .block
	move.l a0, d1
	move.l #MODE_NEWFILE, d2
	movea.l NativeCliDosBase, a6
	jsr OPEN(a6)
	rts
	.bend ; opforgeNativeCliOpenOutput

; Write D0 bytes from buffer A0 to file handle D1.
opforgeNativeCliWriteOutput .block
	move.l a0, d2
	move.l d0, d3
	movea.l NativeCliDosBase, a6
	jsr WRITE(a6)
	rts
	.bend ; opforgeNativeCliWriteOutput

	.endsection
	.endmodule
