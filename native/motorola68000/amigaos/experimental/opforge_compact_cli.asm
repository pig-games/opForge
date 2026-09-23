; Provisional Shell entry: opforge_compact PACKAGE.bsp3 SOURCE.asm OUTPUT.bin.
	.module main
	.cpu 68020
	.use experimental.amigaos.binary_app as app
PATH_BYTES = 256
	.section entry, kind=code
	.pub
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	lea DosName, a1
	moveq #36, d0
	movea.l 4.w, a6
	jsr -552(a6)
	tst.l d0
	beq.w unavailable
	move.l d0, DosBase
	movea.l d0, a6
	jsr -534(a6)  ; GetArgStr
	tst.l d0
	beq.w usage
	movea.l d0, a3
	lea PackagePath, a1
	bsr.w nextPath
	bne.w usage
	lea SourcePath, a1
	bsr.w nextPath
	bne.w usage
	lea OutputPath, a1
	bsr.w nextPath
	bne.w usage
	bsr.w skipSpace
	tst.b (a3)
	bne.w usage
	movea.l DosBase, a1
	movea.l 4.w, a6
	jsr -414(a6)
	lea Config, a0
	move.l #PackagePath, app.Frame.PackagePath(a0)
	move.l #SourcePath, app.Frame.SourcePath(a0)
	move.l #OutputPath, app.Frame.OutputPath(a0)
	move.w #1, app.Frame.Mode(a0)
	jsr app.execute
	bra.w done
usage
	movea.l DosBase, a6
	move.l #UsageText, d1
	jsr -948(a6)  ; PutStr
	movea.l DosBase, a1
	movea.l 4.w, a6
	jsr -414(a6)
unavailable
	moveq #20, d0
done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.priv
; A3=argument tail cursor, A1=256-byte destination. D0=0 on success.
nextPath	.block
	bsr.w skipSpace
	tst.b (a3)
	beq.w bad
	move.w #PATH_BYTES-1, d1
copy
	moveq #0, d0
	move.b (a3), d0
	beq.w endPath
	cmpi.b #' ', d0
	beq.w endPath
	cmpi.b #9, d0
	beq.w endPath
	cmpi.b #10, d0
	beq.w endPath
	cmpi.b #13, d0
	beq.w endPath
	cmpi.b #'"', d0  ; quoted paths need a later explicit parser contract
	beq.w bad
	tst.w d1
	beq.w bad
	move.b (a3)+, (a1)+
	subq.w #1, d1
	bra.w copy
endPath
	clr.b (a1)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; nextPath
skipSpace	.block
again
	cmpi.b #' ', (a3)
	beq.w advance
	cmpi.b #9, (a3)
	beq.w advance
	cmpi.b #10, (a3)
	beq.w advance
	cmpi.b #13, (a3)
	bne.w done
advance
	addq.l #1, a3
	bra.w again
done
	rts
	.bend  ; skipSpace
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
UsageText	.byte "Usage: opforge_compact PACKAGE.bsp3 SOURCE.asm OUTPUT.bin", 10, 0
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
Config	.res byte, app.Frame.Mode+2
PackagePath	.res byte, PATH_BYTES
SourcePath	.res byte, PATH_BYTES
OutputPath	.res byte, PATH_BYTES
	.endsection
	.output "build/opforge_compact", format=hunk, sections=entry, code, data, bss
	.endmodule
