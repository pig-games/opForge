; Standalone paged-name proof: publish a byte only after the component succeeds.
; @opforge-owner: tests.amigaos.name_store_harness
	.module tests.amigaos.name_store_harness
	.cpu 68020
	.use tests.amigaos.name_store_probe as probe
	.priv
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
	.pub
	.section entry, kind=code
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	jsr probe.run
	bne.w done
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w done
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	move.l d0, d1
	move.l #Result, d2
	moveq #1, d3
	jsr DOS_WRITE(a6)
	cmpi.l #1, d0
	bne.w closeFile
	clr.l ReturnCode
closeFile
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.endsection
	.priv
	.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:name-store-probe.bin", 0
Result	.byte $5a
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
	.endsection
	.output "name-store-probe.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
