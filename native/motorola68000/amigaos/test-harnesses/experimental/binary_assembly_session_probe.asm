; Callable lifecycle qualification harness. The caller supplies the same valid,
; nonempty frozen Frame used by the normal CLI; output follows normal emission.
; @opforge-owner: tests.amigaos.binary_assembly_session_probe
	.module tests.amigaos.binary_assembly_session_probe
	.cpu 68020
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_assembly_session as session
	.use experimental.amigaos.binary_package as package
	.pub
MAX_STEPS = 1048576
Saved	.struct
Count	.long ?
Base	.long ?
Steps	.long ?
.endstruct
SAVED_BYTES = Saved.Steps+4
	.section code, kind=code
; A0=Frame. D0/CCR=status; other registers preserved. Expected API rejections
; must preserve the active owner, and strict emission must use that same owner.
run	.block
	movem.l d1-d4/a0-a2/a5, -(sp)
	suba.w #SAVED_BYTES, sp
	movea.l a0, a5
	movea.l assembly.Frame.Context(a5), a1
	move.l package.Context.Count(a1), Saved.Count(sp)
	move.l assembly.Frame.Records(a5), Saved.Base(sp)
	clr.l Saved.Steps(sp)
	tst.l assembly.Frame.RecordBytes(a5)
	beq.w bad
	; A semantic initialization failure must release its partially built owner.
	clr.l package.Context.Count(a1)
	jsr assembly.beginSizing
	movea.l assembly.Frame.Context(a5), a1
	move.l Saved.Count(sp), package.Context.Count(a1)
	tst.l d0
	beq.w bad
	jsr assembly.release
	bne.w bad
	jsr assembly.beginSizing
	bne.w bad
	; A rejected full driver must not release this already active owner.
	jsr assembly.assemble
	tst.l d0
	beq.w bad
	; Sealing before any traversal must reject without destroying the owner.
	jsr assembly.finishSizing
	tst.l d0
	beq.w bad
	move.l assembly.Frame.RecordBytes(a5), d0
	subq.l #1, d0
	moveq #1, d1
	jsr assembly.advance
	tst.l d0
	beq.w bad
	; A relocated base must reject before owners dereference their pinned views.
	addq.l #1, assembly.Frame.Records(a5)
	move.l assembly.Frame.RecordBytes(a5), d0
	moveq #1, d1
	jsr assembly.advance
	move.l Saved.Base(sp), assembly.Frame.Records(a5)
	tst.l d0
	beq.w bad
resume
	addq.l #1, Saved.Steps(sp)
	cmpi.l #MAX_STEPS, Saved.Steps(sp)
	bhi.w bad
	move.l assembly.Frame.RecordBytes(a5), d0
	moveq #1, d1
	jsr assembly.advance
	bne.w bad
	cmpi.l #session.ADVANCED, d1
	beq.w resume
	cmpi.l #session.FRONTIER, d1
	beq.w seal
	cmpi.l #session.TERMINATED, d1
	bne.w bad
seal
	jsr assembly.finishSizing
	bne.w bad
	jsr assembly.emit
	bne.w bad
	jsr assembly.release
	bne.w bad
	; Releasing again is idempotent and must preserve final Frame.Used.
	jsr assembly.release
	bra.w done
bad
	move.l Saved.Base(sp), assembly.Frame.Records(a5)
	movea.l assembly.Frame.Context(a5), a1
	move.l Saved.Count(sp), package.Context.Count(a1)
	jsr assembly.release
	moveq #1, d0
done
	adda.w #SAVED_BYTES, sp
	movem.l (sp)+, d1-d4/a0-a2/a5
	tst.l d0
	rts
	.bend  ; run
	.endsection
	.endmodule
