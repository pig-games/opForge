; Validate the immutable offset-based signature plan before matching.
; @opforge-owner: prvm.amigaos.statement_plan
	.module prvm.amigaos.statement_plan
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_inputs as inputs
	.pub
	.section code, kind=code
; A0/D0=plan/bytes, A1/D1=workspace/bytes, A2=budget cell.
; D0=status/CCR, D1=row count, D2=payload base; other registers preserved.
; Workspace holds a checked four-byte stack entry per possible plan row.
validate	.block
	movem.l d3-d7/a0-a6, -(sp)
	move.l d0, -(sp)  ; saved complete plan extent (zero offset)
	movea.l a0, a4
	movea.l a1, a3
	movea.l a2, a6
	move.l a0, d2
	btst #0, d2
	bne.w invalid
	cmpi.l #abi.PLAN_HEADER_BYTES, d0
	blo.w invalid
	jsr inputs.span
	bne.w finish
	move.l (a4), d0  ; PlanHeader.Extent (zero offset)
	cmp.l (sp), d0
	bne.w invalid
	move.l abi.PlanHeader.RowCount(a4), d6
	cmpi.l #$05555554, d6  ; 24-byte rows plus header fit signed extent
	bhi.w invalid
	move.l d6, d0
	lsl.l #3, d0
	move.l d0, d5
	add.l d0, d0
	add.l d0, d5  ; abi.ATOM_ROW_BYTES = 3*8
	addi.l #abi.PLAN_HEADER_BYTES, d5
	cmp.l (sp), d5
	bhi.w invalid
	move.l d6, d0
	lsl.l #2, d0  ; one four-byte nesting stack index per row
	cmp.l d1, d0
	bhi.w overflow
	movea.l a3, a0
	move.l a0, d2
	btst #0, d2
	bne.w invalid
	jsr inputs.span
	bne.w finish
	lea abi.PLAN_HEADER_BYTES(a4), a5
	moveq #0, d7
	moveq #0, d4
	moveq #0, d3
	moveq #0, d2
next
	cmp.l d6, d7
	beq.w scores
	movea.l a6, a0
	jsr inputs.tick
	bne.w finish
	tst.w abi.AtomRow.Reserved(a5)
	bne.w invalid
	move.l abi.AtomRow.ColStart(a5), d0
	cmp.l abi.AtomRow.ColEnd(a5), d0
	bhi.w invalid
	move.l abi.AtomRow.PayloadOffset(a5), d0
	cmp.l d5, d0
	blo.w invalid
	move.l (sp), d1
	cmp.l d1, d0
	bhi.w invalid
	sub.l d0, d1
	cmp.l abi.AtomRow.PayloadBytes(a5), d1
	blo.w invalid
	tst.l d4
	bne.w kind
	cmpi.b #abi.ATOM_CLOSE, (a5)  ; AtomRow.Kind (zero offset)
	beq.w kind
	addq.l #1, d2
	cmpi.b #abi.ATOM_LITERAL, (a5)
	bne.w kind
	addq.l #1, d3
kind
	cmpi.b #abi.ATOM_LITERAL, (a5)
	beq.w literal
	cmpi.b #abi.ATOM_CAPTURE, (a5)
	beq.w capture
	cmpi.b #abi.ATOM_OPEN, (a5)
	beq.w open
	cmpi.b #abi.ATOM_CLOSE, (a5)
	beq.w close
	bra.w invalid
literal
	tst.b abi.AtomRow.CaptureType(a5)
	bne.w invalid
	bra.w noPair
capture
	cmpi.b #abi.TYPE_FIRST, abi.AtomRow.CaptureType(a5)
	blo.w invalid
	cmpi.b #abi.TYPE_LAST, abi.AtomRow.CaptureType(a5)
	bhi.w invalid
	tst.l abi.AtomRow.PayloadBytes(a5)
	beq.w invalid
noPair
	cmpi.l #abi.NONE, abi.AtomRow.PairedRow(a5)
	bne.w invalid
	bra.w advance
open
	tst.b abi.AtomRow.CaptureType(a5)
	bne.w invalid
	tst.l abi.AtomRow.PayloadBytes(a5)
	bne.w invalid
	move.l abi.AtomRow.PairedRow(a5), d0
	cmp.l d7, d0
	bls.w invalid
	cmp.l d6, d0
	bhs.w invalid
	move.l d7, (a3)+
	addq.l #1, d4
	bra.w advance
close
	tst.b abi.AtomRow.CaptureType(a5)
	bne.w invalid
	tst.l abi.AtomRow.PayloadBytes(a5)
	bne.w invalid
	tst.l d4
	beq.w invalid
	subq.l #1, d4
	move.l -(a3), d0
	cmp.l abi.AtomRow.PairedRow(a5), d0
	bne.w invalid
	bsr.w rowAddress
	cmp.l abi.AtomRow.PairedRow(a0), d7
	bne.w invalid
advance
	adda.w #abi.ATOM_ROW_BYTES, a5
	addq.l #1, d7
	bra.w next
scores
	tst.l d4
	bne.w invalid
	cmp.l abi.PlanHeader.LiteralScore(a4), d3
	bne.w invalid
	cmp.l abi.PlanHeader.AtomScore(a4), d2
	bne.w invalid
	move.l d6, d1
	move.l d5, d2
	moveq #abi.OK, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	bra.w finish
invalid
	moveq #abi.INVALID_ARGUMENT, d0
finish
	moveq #0, d1
	moveq #0, d2
done
	addq.l #4, sp
	tst.l d0
	movem.l (sp)+, d3-d7/a0-a6
	rts
	.bend  ; validate
	.priv
; D0=checked row index, A4=plan. A0=row; clobbers D0/D1, CCR unspecified.
rowAddress	.block
	lsl.l #3, d0
	move.l d0, d1
	add.l d0, d0
	add.l d1, d0
	lea abi.PLAN_HEADER_BYTES(a4), a0
	adda.l d0, a0
	rts
	.bend  ; rowAddress
	.endsection
	.endmodule
