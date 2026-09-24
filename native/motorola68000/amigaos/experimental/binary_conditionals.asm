; Preparation-only conditional selection over numeric writer records.
; Filtered lines become empty records before binding and expression lowering.
; @opforge-owner: experimental.amigaos.binary_conditionals
	.module experimental.amigaos.binary_conditionals
	.cpu 68020
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_imports as imports
	.pub
LIMIT = 16
Slot	.struct
Parent	.byte ?
Taken	.byte ?
ElseSeen	.byte ?
Reserved	.byte ?
	.endstruct
State	.struct
Depth	.word ?
Active	.word ?
	.endstruct
SLOTS = State.Active+2
SCRATCH_BYTES = SLOTS+LIMIT*4
	.section code, kind=code

; A0=state. Start a physical source with no open conditionals.
begin	.block
	clr.w State.Depth(a0)
	move.w #1, State.Active(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. D0/CCR=status; reject a conditional crossing a file boundary.
endFile	.block
	moveq #0, d0
	tst.w State.Depth(a0)
	beq.w done
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; endFile

; A0=writer record,A1=scope state,A2=conditional state.
; D0/CCR=status, D1=one when the normal scope/preparation path should run.
; Control lines and inactive body lines are consumed here; no text is retained.
line	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	moveq #0, d1
	move.b (a5), d1
	addq.w #1, d1
	cmpi.w #9, d1
	blo.w ordinary
	lea 4(a5), a3
	cmpi.b #7, (a3)
	bne.w ordinary
	cmpi.b #1, 1(a3)
	bhi.w ordinary
	tst.b 4(a3)
	bne.w ordinary
	moveq #0, d0
	move.w 2(a3), d0
	sub.w layout.State.Base(a6), d0
	bcs.w ordinary
	cmp.w layout.State.Count(a6), d0
	bhs.w ordinary
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a0
	adda.l d0, a0
	moveq #0, d2
	move.w records.Entry.Length(a0), d2
	sub.w records.Entry.Leaf(a0), d2
	lea layout.ARENA(a6), a1
	moveq #0, d0
	move.w records.Entry.Name(a0), d0
	add.w records.Entry.Leaf(a0), d0
	adda.l d0, a1
	cmpi.w #2, d2
	bne.w checkElse
	move.w (a1), d0
	ori.w #$2020, d0
	cmpi.w #$6966, d0  ; if
	beq.w ifLine
checkElse
	cmpi.w #4, d2
	bne.w checkEndif
	move.l (a1), d0
	ori.l #$20202020, d0
	cmpi.l #$656c7365, d0  ; else
	beq.w elseLine
checkEndif
	cmpi.w #5, d2
	bne.w ordinary
	move.l (a1), d0
	ori.l #$20202020, d0
	cmpi.l #$656e6469, d0  ; endi
	bne.w ordinary
	moveq #0, d0
	move.b 4(a1), d0
	ori.b #$20, d0
	cmpi.b #'f', d0
	bne.w ordinary
	bra.w endifLine
ifLine
	moveq #0, d2
	move.w State.Depth(a4), d2
	cmpi.w #LIMIT, d2
	bhs.w bad
	lsl.w #2, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	move.w State.Active(a4), d0
	move.b d0, Slot.Parent(a2)
	clr.b Slot.Taken(a2)
	clr.b Slot.ElseSeen(a2)
	tst.w State.Active(a4)
	beq.w ifStored
	movea.l a5, a0
	moveq #0, d0
	move.b (a0), d0
	lea 1(a0, d0.w), a1
	lea 9(a0), a0
	movea.l a6, a2
	jsr imports.evaluateScoped
	bne.w bad
	tst.l d1
	beq.w ifStored
	moveq #0, d2
	move.w State.Depth(a4), d2
	lsl.w #2, d2
	lea SLOTS(a4), a2
	move.b #1, Slot.Taken(a2, d2.w)
ifStored
	moveq #0, d2
	move.w State.Depth(a4), d2
	lsl.w #2, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	addq.w #1, State.Depth(a4)
	moveq #0, d0
	move.b Slot.Parent(a2), d0
	and.b Slot.Taken(a2), d0
	move.w d0, State.Active(a4)
	bra.w consumed
elseLine
	moveq #0, d0
	move.b (a5), d0
	cmpi.w #8, d0
	bne.w bad  ; no operands
	moveq #0, d2
	move.w State.Depth(a4), d2
	beq.w bad
	subq.w #1, d2
	lsl.w #2, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	tst.b Slot.ElseSeen(a2)
	bne.w bad
	move.b #1, Slot.ElseSeen(a2)
	moveq #0, d0
	move.b Slot.Taken(a2), d0
	eori.b #1, d0
	and.b Slot.Parent(a2), d0
	move.w d0, State.Active(a4)
	bra.w consumed
endifLine
	moveq #0, d0
	move.b (a5), d0
	cmpi.w #8, d0
	bne.w bad  ; no operands
	moveq #0, d2
	move.w State.Depth(a4), d2
	beq.w bad
	subq.w #1, State.Depth(a4)
	subq.w #1, d2
	lsl.w #2, d2
	lea SLOTS(a4), a2
	moveq #0, d0
	move.b Slot.Parent(a2, d2.w), d0
	move.w d0, State.Active(a4)
	bra.w consumed
ordinary
	move.w State.Active(a4), d1
	moveq #0, d0
	bra.w done
consumed
	moveq #0, d1
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line
	.endsection
	.endmodule
