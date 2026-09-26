; Shared scalar layout preparation. State contains numeric identities and offsets;
; field and size records are ordinary numeric assignments, never emitted storage.
; @opforge-owner: experimental.amigaos.binary_structs
	.module experimental.amigaos.binary_structs
	.cpu 68020
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_imports as imports
	.pub
State	.struct
Active	.word ?
Parent	.word ?
Name	.word ?
ByteId	.word ?
WordId	.word ?
LongId	.word ?
ResId	.word ?
Reserved	.word ?
Size	.long ?
	.endstruct
SCRATCH_BYTES = State.Size+4
KEY_STRUCT = 20
KEY_ENDSTRUCT = 21
KEY_DB = 22
KEY_DW = 23
	.section code, kind=code
; A0=state. D0/CCR=status; other registers preserved.
begin	.block
	clr.w State.Active(a0)
	clr.l State.Size(a0)
	moveq #0, d0
	rts
	.bend  ; begin
; A0=state,A1=package header. Preserve registers; CCR unspecified.
configure	.block
	move.w package.Header.ByteDirective(a1), State.ByteId(a0)
	move.w package.Header.WordDirective(a1), State.WordId(a0)
	move.w package.Header.LongDirective(a1), State.LongId(a0)
	move.w package.Header.ResDirective(a1), State.ResId(a0)
	rts
	.bend  ; configure
; D0=record capacity; A0=normalized record,A1=scope state,A2=layout state,
; A3=scope keyword
; callback (D0=id,A6=scope). D0/CCR=status; all other registers preserved.
; Excluded conditional records never enter this routine. Scalar placeholders
; have no alignment. A body accepts only labeled scalar fields and its close.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d2
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	moveq #0, d6
	move.b (a5), d6
	addq.w #1, d6
	lea 4(a5), a0
	moveq #-1, d7
	cmpi.w #9, d6
	blo.w noLabel
	cmpi.b #1, (a0)
	bhi.w noLabel
	cmpi.b #5, 4(a0)
	bne.w noLabel
	moveq #0, d7
	move.w 1(a0), d7
	addq.l #5, a0
noLabel
	movea.l a5, a1
	adda.w d6, a1
	cmpa.l a1, a0
	beq.w emptyLine
	cmpi.b #7, (a0)
	bne.w ordinary
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w bad
	moveq #0, d5
	move.w 2(a0), d5
	move.l d5, d0
	jsr (a3)
	cmpi.l #KEY_STRUCT, d0
	beq.w open
	cmpi.l #KEY_ENDSTRUCT, d0
	beq.w close
	tst.w State.Active(a4)
	beq.w ok
	tst.l d7
	bmi.w bad
	moveq #1, d4
	cmp.w State.ByteId(a4), d5
	beq.w scalar
	cmpi.l #KEY_DB, d0
	beq.w scalar
	moveq #2, d4
	cmp.w State.WordId(a4), d5
	beq.w scalar
	cmpi.l #KEY_DW, d0
	beq.w scalar
	moveq #4, d4
	cmp.w State.LongId(a4), d5
	beq.w scalar
	cmp.w State.ResId(a4), d5
	beq.w reserve
	bra.w bad
scalar
	addq.l #5, a0
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #1, d0
	bne.w bad
	cmpi.b #9, (a0)
	bne.w bad
	bra.w field
reserve
	addq.l #5, a0
	jsr imports.evaluateRange
	bne.w bad
	tst.l d1
	bmi.w bad
	move.l d1, d4
field
	move.l State.Size(a4), d1
	add.l d4, State.Size(a4)
	bvs.w bad
	bra.w assignment
open
	tst.w State.Active(a4)
	bne.w bad
	addq.l #5, a0
	tst.l d7
	bpl.w labeledOpen
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	tst.b 3(a0)
	bne.w bad
	moveq #0, d7
	move.w 1(a0), d7
	bra.w enter
labeledOpen
	cmpa.l a1, a0
	bne.w bad
enter
	move.l d7, d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	lsl.l #4, d1
	lea layout.ENTRIES(a6), a0
	adda.l d1, a0
	btst #0, records.Entry.Flags+1(a0)
	bne.w bad
	move.w layout.State.Current(a6), State.Parent(a4)
	move.w layout.State.Current(a6), records.Entry.Owner(a0)
	move.w d7, State.Name(a4)
	addq.w #1, d0
	move.w d0, layout.State.Current(a6)
	move.w #1, State.Active(a4)
	clr.l State.Size(a4)
	bra.w consume
close
	tst.w State.Active(a4)
	beq.w bad
	tst.l d7
	bpl.w bad
	addq.l #5, a0
	cmpa.l a1, a0
	bne.w bad
	move.w State.Parent(a4), layout.State.Current(a6)
	clr.w State.Active(a4)
	moveq #0, d7
	move.w State.Name(a4), d7
	move.l State.Size(a4), d1
assignment
	cmpi.l #14, d2
	blo.w bad
	; 14-byte writer assignment: name, equals, numeric i32.
	move.b #13, (a5)
	move.b #1, 4(a5)
	move.l d7, d0
	lsr.w #8, d0
	move.b d0, 5(a5)
	move.b d7, 6(a5)
	clr.b 7(a5)
	move.b #34, 8(a5)
	move.b #2, 9(a5)
	move.l d1, d0
	lsr.l #8, d0
	move.b d0, 12(a5)
	lsr.l #8, d0
	move.b d0, 11(a5)
	lsr.l #8, d0
	move.b d0, 10(a5)
	move.b d1, 13(a5)
	bra.w ok
emptyLine
	tst.l d7
	bpl.w ordinary
	bra.w ok
ordinary
	tst.w State.Active(a4)
	bne.w bad
	bra.w ok
consume
	move.b #3, (a5)
	clr.b 1(a5)
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line
	.endsection
	.endmodule
