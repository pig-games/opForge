; Shared struct layout preparation; geometric fields remain scalar constants.
; Closing declarations retain nominal schemas and never emit storage.
; @opforge-owner: experimental.amigaos.binary_structs
	.module experimental.amigaos.binary_structs
	.cpu 68020
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_imports as imports
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_struct_values as structs
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as source
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
Fields	.res memory.Block.Used+4
FieldBinder	.long ?  ; scope-owned bare member identity callback
	.endstruct
SCRATCH_BYTES = State.FieldBinder+4
KEY_STRUCT = 20
KEY_ENDSTRUCT = 21
KEY_DB = 22
KEY_DW = 23
	.section code, kind=code
; A0=state. D0/CCR=status; other registers preserved.
begin	.block
	clr.w State.Active(a0)
	clr.l State.Size(a0)
	clr.l State.Fields+memory.Block.Pointer(a0)
	clr.l State.Fields+memory.Block.Capacity(a0)
	clr.l State.Fields+memory.Block.Used(a0)
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
; A0=state. Release transient field construction storage; other registers preserved.
release	.block
	move.l a0, -(sp)
	lea State.Fields(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l (sp)+, a0
	rts
	.bend  ; release
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
	move.l d2, -(sp)
	jsr imports.evaluateRange
	tst.l d0
	bne.w reserveBad
	tst.l d2
	bne.w reserveBad
	move.l (sp)+, d2
	tst.l d1
	bmi.w bad
	move.l d1, d4
	bra.w field
reserveBad
	move.l (sp)+, d2
	bra.w bad

field
	move.l State.Size(a4), d1
	add.l d4, State.Size(a4)
	bcs.w bad
	move.l d1, -(sp)
	movea.l State.FieldBinder(a4), a2
	move.l a2, d0
	beq.w fieldBad
	movea.l a6, a0
	move.l d7, d0
	jsr (a2)
	tst.l d0
	bne.w fieldBad
	move.l d1, d5
	lea State.Fields(a4), a0
	move.l memory.Block.Used(a0), d0
	addi.l #values.STRUCT_FIELD_BYTES, d0
	jsr memory.reserve
	bne.w fieldBad
	movea.l State.Fields+memory.Block.Pointer(a4), a0
	adda.l State.Fields+memory.Block.Used(a4), a0
	move.l d5, values.StructField.Member(a0)
	move.l (sp)+, d1
	move.l d1, values.StructField.Offset(a0)
	move.l d4, values.StructField.Size(a0)
	addi.l #values.STRUCT_FIELD_BYTES, State.Fields+memory.Block.Used(a4)
	bra.w assignment
fieldBad
	addq.l #4, sp
	bra.w bad
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
	mulu.w #records.ENTRY_BYTES, d1
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d1, a0
	btst #0, records.Entry.Flags+1(a0)
	bne.w bad
	move.w layout.State.Current(a6), State.Parent(a4)
	move.w layout.State.Current(a6), records.Entry.Owner(a0)
	ori.w #records.STRUCT_TYPE, records.Entry.Flags(a0)  ; retained at close
	move.w d7, State.Name(a4)
	addq.w #1, d0
	move.w d0, layout.State.Current(a6)
	move.w #1, State.Active(a4)
	clr.l State.Size(a4)
	clr.l State.Fields+memory.Block.Used(a4)
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
	cmpi.l #13, d2
	blo.w bad
	move.l State.Size(a4), d1
	movea.l State.Fields+memory.Block.Pointer(a4), a1
	move.l State.Fields+memory.Block.Used(a4), d4
	move.l d4, d3
	divu.w #values.STRUCT_FIELD_BYTES, d3
	andi.l #$ffff, d3
	movea.l a6, a0
	adda.l #layout.IMPORT_STATE+imports.KNOWN_OWNER, a0
	move.l d7, d0
	jsr structs.appendSeed
	tst.l d0
	bne.w bad
	; The seed offset is the only declaration payload; geometry stays in Seeds.
	move.b #12, (a5)
	move.b #1, 4(a5)
	move.l d7, d0
	lsr.w #8, d0
	move.b d0, 5(a5)
	move.b d7, 6(a5)
	clr.b 7(a5)
	move.b #source.TOKEN_STRUCT_DECLARATION, 8(a5)
	move.l d2, 9(a5)
	bra.w ok
assignment
	cmpi.l #14, d2
	blo.w bad
	; 14-byte writer assignment: name, equals, numeric u32 magnitude.
	move.b #13, (a5)
	move.b #1, 4(a5)
	move.l d7, d0
	lsr.w #8, d0
	move.b d0, 5(a5)
	move.b d7, 6(a5)
	clr.b 7(a5)
	move.b #34, 8(a5)
	move.b #2, 9(a5)
	move.l d1, 10(a5)
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
