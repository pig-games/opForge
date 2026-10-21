; Serialized module template tables. Offsets are module- or pool-relative;
; transient capture owners are never copied as descriptors into this image.
build	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l NameState+names.State.Rows+memory.Block.Used, d4
	move.l NameState+names.State.Names+memory.Block.Used, d5
	move.l #HEADER_BYTES, d6
	add.l d4, d6
	bcs.w bad
	add.l d5, d6
	bcs.w bad
	addq.l #3, d6
	bcs.w bad
	andi.l #$fffffffc, d6
	move.l d6, d7
	add.l Table+memory.Block.Used, d7
	bcs.w bad
	move.l d7, d0
	add.l Data+memory.Block.Used, d0
	bcs.w bad
	move.l d0, d3
	lea Image, a0
	jsr memory.reserveExact
	bne.w bad
	move.l d3, Image+memory.Block.Used
	movea.l Image+memory.Block.Pointer, a4
	move.l #MAGIC, Header.Magic(a4)
	move.l d3, Header.Bytes(a4)
	move.l NameState+names.State.Base, Header.Base(a4)
	move.l d4, d0
	moveq #names.ROW_BYTES, d1
	divu.l d1, d0
	move.l d0, Header.BindingCount(a4)
	move.l #HEADER_BYTES, Header.Bindings(a4)
	move.l d4, d0
	addi.l #HEADER_BYTES, d0
	move.l d0, Header.Names(a4)
	move.l d5, Header.NameBytes(a4)
	move.l Table+memory.Block.Used, d0
	moveq #ROW_BYTES, d1
	divu.l d1, d0
	move.l d0, Header.Count(a4)
	move.l d6, Header.Rows(a4)
	move.l d7, Header.Data(a4)
	move.l Data+memory.Block.Used, Header.DataBytes(a4)
	lea HEADER_BYTES(a4), a1
	movea.l NameState+names.State.Rows+memory.Block.Pointer, a0
	move.l d4, d0
	bsr.w copy
	movea.l a4, a1
	adda.l Header.Names(a4), a1
	movea.l NameState+names.State.Names+memory.Block.Pointer, a0
	move.l d5, d0
	bsr.w copy
	movea.l a4, a0
	adda.l Header.Names(a4), a0
	adda.l d5, a0
	movea.l a4, a1
	adda.l d6, a1
padding
	cmpa.l a1, a0
	bhs.w rows
	clr.b (a0)+
	bra.w padding
rows
	movea.l Table+memory.Block.Pointer, a0
	move.l Table+memory.Block.Used, d0
	bsr.w copy
	movea.l a4, a1
	adda.l d7, a1
	movea.l Data+memory.Block.Pointer, a0
	move.l Data+memory.Block.Used, d0
	bsr.w copy
	movea.l a4, a0
	move.l d3, d0
	bsr.w validate
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; build
	.pub
; A0=immutable capsule,D0=exact bytes. D0/CCR=status; others preserved.
; Validate complete tables and each body capsule before installation mutates
; destination names or definitions. Names themselves are checked by their owner.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d7
	cmpi.l #HEADER_BYTES, d7
	blo.w bad
	cmpi.l #memory.LIMIT, d7
	bhi.w bad
	move.l a6, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	add.l d7, d0
	bcs.w bad
	cmpi.l #MAGIC, Header.Magic(a6)
	bne.w bad
	cmp.l Header.Bytes(a6), d7
	bne.w bad
	cmpi.l #65535, Header.Base(a6)
	bhi.w bad
	cmpi.l #HEADER_BYTES, Header.Bindings(a6)
	bne.w bad
	move.l Header.BindingCount(a6), d0
	cmpi.l #65535, d0
	bhi.w bad
	add.l Header.Base(a6), d0
	cmpi.l #65536, d0
	bhi.w bad
	move.l Header.BindingCount(a6), d0
	mulu.l #names.ROW_BYTES, d0
	addi.l #HEADER_BYTES, d0
	cmp.l Header.Names(a6), d0
	bne.w bad
	add.l Header.NameBytes(a6), d0
	bcs.w bad
	addq.l #3, d0
	bcs.w bad
	andi.l #$fffffffc, d0
	cmp.l Header.Rows(a6), d0
	bne.w bad
	move.l Header.Count(a6), d1
	cmpi.l #memory.LIMIT/ROW_BYTES, d1
	bhi.w bad
	mulu.l #ROW_BYTES, d1
	add.l d1, d0
	bcs.w bad
	cmp.l Header.Data(a6), d0
	bne.w bad
	add.l Header.DataBytes(a6), d0
	bcs.w bad
	cmp.l d7, d0
	bne.w bad
	movea.l a6, a5
	adda.l Header.Bindings(a6), a5
	move.l Header.BindingCount(a6), d6
	move.l Header.Base(a6), d5
binding
	tst.l d6
	beq.w bindingsReady
	cmp.l names.Row.Id(a5), d5
	bne.w bad
	cmpi.l #names.MEMBER, names.Row.Namespace(a5)
	bhi.w bad
	movea.l a5, a0
	jsr names.validateEntities
	bne.w bad
	addq.l #1, d5
	adda.w #names.ROW_BYTES, a5
	subq.l #1, d6
	bra.w binding
bindingsReady
	movea.l a6, a5
	adda.l Header.Rows(a6), a5
	move.l Header.Count(a6), d6
	moveq #0, d5
row
	tst.l d6
	beq.w good
	cmpi.l #1, Row.Visibility(a5)
	bhi.w bad
	cmpi.l #65535, Row.Entity(a5)
	bhi.w bad
	cmp.l Row.Offset(a5), d5
	bne.w bad
	move.l Row.Bytes(a5), d0
	add.l d5, d0
	bcs.w bad
	cmp.l Header.DataBytes(a6), d0
	bhi.w bad
	move.l d0, d5
	movea.l a6, a0
	adda.l Header.Data(a6), a0
	adda.l Row.Offset(a5), a0
	move.l Row.Bytes(a5), d0
	jsr templates.validateClosed
	bne.w bad
	movea.l a0, a1
	adda.l templates.ClosedHeader.Definition(a0), a1
	moveq #0, d1
	move.w templates.Def.Name(a1), d1
	cmp.l Row.Name(a5), d1
	bne.w bad
	tst.l Row.StatementBytes(a5)
	bne.w statement
	tst.l Row.Statement(a5)
	bne.w bad
	cmpi.w #templates.KIND_MACRO, templates.Def.Kind(a1)
	bhi.w bad
	bra.w nextRow
statement
	cmpi.w #templates.KIND_EXTERNAL, templates.Def.Kind(a1)
	bne.w bad
	cmp.l Row.Statement(a5), d5
	bne.w bad
	add.l Row.StatementBytes(a5), d5
	bcs.w bad
	cmp.l Header.DataBytes(a6), d5
	bhi.w bad
	movea.l a6, a0
	adda.l Header.Data(a6), a0
	adda.l Row.Statement(a5), a0
	move.l Row.StatementBytes(a5), d0
	jsr statementImage.validate
	bne.w bad
nextRow
	adda.w #ROW_BYTES, a5
	subq.l #1, d6
	bra.w row
good
	cmp.l Header.DataBytes(a6), d5
	bne.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validate
	.priv
