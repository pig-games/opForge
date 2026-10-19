; Owned numeric geometry and placement recipes for one prepared module.
	.module experimental.amigaos.binary_module_layout
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_module_image as image
	.use experimental.amigaos.binary_block_index as blocks
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_hunk_references as hunkrefs
	.pub
State	.struct
Records	.long ?
Bytes	.long ?
Index	.long ?
Count	.long ?
Names	.long ?
Geometry	.res memory.Block.Used+4
Inputs	.res memory.Block.Used+4
Owners	.res memory.Block.Used+4
Pending	.res memory.Block.Used+4
Relocs	.res memory.Block.Used+4
	.endstruct
STATE_BYTES = State.Relocs+memory.Block.Used+4
	.priv
; Shared section-kind value, independent of the selected instruction package.
CODE_KIND = 1
Row	.struct
Section	.long ?
StartPC	.long ?
NextPC	.long ?
Offset	.long ?
Bytes	.long ?
	.endstruct
ROW_BYTES = Row.Bytes+4
PendingRow	.struct
Source	.long ?
SourcePC	.long ?
Symbol	.long ?
Section	.long ?
Value	.long ?
Span	.long ?
Program	.long ?
Arguments	.long ?
Position	.long ?
	.endstruct
PENDING_BYTES = PendingRow.Position+4
OK = 0
INVALID = 1
	.section code, kind=code
	.pub
; A0=zero State,A1=records,D0=bytes,A2=index,D1=name count.
; Records/index are borrowed until finish. Failed begin retains owned cleanup.
; All entry points preserve registers except D0/CCR; release is idempotent.
begin	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l a1, State.Records(a6)
	move.l d0, State.Bytes(a6)
	move.l a2, State.Index(a6)
	move.l d1, State.Names(a6)
	cmpi.l #65536, d1
	bhi.w bad
	move.l blocks.Scratch.Count(a2), d2
	beq.w bad
	cmpi.l #memory.LIMIT/ROW_BYTES, d2
	bhi.w bad
	cmpi.l #65535, d2
	bhi.w bad
	move.l d2, State.Count(a6)
	move.l d2, d0
	mulu.l #ROW_BYTES, d0
	lea State.Geometry(a6), a0
	jsr memory.reserveExact
	bne.w bad
	move.l d2, d0
	mulu.l #image.INPUT_BYTES, d0
	lea State.Inputs(a6), a0
	jsr memory.reserveExact
	bne.w bad
	move.l State.Names(a6), d0
	add.l d0, d0
	lea State.Owners(a6), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l State.Owners+memory.Block.Pointer(a6), a0
	move.l State.Names(a6), d0
	beq.w rows
clearOwners
	clr.w (a0)+
	subq.l #1, d0
	bne.w clearOwners
rows
	movea.l State.Geometry+memory.Block.Pointer(a6), a3
	movea.l State.Inputs+memory.Block.Pointer(a6), a4
	movea.l State.Index(a6), a2
	adda.w #blocks.HEADER_BYTES, a2
	moveq #1, d3
nextRow
	tst.w blocks.Span.Parent(a2)
	bne.w bad
	move.l #$ffffffff, Row.Section(a3)
	clr.l Row.StartPC(a3)
	clr.l Row.NextPC(a3)
	clr.l Row.Offset(a3)
	clr.l Row.Bytes(a3)
	move.l d3, image.Input.Id(a4)
	clr.l image.Input.Data(a4)
	clr.l image.Input.Bytes(a4)
	adda.w #ROW_BYTES, a3
	adda.w #image.INPUT_BYTES, a4
	adda.w #blocks.SPAN_BYTES, a2
	addq.l #1, d3
	cmp.l State.Count(a6), d3
	bls.w nextRow
	move.l State.Count(a6), d0
	mulu.l #ROW_BYTES, d0
	move.l d0, State.Geometry+memory.Block.Used(a6)
	move.l State.Count(a6), d0
	mulu.l #image.INPUT_BYTES, d0
	move.l d0, State.Inputs+memory.Block.Used(a6)
	move.l State.Names(a6), d0
	add.l d0, d0
	move.l d0, State.Owners+memory.Block.Used(a6)
	moveq #0, d7
record
	cmp.l State.Bytes(a6), d7
	bhs.w good
	move.l State.Bytes(a6), d0
	sub.l d7, d0
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	blo.w bad
	movea.l State.Records(a6), a2
	adda.l d7, a2
	moveq #0, d6
	move.b source.RecordHeader.Length(a2), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	cmp.l d0, d6
	bhi.w bad
	cmpi.l #source.DECLARATION_HEADER_BYTES, d6
	blo.w nextRecord
	cmpi.b #source.TOKEN_LABEL_DECLARATION, source.DeclarationRecord.Marker(a2)
	bne.w nextRecord
	cmpi.b #source.TOKEN_NAME_MAX, source.DeclarationRecord.Name+source.NameToken.Kind(a2)
	bhi.w nextRecord
	moveq #0, d5
	move.w source.DeclarationRecord.Name+source.NameToken.Id(a2), d5
	cmp.l State.Names(a6), d5
	bhs.w bad
	move.l d7, d0
	bsr.w owner
	tst.l d0
	beq.w nextRecord
	movea.l State.Owners+memory.Block.Pointer(a6), a1
	add.l d5, d5
	move.w d0, 0(a1, d5.l)
nextRecord
	add.l d6, d7
	bra.w record
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; begin

; A0=State,A1=assembly Frame,D1=PC,D2=output offset,D3=bytes.
emitted	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l assembly.Frame.RecordOffset(a5), d0
	bsr.w owner
	tst.l d0
	beq.w bad
	movea.l assembly.Frame.Sections(a5), a2
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a2)
	bne.w bad
	tst.w sections.State.HunkSelected(a2)
	beq.w bad
	cmpi.w #CODE_KIND, sections.State.ActiveKind(a2)
	bne.w bad
	moveq #0, d4
	move.w sections.State.HunkCurrent(a2), d4
	cmpi.l #$ffffffff, Row.Section(a3)
	bne.w existing
	move.l d4, Row.Section(a3)
	move.l d1, Row.StartPC(a3)
	move.l d1, Row.NextPC(a3)
	move.l d2, Row.Offset(a3)
	move.l d2, image.Input.Data(a4)
existing
	cmp.l Row.Section(a3), d4
	bne.w bad
	cmp.l Row.NextPC(a3), d1
	bne.w bad
	move.l Row.Offset(a3), d0
	add.l Row.Bytes(a3), d0
	bcs.w bad
	cmp.l d2, d0
	bne.w bad
	move.l Row.Bytes(a3), d0
	add.l d3, d0
	bcs.w bad
	move.l d0, Row.Bytes(a3)
	move.l d0, image.Input.Bytes(a4)
	add.l d3, d1
	bcs.w bad
	move.l d1, Row.NextPC(a3)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; emitted

; A0=State,A1=Frame,D1=PC,D2=section,D3=instruction length.
instruction	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #PENDING_BYTES, sp
	movea.l a0, a6
	movea.l a1, a5
	move.l d1, PendingRow.SourcePC(sp)
	move.l d3, PendingRow.Span(sp)
	jsr encoding.outputPositionDependent
	tst.l d0
	beq.w good
	jsr encoding.outputPositionRecipe
	bne.w bad
	move.l d1, PendingRow.Program(sp)
	move.l d2, PendingRow.Arguments(sp)
	move.l d3, PendingRow.Value(sp)
	subq.l #1, d4
	cmpi.l #sections.HUNK_SLOT_COUNT, d4
	bhs.w bad
	move.l d4, PendingRow.Section(sp)
	cmp.l State.Names(a6), d5
	bhs.w bad
	move.l d5, PendingRow.Symbol(sp)
	move.l d5, d0
	add.l d0, d0
	movea.l State.Owners+memory.Block.Pointer(a6), a0
	tst.w 0(a0, d0.l)
	beq.w bad
	move.l #1, PendingRow.Position(sp)
	move.l assembly.Frame.RecordOffset(a5), d0
	bsr.w owner
	tst.l d0
	beq.w bad
	move.l d0, PendingRow.Source(sp)
	movea.l sp, a1
	bsr.w appendPending
	bra.w done
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #PENDING_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; instruction

; A0=State,A1=Frame,D0=source section,D1=target section,
; D2=field section offset,D3=original symbol,D4=normalized addend.
absolute	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #PENDING_BYTES, sp
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d7
	; The current absolute image recipe writes BE32. Other data byte orders
	; require their own explicit recipe, rather than silently patching wrongly.
	movea.l assembly.Frame.Context(a5), a2
	movea.l package.Context.Package(a2), a0
	tst.w package.Header.LittleEndian(a0)
	bne.w bad
	cmp.l State.Names(a6), d3
	bhs.w bad
	move.l d3, PendingRow.Symbol(sp)
	move.l d3, d0
	add.l d0, d0
	movea.l State.Owners+memory.Block.Pointer(a6), a0
	tst.w 0(a0, d0.l)
	beq.w bad
	move.l d1, PendingRow.Section(sp)
	addq.l #1, d1
	movea.l assembly.Frame.Context(a5), a2
	jsr hunkrefs.sectionBase
	bne.w bad
	add.l d4, d1
	bcs.w bad
	move.l d1, PendingRow.Value(sp)
	move.l d7, d1
	addq.l #1, d1
	jsr hunkrefs.sectionBase
	bne.w bad
	add.l d2, d1
	bcs.w bad
	move.l d1, PendingRow.SourcePC(sp)
	move.l #4, PendingRow.Span(sp)
	move.l #image.PROGRAM_ABSOLUTE, PendingRow.Program(sp)
	clr.l PendingRow.Arguments(sp)
	clr.l PendingRow.Position(sp)
	move.l assembly.Frame.RecordOffset(a5), d0
	bsr.w owner
	tst.l d0
	beq.w bad
	move.l d0, PendingRow.Source(sp)
	movea.l sp, a1
	bsr.w appendPending
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #PENDING_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; absolute

; A0=State,A1=final assembly Frame. Materialize pointers only after geometry closes.
finish	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	movea.l State.Geometry+memory.Block.Pointer(a6), a3
	movea.l State.Inputs+memory.Block.Pointer(a6), a4
	move.l State.Count(a6), d7
rows
	move.l Row.Section(a3), d0
	cmpi.l #sections.HUNK_SLOT_COUNT, d0
	bhs.w bad
	tst.l Row.Bytes(a3)
	beq.w bad
	mulu.l #sections.HUNK_SLOT_BYTES, d0
	movea.l assembly.Frame.Sections(a5), a2
	adda.l d0, a2
	adda.w #sections.HUNK_SLOTS, a2
	tst.w sections.HunkSlot.Seen(a2)
	beq.w bad
	cmpi.w #CODE_KIND, sections.HunkSlot.Kind(a2)
	bne.w bad
	move.l assembly.Frame.Output(a5), d0
	add.l Row.Offset(a3), d0
	bcs.w bad
	move.l d0, image.Input.Data(a4)
	adda.w #ROW_BYTES, a3
	adda.w #image.INPUT_BYTES, a4
	subq.l #1, d7
	bne.w rows
	moveq #0, d6
section
	moveq #0, d4
	move.l State.Count(a6), d7
	movea.l State.Geometry+memory.Block.Pointer(a6), a3
sum
	cmp.l Row.Section(a3), d6
	bne.w skip
	add.l Row.Bytes(a3), d4
	bcs.w bad
skip
	adda.w #ROW_BYTES, a3
	subq.l #1, d7
	bne.w sum
	movea.l assembly.Frame.Sections(a5), a2
	move.l d6, d0
	mulu.l #sections.HUNK_SLOT_BYTES, d0
	adda.l d0, a2
	adda.w #sections.HUNK_SLOTS, a2
	cmp.l sections.HunkSlot.Allocation(a2), d4
	bne.w bad
	addq.l #1, d6
	cmpi.l #sections.HUNK_SLOT_COUNT, d6
	blo.w section
	moveq #0, d7
pending
	cmp.l State.Pending+memory.Block.Used(a6), d7
	bhs.w good
	movea.l State.Pending+memory.Block.Pointer(a6), a2
	adda.l d7, a2
	move.l PendingRow.Source(a2), d6
	move.l d6, d0
	bsr.w geometry
	movea.l a3, a4
	move.l PendingRow.Symbol(a2), d0
	cmp.l State.Names(a6), d0
	bhs.w bad
	add.l d0, d0
	movea.l State.Owners+memory.Block.Pointer(a6), a0
	moveq #0, d5
	move.w 0(a0, d0.l), d5
	beq.w bad
	move.l d5, d0
	bsr.w geometry
	move.l PendingRow.Section(a2), d0
	cmp.l Row.Section(a3), d0
	bne.w bad
	move.l PendingRow.Value(a2), d4
	sub.l Row.StartPC(a3), d4
	bcs.w bad
	cmp.l Row.Bytes(a3), d4
	bhi.w bad
	move.l PendingRow.SourcePC(a2), d3
	sub.l Row.StartPC(a4), d3
	bcs.w bad
	move.l d3, d0
	add.l PendingRow.Span(a2), d0
	bcs.w bad
	cmp.l Row.Bytes(a4), d0
	bhi.w bad
	tst.l PendingRow.Position(a2)
	beq.w retain
	cmp.l d5, d6
	beq.w advance
retain
	move.l State.Relocs+memory.Block.Used(a6), d2
	move.l d2, d0
	addi.l #image.RELOC_BYTES, d0
	bcs.w bad
	lea State.Relocs(a6), a0
	jsr memory.reserve
	bne.w bad
	movea.l State.Relocs+memory.Block.Pointer(a6), a0
	adda.l d2, a0
	move.l d6, image.Reloc.Block(a0)
	move.l d3, image.Reloc.Offset(a0)
	move.l d5, image.Reloc.Target(a0)
	move.l d4, image.Reloc.Addend(a0)
	move.l PendingRow.Span(a2), image.Reloc.Span(a0)
	move.l PendingRow.Program(a2), image.Reloc.Program(a0)
	move.l PendingRow.Arguments(a2), image.Reloc.Arguments(a0)
	addi.l #image.RELOC_BYTES, State.Relocs+memory.Block.Used(a6)
advance
	addi.l #PENDING_BYTES, d7
	bra.w pending
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finish

release	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	lea State.Geometry(a2), a0
	jsr memory.release
	lea State.Inputs(a2), a0
	jsr memory.release
	lea State.Owners(a2), a0
	jsr memory.release
	lea State.Pending(a2), a0
	jsr memory.release
	lea State.Relocs(a2), a0
	jsr memory.release
	movea.l a2, a0
	move.l #STATE_BYTES/4, d1
clear
	clr.l (a0)+
	subq.l #1, d1
	bne.w clear
	moveq #OK, d0
	movem.l (sp)+, d1-d2/a0-a2
	rts
	.bend  ; release
	.priv
; A6=State,D0=record offset. D0=ordinal or zero,A3=geometry,A4=input.
owner	.block
	movem.l d1-d2/a0, -(sp)
	movea.l State.Index(a6), a0
	adda.w #blocks.HEADER_BYTES, a0
	moveq #1, d1
next
	cmp.l State.Count(a6), d1
	bhi.w absent
	cmp.l blocks.Span.Start(a0), d0
	blo.w advance
	cmp.l blocks.Span.End(a0), d0
	blo.w found
advance
	adda.w #blocks.SPAN_BYTES, a0
	addq.l #1, d1
	bra.w next
found
	move.l d1, d0
	bsr.w geometry
	subq.l #1, d1
	mulu.l #image.INPUT_BYTES, d1
	movea.l State.Inputs+memory.Block.Pointer(a6), a4
	adda.l d1, a4
	bra.w done
absent
	moveq #OK, d0
done
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; owner
; A6=State,D0=validated one-based ordinal. A3=row; D0 preserved.
geometry	.block
	move.l d0, -(sp)
	subq.l #1, d0
	mulu.l #ROW_BYTES, d0
	movea.l State.Geometry+memory.Block.Pointer(a6), a3
	adda.l d0, a3
	move.l (sp)+, d0
	rts
	.bend  ; geometry
; A6=State,A1=pending row. Appends an owned copy; preserves other registers.
appendPending	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l State.Pending+memory.Block.Used(a6), d2
	move.l d2, d0
	addi.l #PENDING_BYTES, d0
	bcs.w bad
	lea State.Pending(a6), a0
	jsr memory.reserve
	bne.w bad
	movea.l State.Pending+memory.Block.Pointer(a6), a0
	adda.l d2, a0
	move.l #PENDING_BYTES/4, d1
copy
	move.l (a1)+, (a0)+
	subq.l #1, d1
	bne.w copy
	addi.l #PENDING_BYTES, State.Pending+memory.Block.Used(a6)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; appendPending
	.endsection
	.endmodule
