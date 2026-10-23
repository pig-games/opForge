; Numeric section/block carriers for immutable compiled-module fragments.
; @opforge-owner: opforge.assembler.modules.carrier
	.module opforge.assembler.modules.carrier
	.cpu 68020
	.use opforge.assembler.modules.image as image
	.use opforge.assembler.modules.records as records
	.use opforge.platform.memory as memory
	.use opforge.assembler.source.records as source
	.use opforge.assembler.layout.sections as sections
	.pub
Owner	.struct
Local	.long ?  ; topmost image.Block.Owner ordinal; zero is mandatory content
Global	.long ?  ; hidden ordinary wrapper label; zero only for Local=0
Refs	.long ?  ; borrowed global u32 dependency IDs
RefCount	.long ?
PublicIDs	.long ?  ; borrowed global u32 IDs owned by this whole block
PublicCount	.long ?
	.endstruct
OWNER_BYTES = Owner.PublicCount+4
Section	.struct
Slot	.long ?  ; global concrete section slot, 0..7
Alignment	.long ?  ; shared declaration alignment, independent of fragments
	.endstruct
SECTION_BYTES = Section.Alignment+4
Frame	.struct
Image	.long ?
Bytes	.long ?
ModuleId	.long ?
Owners	.long ?  ; sparse Owner rows in strictly increasing Local order
OwnerCount	.long ?
Sections	.long ?  ; eight Section rows indexed by image-local section
Destination	.long ?  ; owned prepared-record memory.Block; append transaction
Line	.long ?  ; source provenance, unsigned16
	.endstruct
FRAME_BYTES = Frame.Line+4
OK = 0
INVALID = 1
	.priv
	.section bss, kind=bss
Active	.res long, 1
Caller	.res long, 1
Temporary	.res byte, memory.Block.Used+4
Seen	.res byte, memory.Block.Used+4
CurrentOwner	.res long, 1
CurrentSection	.res long, 1
CurrentKind	.res long, 1
CurrentAlignment	.res long, 1
OwnerOpen	.res long, 1
SlotKinds	.res long, 8
SlotAlignments	.res long, 8
	.endsection
	.section code, kind=code
	.pub
; A0=Frame with immutable disjoint image/maps/lists. D0/CCR=status; others kept.
; Appends numeric records only. Public address IDs receive ownership metadata,
; while hidden wrapper labels retain ordinary whole-block selection behavior.
; Owner zero is never wrapped/omitted. Nonzero owners must form one contiguous
; image run; reopening one would split a whole block and rejects. Input rows
; own no transferred resources. Failure preserves destination Used and payload;
; its capacity may grow. Sequential session; all temporary owners retire.
build	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l Active
	bne.w busy
	move.l #1, Active
	move.l a0, Caller
	movea.l a0, a5
	move.l Frame.ModuleId(a5), d0
	cmpi.l #65535, d0
	bhi.w bad
	cmpi.l #65535, Frame.Line(a5)
	bhi.w bad
	movea.l Frame.Destination(a5), a0
	move.l a0, d0
	beq.w bad
	move.l memory.Block.Used(a0), d0
	cmp.l memory.Block.Capacity(a0), d0
	bhi.w bad
	movea.l Frame.Image(a5), a0
	move.l Frame.Bytes(a5), d0
	jsr image.validate
	bne.w bad
	bsr.w validateMaps
	bne.w bad
	move.l Frame.OwnerCount(a5), d0
	lea Seen, a0
	jsr memory.reserveExact
	bne.w bad
	move.l Frame.OwnerCount(a5), d0
	move.l d0, Seen+memory.Block.Used
	movea.l Seen+memory.Block.Pointer, a0
clearSeen
	tst.l d0
	beq.w imageReady
	clr.b (a0)+
	subq.l #1, d0
	bra.w clearSeen
imageReady
	lea SlotKinds, a0
	moveq #15, d0
clearSlots
	clr.l (a0)+
	dbra d0, clearSlots
	move.l #-1, CurrentOwner
	move.l #-1, CurrentSection
	clr.l OwnerOpen
	movea.l Frame.Image(a5), a4
	move.l image.Header.BlockCount(a4), d7
	adda.l image.Header.Blocks(a4), a4
next
	tst.l d7
	beq.w finish
	move.l image.Block.Owner(a4), d1
	cmp.l CurrentOwner, d1
	beq.w ownerReady
	bsr.w closeOwner
	bne.w bad
	move.l image.Block.Owner(a4), CurrentOwner
	bsr.w findOwner
	bne.w ownerMissing
	tst.l d1
	beq.w ownerFound
	movea.l Seen+memory.Block.Pointer, a0
	tst.b 0(a0, d2.l)
	bne.w bad
	move.b #1, 0(a0, d2.l)
ownerFound
	; The wrapper's ordinary address label must execute inside its first section.
	bsr.w selectSection
	bne.w bad
	tst.l d1
	beq.w rootLists
	move.l Owner.Global(a2), d1
	bsr.w openOwner
	bne.w bad
rootLists
	bsr.w ownerLists
	bne.w bad
	bra.w ownerReady
ownerMissing
	tst.l d1
	bne.w bad
ownerReady
	bsr.w selectSection
	bne.w bad
	lea Temporary, a0
	move.l Frame.ModuleId(a5), d1
	move.l image.Block.Id(a4), d2
	move.l Frame.Line(a5), d3
	jsr records.appendFragment
	bne.w bad
	adda.w #image.BLOCK_BYTES, a4
	subq.l #1, d7
	bra.w next
finish
	bsr.w closeOwner
	bne.w bad
	bsr.w closeSection
	bne.w bad
	bsr.w publish
	bne.w bad
	moveq #OK, d7
	bra.w cleanup
bad
	moveq #INVALID, d7
cleanup
	lea Temporary, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Seen, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l Caller
	clr.l Active
	move.l d7, d0
	bra.w done
busy
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; build
	.priv
; A5=Frame. Validate map ordering, hidden wrapper IDs and section constraints.
validateMaps	.block
	movem.l d1-d7/a0-a3, -(sp)
	move.l Frame.OwnerCount(a5), d7
	cmpi.l #65535, d7
	bhi.w bad
	movea.l Frame.Owners(a5), a0
	tst.l d7
	beq.w sectionsReady
	move.l a0, d0
	beq.w bad
	move.l d7, d0
	mulu.l #OWNER_BYTES, d0
	add.l a0, d0
	bcs.w bad
	moveq #0, d6
owners
	move.l Owner.Local(a0), d1
	cmpi.l #65535, d1
	bhi.w bad
	tst.l d6
	beq.w first
	cmp.l -OWNER_BYTES+Owner.Local(a0), d1
	bls.w bad
first
	move.l Owner.Global(a0), d2
	tst.l d1
	beq.w root
	tst.l d2
	beq.w bad
	cmpi.l #65535, d2
	bhi.w bad
	movea.l Frame.Owners(a5), a1
	move.l d6, d5
unique
	tst.l d5
	beq.w checked
	cmp.l Owner.Global(a1), d2
	beq.w bad
	adda.w #OWNER_BYTES, a1
	subq.l #1, d5
	bra.w unique
root
	tst.l d2
	bne.w bad
checked
	adda.w #OWNER_BYTES, a0
	addq.l #1, d6
	subq.l #1, d7
	bne.w owners
sectionsReady
	movea.l Frame.Sections(a5), a0
	move.l a0, d0
	beq.w bad
	addi.l #8*SECTION_BYTES, d0
	bcs.w bad
	moveq #7, d7
sectionRow
	cmpi.l #sections.HUNK_SLOT_COUNT, Section.Slot(a0)
	bhs.w bad
	move.l Section.Alignment(a0), d1
	beq.w bad
	move.l d1, d2
	subq.l #1, d2
	and.l d1, d2
	bne.w bad
	adda.w #SECTION_BYTES, a0
	dbra d7, sectionRow
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; validateMaps

; D1=local owner. D0/CCR=status,A2=row,D2=row index. Others preserved.
findOwner	.block
	movem.l d3-d6/a0-a1, -(sp)
	moveq #0, d3
	move.l Frame.OwnerCount(a5), d4
next
	cmp.l d4, d3
	bhs.w bad
	move.l d4, d2
	sub.l d3, d2
	lsr.l #1, d2
	add.l d3, d2
	move.l d2, d5
	mulu.l #OWNER_BYTES, d5
	movea.l Frame.Owners(a5), a2
	adda.l d5, a2
	cmp.l Owner.Local(a2), d1
	beq.w found
	blo.w lower
	move.l d2, d3
	addq.l #1, d3
	bra.w next
lower
	move.l d2, d4
	bra.w next
found
	moveq #OK, d0
	bra.w done
bad
	suba.l a2, a2
	moveq #INVALID, d0
done
	movem.l (sp)+, d3-d6/a0-a1
	tst.l d0
	rts
	.bend  ; findOwner

; D0=record bytes,D1=flags. A1=new bounded record; D0/CCR=status.
append	.block
	movem.l d1-d5/a0, -(sp)
	move.l d0, d4
	move.l d1, d5
	lea Temporary, a0
	move.l memory.Block.Used(a0), d3
	add.l d3, d0
	bcs.w bad
	move.l #source.RECORD_LIMIT, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d3, a1
	add.l d4, memory.Block.Used(a0)
	subq.l #1, d4
	move.b d4, source.RecordHeader.Length(a1)
	move.b d5, source.RecordHeader.Flags(a1)
	move.l Frame.Line(a5), d3
	move.w d3, source.RecordHeader.Line(a1)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d5/a0
	tst.l d0
	rts
	.bend  ; append

; D1=hidden wrapper ID. Others preserved; label is ordinary numeric syntax.
openOwner	.block
	movem.l d1-d2/a1, -(sp)
	move.l d1, d2
	moveq #source.DECLARATION_HEADER_BYTES, d0
	moveq #source.FLAG_BLOCK_OPEN, d1
	bsr.w append
	bne.w done
	clr.b source.DeclarationRecord.Name+source.NameToken.Kind(a1)
	move.w d2, source.DeclarationRecord.Name+source.NameToken.Id(a1)
	clr.b source.DeclarationRecord.Name+source.NameToken.Qualifier(a1)
	move.b #source.TOKEN_LABEL_DECLARATION, source.DeclarationRecord.Marker(a1)
	move.l #1, OwnerOpen
done
	movem.l (sp)+, d1-d2/a1
	tst.l d0
	rts
	.bend  ; openOwner

closeOwner	.block
	movem.l d1/a1, -(sp)
	tst.l OwnerOpen
	beq.w good
	moveq #source.RECORD_HEADER_BYTES, d0
	moveq #source.FLAG_BLOCK_CLOSE, d1
	bsr.w append
	bne.w done
	clr.l OwnerOpen
good
	moveq #OK, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; closeOwner

; A4=image fragment. Resolve local section through the explicit global map.
selectSection	.block
	movem.l d1-d6/a0-a2, -(sp)
	move.l image.Block.Section(a4), d4
	cmpi.l #sections.HUNK_SLOT_COUNT, d4
	bhs.w bad
	mulu.l #SECTION_BYTES, d4
	movea.l Frame.Sections(a5), a2
	adda.l d4, a2
	move.l Section.Slot(a2), d4
	move.l image.Block.Kind(a4), d5
	move.l Section.Alignment(a2), d6
	move.l d4, d0
	lsl.l #2, d0
	lea SlotKinds, a0
	tst.l 0(a0, d0.l)
	beq.w firstKind
	cmp.l 0(a0, d0.l), d5
	bne.w bad
	lea SlotAlignments, a0
	cmp.l 0(a0, d0.l), d6
	bne.w bad
	bra.w kindReady
firstKind
	move.l d5, 0(a0, d0.l)
	lea SlotAlignments, a0
	move.l d6, 0(a0, d0.l)
kindReady
	cmp.l CurrentSection, d4
	bne.w open
	cmp.l CurrentKind, d5
	bne.w bad
	cmp.l CurrentAlignment, d6
	bne.w bad
	bra.w good
open
	bsr.w closeSection
	bne.w done
	moveq #source.CONCRETE_SECTION_BYTES, d0
	moveq #source.FLAG_LAYOUT, d1
	bsr.w append
	bne.w done
	move.b #sections.CONTROL_EXTRA_SECTION, source.LayoutRecord.Opcode(a1)
	move.b d5, source.ConcreteSection.Kind(a1)
	move.b d4, source.ConcreteSection.Slot(a1)
	move.l d6, source.ConcreteSection.Alignment(a1)
	move.l d4, CurrentSection
	move.l d5, CurrentKind
	move.l d6, CurrentAlignment
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d6/a0-a2
	tst.l d0
	rts
	.bend  ; selectSection

closeSection	.block
	movem.l d1/a1, -(sp)
	cmpi.l #-1, CurrentSection
	beq.w good
	moveq #source.LayoutRecord.Opcode+1, d0
	moveq #source.FLAG_LAYOUT, d1
	bsr.w append
	bne.w done
	move.b #sections.CONTROL_END_SECTION, source.LayoutRecord.Opcode(a1)
	move.l #-1, CurrentSection
good
	moveq #OK, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; closeSection

; A2=owner row. Split lists only at the shared packed-list record limit.
ownerLists	.block
	movem.l d1-d7/a0-a3, -(sp)
	movea.l Owner.PublicIDs(a2), a3
	move.l Owner.PublicCount(a2), d7
	moveq #1, d6
nextList
	cmpi.l #65535, d7
	bhi.w bad
	move.l d7, d0
	lsl.l #2, d0
	move.l a3, d1
	tst.l d7
	beq.w listReady
	tst.l d1
	beq.w bad
	add.l d0, d1
	bcs.w bad
listReady
	tst.l d7
	beq.w following
	move.l d7, d5
	cmpi.l #records.REFERENCE_LIMIT, d5
	bls.w chunk
	move.l #records.REFERENCE_LIMIT, d5
chunk
	lea Temporary, a0
	movea.l a3, a1
	move.l d5, d1
	move.l Frame.Line(a5), d3
	tst.l d6
	beq.w references
	jsr records.appendOwners
	bra.w written
references
	jsr records.appendReferences
written
	bne.w bad
	move.l d5, d0
	lsl.l #2, d0
	adda.l d0, a3
	sub.l d5, d7
	bra.w listReady
following
	tst.l d6
	beq.w good
	moveq #0, d6
	movea.l Owner.Refs(a2), a3
	move.l Owner.RefCount(a2), d7
	bra.w nextList
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; ownerLists

; Commit the complete temporary stream only after every image/map/list passes.
publish	.block
	movem.l d1-d5/a0-a2, -(sp)
	movea.l Frame.Destination(a5), a0
	move.l memory.Block.Used(a0), d4
	move.l Temporary+memory.Block.Used, d5
	move.l d4, d0
	add.l d5, d0
	bcs.w bad
	move.l #source.RECORD_LIMIT, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d4, a1
	movea.l Temporary+memory.Block.Pointer, a2
	move.l d5, d0
copy
	tst.l d0
	beq.w copied
	move.b (a2)+, (a1)+
	subq.l #1, d0
	bra.w copy
copied
	add.l d5, memory.Block.Used(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d5/a0-a2
	tst.l d0
	rts
	.bend  ; publish
	.endsection
	.endmodule
