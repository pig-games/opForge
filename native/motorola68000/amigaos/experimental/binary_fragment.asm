; In-memory fragment envelope over one complete prepared outer block.
; @opforge-owner: experimental.amigaos.binary_fragment
	.module experimental.amigaos.binary_fragment
	.cpu 68020
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_block_index as blocks
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_fragment_sites as sites
	.include "memory_telemetry.i"
	.pub
STATUS_OK = 0
STATUS_INVALID = 1
NO_ID = $ffff
NO_OFFSET = $ffffffff
BACKING_OWNER = 1; this envelope has one complete prepared-source owner
MODULE_LIMIT = 512; frozen assembly module-boundary capacity
Descriptor	.struct
Identity	.long ?
Owner	.long ?
Start	.long ?
Bytes	.long ?
Entry	.word ?
Reserved	.word ?
	.endstruct
DESCRIPTOR_BYTES = Descriptor.Reserved+2
Record	.struct
Local	.long ?
Bytes	.long ?
Declaration	.word ?
Line	.word ?
Module	.long ?
Section	.long ?  ; backing-relative numeric section-control record
	.endstruct
RECORD_BYTES = Record.Section+4
State	.struct
Backing	.res memory.Block.Used+4
Index	.res memory.Block.Used+4
Maps	.res memory.Block.Used+4
Fragment	.res DESCRIPTOR_BYTES
MapCount	.long ?
Sites	.res sites.TABLE_BYTES
SiteCount	.long ?
	.endstruct
STATE_BYTES = State.SiteCount+4
	.section bss, kind=bss
	.priv
Bridge	.res byte, STATE_BYTES
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
Prepared	.res long, 1
CopiedBytes	.res long, 1
MappedRecords	.res long, 1
	.priv
.endif
.endif
	.endsection
	.section code, kind=code
	.pub
; A0=Descriptor,A1=backing base,D0=backing bytes.
; D0/CCR=status,A1=bounded view,D1=extent; others preserved.
; Failure publishes no view. Identities are stable owner-local numbers.
resolve	.block
	move.l d2, -(sp)
	move.l a1, d2
	beq.w bad
	tst.l Descriptor.Identity(a0)
	beq.w bad
	cmpi.l #BACKING_OWNER, Descriptor.Owner(a0)
	bne.w bad
	tst.w Descriptor.Reserved(a0)
	bne.w bad
	move.l Descriptor.Start(a0), d2
	cmp.l d0, d2
	bhi.w bad
	sub.l d2, d0
	move.l Descriptor.Bytes(a0), d1
	beq.w bad
	cmp.l d0, d1
	bhi.w bad
	move.l a1, d0
	add.l d2, d0
	bcs.w bad
	movea.l d0, a1
	add.l d1, d0
	bcs.w bad
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #0, d1
	movea.l d1, a1
	moveq #STATUS_INVALID, d0
done
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; resolve

; A0=zero-initialized State,A1=nonempty assembly.Frame before beginSizing.
; D0/CCR=status; other registers preserved. The Frame is never published here.
; Success owns a complete copied stream and offset-only envelope. Failure frees
; every candidate allocation and resets State. Sealed symbols remain borrowed.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	.MEMORY_COUNTER_CLEAR Prepared
	.MEMORY_COUNTER_CLEAR CopiedBytes
	.MEMORY_COUNTER_CLEAR MappedRecords
	move.l State.Backing+memory.Block.Pointer(a6), d0
	or.l State.Index+memory.Block.Pointer(a6), d0
	or.l State.Maps+memory.Block.Pointer(a6), d0
	bne.w occupied
	move.l assembly.Frame.RecordBytes(a5), d7
	beq.w bad
	movea.l assembly.Frame.Records(a5), a0
	move.l a0, d0
	beq.w bad
	cmpi.l #source.RECORD_LIMIT, d7
	bhi.w bad
	move.l d7, d0
	lea State.Index(a6), a1
	jsr blocks.index
	bne.w bad
	move.l d1, d6
	movea.l State.Index+memory.Block.Pointer(a6), a4
	adda.w #blocks.HEADER_BYTES, a4
	moveq #0, d5
find
	tst.l d6
	beq.w noFragment
	tst.w blocks.Span.Parent(a4)
	bne.w next
	movea.l assembly.Frame.Records(a5), a0
	adda.l blocks.Span.Start(a4), a0
	btst #source.FLAG_OMIT_BIT, source.RecordHeader.Flags(a0)
	beq.w found
next
	addq.l #1, d5
	subq.l #1, d6
	adda.w #blocks.SPAN_BYTES, a4
	bra.w find
found
	addq.l #1, d5
	move.l d5, State.Fragment+Descriptor.Identity(a6)
	move.l #BACKING_OWNER, State.Fragment+Descriptor.Owner(a6)
	move.l blocks.Span.Start(a4), d5
	move.l d5, State.Fragment+Descriptor.Start(a6)
	move.l blocks.Span.End(a4), d0
	sub.l d5, d0
	move.l d0, State.Fragment+Descriptor.Bytes(a6)
	move.w blocks.Span.Entry(a4), State.Fragment+Descriptor.Entry(a6)
	move.l d0, d6
	lea State.Fragment(a6), a0
	movea.l assembly.Frame.Records(a5), a1
	move.l d7, d0
	bsr.w resolve
	bne.w bad
	; The index already validated framing and both span boundaries. Count only
	; this span's records; byte-based worst-case reservation rejects long records.
	moveq #0, d0
countRecords
	moveq #0, d2
	move.b source.RecordHeader.Length(a1), d2
	addq.l #1, d2
	adda.l d2, a1
	sub.l d2, d6
	addq.l #1, d0
	tst.l d6
	bne.w countRecords
	mulu.l #RECORD_BYTES, d0
	lea State.Maps(a6), a0
	jsr memory.reserveExact
	bne.w bad
	bsr.w mapRecords
	bne.w bad
noFragment
	lea State.Sites(a6), a0
	movea.l a5, a1
	jsr sites.observe
	move.l d1, State.SiteCount(a6)
	; A block-free input still uses the complete relocated reference driver.
	lea State.Backing(a6), a0
	move.l d7, d0
	move.l #source.RECORD_LIMIT, d1
	jsr memory.reserveBounded
	bne.w bad
	move.l d7, State.Backing+memory.Block.Used(a6)
	movea.l assembly.Frame.Records(a5), a0
	movea.l State.Backing+memory.Block.Pointer(a6), a1
	move.l d7, d1
copy
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w copy
	tst.l State.Fragment+Descriptor.Identity(a6)
	beq.w ready
	lea State.Fragment(a6), a0
	movea.l State.Backing+memory.Block.Pointer(a6), a1
	move.l d7, d0
	bsr.w resolve
	bne.w bad
ready
	.MEMORY_COUNTER_INC Prepared
	.MEMORY_COUNTER_ADD CopiedBytes, d7
	moveq #STATUS_OK, d0
	bra.w done
bad
	movea.l a6, a0
	bsr.w release
occupied
	moveq #STATUS_INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepare

; A0=State. Idempotent retirement; registers preserved, CCR reflects caller D0.
release	.block
	movem.l d0-d1/a0-a1, -(sp)
	movea.l a0, a1
	lea State.Backing(a1), a0
	jsr memory.release
	lea State.Index(a1), a0
	jsr memory.release
	lea State.Maps(a1), a0
	jsr memory.release
	movea.l a1, a0
	moveq #STATE_BYTES/4-1, d1
clear
	clr.l (a0)+
	dbra d1, clear
	movem.l (sp)+, d0-d1/a0-a1
	tst.l d0
	rts
	.bend  ; release

; A0=assembly.Frame. D0/CCR=status; other registers preserved.
; Session-exclusive bridge: prepare, rebind before sizing, unchanged driver,
; restore original borrowed records, retire candidate on every exit.
run	.block
	tst.l assembly.Frame.RecordBytes(a0)
	bne.w buffered
	; Empty streams have no backing to relocate; retain the reference lifecycle.
	jmp assembly.assemble
buffered
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	move.l assembly.Frame.Records(a2), d2
	movea.l a2, a1
	lea Bridge, a0
	bsr.w prepare
	bne.w done
	move.l Bridge+State.Backing+memory.Block.Pointer, assembly.Frame.Records(a2)
	movea.l a2, a0
	jsr assembly.assemble
	move.l d2, assembly.Frame.Records(a2)
	lea Bridge, a0
	bsr.w release
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; run
	.priv

; A6=State,A5=Frame,D7=validated complete bytes. D0/CCR=status.
; Uses only framing, canonical declaration prefixes and numeric layout markers.
; Module IDs are source-order ordinals; section IDs are backing record offsets.
mapRecords	.block
	movea.l assembly.Frame.Records(a5), a3
	movea.l State.Maps+memory.Block.Pointer(a6), a4
	moveq #0, d6  ; whole backing offset
	moveq #0, d5  ; module ordinal
	moveq #NO_OFFSET, d4
	movea.l assembly.Frame.ModuleStarts(a5), a2
	move.l assembly.Frame.ModuleCount(a5), d3
	cmpi.l #MODULE_LIMIT, d3
	bhi.w bad
	tst.l d3
	beq.w scan
	move.l a2, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l d3, d0
	lsl.l #2, d0
	lsr.l #2, d0
	cmp.l d3, d0
	bne.w bad
	move.l d3, d0
	lsl.l #2, d0
	add.l a2, d0
	bcs.w bad
scan
	; Ordered starts are offsets into this same frozen complete allocation.
module
	tst.l d3
	beq.w record
	cmp.l (a2), d6
	blo.w record
	cmp.l (a2), d6
	bne.w bad
	addq.l #1, d5
	addq.l #4, a2
	subq.l #1, d3
	bra.w module
record
	cmp.l d7, d6
	beq.w good
	moveq #0, d2
	move.b source.RecordHeader.Length(a3), d2
	addq.l #1, d2
	moveq #0, d0
	move.b source.RecordHeader.Flags(a3), d0
	andi.b #source.FLAG_LAYOUT, d0
	beq.w inFragment
	cmpi.l #sections.ControlRecord.Opcode+1, d2
	blo.w bad
	moveq #0, d0
	move.b sections.ControlRecord.Opcode(a3), d0
	cmpi.b #sections.CONTROL_END_SECTION, d0
	bne.w sectionOpen
	moveq #NO_OFFSET, d4
	bra.w inFragment
sectionOpen
	cmpi.b #sections.CONTROL_LOGICAL_SECTION, d0
	beq.w sectionOrigin
	cmpi.b #sections.CONTROL_FIRST_SECTION, d0
	beq.w sectionOrigin
	cmpi.b #sections.CONTROL_MAPPED_CONCRETE, d0
	beq.w sectionOrigin
	cmpi.b #sections.CONTROL_SECOND_SECTION, d0
	beq.w sectionOrigin
	cmpi.b #sections.CONTROL_SECOND_LOGICAL_SECTION, d0
	beq.w sectionOrigin
	cmpi.b #sections.CONTROL_SECOND_MAPPED_CONCRETE, d0
	beq.w sectionOrigin
	cmpi.b #sections.CONTROL_EXTRA_SECTION, d0
	bne.w inFragment
sectionOrigin
	move.l d6, d4
inFragment
	cmp.l State.Fragment+Descriptor.Start(a6), d6
	blo.w next
	move.l d6, d0
	sub.l State.Fragment+Descriptor.Start(a6), d0
	cmp.l State.Fragment+Descriptor.Bytes(a6), d0
	bhs.w next
	move.l d0, Record.Local(a4)
	move.l d2, Record.Bytes(a4)
	move.w #NO_ID, Record.Declaration(a4)
	move.w source.RecordHeader.Line(a3), Record.Line(a4)
	move.l d5, Record.Module(a4)
	move.l d4, Record.Section(a4)
	cmpi.l #source.DECLARATION_HEADER_BYTES, d2
	blo.w mapped
	cmpi.b #source.TOKEN_NAME_MAX, source.DeclarationRecord.Name+source.NameToken.Kind(a3)
	bhi.w mapped
	moveq #0, d0
	move.b source.DeclarationRecord.Marker(a3), d0
	cmpi.b #source.TOKEN_LABEL_DECLARATION, d0
	beq.w declaration
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, d0
	beq.w declaration
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, d0
	beq.w declaration
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, d0
	beq.w declaration
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, d0
	beq.w declaration
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, d0
	bne.w mapped
declaration
	move.w source.DeclarationRecord.Name+source.NameToken.Id(a3), Record.Declaration(a4)
mapped
	adda.w #RECORD_BYTES, a4
	addq.l #1, State.MapCount(a6)
	addi.l #RECORD_BYTES, State.Maps+memory.Block.Used(a6)
	.MEMORY_COUNTER_INC MappedRecords
next
	add.l d2, d6
	adda.l d2, a3
	bra.w scan
good
	tst.l d3
	bne.w bad
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_INVALID, d0
	rts
	.bend  ; mapRecords
	.endsection
	.endmodule
