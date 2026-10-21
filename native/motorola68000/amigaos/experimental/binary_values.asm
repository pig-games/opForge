; Immutable offset-addressed compound values; scalar cells stay caller-owned.
; @opforge-owner: experimental.amigaos.binary_values
	.module experimental.amigaos.binary_values
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_ranges as ranges
	.pub
Owner	.struct
Arena	.res memory.Block.Used+4
Kinds	.res memory.Block.Used+4
Count	.long ?
Work	.res memory.Block.Used+4
Bindings	.res memory.Block.Used+4  ; optional immutable numeric fallback plans
Seeds	.res memory.Block.Used+4  ; immutable struct schemas retained across replay
SchemaReplay	.res memory.Block.Used+4  ; eligible seed offsets, retained across reset/rewind
DeclarationActivity	.long ?  ; borrowed execution owner; detached before its maps release
.endstruct
OWNER_BYTES = Owner.DeclarationActivity+4
SCALAR = 0
LIST = 1
RANGE = 2
TYPED_LIST = 3
ADDRESS_RECORD = 4
STRUCT_TYPE = 5
STRUCT_LITERAL = 6
OK = 0
MALFORMED = 1
STORAGE = 2
BOUNDS = 3
Record	.struct
Kind	.long ?
Count	.long ?
.endstruct
Cell	.struct
Low	.long ?
High	.long ?
Kind	.long ?
Provenance	.long ?
.endstruct
Field	.struct
Member	.long ?
Value	.res 16
.endstruct
StructRecord	.struct
Kind	.long ?
Count	.long ?
Nominal	.long ?
Size	.long ?  ; zero for a literal
.endstruct
StructField	.struct
Member	.long ?
Offset	.long ?
Size	.long ?
.endstruct
STRUCT_BASE_BYTES = StructRecord.Size+4
STRUCT_FIELD_BYTES = StructField.Size+4
CELL_BYTES = 16
FIELD_BYTES = 20
RECORD_BASE_BYTES = 24
HEADER_BYTES = 8
ELEMENT_BYTES = 8
MAX_ELEMENTS = (memory.LIMIT-HEADER_BYTES)/ELEMENT_BYTES
MAX_SECTION_ID = 8; scalar provenance uses zero=absolute, -1=unsafe
EQUALITY_DEPTH_LIMIT = 32
EQUALITY_WORK_LIMIT = memory.LIMIT
	.section code, kind=code

; A0=Owner,D0=symbol count. D0/CCR=status; others preserved.
; Sets Count and clears the transient execution hook. Caller owns existing
; storage lifecycle and retained references; initialize outside active assembly.
init	.block
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	move.l d0, Owner.Count(a0)
	clr.l Owner.DeclarationActivity(a0)
	moveq #OK, d0
	rts
bad
	moveq #STORAGE, d0
	rts
	.bend  ; init

; A0=Owner,D0=new absolute symbol count. D0/CCR=status; others preserved.
; Grows kinds before committing Count; an unallocated scalar table stays empty.
growSymbols	.block
	movem.l d1/d3/a0-a2, -(sp)
	move.l d0, d1
	cmpi.l #memory.LIMIT, d1
	bhi.w noSpace
	cmp.l Owner.Count(a0), d1
	blo.w invalid
	movea.l a0, a2
	lea Owner.Arena(a0), a1
	bsr.w block
	bne.w done
	lea Owner.Kinds(a0), a1
	bsr.w kinds
	bne.w done
	tst.l memory.Block.Pointer(a1)
	beq.w commit
	movea.l a1, a0
	move.l d1, d0
	jsr memory.reserve
	tst.l d0
	bne.w noSpace
	movea.l memory.Block.Pointer(a0), a1
	move.l Owner.Count(a2), d3
	adda.l d3, a1
	sub.l d1, d3
	beq.w used
clear
	clr.b (a1)+
	addq.l #1, d3
	bne.w clear
used
	move.l d1, memory.Block.Used(a0)
commit
	move.l d1, Owner.Count(a2)
	moveq #OK, d0
	bra.w done
invalid
	moveq #MALFORMED, d0
	bra.w done
noSpace
	moveq #STORAGE, d0
done
	movem.l (sp)+, d1/d3/a0-a2
	tst.l d0
	rts
	.bend  ; growSymbols

; A0=Owner,D0=retained arena bytes. D0/CCR=status; others preserved.
; Retains imported descriptors and kinds, truncates arena and clears work usage.
rewind	.block
	movem.l d1/d3/a1, -(sp)
	move.l d0, d1
	move.l d1, d3
	andi.l #7, d3
	bne.w invalid
	lea Owner.Arena(a0), a1
	bsr.w block
	bne.w done
	cmp.l memory.Block.Used(a1), d1
	bhi.w invalid
	lea Owner.Kinds(a0), a1
	bsr.w kinds
	bne.w done
	move.l d1, Owner.Arena+memory.Block.Used(a0)
	clr.l Owner.Work+memory.Block.Used(a0)
	moveq #OK, d0
	bra.w done
invalid
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1/d3/a1
	tst.l d0
	rts
	.bend  ; rewind

; A0=Owner. D0/CCR=status; others preserved.
; Retains allocations and numeric binding plans, clears kinds.
reset	.block
	movem.l d1-d3/a0-a1, -(sp)
	lea Owner.Arena(a0), a1
	bsr.w block
	bne.w done
	lea Owner.Kinds(a0), a1
	bsr.w kinds
	bne.w done
	clr.l Owner.Arena+memory.Block.Used(a0)
	clr.l Owner.Work+memory.Block.Used(a0)
	move.l Owner.Count(a0), d1
	movea.l memory.Block.Pointer(a1), a1
	move.l a1, d0
	beq.w good
	tst.l d1
	beq.w good
clear
	clr.b (a1)+
	subq.l #1, d1
	bne.w clear
good
	moveq #OK, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; reset

; A0=valid Owner. D0/CCR=OK; others preserved. Frees all owned blocks.
release	.block
	move.l a0, -(sp)
	lea Owner.Arena(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea Owner.Kinds(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea Owner.Work(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea Owner.Bindings(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea Owner.Seeds(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea Owner.SchemaReplay(a0), a0
	jsr memory.release
	movea.l (sp)+, a0
	clr.l Owner.Arena+memory.Block.Used(a0)
	clr.l Owner.Kinds+memory.Block.Used(a0)
	clr.l Owner.Work+memory.Block.Used(a0)
	clr.l Owner.Bindings+memory.Block.Used(a0)
	clr.l Owner.Seeds+memory.Block.Used(a0)
	clr.l Owner.SchemaReplay+memory.Block.Used(a0)
	clr.l Owner.Count(a0)
	clr.l Owner.DeclarationActivity(a0)
	moveq #OK, d0
	rts
	.bend  ; release

; A0=Owner,A1=external scalar pairs,D0=count,D1=source bytes.
; D0/CCR=status,D2=offset on success; others preserved. Source must not alias
; Arena. One reserve precedes all writes; empty lists do not read the source.
appendList	.block
	movem.l d1/d3-d7/a0-a3, -(sp)
	move.l d0, d4
	cmpi.l #MAX_ELEMENTS, d4
	bhi.w noSpace
	lsl.l #3, d0
	cmp.l d1, d0
	bhi.w invalid
	move.l d0, d5
	movea.l a0, a2
	movea.l a1, a3
	lea Owner.Arena(a2), a1
	bsr.w block
	bne.w done
	move.l memory.Block.Used(a1), d2
	move.l d2, d6
	andi.l #7, d6
	bne.w invalid
	move.l d2, d6
	add.l d5, d6
	bcs.w noSpace
	addi.l #HEADER_BYTES, d6
	bcs.w noSpace
	cmpi.l #memory.LIMIT, d6
	bhi.w noSpace
	tst.l d5
	beq.w reserve
	move.l a3, d0
	beq.w invalid
	btst #0, d0
	bne.w invalid
	add.l d5, d0
	bcs.w invalid
reserve
	movea.l a1, a0
	move.l d6, d0
	jsr memory.reserve
	tst.l d0
	bne.w noSpace
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	move.l #LIST, (a1)+
	move.l d4, (a1)+
	tst.l d4
	beq.w copied
copy
	move.l (a3)+, (a1)+
	move.l (a3)+, (a1)+
	subq.l #1, d4
	bne.w copy
copied
	move.l d6, memory.Block.Used(a0)
	moveq #OK, d0
	bra.w done
invalid
	moveq #MALFORMED, d0
	bra.w done
noSpace
	moveq #STORAGE, d0
done
	movem.l (sp)+, d1/d3-d7/a0-a3
	tst.l d0
	rts
	.bend  ; appendList

; A0=Owner,A1=external start/end/optional step pairs,D0=canonical flags
; (bit0 has_step,bit1 inclusive),D1=source bytes (at least16/24).
; D0/CCR=status,D2=offset on success; others preserved. Source must not alias
; Arena. Normalization precedes reserve; a range always occupies32bytes.
appendRange	.block
	movem.l d1/d3-d6/a0-a3, -(sp)
	suba.l #ranges.BYTES, sp
	movea.l a0, a3
	movea.l sp, a2
	jsr ranges.normalize
	bne.w done
	lea Owner.Arena(a3), a1
	bsr.w block
	bne.w done
	move.l memory.Block.Used(a1), d2
	move.l d2, d3
	andi.l #7, d3
	bne.w invalid
	move.l d2, d6
	addi.l #ranges.BYTES, d6
	bcs.w noSpace
	cmpi.l #memory.LIMIT, d6
	bhi.w noSpace
	movea.l a1, a0
	move.l d6, d0
	jsr memory.reserve
	tst.l d0
	bne.w noSpace
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	moveq #ranges.BYTES/4-1, d4
copy
	move.l (a2)+, (a1)+
	dbf d4, copy
	move.l d6, memory.Block.Used(a0)
	moveq #OK, d0
	bra.w done
invalid
	moveq #MALFORMED, d0
	bra.w done
noSpace
	moveq #STORAGE, d0
done
	adda.l #ranges.BYTES, sp
	movem.l (sp)+, d1/d3-d6/a0-a3
	tst.l d0
	rts
	.bend  ; appendRange

; Copy a complete bounded external descriptor into an immutable owned record.
; A0=destination Owner,A1=source arena,D0=arena bytes,D1=offset,D3=kind.
; D0/CCR=status,D2=destination offset on success; others preserved.
; Rejects source overlap with Arena capacity; validates before allocation.
copyRecord	.block
	movem.l d1/d3-d7/a0-a3, -(sp)
	move.l d3, d7
	beq.w invalid
	cmpi.l #LIST, d7
	beq.w kindReady
	cmpi.l #STRUCT_LITERAL, d7
	bhi.w invalid
kindReady
	move.l d0, d4
	cmpi.l #memory.LIMIT, d4
	bhi.w invalid
	move.l a1, d5
	beq.w invalid
	btst #0, d5
	bne.w invalid
	add.l d4, d5
	bcs.w invalid
	move.l d1, d6
	andi.l #7, d6
	bne.w invalid
	cmp.l d4, d1
	bhi.w invalid
	sub.l d1, d4
	cmpi.l #HEADER_BYTES, d4
	blo.w invalid
	movea.l a1, a2
	lea Owner.Arena(a0), a1
	bsr.w block
	bne.w done
	move.l memory.Block.Pointer(a1), d6
	beq.w separate
	cmp.l d6, d5
	bls.w separate
	add.l memory.Block.Capacity(a1), d6
	move.l a2, d0
	cmp.l d6, d0
	blo.w invalid
separate
	movea.l a2, a1
	adda.l d1, a1
	cmp.l Record.Kind(a1), d7
	bne.w invalid
	cmpi.l #TYPED_LIST, d7
	bhs.w typedRecord
	cmpi.l #RANGE, d7
	beq.w rangeRecord
	subi.l #HEADER_BYTES, d4
	move.l Record.Count(a1), d0
	cmpi.l #MAX_ELEMENTS, d0
	bhi.w invalid
	move.l d0, d6
	lsl.l #3, d6
	cmp.l d4, d6
	bhi.w invalid
	lea HEADER_BYTES(a1), a1
	move.l d4, d1
	bsr.w appendList
	bra.w done
typedRecord
	bsr.w descriptorBytes
	bne.w done
	cmp.l d4, d2
	bhi.w invalid
	cmpi.l #STRUCT_TYPE, d7
	blo.w typedShapeReady
	bsr.w validateStruct
	bne.w done
typedShapeReady
	move.l d2, d6
	movea.l a1, a2
	move.l Record.Count(a1), d4
	cmpi.l #STRUCT_TYPE, d7
	beq.w typedAllocate
	lea HEADER_BYTES(a1), a3
	cmpi.l #STRUCT_LITERAL, d7
	bne.w addressDefault
	lea STRUCT_BASE_BYTES(a1), a3
	bra.w typedCells
addressDefault
	cmpi.l #ADDRESS_RECORD, d7
	bne.w typedCells
	tst.l Cell.Kind(a3)
	bne.w invalid
	cmpi.l #-1, Cell.Provenance(a3)
	beq.w defaultReady
	cmpi.l #MAX_SECTION_ID, Cell.Provenance(a3)
	bhi.w invalid
defaultReady
	adda.l #CELL_BYTES, a3
typedCells
	tst.l d4
	beq.w typedAllocate
	cmpi.l #TYPED_LIST, d7
	beq.w importCell
	addq.l #4, a3
importCell
	tst.l Cell.Kind(a3)
	bne.w invalid
	cmpi.l #-1, Cell.Provenance(a3)
	beq.w importReady
	cmpi.l #MAX_SECTION_ID, Cell.Provenance(a3)
	bhi.w invalid
importReady
	adda.l #CELL_BYTES, a3
	subq.l #1, d4
	bra.w typedCells
typedAllocate
	lea Owner.Arena(a0), a0
	move.l memory.Block.Used(a0), d2
	move.l d2, d0
	andi.l #7, d0
	bne.w invalid
	move.l d2, d5
	add.l d6, d5
	bcs.w invalid
	cmpi.l #memory.LIMIT, d5
	bhi.w invalid
	move.l d5, d0
	jsr memory.reserve
	tst.l d0
	bne.w noSpace
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	lsr.l #2, d6
rawCopy
	move.l (a2)+, (a1)+
	subq.l #1, d6
	bne.w rawCopy
	move.l d5, memory.Block.Used(a0)
	moveq #OK, d0
	bra.w done
noSpace
	moveq #STORAGE, d0
	bra.w done
rangeRecord
	cmpi.l #ranges.BYTES, d4
	blo.w invalid
	jsr ranges.validate
	bne.w done
	lea HEADER_BYTES(a1), a1
	moveq #ranges.HAS_STEP, d0
	moveq #ranges.BYTES-HEADER_BYTES, d1
	bsr.w appendRange
	bra.w done
invalid
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1/d3-d7/a0-a3
	tst.l d0
	rts
	.bend  ; copyRecord

; A0=Owner,D1=offset. D0/CCR=status,D1/D2=length low/high on success.
; Others preserved. Compound scalar lengths saturate at i64::MAX.
compoundLength	.block
	movem.l d3/a1, -(sp)
	bsr.w view
	bne.w done
	cmpi.l #RANGE, Record.Kind(a1)
	beq.w rangeValue
	cmpi.l #ADDRESS_RECORD, Record.Kind(a1)
	beq.w invalid
	cmpi.l #STRUCT_TYPE, Record.Kind(a1)
	bhs.w invalid
	move.l Record.Count(a1), d1
	moveq #0, d2
	bra.w done
invalid
	moveq #MALFORMED, d0
	bra.w done
rangeValue
	jsr ranges.length
done
	movem.l (sp)+, d3/a1
	tst.l d0
	rts
	.bend  ; compoundLength

; A0=Owner,D1=offset,D2/D3=index low/high.
; D0/CCR=status,D1/D2=scalar low/high on success; others preserved.
compoundGet	.block
	movem.l d3-d4/a1, -(sp)
	move.l d3, d4
	bsr.w view
	bne.w done
	cmpi.l #RANGE, Record.Kind(a1)
	beq.w rangeValue
	cmpi.l #ADDRESS_RECORD, Record.Kind(a1)
	beq.w invalid
	tst.l d4
	bne.w outOfBounds
	cmp.l Record.Count(a1), d2
	bhs.w outOfBounds
	cmpi.l #TYPED_LIST, Record.Kind(a1)
	beq.w typedValue
	lsl.l #3, d2
	adda.l d2, a1
	move.l HEADER_BYTES(a1), d1
	move.l HEADER_BYTES+4(a1), d2
	bra.w done
typedValue
	lsl.l #4, d2
	adda.l d2, a1
	tst.l HEADER_BYTES+Cell.Kind(a1)
	bne.w invalid
	tst.l HEADER_BYTES+Cell.Provenance(a1)
	bne.w invalid
	move.l HEADER_BYTES+Cell.Low(a1), d1
	move.l HEADER_BYTES+Cell.High(a1), d2
	bra.w done
invalid
	moveq #MALFORMED, d0
	bra.w done
rangeValue
	move.l d4, d3
	jsr ranges.get
	bra.w done
outOfBounds
	moveq #BOUNDS, d0
done
	movem.l (sp)+, d3-d4/a1
	tst.l d0
	rts
	.bend  ; compoundGet

; A0=Owner,D1=left offset,D2=right offset. D0/CCR=OK iff complete compound
; values have the same kind and contents; MALFORMED covers invalid/unequal values.
; Others preserved; no allocation. Child descriptors compare by value, not offset.
; Depth32 and a shared comparison budget bound cyclic/malicious graphs. Each
; nested descriptor/cell pair saves56bytes plus8 return bytes: at most2048bytes,
; plus public/leaf helper saves (under2200bytes total), without Owner.Work/VM.
equalValues	.block
	movem.l d1-d7/a1-a3, -(sp)
	moveq #EQUALITY_DEPTH_LIMIT, d6
	move.l #EQUALITY_WORK_LIMIT, d7
	bsr.w equalRecord
	movem.l (sp)+, d1-d7/a1-a3
	tst.l d0
	rts
	.bend  ; equalValues
	.priv
; A0=Owner,D1/D2=offsets,D6=remaining depth,D7=shared remaining work.
; D0/CCR=status,D7=remaining work; other registers preserved.
equalRecord	.block
	movem.l d1-d6/a1-a3, -(sp)
	tst.l d6
	beq.w bad
	tst.l d7
	beq.w bad
	subq.l #1, d6
	subq.l #1, d7
	bsr.w view
	bne.w done
	movea.l a1, a2
	move.l d2, d1
	bsr.w view
	bne.w done
	move.l Record.Kind(a1), d5
	cmp.l Record.Kind(a2), d5
	bne.w bad
	cmpi.l #RANGE, d5
	beq.w rangeValue
	move.l Record.Count(a1), d4
	cmp.l Record.Count(a2), d4
	bne.w bad
	cmpi.l #STRUCT_TYPE, d5
	beq.w structType
	cmpi.l #STRUCT_LITERAL, d5
	beq.w structLiteral
	lea HEADER_BYTES(a1), a1
	lea HEADER_BYTES(a2), a2
	cmpi.l #TYPED_LIST, d5
	beq.w cells
	cmpi.l #ADDRESS_RECORD, d5
	beq.w addressRecord
	; Legacy lists still compare their scalar64 pairs in stored order.
	add.l d4, d4
	bra.w words
structType
	lea StructRecord.Nominal(a1), a1
	lea StructRecord.Nominal(a2), a2
	mulu.l #STRUCT_FIELD_BYTES/4, d4
	addq.l #2, d4
	bra.w words
structLiteral
	move.l StructRecord.Nominal(a1), d3
	cmp.l StructRecord.Nominal(a2), d3
	bne.w bad
	lea STRUCT_BASE_BYTES(a1), a1
	lea STRUCT_BASE_BYTES(a2), a2
	movea.l a2, a3
structFields
	tst.l d4
	beq.w good
	movea.l a3, a2
	move.l Field.Member(a1), d3
	move.l Record.Count-STRUCT_BASE_BYTES(a3), d5
findStructField
	tst.l d5
	beq.w bad
	tst.l d7
	beq.w bad
	subq.l #1, d7
	cmp.l Field.Member(a2), d3
	beq.w structFieldFound
	adda.l #FIELD_BYTES, a2
	subq.l #1, d5
	bra.w findStructField
structFieldFound
	lea Field.Value(a1), a1
	lea Field.Value(a2), a2
	bsr.w equalCell
	bne.w done
	adda.l #CELL_BYTES, a1
	subq.l #1, d4
	bra.w structFields
rangeValue
	lea HEADER_BYTES(a1), a1
	lea HEADER_BYTES(a2), a2
	moveq #6, d4
words
	tst.l d4
	beq.w good
	tst.l d7
	beq.w bad
	subq.l #1, d7
	move.l (a1)+, d3
	cmp.l (a2)+, d3
	bne.w bad
	subq.l #1, d4
	bra.w words
addressRecord
	bsr.w equalCell
	bne.w done
	adda.l #CELL_BYTES, a1
	adda.l #CELL_BYTES, a2
fields
	tst.l d4
	beq.w good
	move.l Field.Member(a1), d3
	cmp.l Field.Member(a2), d3
	bne.w bad
	lea Field.Value(a1), a1
	lea Field.Value(a2), a2
	bsr.w equalCell
	bne.w done
	adda.l #CELL_BYTES, a1
	adda.l #CELL_BYTES, a2
	subq.l #1, d4
	bra.w fields
cells
	tst.l d4
	beq.w good
	bsr.w equalCell
	bne.w done
	adda.l #CELL_BYTES, a1
	adda.l #CELL_BYTES, a2
	subq.l #1, d4
	bra.w cells
good
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d6/a1-a3
	tst.l d0
	rts
	.bend  ; equalRecord
; A0=Owner,A1/A2=bounded Cell16,D6=remaining depth,D7=remaining work.
; D0/CCR=status,D7=remaining work; other registers preserved. Compound cells
; require descriptor offsets with zero high/provenance; scalars retain signed64.
equalCell	.block
	movem.l d1-d3/a1-a2, -(sp)
	tst.l d7
	beq.w bad
	subq.l #1, d7
	move.l Cell.Kind(a1), d3
	cmp.l Cell.Kind(a2), d3
	bne.w bad
	cmpi.l #STRUCT_LITERAL, d3
	bhi.w bad
	move.l Cell.Provenance(a1), d0
	cmp.l Cell.Provenance(a2), d0
	bne.w bad
	tst.l d3
	bne.w compound
	cmpi.l #-1, d0
	beq.w scalar
	cmpi.l #MAX_SECTION_ID, d0
	bhi.w bad
scalar
	move.l Cell.Low(a1), d1
	cmp.l Cell.Low(a2), d1
	bne.w bad
	move.l Cell.High(a1), d2
	cmp.l Cell.High(a2), d2
	bne.w bad
	moveq #OK, d0
	bra.w done
compound
	tst.l d0
	bne.w bad
	tst.l Cell.High(a1)
	bne.w bad
	tst.l Cell.High(a2)
	bne.w bad
	move.l Cell.Low(a1), d1
	move.l Cell.Low(a2), d2
	; Match each cell's declared kind to its actual bounded descriptor.
	movem.l d1/d3, -(sp)
	bsr.w view
	movem.l (sp)+, d1/d3
	tst.l d0
	bne.w done
	cmp.l Record.Kind(a1), d3
	bne.w bad
	movem.l d1/d3, -(sp)
	move.l d2, d1
	bsr.w view
	movem.l (sp)+, d1/d3
	tst.l d0
	bne.w done
	cmp.l Record.Kind(a1), d3
	bne.w bad
	bsr.w equalRecord
	bra.w done
bad
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d3/a1-a2
	tst.l d0
	rts
	.bend  ; equalCell
	.pub

; A0=Owner,D1=symbol ID,D2=kind. D0/CCR=status; others preserved.
; Scalar writes avoid allocation; compound cells contain arena offsets.
setKind	.block
	movem.l d1-d3/a0-a2, -(sp)
	cmp.l Owner.Count(a0), d1
	bhs.w outOfBounds
	cmpi.l #STRUCT_LITERAL, d2
	bhi.w invalid
	movea.l a0, a2
	lea Owner.Kinds(a0), a1
	bsr.w kinds
	bne.w done
	tst.l memory.Block.Pointer(a1)
	bne.w store
	tst.l d2
	beq.w good
	movea.l a1, a0
	move.l Owner.Count(a2), d0
	jsr memory.reserve
	tst.l d0
	bne.w noSpace
	move.l Owner.Count(a2), memory.Block.Used(a0)
store
	movea.l memory.Block.Pointer(a1), a0
	move.b d2, 0(a0, d1.l)
good
	moveq #OK, d0
	bra.w done
outOfBounds
	moveq #BOUNDS, d0
	bra.w done
invalid
	moveq #MALFORMED, d0
	bra.w done
noSpace
	moveq #STORAGE, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; setKind

; A0=Owner,D1=symbol ID. D0/CCR=status,D2=kind on success; others preserved.
getKind	.block
	movem.l d3/a1, -(sp)
	cmp.l Owner.Count(a0), d1
	bhs.w outOfBounds
	lea Owner.Kinds(a0), a1
	bsr.w kinds
	bne.w done
	moveq #SCALAR, d2
	tst.l memory.Block.Pointer(a1)
	beq.w good
	movea.l memory.Block.Pointer(a1), a1
	move.b 0(a1, d1.l), d2
	cmpi.l #STRUCT_LITERAL, d2
	bhi.w invalid
good
	moveq #OK, d0
	bra.w done
outOfBounds
	moveq #BOUNDS, d0
	bra.w done
invalid
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d3/a1
	tst.l d0
	rts
	.bend  ; getKind
	.priv

; A1=Block. D0/CCR=status; clobbers D3. Reject inconsistent outOfBounds/pointers.
	.pub
block	.block
	move.l memory.Block.Capacity(a1), d3
	cmpi.l #memory.LIMIT, d3
	bhi.w bad
	cmp.l memory.Block.Used(a1), d3
	blo.w bad
	move.l memory.Block.Pointer(a1), d0
	beq.w empty
	btst #0, d0
	bne.w bad
	tst.l d3
	beq.w bad
	add.l d3, d0
	bcs.w bad
	moveq #OK, d0
	rts
empty
	tst.l d3
	bne.w bad
	moveq #OK, d0
	rts
bad
	moveq #MALFORMED, d0
	rts
	.bend  ; block

	.priv
; A0=Owner,A1=Kinds. D0/CCR=status; clobbers D3.
kinds	.block
	bsr.w block
	bne.w done
	cmpi.l #memory.LIMIT, Owner.Count(a0)
	bhi.w bad
	tst.l memory.Block.Pointer(a1)
	beq.w done
	move.l Owner.Count(a0), d3
	cmp.l memory.Block.Used(a1), d3
	bhi.w bad
	bra.w done
bad
	moveq #MALFORMED, d0
done
	tst.l d0
	rts
	.bend  ; kinds

	.pub
; A0=Owner,D1=offset. D0/CCR=status,A1=ephemeral descriptor; clobbers D3.
; Validates a complete LIST/RANGE record. No serialized pointers.
view	.block
	lea Owner.Arena(a0), a1
	bsr.w block
	bne.w done
	move.l d1, d3
	andi.l #7, d3
	bne.w bad
	move.l memory.Block.Used(a1), d3
	cmpi.l #HEADER_BYTES, d3
	blo.w bad
	subi.l #HEADER_BYTES, d3
	cmp.l d3, d1
	bhi.w bad
	movea.l memory.Block.Pointer(a1), a1
	adda.l d1, a1
	cmpi.l #RANGE, Record.Kind(a1)
	beq.w rangeRecord
	cmpi.l #TYPED_LIST, Record.Kind(a1)
	beq.w typedRecord
	cmpi.l #ADDRESS_RECORD, Record.Kind(a1)
	beq.w typedRecord
	cmpi.l #STRUCT_TYPE, Record.Kind(a1)
	beq.w typedRecord
	cmpi.l #STRUCT_LITERAL, Record.Kind(a1)
	beq.w typedRecord
	cmpi.l #LIST, Record.Kind(a1)
	bne.w bad
	move.l Record.Count(a1), d3
	cmpi.l #MAX_ELEMENTS, d3
	bhi.w bad
	lsl.l #3, d3
	add.l d1, d3
	addi.l #HEADER_BYTES, d3
	cmp.l Owner.Arena+memory.Block.Used(a0), d3
	bhi.w bad
	moveq #OK, d0
	bra.w done
typedRecord
	move.l d2, -(sp)
	bsr.w descriptorBytes
	move.l d2, d3
	move.l (sp)+, d2
	tst.l d0
	bne.w done
	add.l d1, d3
	bcs.w bad
	cmp.l Owner.Arena+memory.Block.Used(a0), d3
	bhi.w bad
	cmpi.l #STRUCT_TYPE, Record.Kind(a1)
	blo.w typedReady
	bsr.w validateStruct
	bra.w done
typedReady
	moveq #OK, d0
	bra.w done
rangeRecord
	move.l d1, d3
	addi.l #ranges.BYTES, d3
	bcs.w bad
	cmp.l Owner.Arena+memory.Block.Used(a0), d3
	bhi.w bad
	jsr ranges.validate
	bra.w done
bad
	moveq #MALFORMED, d0
done
	tst.l d0
	rts
	.bend  ; view

; A1=header already bounded to8bytes. D0/CCR=status,D2=aligned record bytes.
; Others preserved. Structural sizing only; callers validate extent and cells.
	.pub
; A1=readable Record header. D0/CCR=status,D2=complete aligned byte size.
; Other registers preserved. The caller checks the resulting source extent;
; this helper only decodes the current immutable descriptor layout.
descriptorBytes	.block
	move.l d3, -(sp)
	move.l Record.Count(a1), d2
	cmpi.l #LIST, Record.Kind(a1)
	beq.w scalarListSize
	cmpi.l #RANGE, Record.Kind(a1)
	beq.w rangeSize
	cmpi.l #TYPED_LIST, Record.Kind(a1)
	beq.w listSize
	cmpi.l #ADDRESS_RECORD, Record.Kind(a1)
	beq.w recordSize
	cmpi.l #STRUCT_TYPE, Record.Kind(a1)
	beq.w typeSize
	cmpi.l #STRUCT_LITERAL, Record.Kind(a1)
	bne.w bad
	cmpi.l #(memory.LIMIT-STRUCT_BASE_BYTES)/FIELD_BYTES, d2
	bhi.w bad
	mulu.w #FIELD_BYTES, d2
	addi.l #STRUCT_BASE_BYTES, d2
	bra.w aligned
typeSize
	cmpi.l #(memory.LIMIT-STRUCT_BASE_BYTES)/STRUCT_FIELD_BYTES, d2
	bhi.w bad
	mulu.l #STRUCT_FIELD_BYTES, d2
	addi.l #STRUCT_BASE_BYTES, d2
	bra.w aligned
recordSize
	cmpi.l #(memory.LIMIT-RECORD_BASE_BYTES)/FIELD_BYTES, d2
	bhi.w bad
	mulu.w #FIELD_BYTES, d2
	addi.l #RECORD_BASE_BYTES, d2
aligned
	addq.l #7, d2
	andi.l #$fffffff8, d2
	bra.w good
scalarListSize
	cmpi.l #MAX_ELEMENTS, d2
	bhi.w bad
	lsl.l #3, d2
	addi.l #HEADER_BYTES, d2
	bra.w good
rangeSize
	moveq #ranges.BYTES, d2
	bra.w good
listSize
	cmpi.l #(memory.LIMIT-HEADER_BYTES)/CELL_BYTES, d2
	bhi.w bad
	lsl.l #4, d2
	addi.l #HEADER_BYTES, d2
good
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
done
	move.l (sp)+, d3
	tst.l d0
	rts
	.bend  ; descriptorBytes
	.priv

	.priv
; A1=complete bounded type/literal. Validate nominal keys, geometry and cells.
; D0/CCR=status; other registers preserved. No allocation or child recursion.
validateStruct	.block
	movem.l d1-d7/a1-a3, -(sp)
	cmpi.l #65535, StructRecord.Nominal(a1)
	bhi.w bad
	move.l Record.Kind(a1), d7
	move.l Record.Count(a1), d6
	move.l StructRecord.Size(a1), d5
	moveq #STRUCT_FIELD_BYTES, d4
	cmpi.l #STRUCT_TYPE, d7
	beq.w fields
	cmpi.l #STRUCT_LITERAL, d7
	bne.w bad
	tst.l d5
	bne.w bad
	moveq #FIELD_BYTES, d4
fields
	lea STRUCT_BASE_BYTES(a1), a1
	movea.l a1, a3
field
	tst.l d6
	beq.w good
	move.l (a1), d3
	cmpi.l #65535, d3
	bhi.w bad
	movea.l a3, a2
unique
	cmpa.l a1, a2
	beq.w value
	cmp.l (a2), d3
	beq.w bad
	adda.l d4, a2
	bra.w unique
value
	cmpi.l #STRUCT_TYPE, d7
	bne.w scalar
	move.l StructField.Offset(a1), d3
	add.l StructField.Size(a1), d3
	bcs.w bad
	cmp.l d5, d3
	bhi.w bad
	bra.w next
scalar
	tst.l Field.Value+Cell.Kind(a1)
	bne.w bad
	move.l Field.Value+Cell.Provenance(a1), d3
	cmpi.l #-1, d3
	beq.w next
	cmpi.l #MAX_SECTION_ID, d3
	bhi.w bad
next
	adda.l d4, a1
	subq.l #1, d6
	bra.w field
good
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a1-a3
	tst.l d0
	rts
	.bend  ; validateStruct

	.endsection
	.endmodule
