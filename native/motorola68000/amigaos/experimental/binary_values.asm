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
.endstruct
OWNER_BYTES = 40
SCALAR = 0
LIST = 1
RANGE = 2
OK = 0
MALFORMED = 1
STORAGE = 2
BOUNDS = 3
Record	.struct
Kind	.long ?
Count	.long ?
.endstruct
HEADER_BYTES = 8
ELEMENT_BYTES = 8
MAX_ELEMENTS = (memory.LIMIT-HEADER_BYTES)/ELEMENT_BYTES
	.section code, kind=code

; A0=zeroed/released Owner,D0=symbol count. D0/CCR=status; others preserved.
; Initializes only Count; caller must release an existing owner first.
init	.block
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	move.l d0, Owner.Count(a0)
	moveq #OK, d0
	rts
bad
	moveq #STORAGE, d0
	rts
	.bend  ; init

; A0=Owner. D0/CCR=status; others preserved. Retains allocations, clears kinds.
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
	movea.l (sp)+, a0
	clr.l Owner.Arena+memory.Block.Used(a0)
	clr.l Owner.Kinds+memory.Block.Used(a0)
	clr.l Owner.Work+memory.Block.Used(a0)
	clr.l Owner.Count(a0)
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

; A0=Owner,D1=offset. D0/CCR=status,D1/D2=length low/high on success.
; Others preserved. Compound scalar lengths saturate at i64::MAX.
compoundLength	.block
	movem.l d3/a1, -(sp)
	bsr.w view
	bne.w done
	cmpi.l #RANGE, Record.Kind(a1)
	beq.w rangeValue
	move.l Record.Count(a1), d1
	moveq #0, d2
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
	tst.l d4
	bne.w outOfBounds
	cmp.l Record.Count(a1), d2
	bhs.w outOfBounds
	lsl.l #3, d2
	adda.l d2, a1
	move.l HEADER_BYTES(a1), d1
	move.l HEADER_BYTES+4(a1), d2
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
; values have the same kind and contents. Ranges compare canonical descriptors,
; matching AsmValue equality; a LIST and RANGE never compare equal.
; MALFORMED covers invalid/unequal values. Others preserved; no allocation.
equalValues	.block
	movem.l d1-d4/a1-a2, -(sp)
	bsr.w view
	bne.w done
	movea.l a1, a2
	move.l d2, d1
	bsr.w view
	bne.w done
	move.l Record.Kind(a1), d3
	cmp.l Record.Kind(a2), d3
	bne.w bad
	cmpi.l #RANGE, d3
	beq.w rangeValue
	move.l Record.Count(a1), d4
	cmp.l Record.Count(a2), d4
	bne.w bad
	add.l d4, d4
	bra.w contents
rangeValue
	moveq #6, d4
contents
	lea HEADER_BYTES(a1), a1
	lea HEADER_BYTES(a2), a2
	tst.l d4
	beq.w good
compare
	move.l (a1)+, d3
	cmp.l (a2)+, d3
	bne.w bad
	subq.l #1, d4
	bne.w compare
good
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d4/a1-a2
	tst.l d0
	rts
	.bend  ; equalValues

; A0=Owner,D1=symbol ID,D2=kind. D0/CCR=status; others preserved.
; Scalar writes avoid allocation; compound cells contain arena offsets.
setKind	.block
	movem.l d1-d3/a0-a2, -(sp)
	cmp.l Owner.Count(a0), d1
	bhs.w outOfBounds
	cmpi.l #RANGE, d2
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
	cmpi.l #RANGE, d2
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

	.endsection
	.endmodule
