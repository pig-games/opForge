; Immutable offset-addressed compound values; scalar cells stay caller-owned.
; @opforge-owner: experimental.amigaos.binary_values
	.module experimental.amigaos.binary_values
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
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

; A0=Owner,D1=offset. D0/CCR=status,D1=length,D2=0 on success.
; Others preserved. Entire descriptor is validated before reporting its count.
listLength	.block
	movem.l d3/a1, -(sp)
	bsr.w view
	bne.w done
	move.l Record.Count(a1), d1
	moveq #0, d2
done
	movem.l (sp)+, d3/a1
	tst.l d0
	rts
	.bend  ; listLength

; A0=Owner,D1=offset,D2=index low,D3=index high.
; D0/CCR=status,D1/D2=scalar low/high on success; others preserved.
listGet	.block
	movem.l d3-d4/a1, -(sp)
	move.l d3, d4
	bsr.w view
	bne.w done
	tst.l d4
	bne.w outOfBounds
	cmp.l Record.Count(a1), d2
	bhs.w outOfBounds
	lsl.l #3, d2
	adda.l d2, a1
	move.l HEADER_BYTES(a1), d1
	move.l HEADER_BYTES+4(a1), d2
	bra.w done
outOfBounds
	moveq #BOUNDS, d0
done
	movem.l (sp)+, d3-d4/a1
	tst.l d0
	rts
	.bend  ; listGet

; A0=Owner,D1=left offset,D2=right offset. D0/CCR=OK only when both
; complete lists have identical scalar pairs. MALFORMED covers invalid records
; and unequal values. Other registers preserved; no allocation or arena mutation.
equalLists	.block
	movem.l d1-d4/a1-a2, -(sp)
	bsr.w view
	bne.w done
	movea.l a1, a2
	move.l d2, d1
	bsr.w view
	bne.w done
	move.l Record.Count(a1), d4
	cmp.l Record.Count(a2), d4
	bne.w bad
	lea HEADER_BYTES(a1), a1
	lea HEADER_BYTES(a2), a2
	tst.l d4
	beq.w good
compare
	move.l (a1)+, d3
	cmp.l (a2)+, d3
	bne.w bad
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
	.bend  ; equalLists

; A0=Owner,D1=symbol ID,D2=kind. D0/CCR=status; others preserved.
; Scalar writes avoid allocation. RANGE is a reserved tag, with no descriptor API.
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
; Validates the complete LIST record. Never persists or returns serialized pointers.
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
bad
	moveq #MALFORMED, d0
done
	tst.l d0
	rts
	.bend  ; view
	.endsection
	.endmodule
