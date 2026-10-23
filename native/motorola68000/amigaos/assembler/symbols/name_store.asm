; Paged immutable name bytes. Handles encode page index and byte offset.
; @opforge-owner: opforge.assembler.symbols.name_store
	.module opforge.assembler.symbols.name_store
	.cpu 68020
	.use opforge.platform.memory as memory
	.pub
PAGE_BYTES = 65536
MAX_NAME_BYTES = 255
MAX_PAYLOAD_BYTES = 65535*MAX_NAME_BYTES
State	.struct
First	.res memory.Block.Used+4
Pages	.res memory.Block.Used+4
Last	.long ?
End	.long ?
Bytes	.long ?
.endstruct
STATE_BYTES = State.Bytes+4
	.priv
BLOCK_BYTES = memory.Block.Used+4
	.section code, kind=code
	.pub
; A0=zero-initialized State,A1=bytes,D0=length (1..255),D1=payload limit.
; D0/CCR=status (0 success,1 failure),D1=handle on success; others preserved.
; Copy aliases before appending: growth of First can move its allocation.
; Later pages never move. Failure preserves published extents/content; directory
; capacity may grow. Bytes counts payload, End includes unused page tails.
append	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	move.l d0, d7
	cmpi.l #1, d7
	blo.w bad
	cmpi.l #MAX_NAME_BYTES, d7
	bhi.w bad
	cmpi.l #MAX_PAYLOAD_BYTES, d1
	bhi.w bad
	move.l State.Bytes(a5), d6
	add.l d7, d6
	bcs.w bad
	cmp.l d1, d6
	bhi.w bad
	lea State.First(a5), a4
	move.l State.Last(a5), d5
	beq.w currentPage
	move.l d5, d0
	subq.l #1, d0
	mulu.w #BLOCK_BYTES, d0
	movea.l State.Pages+memory.Block.Pointer(a5), a4
	adda.l d0, a4
currentPage
	move.l memory.Block.Used(a4), d4
	move.l d4, d0
	add.l d7, d0
	cmpi.l #PAGE_BYTES, d0
	bhi.w newPage
	tst.l d5
	bne.w copyName
	; First grows geometrically, with the caller's smaller budget as its cap.
	cmpi.l #PAGE_BYTES, d1
	bls.w firstLimit
	move.l #PAGE_BYTES, d1
firstLimit
	movea.l a4, a0
	jsr memory.reserveBounded
	bne.w bad
	bra.w copyName
newPage
	; Reserve the directory before allocating an unpublished, empty row.
	lea State.Pages(a5), a0
	move.l memory.Block.Used(a0), d0
	add.l #BLOCK_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l State.Pages+memory.Block.Pointer(a5), a4
	adda.l State.Pages+memory.Block.Used(a5), a4
	clr.l memory.Block.Pointer(a4)
	clr.l memory.Block.Capacity(a4)
	clr.l memory.Block.Used(a4)
	movea.l a4, a0
	move.l #PAGE_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	; No fallible operation remains; the allocated row is now owned.
	addi.l #BLOCK_BYTES, State.Pages+memory.Block.Used(a5)
	addq.l #1, d5
	moveq #0, d4
copyName
	move.l d5, d1
	swap d1
	or.l d4, d1
	movea.l memory.Block.Pointer(a4), a1
	adda.l d4, a1
	movea.l a6, a0
	move.l d7, d0
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
	add.l d7, d4
	move.l d4, memory.Block.Used(a4)
	move.l d5, State.Last(a5)
	move.l d1, d0
	add.l d7, d0
	move.l d0, State.End(a5)
	move.l d6, State.Bytes(a5)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; append

; A0=State,D0=trusted handle -> A0=bytes. All other registers preserved.
; Unchecked O(1) internal lookup; CCR unspecified.
pointer	.block
	move.l d1, -(sp)
	move.l d0, d1
	swap d1
	andi.l #$ffff, d1
	beq.w first
	subq.l #1, d1
	mulu.w #BLOCK_BYTES, d1
	movea.l State.Pages+memory.Block.Pointer(a0), a0
	movea.l memory.Block.Pointer(a0, d1.l), a0
	bra.w offset
first
	movea.l State.First+memory.Block.Pointer(a0), a0
offset
	move.l d0, d1
	andi.l #$ffff, d1
	adda.l d1, a0
	move.l (sp)+, d1
	rts
	.bend  ; pointer

; A0=State,D0=handle,D1=positive length <=255 -> D0/CCR=status,A1=bytes/0.
; Other registers preserved. Check Used, including holes at sealed page tails.
resolve	.block
	movem.l d2-d3/a2, -(sp)
	suba.l a1, a1
	cmpi.l #1, d1
	blo.w bad
	cmpi.l #MAX_NAME_BYTES, d1
	bhi.w bad
	move.l d0, d2
	swap d2
	andi.l #$ffff, d2
	cmp.l State.Last(a0), d2
	bhi.w bad
	lea State.First(a0), a2
	tst.l d2
	beq.w page
	subq.l #1, d2
	mulu.w #BLOCK_BYTES, d2
	movea.l State.Pages+memory.Block.Pointer(a0), a2
	adda.l d2, a2
page
	move.l d0, d2
	andi.l #$ffff, d2
	move.l d2, d3
	add.l d1, d3
	cmp.l memory.Block.Used(a2), d3
	bhi.w bad
	movea.l memory.Block.Pointer(a2), a1
	adda.l d2, a1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d3/a2
	tst.l d0
	rts
	.bend  ; resolve

; A0=State. Free every page and directory, then zero State; all registers kept.
; First may have been repurposed as an ordinary Block after name retirement.
; CCR unspecified. Repeated release is safe.
release	.block
	movem.l d0-d1/a0-a2, -(sp)
	movea.l a0, a2
	lea State.First(a2), a0
	jsr memory.release
	movea.l State.Pages+memory.Block.Pointer(a2), a1
	move.l State.Pages+memory.Block.Used(a2), d1
	beq.w directory
pages
	movea.l a1, a0
	jsr memory.release
	adda.w #BLOCK_BYTES, a1
	sub.l #BLOCK_BYTES, d1
	bne.w pages
directory
	lea State.Pages(a2), a0
	jsr memory.release
	moveq #STATE_BYTES/4-1, d0
clear
	clr.l (a2)+
	dbra d0, clear
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; release
	.endsection
	.endmodule
