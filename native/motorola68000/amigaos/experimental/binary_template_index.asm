; Preparation-only index of numeric template names and leaf buckets.
; The chains retain definition order through index-plus-one cursors, never
; pointers into a growable allocation. Name and bucket policy belong to callers.
; @opforge-owner: experimental.amigaos.binary_template_index
	.module experimental.amigaos.binary_template_index
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.pub
State	.struct
Count	.word ?
Reserved	.word ?
	.endstruct
HEADS = State.Reserved+2
TAILS = HEADS+256*2
EXACT = TAILS+256*2
NODES = EXACT+256*2
SCRATCH_BYTES = NODES+memory.Block.Used+4
Node	.struct
Name	.word ?
Next	.word ?
ExactNext	.word ?
	.endstruct
NODE_BYTES = Node.ExactNext+2
	.section code, kind=code

; A0=zero-initialized state or a prior session. Release nodes and clear the
; count and all bucket heads/tails. D0/CCR=zero; other registers preserved.
begin	.block
	bsr.w finish
	movem.l d1/a0, -(sp)
	clr.w State.Count(a0)
	clr.w State.Reserved(a0)
	lea HEADS(a0), a0
	move.w #256*3-1, d1
clear
	clr.w (a0)+
	dbra d1, clear
	movem.l (sp)+, d1/a0
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. Release the node pool and reset its used extent.
; D0/CCR=zero; other registers preserved. Safe on zero-initialized state.
finish	.block
	move.l a0, -(sp)
	lea NODES(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l (sp)+, a0
	moveq #0, d0
	rts
	.bend  ; finish

; A0=state, D0=numeric name ID (word), D1=leaf bucket (0..255).
; Append one definition to its leaf bucket and prepend it to the exact-name
; hash chain. D0/CCR=status (zero on success); other registers preserved.
; On failure no count or chain changes; a successful reserve may grow the pool.
add	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	move.w d0, d4
	move.l d1, d7
	cmpi.l #255, d7
	bhi.w bad
	add.w d7, d7
	moveq #0, d3
	move.w State.Count(a4), d3
	cmpi.l #65535, d3
	beq.w bad
	move.l d3, d2
	mulu.w #NODE_BYTES, d2
	move.l d2, d0
	addi.l #NODE_BYTES, d0
	lea NODES(a4), a0
	jsr memory.reserve
	bne.w bad
	movea.l NODES+memory.Block.Pointer(a4), a6
	lea 0(a6, d2.l), a5
	move.w d4, Node.Name(a5)
	clr.w Node.Next(a5)
	moveq #0, d5
	move.b d4, d5
	add.w d5, d5
	lea EXACT(a4), a0
	move.w 0(a0, d5.w), Node.ExactNext(a5)
	addq.w #1, d3
	move.w d3, 0(a0, d5.w)
	moveq #0, d5
	lea TAILS(a4), a0
	move.w 0(a0, d7.w), d5
	beq.w firstLeaf
	subq.w #1, d5
	mulu.w #NODE_BYTES, d5
	move.w d3, Node.Next(a6, d5.l)
	bra.w publish
firstLeaf
	move.w d3, HEADS(a4, d7.w)
publish
	move.w d3, 0(a0, d7.w)
	move.w d3, State.Count(a4)
	move.l d2, d0
	addi.l #NODE_BYTES, d0
	move.l d0, NODES+memory.Block.Used(a4)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; add

; A0=state, D0=leaf bucket (0..255). D1=first cursor, or zero.
; All other registers preserved; CCR reflects D1.
first	.block
	moveq #0, d1
	move.b d0, d1
	add.w d1, d1
	move.w HEADS(a0, d1.w), d1
	tst.w d1
	rts
	.bend  ; first

; A0=state, D1=cursor from first/next (zero ends the chain).
; D1=next cursor, or zero. All other registers preserved; CCR reflects D1.
next	.block
	movem.l d0/a0, -(sp)
	tst.w d1
	beq.w endChain
	cmp.w State.Count(a0), d1
	bhi.w endChain
	moveq #0, d0
	move.w d1, d0
	subq.l #1, d0
	mulu.w #NODE_BYTES, d0
	movea.l NODES+memory.Block.Pointer(a0), a0
	moveq #0, d1
	move.w Node.Next(a0, d0.l), d1
	bra.w doneNext
endChain
	moveq #0, d1
doneNext
	movem.l (sp)+, d0/a0
	tst.w d1
	rts
	.bend  ; next

; A0=state, D0=numeric name ID (word). D1=matching definition cursor,
; or zero. ExactNext resolves hash collisions against the full numeric ID.
; All other registers preserved; CCR reflects D1.
find	.block
	movem.l d2/a0-a1, -(sp)
	moveq #0, d1
	move.b d0, d1
	add.w d1, d1
	lea EXACT(a0), a0
	move.w 0(a0, d1.w), d1
	movea.l NODES-EXACT+memory.Block.Pointer(a0), a1
scan
	tst.w d1
	beq.w doneFind
	moveq #0, d2
	move.w d1, d2
	subq.l #1, d2
	mulu.w #NODE_BYTES, d2
	cmp.w Node.Name(a1, d2.l), d0
	beq.w doneFind
	move.w Node.ExactNext(a1, d2.l), d1
	bra.w scan
doneFind
	movem.l (sp)+, d2/a0-a1
	tst.w d1
	rts
	.bend  ; find
	.endsection
	.endmodule
