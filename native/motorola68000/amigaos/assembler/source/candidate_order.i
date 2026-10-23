; Canonicalize opaque discovery identities before declarations assign file IDs.
; Entry slot zero keeps its priority. Module execution still follows .use edges;
; explicit include/module root lists retain their original search order.
;
; A0=owned contiguous PATH_BYTES path slots,D0=count. Every path is NUL
; terminated within its slot. D0/CCR=0; all other registers preserved. Uses one
; PATH_BYTES stack row; no allocation, retained pointer or source interpretation.
orderCandidatePaths	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #PATH_BYTES, sp
	movea.l a0, a6
	move.l d0, d7
	moveq #2, d6
next
	cmp.l d7, d6
	bhs.w good
	move.l d6, d0
	mulu.l #PATH_BYTES, d0
	lea 0(a6, d0.l), a4
	movea.l a4, a0
	movea.l sp, a1
	move.w #PATH_BYTES/4-1, d0
saveRow
	move.l (a0)+, (a1)+
	dbra d0, saveRow
	move.l d6, d5
find
	cmpi.l #1, d5
	bls.w insert
	lea -PATH_BYTES(a4), a2
	movea.l sp, a1
	movea.l a2, a0
compare
	moveq #0, d0
	moveq #0, d1
	move.b (a1)+, d0
	move.b (a0)+, d1
	cmp.l d1, d0
	blo.w shift
	bhi.w insert
	tst.b d0
	bne.w compare
	bra.w insert
shift
	movea.l a2, a0
	movea.l a4, a1
	move.w #PATH_BYTES/4-1, d0
shiftRow
	move.l (a0)+, (a1)+
	dbra d0, shiftRow
	movea.l a2, a4
	subq.l #1, d5
	bra.w find
insert
	movea.l sp, a0
	movea.l a4, a1
	move.w #PATH_BYTES/4-1, d0
insertRow
	move.l (a0)+, (a1)+
	dbra d0, insertRow
	addq.l #1, d6
	bra.w next
good
	adda.w #PATH_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	moveq #0, d0
	rts
	.bend  ; orderCandidatePaths
