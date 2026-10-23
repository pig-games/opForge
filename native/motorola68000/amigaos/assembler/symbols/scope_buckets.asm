; Preparation-only ownership and stable growth of the source-name bucket index.
; @opforge-owner: opforge.assembler.symbols.scope_buckets
	.module opforge.assembler.symbols.scope_buckets
	.cpu 68020
	.use opforge.assembler.symbols.lexical_store as lexical
	.use opforge.assembler.symbols.name_store as nameStore
	.use opforge.assembler.symbols.scope_layout as layout
	.use opforge.assembler.symbols.binding_records as records
	.use opforge.platform.memory as memory
	.include "hotspot_telemetry.i"
	.pub
BLOCK_BYTES = memory.Block.Used+4
	.section code, kind=code

; A0=fresh/released scope state. D0/CCR=status; all other registers preserved.
; Allocation publishes the initial empty index only on success.
begin	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	tst.l layout.BUCKETS_POINTER(a2)
	bne.w bad
	lea layout.BUCKETS(a2), a0
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	clr.l layout.State.BucketCount(a2)
	clr.l layout.State.BucketMask(a2)
	clr.l layout.State.IndexedCount(a2)
	move.l #layout.BUCKET_INITIAL*2, d0
	jsr memory.reserveExact
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	move.w #layout.BUCKET_INITIAL-1, d1
clear
	clr.w (a1)+
	dbra d1, clear
	move.l #layout.BUCKET_INITIAL*2, memory.Block.Used(a0)
	move.l #layout.BUCKET_INITIAL, layout.State.BucketCount(a2)
	move.l #layout.BUCKET_INITIAL-1, layout.State.BucketMask(a2)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; begin

; A0=scope state. Safe on empty/partially initialized state; preserves registers; CCR unspecified.
release	.block
	movem.l a0, -(sp)
	clr.l layout.State.BucketCount(a0)
	clr.l layout.State.BucketMask(a0)
	clr.l layout.State.IndexedCount(a0)
	lea layout.BUCKETS(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, a0
	rts
	.bend  ; release

; A0=scope state, before inserting one absent source name. D0/CCR=status,
; D1=one if grown, otherwise zero; all other registers preserved.
; Keep load <=1 until the bounded maximum. Only old bucket membership is walked:
; unindexed iterator/import/binding proxies use Entry.Next for other chains.
; Allocation is the only fallible operation and precedes every link mutation.
; Each old chain is split stably into its two new buckets, preserving precedence.
ensure	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	moveq #0, d1
	move.l layout.State.BucketCount(a6), d6
	beq.w bad
	cmpi.l #layout.BUCKET_MAX, d6
	bhs.w good
	cmp.l layout.State.IndexedCount(a6), d6
	bhi.w good
	suba.w #BLOCK_BYTES, sp
	movea.l sp, a0
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	move.l d6, d0
	lsl.l #2, d0
	jsr memory.reserveExact
	bne.w allocationFailed
	move.l d6, d5
	add.l d5, d5
	subq.l #1, d5
	movea.l memory.Block.Pointer(sp), a1
	move.l d5, d0
clear
	clr.w (a1)+
	dbra d0, clear
	movea.l layout.BUCKETS_POINTER(a6), a5
	moveq #0, d7
bucket
	suba.l a4, a4
	suba.l a3, a3
	moveq #0, d4
	move.w 0(a5, d7.w), d4
chain
	tst.w d4
	beq.w nextBucket
	move.l d4, d0
	subq.l #1, d0
	movem.l a0-a1, -(sp)
	lea layout.LEXICAL_STATE(a6), a0
	lea LexicalEntry, a1
	jsr lexical.readEntry
	movem.l (sp)+, a0-a1
	bne.w storeFailed
	lea LexicalEntry, a2
	moveq #0, d3
	move.w records.Entry.Next(a2), d3
	lea layout.LEXICAL_STATE(a6), a0
	lea LexicalName, a1
	move.l records.Entry.Name(a2), d0
	moveq #0, d1
	move.w records.Entry.Length(a2), d1
	jsr lexical.copyName
	bne.w storeFailed
	moveq #0, d0
	move.w records.Entry.Length(a2), d0
	; Total bucket hashing includes these rehash spelling-byte reads.
	.HOTSPOT_COUNT HOTSPOT_LOOKUP_HASH_BYTES, d0
	moveq #0, d2
hash
	moveq #0, d1
	move.b (a1)+, d1
	cmpi.b #'A', d1
	blo.w folded
	cmpi.b #'Z', d1
	bhi.w folded
	addi.b #32, d1
folded
	movea.l d2, a0
	lsl.l #5, d2
	add.l a0, d2
	add.l d1, d2
	subq.l #1, d0
	bne.w hash
	and.l d5, d2
	add.l d2, d2
	clr.w records.Entry.Next(a2)
	move.l d4, d0
	subq.l #1, d0
	lea layout.LEXICAL_STATE(a6), a0
	lea LexicalEntry, a1
	jsr lexical.writeEntry
	bne.w storeFailed
	cmp.l d7, d2
	bne.w upper
	move.l a4, d0
	movea.l d4, a4
	bra.w linkTail
upper
	move.l a3, d0
	movea.l d4, a3
linkTail
	tst.l d0
	beq.w bucketHead
	subq.l #1, d0
	move.l d0, LinkIndex
	jsr lexical.readEntry
	bne.w storeFailed
	move.w d4, records.Entry.Next(a1)
	; The accessor returns status; retain the predecessor index for writeback.
	move.l LinkIndex, d0
	jsr lexical.writeEntry
	bne.w storeFailed
	bra.w linked
bucketHead
	movea.l memory.Block.Pointer(sp), a0
	move.w d4, 0(a0, d2.l)
linked
	move.l d3, d4
	bra.w chain
nextBucket
	addq.l #2, d7
	move.l d6, d0
	add.l d0, d0
	cmp.l d0, d7
	blo.w bucket
	; No borrowed table pointer survives this publication boundary.
	lea layout.BUCKETS(a6), a0
	jsr memory.release
	movea.l sp, a1
	move.l memory.Block.Pointer(a1), memory.Block.Pointer(a0)
	move.l memory.Block.Capacity(a1), memory.Block.Capacity(a0)
	move.l d6, d0
	lsl.l #2, d0
	move.l d0, memory.Block.Used(a0)
	add.l d6, d6
	move.l d6, layout.State.BucketCount(a6)
	move.l d5, layout.State.BucketMask(a6)
	adda.w #BLOCK_BYTES, sp
	moveq #1, d1
	bra.w good
storeFailed
	movea.l sp, a0
	jsr memory.release
allocationFailed
	adda.w #BLOCK_BYTES, sp
bad
	moveq #0, d1
	moveq #1, d0
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; ensure
	.endsection
	.section bss, kind=bss
; Accessor scratch is consumed before any callback; no Entry view escapes.
LexicalEntry	.res byte, records.ENTRY_BYTES
LexicalName	.res byte, records.NAME_BYTES
LinkIndex	.res long, 1
	.endsection
	.endmodule
