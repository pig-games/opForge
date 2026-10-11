; Sparse immutable numeric dotted-member plans; independent of typed value arena.
; @opforge-owner: experimental.amigaos.binary_numeric_bindings
	.module experimental.amigaos.binary_numeric_bindings
	.cpu 68020
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_binding_layout as layout
	.pub
HEADER_BYTES = layout.HEADER_BYTES
CANDIDATE_BYTES = layout.CANDIDATE_HEADER_BYTES
	.section code, kind=code
; A0=Owner,A1=external concatenated BE plans,D0=bytes. D0/CCR=status.
; Other registers preserved. Validate before reserve/copy; source cannot alias
; Bindings. Empty input clears usage. Pass reset/rewind retain installed plans.
install	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a4
	movea.l a1, a3
	move.l d0, d7
	cmpi.l #memory.LIMIT, d7
	bhi.w invalid
	lea values.Owner.Bindings(a0), a1
	jsr values.block
	bne.w done
	tst.l d7
	beq.w clear
	move.l a3, d5
	beq.w invalid
	btst #0, d5
	bne.w invalid
	move.l d5, d6
	add.l d7, d6
	bcs.w invalid
	move.l memory.Block.Pointer(a1), d0
	beq.w separate
	cmp.l d0, d6
	bls.w separate
	add.l memory.Block.Capacity(a1), d0
	cmp.l d0, d5
	blo.w invalid
separate
	movea.l a3, a1
	move.l d7, d4
scan
	tst.l d4
	beq.w validated
	move.l d4, d2
	movea.l a4, a0
	bsr.w validatePlan
	bne.w done
	; Duplicate full IDs would make lookup depend on serialization order.
	movea.l a3, a2
	move.w layout.Plan.FullId(a1), d1
unique
	cmpa.l a1, a2
	beq.w next
	cmp.w layout.Plan.FullId(a2), d1
	beq.w invalid
	adda.l layout.Plan.Bytes(a2), a2
	bra.w unique
next
	adda.l d2, a1
	sub.l d2, d4
	bra.w scan
validated
	lea values.Owner.Bindings(a4), a0
	move.l d7, d0
	jsr memory.reserve
	tst.l d0
	bne.w storage
	movea.l memory.Block.Pointer(a0), a1
	move.l d7, d1
copy
	move.b (a3)+, (a1)+
	subq.l #1, d1
	bne.w copy
	move.l d7, memory.Block.Used(a0)
	moveq #values.OK, d0
	bra.w done
clear
	clr.l memory.Block.Used(a1)
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
	bra.w done
storage
	moveq #values.STORAGE, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; install
; A0=Owner,D1=full symbol ID. D0/CCR=OK found,BOUNDS absent,MALFORMED invalid.
; A1=ephemeral plan,D2=planbytes on success. Other registers preserved.
; No allocation; the returned pointer is invalidated only by install/release.
getBinding	.block
	movem.l d3-d4, -(sp)
	cmp.l values.Owner.Count(a0), d1
	bhs.w invalid
	lea values.Owner.Bindings(a0), a1
	tst.l memory.Block.Used(a1)
	beq.w absent
	jsr values.block
	bne.w done
	move.l memory.Block.Used(a1), d4
	movea.l memory.Block.Pointer(a1), a1
scan
	tst.l d4
	beq.w absent
	move.l d4, d2
	bsr.w validatePlan
	bne.w done
	moveq #0, d3
	move.w layout.Plan.FullId(a1), d3
	cmp.l d3, d1
	beq.w done
	adda.l d2, a1
	sub.l d2, d4
	bra.w scan
absent
	moveq #values.BOUNDS, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d3-d4
	tst.l d0
	rts
	.bend  ; getBinding
; A0=Owner. D0/CCR=OK; other registers preserved. Clear runtime plans explicitly.
clear	.block
	clr.l values.Owner.Bindings+memory.Block.Used(a0)
	moveq #values.OK, d0
	rts
	.bend  ; clear
	.priv
; A0=Owner,A1=bounded header,D2=remaining bytes. D0/CCR=status,D2=planbytes.
; Other registers preserved. Bounds every chain before reading its numeric IDs.
validatePlan	.block
	movem.l d1/d3-d7/a2, -(sp)
	cmpi.l #HEADER_BYTES, d2
	blo.w invalid
	move.l layout.Plan.Bytes(a1), d3
	cmpi.l #HEADER_BYTES+CANDIDATE_BYTES, d3
	blo.w invalid
	cmp.l d2, d3
	bhi.w invalid
	btst #0, d3
	bne.w invalid
	moveq #0, d0
	move.w layout.Plan.FullId(a1), d0
	cmp.l values.Owner.Count(a0), d0
	bhs.w invalid
	moveq #0, d4
	move.w layout.Plan.CandidateCount(a1), d4
	beq.w invalid
	move.l d3, d5
	subi.l #HEADER_BYTES, d5
	lea HEADER_BYTES(a1), a2
candidate
	cmpi.l #CANDIDATE_BYTES, d5
	blo.w invalid
	moveq #0, d0
	move.w layout.Candidate.Base(a2), d0
	cmp.l values.Owner.Count(a0), d0
	bhs.w invalid
	cmp.w layout.Plan.FullId(a1), d0
	beq.w invalid
	moveq #0, d6
	move.w layout.Candidate.MemberCount(a2), d6
	adda.l #CANDIDATE_BYTES, a2
	subi.l #CANDIDATE_BYTES, d5
	move.l d6, d7
	add.l d7, d7
	cmp.l d5, d7
	bhi.w invalid
	sub.l d7, d5
	tst.l d6
	beq.w membersReady
member
	moveq #0, d0
	move.w (a2)+, d0
	cmp.l values.Owner.Count(a0), d0
	bhs.w invalid
	subq.l #1, d6
	bne.w member
membersReady
	subq.l #1, d4
	bne.w candidate
	tst.l d5
	bne.w invalid
	move.l d3, d2
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1/d3-d7/a2
	tst.l d0
	rts
	.bend  ; validatePlan
	.endsection
	.endmodule
