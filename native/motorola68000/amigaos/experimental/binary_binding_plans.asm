; Preparation-owned numeric alternatives for an unresolved full value name.
; @opforge-owner: experimental.amigaos.binary_binding_plans
	.module experimental.amigaos.binary_binding_plans
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_binding_layout as layout
	.pub
PLAN_BYTES = layout.HEADER_BYTES
CANDIDATE_BYTES = layout.CANDIDATE_HEADER_BYTES
State	.struct
Block	.res memory.Block.Used+4
Current	.long ?
.endstruct
STATE_BYTES = State.Current+4
	.section code, kind=code
; A0=State,D0=full origin ID. Begin one plan. Other registers preserved.
start	.block
	movem.l d1-d2/a1, -(sp)
	move.l d0, d1
	move.l memory.Block.Used(a0), d2
	move.l d2, d0
	addi.l #PLAN_BYTES, d0
	jsr memory.reserve
	bne.w done
	move.l d2, State.Current(a0)
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	move.w d1, layout.Plan.FullId(a1)
	clr.w layout.Plan.CandidateCount(a1)
	move.l #PLAN_BYTES, layout.Plan.Bytes(a1)
	move.l d2, d1
	addi.l #PLAN_BYTES, d1
	move.l d1, memory.Block.Used(a0)
done
	movem.l (sp)+, d1-d2/a1
	tst.l d0
	rts
	.bend  ; start
; A0=State,D0=base ID,A1=bounded BE16 member IDs,D1=count (1..127).
; D0/CCR=status; others preserved. Atomic append; no source pointers retained.
append	.block
	movem.l d1-d5/a1-a3, -(sp)
	move.l d0, d3
	move.l d1, d4
	beq.w bad
	cmpi.l #127, d4
	bhi.w bad
	move.l memory.Block.Used(a0), d2
	move.l d4, d0
	add.l d0, d0
	addi.l #CANDIDATE_BYTES, d0
	add.l d2, d0
	bcs.w bad
	move.l d0, d5
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a2
	movea.l a2, a3
	adda.l State.Current(a0), a3
	adda.l d2, a2
	move.w d3, layout.Candidate.Base(a2)
	move.w d4, layout.Candidate.MemberCount(a2)
	adda.w #CANDIDATE_BYTES, a2
copy
	move.w (a1)+, (a2)+
	subq.l #1, d4
	bne.w copy
	addq.w #1, layout.Plan.CandidateCount(a3)
	move.l d5, d0
	sub.l State.Current(a0), d0
	move.l d0, layout.Plan.Bytes(a3)
	move.l d5, memory.Block.Used(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a1-a3
	tst.l d0
	rts
	.bend  ; append
; A0=State,A1=Entry array,D0=source base,D1=count. Finalize once after binding.
; Remove plans with resolved full declarations; remap bases via Entry.Target.
; D0/CCR=status; others preserved. Member IDs retain their bare global identity.
finalize	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d6
	move.l d1, d7
	movea.l memory.Block.Pointer(a6), a0
	movea.l a0, a3
	movea.l a0, a4
	adda.l memory.Block.Used(a6), a4
plan
	cmpa.l a4, a0
	beq.w complete
	moveq #0, d0
	move.w layout.Plan.FullId(a0), d0
	bsr.w entry
	bne.w done
	btst #0, records.Entry.Flags+1(a1)
	bne.w skip
	moveq #0, d0
	move.w records.Entry.Target(a1), d0
	bsr.w entry
	bne.w done
	btst #0, records.Entry.Flags+1(a1)
	bne.w skip
	move.l layout.Plan.Bytes(a0), d5
	movea.l a0, a2
	adda.l d5, a2
	moveq #0, d4
	move.w layout.Plan.CandidateCount(a0), d4
	move.w layout.Plan.FullId(a0), (a3)+
	move.w d4, (a3)+
	move.l d5, (a3)+
	adda.w #PLAN_BYTES, a0
candidate
	moveq #0, d0
	move.w layout.Candidate.Base(a0), d0
	bsr.w entry
	bne.w done
	move.w records.Entry.Target(a1), (a3)+
	moveq #0, d3
	move.w layout.Candidate.MemberCount(a0), d3
	move.w d3, (a3)+
	adda.w #CANDIDATE_BYTES, a0
member
	move.w (a0)+, (a3)+
	subq.l #1, d3
	bne.w member
	subq.l #1, d4
	bne.w candidate
	movea.l a2, a0
	bra.w plan
skip
	adda.l layout.Plan.Bytes(a0), a0
	bra.w plan
complete
	move.l a3, d0
	sub.l memory.Block.Pointer(a6), d0
	move.l d0, memory.Block.Used(a6)
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finalize
	.priv
; D0=source ID,D6=base,D7=count,A5=entries. D0/CCR=status,A1=Entry.
entry	.block
	sub.l d6, d0
	bcs.w bad
	cmp.l d7, d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l a5, a1
	adda.l d0, a1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; entry
	.endsection
	.endmodule
