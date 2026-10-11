; Preparation-owned numeric alternatives for an unresolved full value name.
; @opforge-owner: experimental.amigaos.binary_binding_plans
	.module experimental.amigaos.binary_binding_plans
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_binding_layout as layout
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_owner_guards as guards
	.pub
PLAN_BYTES = layout.HEADER_BYTES
CANDIDATE_BYTES = layout.CANDIDATE_HEADER_BYTES
MAX_MEMBERS = 127
AMBIGUOUS_OWNER = $ffff
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
	cmpi.l #MAX_MEMBERS, d4
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
; A0=State,D0=proxy ID,D1=conditional complete declaration ID.
; Prepend a whole-value alternative without retaining lexical spelling.
; D0/CCR=status,D2=changed (0/1); others preserved. IDs remap in finalize.
prependWhole	.block
	movem.l d1/d3-d7/a0-a3, -(sp)
	move.l d0, d6
	move.l d1, d7
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d4
	moveq #0, d3
find
	cmp.l d4, d3
	bhs.w bad
	move.l d4, d0
	sub.l d3, d0
	cmpi.l #PLAN_BYTES, d0
	blo.w bad
	cmp.w layout.Plan.FullId(a1, d3.l), d6
	beq.w found
	move.l layout.Plan.Bytes(a1, d3.l), d0
	cmpi.l #PLAN_BYTES+CANDIDATE_BYTES, d0
	blo.w bad
	add.l d0, d3
	bcs.w bad
	cmp.l d4, d3
	bhi.w bad
	bra.w find
found
	move.l layout.Plan.Bytes(a1, d3.l), d0
	cmpi.l #PLAN_BYTES+CANDIDATE_BYTES, d0
	blo.w bad
	add.l d3, d0
	bcs.w bad
	cmp.l d4, d0
	bhi.w bad
	cmpi.w #$ffff, layout.Plan.CandidateCount(a1, d3.l)
	beq.w bad
	tst.w PLAN_BYTES+layout.Candidate.MemberCount(a1, d3.l)
	bne.w reserve
	cmp.w PLAN_BYTES+layout.Candidate.Base(a1, d3.l), d7
	beq.w unchanged  ; binding sweeps may revisit an already captured occurrence
reserve
	move.l d4, d0
	addi.l #CANDIDATE_BYTES, d0
	bcs.w bad
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a3
	movea.l a3, a1
	adda.l d4, a1
	lea CANDIDATE_BYTES(a1), a2
	move.l d4, d5
	sub.l d3, d5
	subi.l #PLAN_BYTES, d5
shift
	tst.l d5
	beq.w insert
	move.b -(a1), -(a2)
	subq.l #1, d5
	bra.w shift
insert
	suba.w #CANDIDATE_BYTES, a2  ; backward copy ends at the first shifted candidate
	move.w d7, layout.Candidate.Base(a2)
	clr.w layout.Candidate.MemberCount(a2)
	addq.w #1, layout.Plan.CandidateCount(a3, d3.l)
	addi.l #CANDIDATE_BYTES, layout.Plan.Bytes(a3, d3.l)
	addi.l #CANDIDATE_BYTES, memory.Block.Used(a0)
	moveq #1, d2
	bra.w good
unchanged
	moveq #0, d2
good
	moveq #0, d0
	bra.w done
bad
	moveq #0, d2
	moveq #1, d0
done
	movem.l (sp)+, d1/d3-d7/a0-a3
	tst.l d0
	rts
	.bend  ; prependWhole

; A0=State,A1=Entry array,D0=source base,D1=count. Finalize once after binding.
; Remove plans with resolved full declarations; remap bases via Entry.Target.
; A longest unconditional declaration fixes binding before its value kind exists.
; Compact that plan to one candidate; dormant conditional candidates stay deferred.
; D0/CCR=status; others preserved. Member IDs retain their bare global identity.
FinalizeFrame	.struct
Output	.long ?
.endstruct
FINALIZE_FRAME_BYTES = FinalizeFrame.Output+4
finalize	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #FINALIZE_FRAME_BYTES, sp
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d6
	move.l d1, d7
	move.l d6, d5
	add.l d7, d5
	bcs.w invalid
	movea.l memory.Block.Pointer(a6), a0
	movea.l a0, a3
	movea.l a0, a4
	adda.l memory.Block.Used(a6), a4
plan
	moveq #0, d2
	cmpa.l a4, a0
	beq.w complete
	moveq #0, d0
	move.w layout.Plan.FullId(a0), d0
	bsr.w entry
	bne.w done
	btst #0, records.Entry.Flags+1(a1)
	bne.w discard
	moveq #0, d0
	move.w records.Entry.Target(a1), d0
	bsr.w entry
	bne.w done
	btst #0, records.Entry.Flags+1(a1)
	bne.w discard
shape
	move.l layout.Plan.Bytes(a0), d1
	cmpi.l #PLAN_BYTES+CANDIDATE_BYTES, d1
	blo.w invalid
	movea.l a0, a2
	adda.l d1, a2
	cmpa.l a4, a2
	bhi.w invalid
	moveq #0, d4
	move.w layout.Plan.CandidateCount(a0), d4
	beq.w invalid
	cmpi.l #3, d2  ; discarded full declarations still validate every candidate
	beq.w selectionReady
	; zero=alternatives, one=selected first, two=tail validation
	moveq #0, d0
	move.w PLAN_BYTES+layout.Candidate.Base(a0), d0
	bsr.w entry
	bne.w done
	moveq #0, d0
	move.w records.Entry.Target(a1), d0
	bsr.w entry
	bne.w done
	btst #0, records.Entry.Flags+1(a1)
	beq.w selectionReady
	move.w records.Entry.Flags(a1), d0
	andi.w #records.CONDITIONAL_VALUE, d0
	bne.w selectionReady
	moveq #1, d2
selectionReady
	cmpi.l #3, d2
	beq.w candidateStart
	move.l a3, FinalizeFrame.Output(sp)
	move.w layout.Plan.FullId(a0), (a3)+
	move.w d4, (a3)+
	move.l d1, (a3)+
candidateStart
	adda.w #PLAN_BYTES, a0
candidate
	move.l a2, d0
	sub.l a0, d0
	cmpi.l #CANDIDATE_BYTES, d0
	blo.w invalid
	moveq #0, d0
	move.w layout.Candidate.Base(a0), d0
	bsr.w entry
	bne.w done
	moveq #0, d0
	move.w records.Entry.Target(a1), d0
	move.l d0, d1
	bsr.w entry
	bne.w done
	cmpi.l #2, d2
	bhs.w memberHeader
	move.w d1, (a3)+
memberHeader
	moveq #0, d3
	move.w layout.Candidate.MemberCount(a0), d3
	cmpi.l #MAX_MEMBERS, d3
	bhi.w invalid
	cmpi.l #2, d2
	bhs.w memberBounds
	move.w d3, (a3)+
memberBounds
	adda.w #CANDIDATE_BYTES, a0
	move.l d3, d0
	add.l d0, d0
	add.l a0, d0
	bcs.w invalid
	cmp.l a2, d0
	bhi.w invalid
	tst.l d3
	beq.w membersReady
member
	moveq #0, d0
	move.w (a0)+, d0
	cmp.l d5, d0
	bhs.w invalid
	cmpi.l #2, d2
	bhs.w nextMember
	move.w d0, (a3)+
nextMember
	subq.l #1, d3
	bne.w member
membersReady
	cmpi.l #1, d2
	bne.w nextCandidate
	moveq #2, d2
nextCandidate
	subq.l #1, d4
	bne.w candidate
	cmpa.l a2, a0
	bne.w invalid
	cmpi.l #3, d2
	beq.w nextPlan
	movea.l FinalizeFrame.Output(sp), a1
	cmpi.l #2, d2
	bne.w planBytes
	move.w #1, layout.Plan.CandidateCount(a1)
planBytes
	move.l a3, d0
	sub.l a1, d0
	move.l d0, layout.Plan.Bytes(a1)
nextPlan
	movea.l a2, a0
	bra.w plan
discard
	moveq #3, d2
	bra.w shape
complete
	move.l a3, d0
	sub.l memory.Block.Pointer(a6), d0
	move.l d0, memory.Block.Used(a6)
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	adda.w #FINALIZE_FRAME_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finalize

; A0=State,A1=entries,D0=source base,D1=count. D0/CCR=status,D2=need.
; Only multiple alternatives or a conditional candidate require this bounded
; acquisition slice. Validate the numeric container before allocating proofs.
needsGuards	.block
	movem.l d1/d3-d7/a0-a4, -(sp)
	move.l d0, d6
	move.l d1, d7
	moveq #0, d5
	movea.l memory.Block.Pointer(a0), a2
	movea.l a2, a4
	adda.l memory.Block.Used(a0), a4
plan
	cmpa.l a4, a2
	beq.w good
	bhi.w bad
	move.l a4, d0
	sub.l a2, d0
	cmpi.l #PLAN_BYTES, d0
	blo.w bad
	move.l layout.Plan.Bytes(a2), d1
	cmpi.l #PLAN_BYTES+CANDIDATE_BYTES, d1
	blo.w bad
	cmp.l d0, d1
	bhi.w bad
	movea.l a2, a3
	adda.l d1, a3
	moveq #0, d4
	move.w layout.Plan.CandidateCount(a2), d4
	beq.w bad
	cmpi.l #1, d4
	bls.w candidateStart
	moveq #1, d5
candidateStart
	adda.w #PLAN_BYTES, a2
candidate
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #CANDIDATE_BYTES, d0
	blo.w bad
	moveq #0, d1
	move.w layout.Candidate.Base(a2), d1
	sub.l d6, d1
	bcs.w bad
	cmp.l d7, d1
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d1
	move.w records.Entry.Flags(a1, d1.l), d1
	andi.w #records.CONDITIONAL_VALUE, d1
	beq.w extent
	moveq #1, d5
extent
	moveq #0, d1
	move.w layout.Candidate.MemberCount(a2), d1
	cmpi.l #MAX_MEMBERS, d1
	bhi.w bad
	add.l d1, d1
	addi.l #CANDIDATE_BYTES, d1
	cmp.l d0, d1
	bhi.w bad
	adda.l d1, a2
	subq.l #1, d4
	bne.w candidate
	cmpa.l a3, a2
	bne.w bad
	bra.w plan
good
	move.l d5, d2
	moveq #0, d0
	bra.w done
bad
	moveq #0, d2
	moveq #1, d0
done
	movem.l (sp)+, d1/d3-d7/a0-a4
	tst.l d0
	rts
	.bend  ; needsGuards

; A0=State,A1=temporary guard certificates. Compact certified inactive bases;
; the first certified active declaration commits before value availability/kind.
; An uncertified earlier alternative prevents a later declaration from committing.
; Validate all tails, even after selection. D0/CCR=status; others preserved.
SealFrame	.struct
Plan	.long ?
Candidates	.word ?
Blocked	.word ?
Selected	.word ?
Reserved	.word ?
.endstruct
SEAL_FRAME_BYTES = SealFrame.Reserved+2
seal	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #SEAL_FRAME_BYTES, sp
	movea.l a0, a6
	movea.l a1, a5
	movea.l memory.Block.Pointer(a6), a0
	movea.l a0, a3
	movea.l a0, a4
	adda.l memory.Block.Used(a6), a4
plan
	cmpa.l a4, a0
	beq.w complete
	bhi.w bad
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #PLAN_BYTES, d0
	blo.w bad
	move.l layout.Plan.Bytes(a0), d1
	cmpi.l #PLAN_BYTES+CANDIDATE_BYTES, d1
	blo.w bad
	cmp.l d0, d1
	bhi.w bad
	movea.l a0, a2
	adda.l d1, a2
	moveq #0, d7
	move.w layout.Plan.CandidateCount(a0), d7
	beq.w bad
	move.l a3, SealFrame.Plan(sp)
	clr.w SealFrame.Candidates(sp)
	clr.w SealFrame.Blocked(sp)
	clr.w SealFrame.Selected(sp)
	move.w layout.Plan.FullId(a0), (a3)+
	clr.w (a3)+
	clr.l (a3)+
	adda.w #PLAN_BYTES, a0
candidate
	move.l a2, d0
	sub.l a0, d0
	cmpi.l #CANDIDATE_BYTES, d0
	blo.w bad
	moveq #0, d6
	move.w layout.Candidate.Base(a0), d6
	moveq #0, d5
	move.w layout.Candidate.MemberCount(a0), d5
	cmpi.l #MAX_MEMBERS, d5
	bhi.w bad
	move.l d5, d4
	add.l d4, d4
	addi.l #CANDIDATE_BYTES, d4
	cmp.l d0, d4
	bhi.w bad
	movea.l a0, a1
	adda.l d4, a1
	; Validate IDs through the source table, including removed alternatives.
	cmp.l guards.State.Base(a5), d6
	blo.w bad
	move.l guards.State.Base(a5), d0
	add.l guards.State.Count(a5), d0
	cmp.l d0, d6
	bhs.w bad
	move.l d5, d3
	moveq #CANDIDATE_BYTES, d2
memberBounds
	tst.l d3
	beq.w activity
	moveq #0, d1
	move.w 0(a0, d2.l), d1
	cmp.l d0, d1
	bhs.w bad
	addq.l #2, d2
	subq.l #1, d3
	bra.w memberBounds
activity
	move.l a0, -(sp)
	movea.l a5, a0
	move.l d6, d0
	jsr guards.declaration
	movea.l (sp)+, a0
	cmpi.l #guards.INACTIVE, d0
	beq.w next
	tst.w SealFrame.Selected(sp)
	bne.w next
	cmpi.l #guards.ACTIVE, d0
	bne.w uncertain
	tst.w SealFrame.Blocked(sp)
	bne.w copy
	move.w #1, SealFrame.Selected(sp)
	bra.w copy
uncertain
	move.w #1, SealFrame.Blocked(sp)
copy
	addq.w #1, SealFrame.Candidates(sp)
	move.l d4, d0
copyByte
	move.b (a0)+, (a3)+
	subq.l #1, d0
	bne.w copyByte
next
	movea.l a1, a0
	subq.l #1, d7
	bne.w candidate
	cmpa.l a2, a0
	bne.w bad
	tst.w SealFrame.Candidates(sp)
	beq.w bad  ; every actual declaration is certified inactive
	movea.l SealFrame.Plan(sp), a1
	move.w SealFrame.Candidates(sp), layout.Plan.CandidateCount(a1)
	move.l a3, d0
	sub.l a1, d0
	move.l d0, layout.Plan.Bytes(a1)
	bra.w plan
complete
	move.l a3, d0
	sub.l memory.Block.Pointer(a6), d0
	move.l d0, memory.Block.Used(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #SEAL_FRAME_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seal

OwnerIndexFrame	.struct
Defined	.long ?
Entries	.long ?
Guards	.long ?
Committed	.word ?
Ambiguous	.word ?
.endstruct
OWNER_INDEX_FRAME_BYTES = OwnerIndexFrame.Ambiguous+2
; A0=finalized State,A1=known values Owner,A2=source-indexed BE16 owner slots,
; A3=source-indexed defined cells,A4=source Entry array,D0=base,D1=count.
; An unconditional declaration commits ownership regardless of current value
; kind. Non-declaration snapshots and dormant conditional prefixes retain the
; conservative possible-owner check; a known immutable compound ends it.
; Publish a common possible owner, or AMBIGUOUS_OWNER for selection-time rejection.
; Full declarations already won in finalize. A zero owner is a real possibility.
; Validate every numeric boundary, including unselected tails. No allocation.
; D0/CCR=status; other registers preserved. Run once after declaration indexing.
indexOwners	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #OWNER_INDEX_FRAME_BYTES, sp
	move.l a3, OwnerIndexFrame.Defined(sp)
	move.l a4, OwnerIndexFrame.Entries(sp)
	move.l d2, OwnerIndexFrame.Guards(sp)
	move.l d0, d6
	move.l d1, d7
	movea.l a1, a5
	movea.l a2, a4
	move.l d6, d0
	add.l d7, d0
	bcs.w bad
	cmp.l values.Owner.Count(a5), d0
	bhi.w bad
	move.l memory.Block.Used(a0), d0
	cmp.l memory.Block.Capacity(a0), d0
	bhi.w bad
	movea.l memory.Block.Pointer(a0), a0
	move.l a0, d2
	tst.l d0
	beq.w arenaReady
	tst.l d2
	beq.w bad
arenaReady
	add.l d0, d2
	bcs.w bad
	movea.l d2, a6
plan
	cmpa.l a6, a0
	beq.w good
	bhi.w bad
	move.l a6, d0
	sub.l a0, d0
	cmpi.l #PLAN_BYTES, d0
	blo.w bad
	move.l layout.Plan.Bytes(a0), d2
	cmpi.l #PLAN_BYTES+CANDIDATE_BYTES, d2
	blo.w bad
	cmp.l d0, d2
	bhi.w bad
	btst #0, d2
	bne.w bad
	movea.l a0, a3
	adda.l d2, a3
	moveq #0, d5
	move.w layout.Plan.FullId(a0), d5
	sub.l d6, d5
	bcs.w bad
	cmp.l d7, d5
	bhs.w bad
	moveq #0, d4
	move.w layout.Plan.CandidateCount(a0), d4
	beq.w bad
	lea PLAN_BYTES(a0), a2
	moveq #-1, d3
	clr.w OwnerIndexFrame.Committed(sp)
	clr.w OwnerIndexFrame.Ambiguous(sp)
candidate
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #CANDIDATE_BYTES, d0
	blo.w bad
	moveq #0, d1
	move.w layout.Candidate.Base(a2), d1
	move.l d1, d0
	sub.l d6, d0
	bcs.w bad
	cmp.l d7, d0
	bhs.w bad
	cmp.l d5, d0
	beq.w bad  ; runtime plans forbid a full occurrence as its own base
	moveq #0, d2
	move.w layout.Candidate.MemberCount(a2), d2
	cmpi.l #MAX_MEMBERS, d2
	bhi.w bad
	adda.w #CANDIDATE_BYTES, a2
	move.l a3, d0
	sub.l a2, d0
	add.l d2, d2
	cmp.l d0, d2
	bhi.w bad
	lsr.l #1, d2
	beq.w membersReady
member
	moveq #0, d0
	move.w (a2)+, d0
	cmp.l values.Owner.Count(a5), d0
	bhs.w bad
	subq.l #1, d2
	bne.w member
membersReady
	tst.w OwnerIndexFrame.Committed(sp)
	bne.w nextCandidate
	move.l d1, d0
	sub.l d6, d0
	movea.l OwnerIndexFrame.Entries(sp), a0
	mulu.w #records.ENTRY_BYTES, d0
	move.w records.Entry.Flags(a0, d0.l), d2
	btst #0, d2
	beq.w dynamicOwner
	andi.w #records.CONDITIONAL_VALUE, d2
	bne.w dynamicOwner
	move.w #1, OwnerIndexFrame.Committed(sp)
	bra.w possibleOwner
dynamicOwner
	move.l OwnerIndexFrame.Guards(sp), d2
	beq.w snapshotOwner
	move.l a0, -(sp)
	movea.l d2, a0
	move.l d1, d0
	jsr guards.declaration
	movea.l (sp)+, a0
	cmpi.l #guards.ACTIVE, d0
	beq.w certifiedOwner
	move.l d1, d0
	sub.l d6, d0
	mulu.w #records.ENTRY_BYTES, d0
snapshotOwner
	move.w records.Entry.Flags(a0, d0.l), d2
	andi.w #records.CONDITIONAL_VALUE, d2
	bne.w possibleOwner  ; an uncertified snapshot cannot prove a branch inactive
	move.w records.Entry.Flags(a0, d0.l), d2
	andi.w #records.MUTABLE, d2
	bne.w possibleOwner
	move.l d1, d0
	sub.l d6, d0
	movea.l OwnerIndexFrame.Defined(sp), a0
	tst.b 0(a0, d0.l)
	beq.w possibleOwner
	movea.l a5, a0
	jsr values.getKind
	bne.w bad
	cmpi.l #values.ADDRESS_RECORD, d2
	blo.w nextCandidate
	cmpi.l #values.STRUCT_LITERAL, d2
	bhi.w nextCandidate
certifiedOwner
	move.w #1, OwnerIndexFrame.Committed(sp)
possibleOwner
	move.l d1, d0
	sub.l d6, d0
	add.l d0, d0
	moveq #0, d2
	move.w 0(a4, d0.l), d2
	tst.l d3
	bmi.w firstOwner
	cmp.l d3, d2
	beq.w nextCandidate
	move.w #1, OwnerIndexFrame.Ambiguous(sp)
	bra.w nextCandidate
firstOwner
	move.l d2, d3
nextCandidate
	subq.l #1, d4
	bne.w candidate
	cmpa.l a3, a2
	bne.w bad
	tst.l d3
	bmi.w nextPlan
	tst.w OwnerIndexFrame.Ambiguous(sp)
	beq.w publishOwner
	move.l #AMBIGUOUS_OWNER, d3
publishOwner
	add.l d5, d5
	move.w d3, 0(a4, d5.l)
nextPlan
	movea.l a3, a0
	bra.w plan
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #OWNER_INDEX_FRAME_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; indexOwners
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
