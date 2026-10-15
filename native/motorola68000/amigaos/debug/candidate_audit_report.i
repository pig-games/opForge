; Separate scalar schema. Report before memory_profile.save closes its timer.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.priv
; A0=dos.library. Passive terminal report; no report field establishes parity.
candidateReport	.block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea CandidateState, a6
	bsr.w candidateFinalize
	move.l CandidateFields.Created(a6), d0
	cmp.l CandidateFields.Vectors+CandidateVector.Records(a6), d0
	bne.w invalid
	move.l CandidateFields.PlanBytes(a6), d0
	cmp.l CandidateFields.Vectors+CandidateVector.PlanBytes(a6), d0
	bne.w invalid
	tst.l CandidateFields.ClockActive(a6)
	beq.w ready
	ori.l #CANDIDATE_UNFINISHED, CandidateFields.Errors(a6)
	bra.w ready
invalid
	ori.l #CANDIDATE_INVALID, CandidateFields.Errors(a6)
ready
	move.l #400000, d0
	moveq #1, d1
	move.l CandidateFields.Errors(a6), d2
	move.l #CANDIDATE_STATE_BYTES, d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #400001, d0
	move.l CandidateFields.Created(a6), d1
	move.l CandidateFields.PlanBytes(a6), d2
	move.l CandidateFields.Duplicates(a6), d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #400002, d0
	move.l CandidateFields.Frequency(a6), d1
	move.l #OPFORGE_CANDIDATE_SAMPLE_PERIOD, d2
	move.l #OPFORGE_CANDIDATE_PHASE_SEED, d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #400003, d0
	move.l CandidateFields.TimedCalls(a6), d1
	move.l CandidateFields.TimedSamples(a6), d2
	move.l CandidateFields.Unassigned(a6), d3
	movea.l a4, a0
	jsr memory_profile.progress
	lea CandidateFields.Vectors(a6), a5
	move.l #400010, d4
	moveq #CANDIDATE_GROUPS*5-1, d7
next
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d7, next
	.TEMPLATE_RESTORE
	rts
	.bend  ; candidateReport
.endif
.endif
.endif

; Gross acquisition includes successful temporary captures not retained by the
; graph. Subtract retained totals conservatively, then publish that residual in
; its own category. Repeated terminal calls are idempotent (residual becomes0).
	.priv
candidateFinalize	.block
	lea CandidateFields.Record(a6), a5
	movea.l a5, a0
	moveq #CANDIDATE_VECTOR_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	lea CandidateFields.Gross(a6), a0
	lea CandidateFields.Vectors(a6), a1
	movea.l a5, a2
	moveq #CandidateVector.TicksHigh/4-1, d0
scalar
	move.l (a0)+, d1
	sub.l (a1)+, d1
	bcs.w invalid
	move.l d1, (a2)+
	dbra d0, scalar
	move.l CandidateFields.Gross+CandidateVector.TicksLow(a6), d1
	sub.l CandidateFields.Vectors+CandidateVector.TicksLow(a6), d1
	move.l CandidateFields.Gross+CandidateVector.TicksHigh(a6), d2
	move.l CandidateFields.Vectors+CandidateVector.TicksHigh(a6), d0
	subx.l d0, d2
	bcs.w invalid
	move.l d1, CandidateVector.TicksLow(a5)
	move.l d2, CandidateVector.TicksHigh(a5)
	lea CandidateFields.Vectors(a6), a2
	bsr.w candidateAddVector
	lea CandidateFields.Vectors+CANDIDATE_NOT_RETAINED_GROUP*CANDIDATE_VECTOR_BYTES(a6), a2
	bsr.w candidateAddVector
	rts
invalid
	ori.l #CANDIDATE_INVALID, CandidateFields.Errors(a6)
	rts
	.bend  ; candidateFinalize
