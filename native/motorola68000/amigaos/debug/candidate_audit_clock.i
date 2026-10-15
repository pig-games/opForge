; Candidate generation is non-nesting. No clock calls exist in count-only images.
	.pub
; Begin one original acquisition window. Passive on every path.
candidateBegin	.block
.ifdef OPFORGE_CANDIDATE_TIMING
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.TEMPLATE_SAVE
	lea CandidateState, a6
	tst.l CandidateFields.ClockActive(a6)
	bne.w invalid
	addq.l #1, CandidateFields.TimedCalls(a6)
	bcs.w invalid
	move.l #OPFORGE_CANDIDATE_SAMPLE_PERIOD, d0
	cmpi.l #64, d0
	beq.w period
	cmpi.l #128, d0
	beq.w period
	cmpi.l #256, d0
	bne.w invalid
period
	move.l CandidateFields.TimedCalls(a6), d0
	subq.l #1, d0
	addi.l #OPFORGE_CANDIDATE_PHASE_SEED, d0
	andi.l #OPFORGE_CANDIDATE_SAMPLE_PERIOD-1, d0
	bne.w done
	lea CandidateFields.ClockStart(a6), a0
	jsr memory_profile.readClock
	beq.w invalid
	tst.l CandidateFields.Frequency(a6)
	beq.w frequency
	cmp.l CandidateFields.Frequency(a6), d0
	bne.w invalid
frequency
	move.l d0, CandidateFields.Frequency(a6)
	move.l #1, CandidateFields.ClockActive(a6)
	bra.w done
invalid
	ori.l #CANDIDATE_CLOCK, CandidateFields.Errors(a6)
done
	.TEMPLATE_RESTORE
.endif
.endif
.endif
.endif
	rts
	.bend  ; candidateBegin
; A0=capture.Frame immediately after candidate generation. Preserve result
; status, registers, CCR and stack; successful capture publication counts work.
candidateEnd	.block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea CandidateState, a6
	clr.l capture.Frame.CandidateTicks(a4)
	clr.l capture.Frame.CandidateSample(a4)
.ifdef OPFORGE_CANDIDATE_TIMING
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	tst.l CandidateFields.ClockActive(a6)
	beq.w done
	clr.l CandidateFields.ClockActive(a6)
	lea CandidateFields.ClockEnd(a6), a0
	jsr memory_profile.readClock
	beq.w invalid
	cmp.l CandidateFields.Frequency(a6), d0
	bne.w invalid
	move.l CandidateFields.ClockEnd+CandidateStamp.Low(a6), d1
	move.l CandidateFields.ClockEnd(a6), d2
	sub.l CandidateFields.ClockStart+CandidateStamp.Low(a6), d1
	move.l CandidateFields.ClockStart(a6), d0
	subx.l d0, d2
	tst.l d2
	bne.w invalid  ; per-record elapsed must fit u32; aggregate uses u64
	move.l d1, capture.Frame.CandidateTicks(a4)
	move.l #1, capture.Frame.CandidateSample(a4)
	addq.l #1, CandidateFields.TimedSamples(a6)
	bcc.w done
invalid
	ori.l #CANDIDATE_CLOCK, CandidateFields.Errors(a6)
done
.endif
.endif
.endif
.endif
	.TEMPLATE_RESTORE
	rts
	.bend  ; candidateEnd
