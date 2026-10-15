; Audit-only record fields are writable once at replay; semantic capture fields
; remain immutable. Accepted-call counts are consulted only at owner release.
	.pub
; D0=handle or0,D1=pool,D3=template session,A0=fresh capture.Record.
; Preserve every register, CCR and stack. Duplicate physical replay is an error,
; never silently counted as a second acquisition or another removable body.
candidateOwner	.block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea TemplateState, a6
	tst.l capture.Record.CandidateOwner(a4)
	bne.w duplicate
	move.l #CANDIDATE_NON_MACRO, capture.Record.CandidateOwner(a4)
	tst.l d0
	beq.w done
	bsr.w templateFind
	beq.w unknown
	cmpi.l #CANDIDATE_MACRO_KIND, TemplateDefinition.Kind(a1)
	bne.w done  ; only ordinary macros, not segments/statement templates
	move.l d0, capture.Record.CandidateOwner(a4)
	bra.w done
duplicate
	ori.l #CANDIDATE_DUPLICATE, CandidateState+CandidateFields.Errors
	addq.l #1, CandidateState+CandidateFields.Duplicates
	bra.w done
unknown
	move.l #CANDIDATE_UNKNOWN, capture.Record.CandidateOwner(a4)
	ori.l #CANDIDATE_INVALID, CandidateState+CandidateFields.Errors
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; candidateOwner

; A0=borrowed read-only scalar state; all other registers and CCR preserved.
getCandidates	.block
	lea CandidateState, a0
	rts
	.bend  ; getCandidates
	.pub
; A0=capture.Frame,D1=constructor handle,D0/CCR=constructor status. Passive;
; count only successful constructors. A borrowed capture view lives this call.
candidateCreated	.block
	.TEMPLATE_SAVE
	tst.l d0
	bne.w done
	movea.l capture.Frame.Arena(a0), a0
	jsr capture.resolve
	bne.w invalid
	movea.l a1, a4
	lea CandidateState, a6
	bsr.w candidateReadRecord
	lea CandidateFields.Gross(a6), a2
	bsr.w candidateAddVector
	addq.l #1, CandidateFields.Created(a6)
	bcs.w overflow
	move.l capture.Record.PlanBytes(a4), d0
	add.l d0, CandidateFields.PlanBytes(a6)
	bcc.w done
overflow
	ori.l #CANDIDATE_OVERFLOW, CandidateFields.Errors(a6)
	bra.w done
invalid
	ori.l #CANDIDATE_INVALID, CandidateState+CandidateFields.Errors
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; candidateCreated
	.priv
candidateClear	.block
	.TEMPLATE_SAVE
	lea CandidateState, a0
	move.l #CANDIDATE_STATE_BYTES/4, d0
next
	clr.l (a0)+
	subq.l #1, d0
	bne.w next
	.TEMPLATE_RESTORE
	rts
	.bend  ; candidateClear
; A0=validated capture before actual release. Classify each acquired record once
; after replay and macro calls; totals also include captures never replayed.
	.pub
candidateRecord	.block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea CandidateState, a6
	bsr.w candidateReadRecord
	cmpi.l #1, CandidateVector.Samples(a5)
	bhi.w invalid
	tst.l CandidateVector.Samples(a5)
	bne.w classify
	tst.l CandidateVector.TicksLow(a5)
	bne.w invalid
classify
	moveq #CANDIDATE_NON_MACRO_GROUP, d7
	move.l capture.Record.CandidateOwner(a4), d0
	cmpi.l #CANDIDATE_NON_MACRO, d0
	beq.w selected
	moveq #CANDIDATE_UNKNOWN_GROUP, d7
	tst.l d0
	beq.w unassigned
	cmpi.l #CANDIDATE_UNKNOWN, d0
	beq.w selected
	cmp.l TemplateState+TemplateFields.Count, d0
	bhi.w invalid
	subq.l #1, d0
	mulu.w #TEMPLATE_DEFINITION_BYTES, d0
	lea TemplateState+TemplateFields.Rows, a1
	adda.l d0, a1
	cmpi.l #CANDIDATE_MACRO_KIND, TemplateDefinition.Kind(a1)
	bne.w invalid
	tst.l TemplateDefinition.Closed(a1)
	beq.w invalid
	moveq #CANDIDATE_NEVER_INVOKED_GROUP, d7
	tst.l TemplateDefinition.Calls(a1)
	beq.w selected
	moveq #CANDIDATE_INVOKED_GROUP, d7
	bra.w selected
unassigned
	addq.l #1, CandidateFields.Unassigned(a6)
	bra.w selected
invalid
	ori.l #CANDIDATE_INVALID, CandidateFields.Errors(a6)
	moveq #CANDIDATE_UNKNOWN_GROUP, d7
selected
	lea CandidateFields.Vectors(a6), a2
	bsr.w candidateAddVector
	mulu.w #CANDIDATE_VECTOR_BYTES, d7
	lea CandidateFields.Vectors(a6), a2
	adda.l d7, a2
	bsr.w candidateAddVector
	.TEMPLATE_RESTORE
	rts
	.bend  ; candidateRecord
	.priv
; A4=validated capture,A6=observer; A5=temporary vector. Private scratch
; D0/D1/A0/A1/CCR are outputs/clobbers. No ownership is inferred here.
candidateReadRecord	.block
	lea CandidateFields.Record(a6), a0
	moveq #CANDIDATE_VECTOR_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	lea CandidateFields.Record(a6), a5
	move.l #1, CandidateVector.Records(a5)
	move.l capture.Record.Bytes(a4), CandidateVector.CaptureBytes(a5)
	move.l capture.Record.PlanBytes(a4), CandidateVector.PlanBytes(a5)
	move.l capture.Record.SourceBytes(a4), CandidateVector.SourceBytes(a5)
	move.l capture.Record.LexemeBytes(a4), CandidateVector.LexemeBytes(a5)
	moveq #0, d0
	move.b capture.Record.Count(a4), d0
	mulu.w #capture.TOKEN_BYTES, d0
	move.l d0, CandidateVector.TokenBytes(a5)
	lea capture.Record.WorkCounts(a4), a0
	lea CandidateVector.CandidateCalls(a5), a1
	moveq #capture.WORK_COUNTER_BYTES-1, d0
counters
	moveq #0, d1
	move.b (a0)+, d1
	move.l d1, (a1)+
	dbra d0, counters
	move.l capture.Record.CandidateSample(a4), CandidateVector.Samples(a5)
	move.l capture.Record.CandidateTicks(a4), CandidateVector.TicksLow(a5)
	rts
	.bend  ; candidateReadRecord

; A5=record,A2=vector,A6=state. Add scalar prefix and one full 64-bit clock.
; Reserved tail is never written. Clobbers D0-D2/A0-A1/CCR.
candidateAddVector	.block
	movea.l a5, a0
	movea.l a2, a1
	moveq #CandidateVector.TicksHigh/4-1, d0
scalar
	move.l (a0)+, d1
	add.l d1, (a1)+
	bcc.w next
	ori.l #CANDIDATE_OVERFLOW, CandidateFields.Errors(a6)
next
	dbra d0, scalar
	move.l CandidateVector.TicksLow(a5), d1
	move.l CandidateVector.TicksHigh(a5), d2
	add.l d1, CandidateVector.TicksLow(a2)
	move.l CandidateVector.TicksHigh(a2), d1
	addx.l d2, d1
	bcc.w store
	ori.l #CANDIDATE_OVERFLOW, CandidateFields.Errors(a6)
store
	move.l d1, CandidateVector.TicksHigh(a2)
done
	rts
	.bend  ; candidateAddVector
