; Bounded audit-only preparation work and retained payload attribution.
; Existing capture records retain acquisition counters; no source parser/journal.
; Attribution is an opportunity upper bound, never a production skip decision.
; @opforge-owner: debug.amigaos.lazy_work
	.module debug.amigaos.lazy_work
	.cpu 68020
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as source
	.include "lazy_units_layout.i"
	.include "binary_block_span.i"
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.use debug.amigaos.memory_profile as memory_profile
.endif
.endif
.endif
	.pub
	.include "lazy_work_layout.i"
	.include "lazy_template_layout.i"
.ifdef OPFORGE_MACRO_PACKING_AUDIT
	.include "macro_packing_layout.i"
.endif
.ifdef OPFORGE_LAZY_CANDIDATES
	.include "candidate_audit_layout.i"
.endif
	.section code, kind=code
	.pub
; Every passive entry preserves registers/CCR/stack, including failure paths.
clear	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea State, a0
	move.l #WORK_STATE_BYTES/4, d0
next
	clr.l (a0)+
	subq.l #1, d0
	bne.w next
	lea State, a0
	move.l #CONTEXT_ORDINARY, WorkFields.PlanContext(a0)
	move.l #-1, WorkFields.LastCapture(a0)
	move.l #CAPTURE_BYTES, WorkFields.TopRows+Top.Metric(a0)
	move.l #PLAN_BIND, WorkFields.TopRows+TOP_BYTES+Top.Metric(a0)
	move.l #BOUND_BYTES, WorkFields.TopRows+TOP_BYTES*2+Top.Metric(a0)
	bsr.w templateClear
.ifdef OPFORGE_MACRO_PACKING_AUDIT
	bsr.w packingClear
.endif
.ifdef OPFORGE_LAZY_CANDIDATES
	bsr.w candidateClear
.endif
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; clear
getState	.block
	lea State, a0
	rts
	.bend  ; getState
; D0=metric byte offset,D1=actual work. Acquisition is counted separately.
count	.block
	move.w ccr, -(sp)
	movem.l d0-d2/a0-a1, -(sp)
	lea State, a1
	cmpi.l #VECTOR_BYTES, d0
	bhs.w invalidInput
	move.l d0, d2
	andi.l #3, d2
	bne.w invalidInput
	lea WorkFields.Totals(a1), a0
	bsr.w addScalar
	lea WorkFields.Sample(a1), a0
	bsr.w addScalar
	bra.w done
invalidInput
	ori.l #INVALID, WorkFields.Errors(a1)
done
	movem.l (sp)+, d0-d2/a0-a1
	move.w (sp)+, ccr
	rts
	.bend  ; count
; A0=four owned u8 counters,D0=index. Saturation invalidates attribution.
acquire	.block
	move.w ccr, -(sp)
	movem.l d0-d2/a0-a1, -(sp)
	lea State, a1
	cmpi.l #capture.WORK_COUNTER_BYTES, d0
	bhs.w invalidInput
	cmpi.b #255, 0(a0, d0.l)
	beq.w counterOverflow
	addq.b #1, 0(a0, d0.l)
	bra.w counted
counterOverflow
	ori.l #OVERFLOW, WorkFields.Errors(a1)
counted
	lsl.l #2, d0
	addi.l #CANDIDATE, d0
	moveq #1, d1
	lea WorkFields.Totals(a1), a0
	bsr.w addScalar
	bra.w done
invalidInput
	ori.l #INVALID, WorkFields.Errors(a1)
done
	movem.l (sp)+, d0-d2/a0-a1
	move.w (sp)+, ccr
	rts
	.bend  ; acquire
; A0=structural state,D1=physical invocation origin. Finish the prior invocation before selecting the deepest
; physical active unit. Generated units own output, never caller preparation.
sampleBegin	.block
	move.w ccr, -(sp)
	movem.l d0-d3/a0-a2, -(sp)
	move.l d1, d3
	bsr.w sampleEnd
	clr.l State+WorkFields.SampleOwner
	move.l Fields.Depth(a0), d2
	cmpi.l #LIMIT, d2
	bhi.w invalidInput
	movea.l a0, a1
	adda.l #Fields.Stack, a1
next
	tst.l d2
	beq.w done
	subq.l #1, d2
	move.l d2, d0
	lsl.l #2, d0
	move.l 0(a1, d0.l), d1
	cmpi.l #LIMIT, d1
	bhs.w invalidInput
	move.l d1, d0
	mulu.w #EVENT_BYTES, d0
	lea Fields.Events(a0), a2
	adda.l d0, a2
	tst.l Event.Generated(a2)
	bne.w next
	addq.l #1, d1
	move.l d1, State+WorkFields.SampleOwner
	cmp.l Event.Origin(a2), d3
	beq.w done
	subq.l #1, d1
	move.l d1, d0
	bsr.w rowAddress
	ori.l #MULTIFILE, Row.Reasons(a2)
	bra.w done
invalidInput
	ori.l #INVALID, State+WorkFields.Errors
done
	movem.l (sp)+, d0-d3/a0-a2
	move.w (sp)+, ccr
	rts
	.bend  ; sampleBegin
; Commit preparation remaining after a zero-output invocation or suspension.
; Unknown owner work stays only in Totals. Every sample is consumed once.
sampleEnd	.block
	move.w ccr, -(sp)
	bsr.w flushSample
	clr.l State+WorkFields.SampleOwner
	move.w (sp)+, ccr
	rts
	.bend  ; sampleEnd
	.priv
; Consume the vector while retaining the active invocation for more output.
flushSample	.block
	move.w ccr, -(sp)
	movem.l d0-d1/a0-a2, -(sp)
	move.l State+WorkFields.SampleOwner, d0
	beq.w clear
	subq.l #1, d0
	cmpi.l #LIMIT, d0
	bhs.w invalidInput
	bsr.w rowAddress
	move.l State+WorkFields.SampleReasons, d1
	or.l d1, Row.Reasons(a2)
	adda.w #Row.Values, a2
	lea State+WorkFields.Sample, a0
	bsr.w addVector
	bra.w clear
invalidInput
	ori.l #INVALID, State+WorkFields.Errors
clear
	clr.l State+WorkFields.SampleReasons
	lea State+WorkFields.Sample, a0
	moveq #METRICS-1, d0
next
	clr.l (a0)+
	dbra d0, next
	movem.l (sp)+, d0-d1/a0-a2
	move.w (sp)+, ccr
	rts
	.bend  ; flushSample
	.pub
boundBegin	.block
	move.w ccr, -(sp)
	move.l d1, State+WorkFields.BoundBefore
	move.w (sp)+, ccr
	rts
	.bend  ; boundBegin
; D0=status,D1=new used. Only committed successful delta is counted.
boundEnd	.block
	move.w ccr, -(sp)
	movem.l d0-d2, -(sp)
	tst.l d0
	bne.w done
	sub.l State+WorkFields.BoundBefore, d1
	bcs.w invalidInput
	moveq #BOUND_BYTES, d0
	jsr count
	move.l d1, d2
	moveq #1, d1
	jsr context
	bra.w done
invalidInput
	ori.l #INVALID, State+WorkFields.Errors
done
	movem.l (sp)+, d0-d2
	move.w (sp)+, ccr
	rts
	.bend  ; boundEnd
	; Generic context observer: category is supplied by its owning frontend.
; No borrowed pointers, template imports, or production decisions.
contextSet	.block
	move.w ccr, -(sp)
	cmpi.l #2, d0
	bhi.w invalidInput
	move.l d0, State+WorkFields.PlanContext
	bra.w done
invalidInput
	ori.l #INVALID, State+WorkFields.Errors
done
	move.w (sp)+, ccr
	rts
	.bend  ; contextSet
; D1=0 calls/1 committed bytes,D2=amount. Groups are call-time observations.
context	.block
	move.w ccr, -(sp)
	movem.l d0-d2/a0-a1, -(sp)
	lea State, a1
	cmpi.l #1, d1
	bhi.w invalidInput
	move.l WorkFields.PlanContext(a1), d0
	cmpi.l #2, d0
	bhi.w invalidInput
	tst.l WorkFields.SampleOwner(a1)
	beq.w group
	addq.l #3, d0
group
	lsl.l #3, d0
	lsl.l #2, d1
	add.l d1, d0
	move.l d2, d1
	lea WorkFields.Contexts(a1), a0
	bsr.w addScalar
	; context transforms D1 while selecting its slot; inventory receives the
	; original metric and amount from this routine's preservation frame.
	move.l 4(sp), d1
	move.l 8(sp), d2
	bsr.w templateCount
	bra.w done
invalidInput
	ori.l #INVALID, WorkFields.Errors(a1)
done
	movem.l (sp)+, d0-d2/a0-a1
	move.w (sp)+, ccr
	rts
	.bend  ; context
	; Successful template Def pool publication, D1=current,D2=previous.
templateDelta	.block
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	sub.l d2, d1
	bcs.w invalidInput
	moveq #TEMPLATE_BYTES, d0
	jsr count
	bra.w done
invalidInput
	ori.l #INVALID, State+WorkFields.Errors
done
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
	rts
	.bend  ; templateDelta
	.priv
; A0=vector,D0=offset,D1=amount,A1=State. CCR unspecified.
addScalar	.block
	move.l a0, -(sp)
	adda.l d0, a0
	add.l d1, (a0)
	bcc.w done
	move.l #-1, (a0)
	ori.l #OVERFLOW, WorkFields.Errors(a1)
done
	movea.l (sp)+, a0
	rts
	.bend  ; addScalar
; A0=source vector,A2=destination vector. Other registers preserved.
addVector	.block
	movem.l d0-d1/a0-a2, -(sp)
	lea State, a1
	moveq #METRICS-1, d0
next
	move.l (a0)+, d1
	add.l d1, (a2)+
	bcc.w advance
	move.l #-1, -4(a2)
	ori.l #OVERFLOW, WorkFields.Errors(a1)
advance
	dbra d0, next
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; addVector
; D0=event index,A2=row. Does not multiply a truncated word.
rowAddress	.block
	mulu.l #ROW_BYTES, d0
	lea State+WorkFields.Rows, a2
	adda.l d0, a2
	rts
	.bend  ; rowAddress
	.pub
; Called from shared flag observer after opening push, before closing pop.
; A0=prepared record,A1=structural State,D2=capture,D3=origin,D5=depth.
publication	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a1, a5
	movea.l a0, a4
	jsr effect
	lea State, a6
	move.l Fields.Count(a5), d0
	cmpi.l #LIMIT, d0
	bhi.w invalidInput
	move.l d0, WorkFields.Count(a6)
	move.l Fields.Depth(a5), d0
	beq.w outside
	cmpi.l #LIMIT, d0
	bhi.w invalidInput
	cmp.l Fields.Count(a5), d0
	bhi.w invalidInput
	subq.l #1, d0
	lsl.l #2, d0
	movea.l a5, a0
	adda.l #Fields.Stack, a0
	move.l 0(a0, d0.l), d0
	cmpi.l #LIMIT, d0
	bhs.w invalidInput
	move.l d0, d7
	bsr.w rowAddress
	movea.l a2, a3
	move.l d7, d0
	mulu.w #EVENT_BYTES, d0
	lea Fields.Events(a5), a0
	adda.l d0, a0
	; Only the generated event itself is deferred at authoritative selection.
	; Expansion depth does not invalidate its enclosing physical unit.
	cmp.l Event.Origin(a0), d3
	beq.w multi
	ori.l #MULTIFILE, Row.Reasons(a3)
multi
	cmp.l WorkFields.LastCapture(a6), d2
	bne.w opening
	cmp.l WorkFields.LastOrigin(a6), d3
	bne.w opening
	move.l WorkFields.LastOwner(a6), d0
	beq.w opening
	subq.l #1, d0
	cmp.l d7, d0
	beq.w opening
	; A generated child sharing the invocation capture is output provenance,
	; not competing physical ownership of the caller preparation.
	tst.l Event.Generated(a0)
	bne.w opening
	move.l d0, d1
	mulu.w #EVENT_BYTES, d1
	lea Fields.Events(a5), a1
	adda.l d1, a1
	tst.l Event.Generated(a1)
	bne.w opening
	ori.l #MULTI_OWNER, Row.Reasons(a3)
	bsr.w rowAddress
	ori.l #MULTI_OWNER, Row.Reasons(a2)
opening
	btst #source.FLAG_BLOCK_OPEN_BIT, source.RecordHeader.Flags(a4)
	beq.w sample
	move.l WorkFields.LastOwner(a6), Row.PreviousOwner(a3)
	lea WorkFields.PreviousSample(a6), a0
	lea Row.Previous(a3), a2
	bsr.w addVector
sample
	; A physical opening publishes the invocation's new owner. For ordinary
	; lines the begin snapshot remains valid even after the closing pop.
	tst.l d5
	bne.w preparation
	btst #source.FLAG_BLOCK_OPEN_BIT, source.RecordHeader.Flags(a4)
	beq.w preparation
	move.l d7, d0
	addq.l #1, d0
	move.l d0, WorkFields.SampleOwner(a6)
preparation
	; Preserve the preceding vector before consuming it. Its source owner is
	; unchanged on physical publications; generated rows remain conservative.
	lea WorkFields.Sample(a6), a0
	lea WorkFields.PreviousSample(a6), a1
	moveq #METRICS-1, d0
saveSample
	move.l (a0)+, (a1)+
	dbra d0, saveSample
	; Materialized bytes follow the actual innermost output owner.
	move.l WorkFields.Sample+PREPARED_COPY(a6), d1
	add.l d1, Row.Values+PREPARED_COPY(a3)
	bcc.w copyOwned
	ori.l #OVERFLOW, WorkFields.Errors(a6)
copyOwned
	clr.l WorkFields.Sample+PREPARED_COPY(a6)
	bsr.w flushSample
	addq.l #1, d7
	move.l d7, WorkFields.LastOwner(a6)
	bra.w saved
outside
	clr.l WorkFields.LastOwner(a6)
saved
	move.l d2, WorkFields.LastCapture(a6)
	move.l d3, WorkFields.LastOrigin(a6)
	; Outside publication has no structural destination, but its preceding
	; sample remains available for authoritative entry-label reconciliation.
	tst.l Fields.Depth(a5)
	bne.w observed
	lea WorkFields.Sample(a6), a0
	lea WorkFields.PreviousSample(a6), a1
	moveq #METRICS-1, d0
copy
	move.l (a0), (a1)+
	clr.l (a0)+
	dbra d0, copy
observed
	; Observe every active ancestor, including an include or reordered child.
	; Disjoint peer units can legitimately arrive in a different physical order.
	move.l Fields.Depth(a5), d7
	movea.l a5, a4
	adda.l #Fields.Stack, a4
ancestry
	tst.l d7
	beq.w done
	subq.l #1, d7
	move.l d7, d0
	lsl.l #2, d0
	move.l 0(a4, d0.l), d6
	cmpi.l #LIMIT, d6
	bhs.w invalidInput
	move.l d6, d0
	bsr.w rowAddress
	move.l Row.LastCapture(a2), d0
	cmp.l d0, d2
	bhs.w monotonic
	ori.l #NONMONOTONIC, Row.Reasons(a2)
monotonic
	move.l d2, Row.LastCapture(a2)
	move.l d6, d0
	mulu.w #EVENT_BYTES, d0
	lea Fields.Events(a5), a0
	adda.l d0, a0
	cmp.l Event.Origin(a0), d3
	beq.w ancestry
	ori.l #MULTIFILE, Row.Reasons(a2)
	bra.w ancestry
invalidInput
	ori.l #INVALID, WorkFields.Errors(a6)
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; publication
	.include "lazy_work_capture.i"
; A0=canonical normalized numeric record. Shared declaration layout only.
effect	.block
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	moveq #0, d0
	move.b source.RecordHeader.Length(a0), d0
	addq.l #1, d0
	cmpi.l #source.RECORD_HEADER_BYTES+1, d0
	blo.w done
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, source.RECORD_HEADER_BYTES(a0)
	beq.w shared
	cmpi.b #1, source.RECORD_HEADER_BYTES(a0)
	bhi.w done
	cmpi.l #source.DECLARATION_HEADER_BYTES, d0
	blo.w done
	moveq #0, d1
	move.b source.DeclarationRecord.Marker(a0), d1
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, d1
	beq.w shared
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, d1
	beq.w shared
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, d1
	beq.w shared
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, d1
	beq.w shared
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, d1
	beq.w shared
	cmpi.b #source.TOKEN_UPDATE_FIRST, d1
	blo.w done
	cmpi.b #source.TOKEN_UPDATE_LAST, d1
	bhi.w done
shared
	ori.l #SEMANTIC_EFFECT, State+WorkFields.SampleReasons
done
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
	rts
	.bend  ; effect
	.include "lazy_work_partition.i"
	.include "lazy_work_templates.i"
.ifdef OPFORGE_MACRO_PACKING_AUDIT
	.include "lazy_work_packing.i"
.endif
.ifdef OPFORGE_LAZY_CANDIDATES
	.include "candidate_audit_observer.i"
	.include "candidate_audit_clock.i"
	.include "candidate_audit_report.i"
.endif
	.include "lazy_work_template_report.i"
	.include "lazy_work_report.i"
	.endsection
	.section bss, kind=bss
	.priv
State	.res byte, WORK_STATE_BYTES
TemplateState	.res byte, TEMPLATE_STATE_BYTES
.ifdef OPFORGE_MACRO_PACKING_AUDIT
PackingState	.res byte, PACKING_STATE_BYTES
.endif
.ifdef OPFORGE_LAZY_CANDIDATES
CandidateState	.res byte, CANDIDATE_STATE_BYTES
.endif
	.endsection
	.endmodule
