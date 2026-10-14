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
	move.l #-1, WorkFields.LastCapture(a0)
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
sampleBegin	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	clr.l State+WorkFields.SampleReasons
	lea State+WorkFields.Sample, a0
	moveq #METRICS-1, d0
next
	clr.l (a0)+
	dbra d0, next
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; sampleBegin
boundBegin	.block
	move.w ccr, -(sp)
	move.l d1, State+WorkFields.BoundBefore
	move.w (sp)+, ccr
	rts
	.bend  ; boundBegin
; D0=status,D1=new used. Only committed successful delta is counted.
boundEnd	.block
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	tst.l d0
	bne.w done
	sub.l State+WorkFields.BoundBefore, d1
	bcs.w invalidInput
	moveq #BOUND_BYTES, d0
	jsr count
	bra.w done
invalidInput
	ori.l #INVALID, State+WorkFields.Errors
done
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
	rts
	.bend  ; boundEnd
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
	tst.l d5
	beq.w origin
	ori.l #GENERATED, Row.Reasons(a3)
origin
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
	move.l WorkFields.SampleReasons(a6), d0
	or.l d0, Row.Reasons(a3)
	lea WorkFields.Sample(a6), a0
	lea Row.Values(a3), a2
	bsr.w addVector
	addq.l #1, d7
	move.l d7, WorkFields.LastOwner(a6)
	bra.w saved
outside
	clr.l WorkFields.LastOwner(a6)
saved
	move.l d2, WorkFields.LastCapture(a6)
	move.l d3, WorkFields.LastOrigin(a6)
	lea WorkFields.Sample(a6), a0
	lea WorkFields.PreviousSample(a6), a1
	moveq #METRICS-1, d0
copy
	move.l (a0), (a1)+
	clr.l (a0)+
	dbra d0, copy
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
	.include "lazy_work_report.i"
	.endsection
	.section bss, kind=bss
	.priv
State	.res byte, WORK_STATE_BYTES
	.endsection
	.endmodule
