; Packing investigation only. No deferred production path, semantic decisions,
; borrowed pointers, or changes to token/capture/template records.
	.pub
; A0=borrowed scalar state for the component probe. Other registers/CCR preserved.
packingGetState	.block
	lea PackingState, a0
	rts
	.bend  ; packingGetState
; Reset the complete bounded observer. Passive, zero stack delta.
packingClear	.block
	.TEMPLATE_SAVE
	lea PackingState, a0
	move.l #PACKING_STATE_BYTES/4, d0
next
	clr.l (a0)+
	subq.l #1, d0
	bne.w next
	.TEMPLATE_RESTORE
	rts
	.bend  ; packingClear
; D0=handle,D1=pool,D2=session,D3=tokens,D4=lexeme bytes,D5=mode(0..2).
; Resolve ownership before executing the writer. Every passive API preserves
; D0-D7/A0-A6, CCR and SP, including invalid and overflow paths.
packingBegin	.block
	.TEMPLATE_SAVE
	move.l d3, d6
	move.l d4, d7
	move.l d5, d4
	bsr.w packingOwner
	move.l d0, d5
	lea PackingState, a6
	tst.l PackingFields.OverflowDepth(a6)
	bne.w overflowFrame
	cmpi.l #PACKING_DEPTH_LIMIT, PackingFields.Depth(a6)
	bhs.w overflowFrame
	; Even invalid input receives a sentinel frame, balancing its later end.
	move.l PackingFields.Depth(a6), d0
	mulu.w #PACKING_FRAME_BYTES, d0
	lea PackingFields.Frames(a6), a3
	adda.l d0, a3
	move.l #-1, PackingFrame.Owner(a3)
	clr.l PackingFrame.Sampled(a3)
	addq.l #1, PackingFields.Depth(a6)
	move.l PackingFields.Depth(a6), d0
	cmp.l PackingFields.MaxDepth(a6), d0
	bls.w maximum
	move.l d0, PackingFields.MaxDepth(a6)
maximum
	cmpi.l #2, d4
	bhi.w invalid
	tst.l d5
	bmi.w done
	move.l d5, PackingFrame.Owner(a3)
	move.l d5, d0
	bsr.w packingRowAddress
	moveq #PackingRow.Writes, d0
	moveq #1, d1
	bsr.w packingAccumulate
	moveq #PackingRow.Tokens, d0
	move.l d6, d1
	bsr.w packingAccumulate
	moveq #PackingRow.LexemeBytes, d0
	move.l d7, d1
	bsr.w packingAccumulate
	move.l d6, d1
	addq.l #1, d1
	bcs.w invalid
	add.l d1, d1
	bcs.w invalid
	moveq #PackingRow.MapBytes, d0
	bsr.w packingAccumulate
	cmpi.l #1, d4
	bne.w prefixDone
	moveq #PackingRow.PrefixWrites, d0
	moveq #1, d1
	bsr.w packingAccumulate
prefixDone
	addq.l #1, PackingFields.Started(a6)
	bcc.w started
	ori.l #PACKING_OVERFLOW, PackingFields.Errors(a6)
started
	bsr.w packingClockBegin
	bra.w done
overflowFrame
	ori.l #PACKING_NESTING, PackingFields.Errors(a6)
	addq.l #1, PackingFields.OverflowDepth(a6)
	bcc.w done
	move.l #-1, PackingFields.OverflowDepth(a6)
	ori.l #PACKING_OVERFLOW, PackingFields.Errors(a6)
	bra.w done
invalid
	ori.l #PACKING_BOUNDS, PackingFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; packingBegin
; D0=writer status (zero success),D1=packed bytes. Inclusive sampled windows
; end after the writer returns; nested windows may count overlapping ticks.
packingEnd	.block
	.TEMPLATE_SAVE
	move.l d0, d6
	move.l d1, d7
	lea PackingState, a6
	tst.l PackingFields.OverflowDepth(a6)
	beq.w normal
	subq.l #1, PackingFields.OverflowDepth(a6)
	bra.w done
normal
	tst.l PackingFields.Depth(a6)
	beq.w mismatch
	subq.l #1, PackingFields.Depth(a6)
	move.l PackingFields.Depth(a6), d0
	mulu.w #PACKING_FRAME_BYTES, d0
	lea PackingFields.Frames(a6), a3
	adda.l d0, a3
	move.l PackingFrame.Owner(a3), d0
	bmi.w done
	cmpi.l #PACKING_ROW_LIMIT, d0
	bhs.w invalid
	bsr.w packingRowAddress
	bsr.w packingClockEnd
	tst.l d6
	bne.w failed
	moveq #PackingRow.PackedBytes, d0
	move.l d7, d1
	bsr.w packingAccumulate
	bra.w completed
failed
	moveq #PackingRow.Failures, d0
	moveq #1, d1
	bsr.w packingAccumulate
completed
	addq.l #1, PackingFields.Completed(a6)
	bcc.w done
	ori.l #PACKING_OVERFLOW, PackingFields.Errors(a6)
	bra.w done
mismatch
	ori.l #PACKING_NESTING, PackingFields.Errors(a6)
	bra.w done
invalid
	ori.l #PACKING_BOUNDS, PackingFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; packingEnd
; D0=metric byte offset,D1=amount. Only Names..CaptureBinds are accepted.
; With no active writer, accumulate Outside alone, never Gross or row zero.
packingCount	.block
	.TEMPLATE_SAVE
	lea PackingState, a6
	cmpi.l #PackingRow.Names, d0
	blo.w invalid
	cmpi.l #PackingRow.CaptureBinds, d0
	bhi.w invalid
	move.l d0, d2
	andi.l #3, d2
	bne.w invalid
	tst.l PackingFields.OverflowDepth(a6)
	bne.w done
	move.l PackingFields.Depth(a6), d2
	beq.w outside
	cmpi.l #PACKING_DEPTH_LIMIT, d2
	bhi.w invalid
	subq.l #1, d2
	mulu.w #PACKING_FRAME_BYTES, d2
	lea PackingFields.Frames(a6), a3
	adda.l d2, a3
	move.l d0, d4
	move.l PackingFrame.Owner(a3), d0
	bmi.w done
	cmpi.l #PACKING_ROW_LIMIT, d0
	bhs.w invalid
	bsr.w packingRowAddress
	move.l d4, d0
	bsr.w packingAccumulate
	bra.w done
outside
	lea PackingFields.Outside(a6), a0
	adda.l d0, a0
	bsr.w packingAdd
	bra.w done
invalid
	ori.l #PACKING_BOUNDS, PackingFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; packingCount
; D0=handle,D1=pool,D2=session,D3=final committed body record bytes.
; Called only after a successful store, while its owning definition is open.
packingStored	.block
	.TEMPLATE_SAVE
	move.l d3, d7
	tst.l d0
	beq.w invalid
	bsr.w packingOwner
	tst.l d0
	bmi.w done
	lea PackingState, a6
	bsr.w packingRowAddress
	moveq #PackingRow.StoredRecords, d0
	moveq #1, d1
	bsr.w packingAccumulate
	moveq #PackingRow.StoredBytes, d0
	move.l d7, d1
	bsr.w packingAccumulate
	bra.w done
invalid
	ori.l #PACKING_IDENTITY, PackingState+PackingFields.Errors
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; packingStored
	.priv
; D0=handle,D1=pool,D2=session. D0=ordinal (0 ordinary), -1 invalid.
; Clobbers D2-D3/A0-A2/A6/CCR; owner identity must still be open.
packingOwner	.block
	tst.l d0
	beq.w done
	move.l d0, -(sp)
	move.l d1, -(sp)
	move.l d2, d3
	lea TemplateState, a6
	bsr.w templateFind
	tst.l d0
	beq.w missing
	move.l (sp), d2
	cmp.l TemplateDefinition.Pool(a1), d2
	bne.w missing
	move.l 4(sp), d2
	cmp.l TemplateDefinition.Handle(a1), d2
	bne.w missing
	tst.l TemplateDefinition.Closed(a1)
	bne.w missing
	adda.w #8, sp
	bra.w done
missing
	adda.w #8, sp
	ori.l #PACKING_IDENTITY, PackingState+PackingFields.Errors
	moveq #-1, d0
done
	rts
	.bend  ; packingOwner
; D0=bounded numeric ordinal, A2=row. Clobbers D0/CCR.
packingRowAddress	.block
	mulu.l #PACKING_ROW_BYTES, d0
	lea PackingState, a2
	adda.l #PackingFields.Rows, a2
	adda.l d0, a2
	rts
	.bend  ; packingRowAddress
; D0=metric byte offset,D1=amount,A2=row,A6=packing state. Independent gross
; and row updates, saturating scalars with explicit report invalidation.
packingAccumulate	.block
	lea PackingFields.Gross(a6), a0
	adda.l d0, a0
	bsr.w packingAdd
	movea.l a2, a0
	adda.l d0, a0
	bsr.w packingAdd
	rts
	.bend  ; packingAccumulate
; A0=scalar,D1=amount,A6=packing state; D2/CCR clobbered.
packingAdd	.block
	move.l (a0), d2
	add.l d1, d2
	bcc.w store
	moveq #-1, d2
	ori.l #PACKING_OVERFLOW, PackingFields.Errors(a6)
store
	move.l d2, (a0)
	rts
	.bend  ; packingAdd
; A3=frame,A6=packing state. Count-only images have no clock calls.
packingClockBegin	.block
.ifdef OPFORGE_PACKING_TIMING
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.l #OPFORGE_PACKING_SAMPLE_PERIOD, d0
	cmpi.l #64, d0
	beq.w period
	cmpi.l #128, d0
	beq.w period
	cmpi.l #256, d0
	bne.w invalid
period
	move.l #OPFORGE_PACKING_PHASE_SEED, d0
	cmpi.l #255, d0
	bhi.w invalid
	move.l PackingFields.Started(a6), d0
	subq.l #1, d0
	addi.l #OPFORGE_PACKING_PHASE_SEED, d0
	andi.l #OPFORGE_PACKING_SAMPLE_PERIOD-1, d0
	bne.w done
	lea PackingFrame.StartHigh(a3), a0
	jsr memory_profile.readClock
	beq.w invalid
	tst.l PackingFields.Frequency(a6)
	beq.w frequency
	cmp.l PackingFields.Frequency(a6), d0
	bne.w invalid
frequency
	move.l d0, PackingFields.Frequency(a6)
	move.l #1, PackingFrame.Sampled(a3)
	bra.w done
invalid
	ori.l #PACKING_CLOCK, PackingFields.Errors(a6)
done
.endif
.endif
.endif
.endif
	rts
	.bend  ; packingClockBegin
; A2=row,A3=frame,A6=state. Accumulate inclusive 64-bit elapsed ticks.
packingClockEnd	.block
.ifdef OPFORGE_PACKING_TIMING
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	tst.l PackingFrame.Sampled(a3)
	beq.w done
	lea PackingFields.ClockEndHigh(a6), a0
	jsr memory_profile.readClock
	beq.w invalid
	cmp.l PackingFields.Frequency(a6), d0
	bne.w invalid
	move.l PackingFields.ClockEndLow(a6), d4
	move.l PackingFields.ClockEndHigh(a6), d5
	sub.l PackingFrame.StartLow(a3), d4
	move.l PackingFrame.StartHigh(a3), d0
	subx.l d0, d5
	bcs.w invalid
	moveq #PackingRow.Samples, d0
	moveq #1, d1
	bsr.w packingAccumulate
	lea PackingFields.Gross(a6), a0
	bsr.w packingAddTicks
	movea.l a2, a0
	bsr.w packingAddTicks
	bra.w done
invalid
	ori.l #PACKING_CLOCK, PackingFields.Errors(a6)
done
.endif
.endif
.endif
.endif
	rts
	.bend  ; packingClockEnd
; A0=vector,D5:D4=elapsed,A6=state. Unsigned 64-bit overflow invalidates.
packingAddTicks	.block
	move.l PackingRow.TicksHigh(a0), d0
	add.l d4, PackingRow.TicksLow(a0)
	addx.l d5, d0
	bcs.w overflow
	move.l d0, PackingRow.TicksHigh(a0)
	bra.w done
overflow
	move.l #-1, PackingRow.TicksHigh(a0)
	move.l #-1, PackingRow.TicksLow(a0)
	ori.l #PACKING_OVERFLOW, PackingFields.Errors(a6)
done
	rts
	.bend  ; packingAddTicks
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
PACKING_PROGRESS	.macro phase, first, second, third
	move.l .phase, d0
	move.l .first, d1
	move.l .second, d2
	move.l .third, d3
	movea.l a4, a0
	jsr memory_profile.progress
.endmacro
	.priv
; A0=DOS base. Terminal report, schema 1; phase keys are decimal constants.
; Each vector is six triples in PackingRow order. Nested inclusive samples
; overlap; sampled time is observed packing work, never inferred avoidable time.
packingReport	.block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea PackingState, a6
	bsr.w packingVerify
	.PACKING_PROGRESS #500000, #1, PackingFields.Errors(a6), PackingFields.Depth(a6)
	.PACKING_PROGRESS #500001, #PACKING_ROW_LIMIT, #PACKING_ROW_BYTES, #PACKING_STATE_BYTES
	.PACKING_PROGRESS #500002, #OPFORGE_PACKING_SAMPLE_PERIOD, #OPFORGE_PACKING_PHASE_SEED, PackingFields.Frequency(a6)
	moveq #0, d5
.ifdef OPFORGE_PACKING_TIMING
	moveq #1, d5
.endif
	.PACKING_PROGRESS #500003, PackingFields.Completed(a6), PackingFields.MaxDepth(a6), d5
	.PACKING_PROGRESS #500004, PackingFields.UsedRows(a6), #0, #0
	move.l #500010, d4
	lea PackingFields.Gross(a6), a5
	bsr.w packingVector
	move.l #500020, d4
	lea PackingFields.Outside(a6), a5
	bsr.w packingVector
	lea PackingState, a3
	adda.l #PackingFields.Rows, a3
	moveq #0, d7
row
	bsr.w packingNonempty
	tst.l d0
	beq.w advance
	move.l d7, d4
	mulu.w #6, d4
	addi.l #500100, d4
	movea.l a3, a5
	bsr.w packingVector
advance
	adda.w #PACKING_ROW_BYTES, a3
	addq.l #1, d7
	cmpi.l #PACKING_ROW_LIMIT, d7
	blo.w row
	.TEMPLATE_RESTORE
	rts
	.bend  ; packingReport
; A5=vector,D4=phase,A4=DOS base; preserves row loop registers.
packingVector	.block
	moveq #5, d5
next
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d5, next
	rts
	.bend  ; packingVector
; A3=row. D0=OR of fields; D1/A0/CCR clobbered.
packingNonempty	.block
	movea.l a3, a0
	moveq #0, d0
	moveq #PACKING_ROW_BYTES/4-1, d1
next
	or.l (a0)+, d0
	dbra d1, next
	rts
	.bend  ; packingNonempty
; Sum fixed rows independently and compare Gross, treating tick halves as u64.
; Reject nonempty undefined ordinals and incomplete inventory identities/ranges.
packingVerify	.block
	tst.l PackingFields.Depth(a6)
	bne.w nesting
	tst.l PackingFields.OverflowDepth(a6)
	beq.w inventory
nesting
	ori.l #PACKING_NESTING, PackingFields.Errors(a6)
inventory
	cmpi.l #TEMPLATE_DEFINITION_LIMIT, TemplateState+TemplateFields.Count
	bls.w countBounded
	ori.l #PACKING_IDENTITY, PackingFields.Errors(a6)
countBounded
	tst.l TemplateState+TemplateFields.Errors
	beq.w allocate
	ori.l #PACKING_IDENTITY, PackingFields.Errors(a6)
allocate
	suba.w #PACKING_ROW_BYTES, sp
	movea.l sp, a5
	movea.l a5, a0
	moveq #PACKING_ROW_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	clr.l PackingFields.UsedRows(a6)
	lea PackingState, a3
	adda.l #PackingFields.Rows, a3
	moveq #0, d7
row
	bsr.w packingNonempty
	tst.l d0
	beq.w advance
	addq.l #1, PackingFields.UsedRows(a6)
	tst.l d7
	beq.w sum
	cmp.l TemplateState+TemplateFields.Count, d7
	bhi.w identity
	cmpi.l #TEMPLATE_DEFINITION_LIMIT, d7
	bhi.w identity
	move.l d7, d0
	subq.l #1, d0
	mulu.w #TEMPLATE_DEFINITION_BYTES, d0
	lea TemplateState, a0
	adda.l #TemplateFields.Rows, a0
	adda.l d0, a0
	tst.l TemplateDefinition.Epoch(a0)
	beq.w identity
	tst.l TemplateDefinition.Handle(a0)
	beq.w identity
	cmpi.l #TEMPLATE_DEFINITION_LIMIT, TemplateDefinition.Handle(a0)
	bhi.w identity
	cmpi.l #1, TemplateDefinition.Pool(a0)
	bhi.w identity
	tst.l TemplateDefinition.Closed(a0)
	beq.w identity
	move.l TemplateDefinition.Last(a0), d0
	cmp.l TemplateDefinition.First(a0), d0
	blo.w identity
	bra.w sum
identity
	ori.l #PACKING_IDENTITY, PackingFields.Errors(a6)
sum
	moveq #0, d6
scalar
	cmpi.l #PackingRow.TicksHigh, d6
	beq.w nextScalar
	cmpi.l #PackingRow.TicksLow, d6
	beq.w ticks
	move.l 0(a5, d6.l), d0
	add.l 0(a3, d6.l), d0
	bcs.w overflow
	move.l d0, 0(a5, d6.l)
	bra.w nextScalar
ticks
	move.l PackingRow.TicksHigh(a5), d0
	move.l PackingRow.TicksHigh(a3), d1
	move.l PackingRow.TicksLow(a3), d2
	add.l d2, PackingRow.TicksLow(a5)
	addx.l d1, d0
	bcs.w overflow
	move.l d0, PackingRow.TicksHigh(a5)
	bra.w nextScalar
overflow
	ori.l #PACKING_OVERFLOW, PackingFields.Errors(a6)
nextScalar
	addq.l #4, d6
	cmpi.l #PACKING_ROW_BYTES, d6
	blo.w scalar
advance
	adda.w #PACKING_ROW_BYTES, a3
	addq.l #1, d7
	cmpi.l #PACKING_ROW_LIMIT, d7
	blo.w row
	lea PackingFields.Gross(a6), a0
	moveq #PACKING_ROW_BYTES/4-1, d2
compare
	move.l (a5)+, d0
	cmp.l (a0)+, d0
	beq.w nextCompare
	ori.l #PACKING_RECONCILE, PackingFields.Errors(a6)
nextCompare
	dbra d2, compare
	move.l PackingFields.Completed(a6), d0
	cmp.l PackingFields.Gross+PackingRow.Writes(a6), d0
	beq.w done
	ori.l #PACKING_RECONCILE, PackingFields.Errors(a6)
done
	adda.w #PACKING_ROW_BYTES, sp
	rts
	.bend  ; packingVerify
.endif
.endif
.endif
