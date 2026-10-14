; Accepted invocations are the consumer boundary, not lookups or successful
; emission. Owner adapters exclude previews and suspended include parents.
; All retained identities are scalars. Each owner's handles expire at finish;
; rows remain observable across epochs. Failed/open definitions stay explicit.
; Every public passive operation preserves D0-D7/A0-A6, CCR and stack depth.
; getTemplates explicitly returns A0 as its sole exception.
TEMPLATE_SAVE .macro
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
.endmacro
TEMPLATE_RESTORE .macro
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
.endmacro
; Internal helper. A6=observer; operands must not depend on A0/D2 or SP.
TEMPLATE_ADD .macro field, amount
	lea .field(a6), a0
	move.l .amount, d2
	bsr.w templateAdd
.endmacro
	.pub
; A0=current scalar inventory; other registers, CCR and stack preserved.
; Borrowed read-only view expires at the next observer operation.
getTemplates .block
	lea TemplateState, a0
	rts
	.bend  ; getTemplates
; A0=borrowed writable owner scalar. Begin one numeric handle lifetime;
; write its session ID, retaining earlier rows and concurrent owners' maps.
templateBegin .block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea TemplateState, a6
	clr.l (a4)
	lea TemplateFields.Owners(a6), a2
	moveq #TEMPLATE_OWNER_LIMIT-1, d0
slot
	tst.l TemplateOwnerSlot.Active(a2)
	beq.w available
	adda.l #TEMPLATE_OWNER_BYTES, a2
	dbra d0, slot
	ori.l #TEMPLATE_EXHAUSTED, TemplateFields.Errors(a6)
	bra.w done
available
	.TEMPLATE_ADD TemplateFields.Epoch, #1
	move.l TemplateFields.Epoch(a6), TemplateOwnerSlot.Epoch(a2)
	move.l TemplateFields.Epoch(a6), (a4)
	move.l #1, TemplateOwnerSlot.Active(a2)
	.TEMPLATE_ADD TemplateFields.Active, #1
	lea TemplateOwnerSlot.Maps(a2), a0
	move.l #TEMPLATE_DEFINITION_LIMIT*2, d0
clearMaps
	clr.l (a0)+
	subq.l #1, d0
	bne.w clearMaps
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templateBegin
	.priv
; Compare independent committed totals. The report decoder also sums detail
; rows, so neither a saturated scalar nor a missing edge can imply completeness.
templateVerify .block
	move.l TemplateFields.RowCalls(a6), d0
	cmp.l TemplateFields.Calls(a6), d0
	bne.w mismatch
	move.l TemplateFields.EdgeCalls(a6), d0
	cmp.l TemplateFields.Calls(a6), d0
	bne.w mismatch
	move.l TemplateFields.EdgeDefaults(a6), d0
	cmp.l TemplateFields.DefaultCalls(a6), d0
	bne.w mismatch
	move.l TemplateFields.EdgeSlots(a6), d0
	cmp.l TemplateFields.DefaultSlots(a6), d0
	bne.w mismatch
	move.l TemplateFields.RowBinds(a6), d0
	add.l TemplateFields.UnattributedCalls(a6), d0
	bcs.w mismatch
	cmp.l TemplateFields.BindingCalls(a6), d0
	bne.w mismatch
	move.l TemplateFields.RowBoundBytes(a6), d0
	add.l TemplateFields.UnattributedBytes(a6), d0
	bcs.w mismatch
	cmp.l TemplateFields.BindingBytes(a6), d0
	bne.w mismatch
	rts
mismatch
	ori.l #TEMPLATE_RECONCILE, TemplateFields.Errors(a6)
	rts
	.bend  ; templateVerify

	.pub
; A0=TemplateDefinePacket, after authoritative Count/index publication.
templateDefine .block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea TemplateState, a6
	tst.l TemplateFields.Active(a6)
	beq.w invalidInput
	move.l TemplateDefinePacket.Handle(a4), d0
	move.l TemplateDefinePacket.Pool(a4), d1
	move.l TemplateDefinePacket.Session(a4), d3
	bsr.w templateMap
	bne.w invalidInput
	tst.l (a2)
	bne.w invalidInput
	move.l TemplateDefinePacket.Kind(a4), d2
	tst.l d1
	beq.w internalKind
	cmpi.l #2, d2
	bne.w invalidInput
	bra.w kindReady
internalKind
	cmpi.l #1, d2
	bhi.w invalidInput
kindReady
	move.l TemplateFields.Count(a6), d0
	cmpi.l #TEMPLATE_DEFINITION_LIMIT, d0
	bhs.w tableFull
	mulu.w #TEMPLATE_DEFINITION_BYTES, d0
	movea.l a6, a3
	adda.l #TemplateFields.Rows, a3
	adda.l d0, a3
	move.l TemplateDefinePacket.Session(a4), TemplateDefinition.Epoch(a3)
	move.l TemplateDefinePacket.Handle(a4), TemplateDefinition.Handle(a3)
	move.l TemplateDefinePacket.Pool(a4), TemplateDefinition.Pool(a3)
	move.l TemplateDefinePacket.Kind(a4), TemplateDefinition.Kind(a3)
	move.l TemplateDefinePacket.Name(a4), TemplateDefinition.Name(a3)
	move.l TemplateDefinePacket.Origin(a4), TemplateDefinition.Origin(a3)
	move.l TemplateDefinePacket.Line(a4), TemplateDefinition.Line(a3)
	move.l TemplateDefinePacket.HeaderPlan(a4), TemplateDefinition.HeaderPlan(a3)
	move.l TemplateDefinePacket.First(a4), TemplateDefinition.First(a3)
	move.l TemplateDefinePacket.First(a4), TemplateDefinition.Last(a3)
	move.l TemplateDefinePacket.DefBytes(a4), TemplateDefinition.DefBytes(a3)
	move.l TemplateDefinePacket.DefaultBytes(a4), TemplateDefinition.DefaultBytes(a3)
	move.l TemplateDefinePacket.TextBytes(a4), TemplateDefinition.TextBytes(a3)
	addq.l #1, TemplateFields.Count(a6)
	move.l TemplateFields.Count(a6), (a2)
	bra.w done
tableFull
	ori.l #TEMPLATE_EXHAUSTED, TemplateFields.Errors(a6)
	bra.w done
invalidInput
	ori.l #TEMPLATE_INVALID, TemplateFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templateDefine

; D0=handle,D1=pool,D2=final BODY offset,D3=session. Empty bodies are valid.
templateClose .block
	.TEMPLATE_SAVE
	move.l d2, d7
	lea TemplateState, a6
	bsr.w templateFind
	beq.w invalidInput
	tst.l TemplateDefinition.Closed(a1)
	bne.w invalidInput
	cmp.l TemplateDefinition.First(a1), d7
	blo.w invalidInput
	move.l d7, TemplateDefinition.Last(a1)
	move.l #1, TemplateDefinition.Closed(a1)
	.TEMPLATE_ADD TemplateFields.Closed, #1
	bra.w done
invalidInput
	ori.l #TEMPLATE_INVALID, TemplateFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templateClose

; D0=handle or zero to clear,D1=pool,D2=session.
templateOwner .block
	.TEMPLATE_SAVE
	lea TemplateState, a6
	clr.l TemplateFields.OwnerRow(a6)
	tst.l d0
	beq.w done
	move.l d2, d3
	bsr.w templateFind
	beq.w invalidInput
	move.l d0, TemplateFields.OwnerRow(a6)
	bra.w done
invalidInput
	ori.l #TEMPLATE_INVALID, TemplateFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templateOwner

; D0=handle,D1=pool,D2=source header line,D3=session; external owner adapter.
templateLine .block
	.TEMPLATE_SAVE
	move.l d2, d7
	lea TemplateState, a6
	bsr.w templateFind
	beq.w invalidInput
	move.l d7, TemplateDefinition.Line(a1)
	bra.w done
invalidInput
	ori.l #TEMPLATE_INVALID, TemplateFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templateLine

; D1=0 binding calls/1 committed bound bytes,D2=amount. Observe only the
; existing definition context. Plan bytes are preparation deltas, never unique
; retained handles. A missing owner remains unattributed and invalidates proof.
templateCount .block
	.TEMPLATE_SAVE
	lea TemplateState, a6
	tst.l State+WorkFields.PlanContext
	bne.w done
	cmpi.l #1, d1
	bhi.w invalidInput
	move.l d2, d7
	move.l d1, d6
	lea TemplateFields.BindingCalls(a6), a0
	lsl.l #2, d1
	adda.l d1, a0
	bsr.w templateAdd
	move.l TemplateFields.OwnerRow(a6), d0
	beq.w unattributed
	cmp.l TemplateFields.Count(a6), d0
	bhi.w invalidInput
	subq.l #1, d0
	mulu.w #TEMPLATE_DEFINITION_BYTES, d0
	movea.l a6, a1
	adda.l #TemplateFields.Rows, a1
	adda.l d0, a1
	move.l TemplateDefinition.Epoch(a1), d3
	bsr.w templateSession
	beq.w invalidInput
	lea TemplateDefinition.PlanBinds(a1), a0
	move.l d6, d0
	lsl.l #2, d0
	adda.l d0, a0
	move.l d7, d2
	bsr.w templateAdd
	lea TemplateFields.RowBinds(a6), a0
	adda.l d0, a0
	move.l d7, d2
	bsr.w templateAdd
	bra.w done
unattributed
	lea TemplateFields.UnattributedCalls(a6), a0
	move.l d6, d0
	lsl.l #2, d0
	adda.l d0, a0
	move.l d7, d2
	bsr.w templateAdd
invalidInput
	ori.l #TEMPLATE_INVALID, TemplateFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templateCount

; A0=TemplateCallPacket, only after queueCall increments production Depth.
; Edges are deduplicated by exact caller row, callee row and source origin.
; Direct physical/include callers use row zero, never a guessed suspended frame.
templateCall .block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea TemplateState, a6
	move.l TemplateCallPacket.DefaultMask(a4), d7
	move.l d7, d0
	andi.l #$fffffe00, d0
	bne.w invalidInput
	moveq #0, d5
	moveq #8, d0
countSlots
	btst d0, d7
	beq.w nextSlot
	addq.l #1, d5
nextSlot
	dbra d0, countSlots
	.TEMPLATE_ADD TemplateFields.Calls, #1
	.TEMPLATE_ADD TemplateFields.DefaultSlots, d5
	tst.l d7
	beq.w callee
	.TEMPLATE_ADD TemplateFields.DefaultCalls, #1
callee
	move.l TemplateCallPacket.Handle(a4), d0
	move.l TemplateCallPacket.Pool(a4), d1
	move.l TemplateCallPacket.Session(a4), d3
	bsr.w templateFind
	beq.w invalidInput
	movea.l a1, a3
	move.l d0, d6
	tst.l TemplateDefinition.Closed(a3)
	beq.w invalidInput
	moveq #0, d4
	move.l TemplateCallPacket.ParentHandle(a4), d0
	beq.w callerReady
	move.l TemplateCallPacket.ParentPool(a4), d1
	move.l TemplateCallPacket.Session(a4), d3
	bsr.w templateFind
	beq.w invalidInput
	move.l d0, d4
callerReady
	tst.l TemplateDefinition.Calls(a3)
	bne.w observed
	move.l d4, TemplateDefinition.FirstCaller(a3)
	move.l TemplateCallPacket.Origin(a4), TemplateDefinition.FirstOrigin(a3)
	move.l TemplateCallPacket.Line(a4), TemplateDefinition.FirstLine(a3)
observed
	lea TemplateDefinition.Calls(a3), a0
	moveq #1, d2
	bsr.w templateAdd
	.TEMPLATE_ADD TemplateFields.RowCalls, #1
	lea TemplateDefinition.DefaultSlots(a3), a0
	move.l d5, d2
	bsr.w templateAdd
	tst.l d7
	beq.w nested
	lea TemplateDefinition.DefaultCalls(a3), a0
	moveq #1, d2
	bsr.w templateAdd
nested
	tst.l d4
	beq.w edge
	lea TemplateDefinition.NestedCalls(a3), a0
	moveq #1, d2
	bsr.w templateAdd
	.TEMPLATE_ADD TemplateFields.NestedCalls, #1
edge
	move.l d4, d0
	eor.l d6, d0
	move.l TemplateCallPacket.Origin(a4), d3
	eor.l d3, d0
	andi.l #TEMPLATE_BUCKETS-1, d0
	lsl.l #2, d0
	movea.l a6, a2
	adda.l #TemplateFields.Buckets, a2
	adda.l d0, a2
	move.l (a2), d0
findEdge
	tst.l d0
	beq.w newEdge
	cmp.l TemplateFields.EdgeCount(a6), d0
	bhi.w invalidInput
	subq.l #1, d0
	mulu.w #TEMPLATE_EDGE_BYTES, d0
	movea.l a6, a5
	adda.l #TemplateFields.Edges, a5
	adda.l d0, a5
	cmp.l TemplateEdge.Caller(a5), d4
	bne.w nextEdge
	cmp.l TemplateEdge.Callee(a5), d6
	bne.w nextEdge
	move.l TemplateCallPacket.Origin(a4), d0
	cmp.l TemplateEdge.Origin(a5), d0
	beq.w edgeReady
nextEdge
	move.l TemplateEdge.Next(a5), d0
	bra.w findEdge
newEdge
	move.l TemplateFields.EdgeCount(a6), d0
	cmpi.l #TEMPLATE_EDGE_LIMIT, d0
	bhs.w tableFull
	mulu.w #TEMPLATE_EDGE_BYTES, d0
	movea.l a6, a5
	adda.l #TemplateFields.Edges, a5
	adda.l d0, a5
	move.l d4, TemplateEdge.Caller(a5)
	move.l d6, TemplateEdge.Callee(a5)
	move.l TemplateCallPacket.Origin(a4), TemplateEdge.Origin(a5)
	move.l TemplateCallPacket.Line(a4), TemplateEdge.Line(a5)
	move.l (a2), TemplateEdge.Next(a5)
	addq.l #1, TemplateFields.EdgeCount(a6)
	move.l TemplateFields.EdgeCount(a6), (a2)
edgeReady
	lea TemplateEdge.Calls(a5), a0
	moveq #1, d2
	bsr.w templateAdd
	.TEMPLATE_ADD TemplateFields.EdgeCalls, #1
	lea TemplateEdge.DefaultSlots(a5), a0
	move.l d5, d2
	bsr.w templateAdd
	.TEMPLATE_ADD TemplateFields.EdgeSlots, d5
	tst.l d7
	beq.w done
	lea TemplateEdge.DefaultCalls(a5), a0
	moveq #1, d2
	bsr.w templateAdd
	.TEMPLATE_ADD TemplateFields.EdgeDefaults, #1
	bra.w done
tableFull
	ori.l #TEMPLATE_EXHAUSTED, TemplateFields.Errors(a6)
	bra.w done
invalidInput
	ori.l #TEMPLATE_INVALID, TemplateFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templateCall

; A0=TemplatePoolsPacket before releases. Reconcile only the selected owner;
; duplicate empty finishes are harmless. Open rows invalidate completeness.
; The plan residual is the whole Used extent: no inferred handle attribution.
templatePools .block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea TemplateState, a6
	move.l TemplatePoolsPacket.Session(a4), d3
	bsr.w templateSession
	bne.w active
	cmp.l TemplateFields.Epoch(a6), d3
	bhi.w invalidInput
	adda.w #TemplatePoolsPacket.InternalBytes, a4
	moveq #TEMPLATE_POOL_BYTES/4-1, d0
emptyFinish
	tst.l (a4)+
	bne.w invalidInput
	dbra d0, emptyFinish
	bra.w done
active
	suba.w #TEMPLATE_POOL_BYTES+4, sp
	move.l d3, TEMPLATE_POOL_BYTES(sp)
	adda.w #TemplatePoolsPacket.InternalBytes, a4
	movea.l sp, a5
	movea.l a5, a0
	moveq #TEMPLATE_POOL_BYTES/4-1, d0
clearPayload
	clr.l (a0)+
	dbra d0, clearPayload
	movea.l a6, a3
	adda.l #TemplateFields.Rows, a3
	move.l TemplateFields.Count(a6), d7
row
	tst.l d7
	beq.w reconcile
	move.l TemplateDefinition.Epoch(a3), d0
	cmp.l TEMPLATE_POOL_BYTES(sp), d0
	bne.w nextRow
	move.l TemplateDefinition.Pool(a3), d0
	lsl.l #2, d0
	movea.l a5, a0
	adda.l d0, a0
	move.l TemplateDefinition.DefBytes(a3), d2
	bsr.w templateAdd
	lea TemplatePoolValues.DefaultBytes(a5), a0
	move.l TemplateDefinition.DefaultBytes(a3), d2
	bsr.w templateAdd
	lea TemplatePoolValues.TextBytes(a5), a0
	move.l TemplateDefinition.TextBytes(a3), d2
	bsr.w templateAdd
	tst.l TemplateDefinition.Closed(a3)
	bne.w body
	ori.l #TEMPLATE_UNCLOSED, TemplateFields.Errors(a6)
	.TEMPLATE_ADD TemplateFields.OpenRows, #1
	bra.w nextRow
body
	move.l TemplateDefinition.Last(a3), d2
	sub.l TemplateDefinition.First(a3), d2
	bcs.w badBody
	lea TemplatePoolValues.BodyBytes(a5), a0
	bsr.w templateAdd
	bra.w nextRow
badBody
	ori.l #TEMPLATE_INVALID, TemplateFields.Errors(a6)
nextRow
	adda.w #TEMPLATE_DEFINITION_BYTES, a3
	subq.l #1, d7
	bra.w row
reconcile
	moveq #0, d6
pool
	move.l 0(a4, d6.l), d4
	lea TemplateFields.Pools(a6), a0
	adda.l d6, a0
	move.l d4, d2
	bsr.w templateAdd
	move.l 0(a5, d6.l), d5
	lea TemplateFields.Payload(a6), a0
	adda.l d6, a0
	move.l d5, d2
	bsr.w templateAdd
	sub.l d5, d4
	bcs.w badReconcile
	lea TemplateFields.Residual(a6), a0
	adda.l d6, a0
	move.l d4, d2
	bsr.w templateAdd
	bra.w nextPool
badReconcile
	ori.l #TEMPLATE_RECONCILE, TemplateFields.Errors(a6)
nextPool
	addq.l #4, d6
	cmpi.l #TEMPLATE_POOL_BYTES, d6
	blo.w pool
	move.l TEMPLATE_POOL_BYTES(sp), d3
	bsr.w templateSession
	clr.l TemplateOwnerSlot.Active(a2)
	subq.l #1, TemplateFields.Active(a6)
	move.l TemplateFields.OwnerRow(a6), d0
	beq.w released
	subq.l #1, d0
	mulu.w #TEMPLATE_DEFINITION_BYTES, d0
	movea.l a6, a1
	adda.l #TemplateFields.Rows, a1
	adda.l d0, a1
	cmp.l TemplateDefinition.Epoch(a1), d3
	bne.w released
	clr.l TemplateFields.OwnerRow(a6)
released
	adda.w #TEMPLATE_POOL_BYTES+4, sp
	.TEMPLATE_ADD TemplateFields.Finished, #1
	bra.w done
invalidInput
	ori.l #TEMPLATE_INVALID, TemplateFields.Errors(a6)
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templatePools

	.priv
; A6=observer,D3=session. A2=live numeric slot or NULL; CCR reflects A2.
; Other registers preserved. No source-owner address is retained.
templateSession .block
	move.l d0, -(sp)
	tst.l d3
	beq.w missing
	lea TemplateFields.Owners(a6), a2
	moveq #TEMPLATE_OWNER_LIMIT-1, d0
next
	cmp.l TemplateOwnerSlot.Epoch(a2), d3
	bne.w advance
	tst.l TemplateOwnerSlot.Active(a2)
	bne.w done
advance
	adda.l #TEMPLATE_OWNER_BYTES, a2
	dbra d0, next
missing
	suba.l a2, a2
done
	move.l (sp)+, d0
	cmpa.w #0, a2
	rts
	.bend  ; templateSession
; A6=observer,D0=handle,D1=pool,D3=session. A2=map cell; D0/CCR=status.
templateMap .block
	bsr.w templateSession
	beq.w bad
	tst.l d0
	beq.w bad
	cmpi.l #TEMPLATE_DEFINITION_LIMIT, d0
	bhi.w full
	cmpi.l #1, d1
	bhi.w bad
	subq.l #1, d0
	lsl.l #2, d0
	move.l d1, d2
	mulu.w #TEMPLATE_DEFINITION_LIMIT*4, d2
	add.l d2, d0
	adda.w #TemplateOwnerSlot.Maps, a2
	adda.l d0, a2
	moveq #0, d0
	rts
full
	ori.l #TEMPLATE_EXHAUSTED, TemplateFields.Errors(a6)
bad
	moveq #1, d0
	rts
	.bend  ; templateMap
; D0=handle,D1=pool,D3=session. D0=row ordinal or0,A1=row,CCR=D0.
templateFind .block
	bsr.w templateMap
	bne.w missing
	move.l (a2), d0
	beq.w missing
	cmp.l TemplateFields.Count(a6), d0
	bhi.w missing
	move.l d0, d2
	subq.l #1, d2
	mulu.w #TEMPLATE_DEFINITION_BYTES, d2
	movea.l a6, a1
	adda.l #TemplateFields.Rows, a1
	adda.l d2, a1
	cmp.l TemplateDefinition.Epoch(a1), d3
	bne.w missing
	tst.l d0
	rts
missing
	moveq #0, d0
	rts
	.bend  ; templateFind
; A0=scalar,D2=unsigned amount,A6=observer; clobbers D3/CCR only.
templateAdd .block
	move.l (a0), d3
	add.l d2, d3
	bcc.w store
	moveq #-1, d3
	ori.l #TEMPLATE_OVERFLOW, TemplateFields.Errors(a6)
store
	move.l d3, (a0)
	rts
	.bend  ; templateAdd
templateClear .block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea TemplateState, a0
	move.l #TEMPLATE_STATE_BYTES/4, d0
next
	clr.l (a0)+
	subq.l #1, d0
	bne.w next
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; templateClear
