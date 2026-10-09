; Counted, iterable and conditional runtime repetition over packed, compiled records.
; The source text and package dictionary are absent during both assembly passes.
; @opforge-owner: experimental.amigaos.binary_repetition
	.module experimental.amigaos.binary_repetition
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_memory as memory
	.use opasm.amigaos.binary_expression as expression
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_repetition_trace as trace
	.use experimental.amigaos.binary_guard_values as guards
	.use experimental.amigaos.binary_repeat_records as snapshots
	.use experimental.amigaos.binary_record_values as records
	.use experimental.amigaos.binary_repeat_inventory as inventory
	.use experimental.amigaos.binary_repeat_frames as frames
	.use prvm.amigaos.packed_loop as parser
	.use prvm.amigaos.abi as abi
	.include "memory_telemetry.i"
	.pub
LIMIT = 16
MAX_ITERATIONS = 65536
SNAPSHOT_ABSOLUTE = 4
Slot	.struct
Body	.long ?
Remaining	.long ?
Iterable	.long ?  ; zero for counted loops, otherwise immutable descriptor kind
Descriptor	.long ?
Index	.long ?
Variable	.long ?
OldLow	.long ?
OldHigh	.long ?
OldKind	.long ?
OldDefined	.byte ?
OldSection	.byte ?
Padding	.word ?
Header	.long ?
Kind	.long ?
Trace	.long ?
Iterations	.long ?
ScopeGroup	.long ?
Label	.long ?  ; -1 for anonymous repetition
Records	.long ?  ; typed list arena offset, published only after close
StartPc	.long ?
StartSection	.long ?
RecordCells	.res memory.Block.Used+4  ; provisional unknown-count collection
	.endstruct
SLOT_BYTES = Slot.RecordCells+memory.Block.Used+4
UNSUPPORTED_LAYOUT = 3
FILTERED_ONE_MAP_MODE = 2
FILTERED_TWO_MAP_MODE = 4
State	.struct
Depth	.word ?
Padding	.word ?
ConditionPc	.long ?
SourceBase	.long ?
Sweep	.long ?
PendingTrace	.long ?
PendingKind	.long ?
PendingHeader	.long ?
PendingOperand	.long ?
Guard	.res guards.STATE_BYTES
Trace	.res trace.STATE_BYTES
Snapshots	.res snapshots.STATE_BYTES
Scopes	.res frames.STATE_BYTES
Slots	.res LIMIT*SLOT_BYTES
	.endstruct
SLOTS = State.Slots
STATE_BYTES = State.Slots+LIMIT*SLOT_BYTES
	.section code, kind=code

; A0=state,A1=inventory,A2=context,A3=records,D0=bytes. Initialize scoped
; replay ownership after preparation. Registers preserved; CCR unspecified.
prepare	.block
	move.l a0, -(sp)
	lea State.Scopes(a0), a0
	jsr frames.prepare
	movea.l (sp)+, a0
	rts
	.bend  ; prepare

; A0=state,A1=stream base,D0=sweep identity. Start one source sweep.
begin	.block
	clr.w State.Depth(a0)
	move.l a1, State.SourceBase(a0)
	move.l d0, State.Sweep(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. A source sweep must not end inside a repetition loop.
end	.block
	moveq #0, d0
	tst.w State.Depth(a0)
	beq.w done
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; end

; A0=current record,A1=bounded stream end,A2=assembly context,A3=state.
; D0=0 ordinary record,1 consumed loop control,2 invalid loop. For a consumed
; control A0 is the next record (possibly the first body record again).
; Preserves D1-D7/A1-A6. All stored addresses are transient execution state,
; never part of the binary source representation.
step	.block
	movem.l d1-d7/a1-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	cmpi.w #9, d0
	blo.w ordinary
	movea.l a5, a4
	adda.w d0, a4
	cmpa.l a6, a4
	bhi.w invalid
	movea.l package.Context.Package(a2), a1
	move.l package.Header.LoopPlanBytes(a1), d1
	adda.l package.Header.LoopPlan(a1), a1
	movea.l a5, a0
	jsr parser.classify
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w invalid
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w closingFor
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w closingWhile
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	beq.w ordinaryExtent
	move.l d2, State.PendingOperand(a3)
	move.l d0, State.PendingKind(a3)
	move.l a5, State.PendingHeader(a3)
	bsr.w openObservation
	bne.w invalid
	cmpi.l #abi.PRVM_LOOP_WHILE, State.PendingKind(a3)
	beq.w whileOpening
	cmpi.l #abi.PRVM_LOOP_BWHILE, State.PendingKind(a3)
	beq.w whileOpening
	move.l a4, d0
	sub.l a5, d0
	bra.w opening
whileOpening
	move.w State.Depth(a3), d7
	cmpi.w #LIMIT, d7
	bhs.w invalid
	movea.l a5, a0
	bsr.w evaluateWhile
	bne.w invalid
	tst.l d1
	beq.w skipBody
	mulu.w #SLOT_BYTES, d7
	lea SLOTS(a3), a1
	adda.w d7, a1
	clr.l Slot.Iterable(a1)
	move.l #frames.DYNAMIC_ITERATIONS, Slot.Remaining(a1)
	move.l a4, Slot.Body(a1)
	bsr.w initializeSlot
	bne.w invalid
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a1)
	bne.w whileReady
	bsr.w openScope
	bne.w invalid
whileReady
	addq.w #1, State.Depth(a3)
	movea.l a4, a0
	bra.w consumed
opening
	move.l State.PendingOperand(a3), d3
	addq.l #3, d3
	cmp.l d3, d0
	blo.w invalid
	subq.l #3, d3
	lea 0(a5, d3.l), a0
	move.w State.Depth(a3), d7
	cmpi.w #LIMIT, d7
	bhs.w invalid
	cmpi.b #source.TOKEN_ITERATOR_BINDING, (a0)
	beq.w iterableOpening
	movea.l a4, a1
	jsr expression.evaluate
	bne.w invalid
	cmpa.l a4, a0
	bne.w invalid
	tst.l d2
	bne.w invalid
	tst.l package.Context.High(a2)
	bne.w invalid
	cmpi.l #MAX_ITERATIONS, d1
	bhi.w invalid
	tst.l d1
	beq.w skipBody
	mulu.w #SLOT_BYTES, d7
	lea SLOTS(a3), a1
	clr.l Slot.Iterable(a1, d7.w)
	move.l a4, Slot.Body(a1, d7.w)
	move.l d1, Slot.Remaining(a1, d7.w)
	adda.w d7, a1
	bsr.w initializeSlot
	bne.w invalid
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a1)
	bne.w countedReady
	bsr.w openScope
	bne.w invalid
countedReady
	addq.w #1, State.Depth(a3)
	movea.l a4, a0
	bra.w consumed
iterableOpening
	addq.l #6, d3
	cmp.l d3, d0
	blo.w invalid
	moveq #0, d6
	move.w source.IteratorToken.Id(a0), d6
	cmp.l package.Context.Count(a2), d6
	bhs.w invalid
	adda.w #source.IteratorToken.Value, a0
	movea.l a4, a1
	jsr expression.evaluateValue
	bne.w invalid
	cmpa.l a4, a0
	bne.w invalid
	tst.l d2
	bne.w invalid
	tst.l package.Context.High(a2)
	bne.w invalid
	tst.l package.Context.Kind(a2)
	beq.w invalid
	cmpi.l #values.TYPED_LIST, package.Context.Kind(a2)
	bhi.w invalid
	move.l d1, d5
	movea.l package.Context.Owner(a2), a0
	move.l a0, d0
	beq.w invalid
	jsr values.compoundLength
	bne.w invalid
	tst.l d2
	bne.w invalid
	cmpi.l #MAX_ITERATIONS, d1
	bhi.w invalid
	tst.l d1
	beq.w skipBody
	move.l d1, d4
	; Validate the entire immutable snapshot before executing any body record.
	moveq #0, d3
validateElement
	move.l d3, d2
	move.l d3, -(sp)
	moveq #0, d3
	move.l d5, d1
	move.l package.Context.Kind(a2), d0
	bsr.w element
	move.l (sp)+, d3
	tst.l d0
	bne.w invalid
	addq.l #1, d3
	cmp.l d4, d3
	blo.w validateElement
	mulu.w #SLOT_BYTES, d7
	lea SLOTS(a3), a5
	adda.w d7, a5
	move.l a4, Slot.Body(a5)
	move.l d4, Slot.Remaining(a5)
	move.l package.Context.Kind(a2), Slot.Iterable(a5)
	move.l d5, Slot.Descriptor(a5)
	movea.l a5, a1
	bsr.w initializeSlot
	bne.w invalid
	move.l d6, Slot.Variable(a5)
	move.l d6, d1
	jsr values.getKind
	bne.w invalid
	move.l d2, Slot.OldKind(a5)
	movea.l package.Context.Values(a2), a1
	move.l d6, d1
	lsl.l #3, d1
	move.l 0(a1, d1.l), Slot.OldLow(a5)
	move.l 4(a1, d1.l), Slot.OldHigh(a5)
	movea.l package.Context.Defined(a2), a1
	move.b 0(a1, d6.l), Slot.OldDefined(a5)
	movea.l package.Context.SectionIds(a2), a1
	move.b 0(a1, d6.l), Slot.OldSection(a5)
	bsr.w bindElement
	bne.w invalid
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a5)
	bne.w iterableReady
	; Bind before local roots are resolved; the iterator has its own lexical ID
	; and is not part of the owned declaration cells restored by enterScope.
	movea.l a5, a1
	bsr.w openScope
	bne.w invalid
	.MEMORY_COUNTER_INC IterableScopes
iterableReady
	addq.w #1, State.Depth(a3)
	movea.l Slot.Body(a5), a0
	bra.w consumed
skipBody
	bsr.w emptyCollection
	bne.w invalid
	moveq #0, d1
	move.l State.PendingTrace(a3), d2
	bsr.w finishObservation
	bne.w invalid
	move.w State.Depth(a3), d7
	cmpi.w #LIMIT, d7
	bhs.w invalid
	move.l d7, d0
	mulu.w #SLOT_BYTES, d0
	lea SLOTS(a3), a0
	move.l State.PendingKind(a3), Slot.Kind(a0, d0.w)
	addq.w #1, d7
	movea.l a4, a0
skipLine
	cmpa.l a6, a0
	bhs.w invalid
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	cmpi.w #4, d0
	blo.w invalid
	movea.l a0, a4
	adda.w d0, a4
	cmpa.l a6, a4
	bhi.w invalid
	movea.l package.Context.Package(a2), a1
	move.l package.Header.LoopPlanBytes(a1), d1
	adda.l package.Header.LoopPlan(a1), a1
	jsr parser.classify
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w invalid
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	beq.w skipNext
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w skipForClose
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w skipWhileClose
	cmpi.w #LIMIT, d7
	bhs.w invalid
	move.l d7, d1
	mulu.w #SLOT_BYTES, d1
	lea SLOTS(a3), a1
	move.l d0, Slot.Kind(a1, d1.w)
	addq.w #1, d7
	bra.w skipNext
skipForClose
	moveq #abi.PRVM_LOOP_MATCHED, d0
	bra.w skipClose
skipWhileClose
	moveq #abi.PRVM_LOOP_WHILE, d0
skipClose
	subq.w #1, d7
	move.l d7, d1
	mulu.w #SLOT_BYTES, d1
	lea SLOTS(a3), a1
	cmpi.l #abi.PRVM_LOOP_MATCHED, d0
	bne.w skipKind
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a1, d1.w)
	beq.w skipMatched
skipKind
	cmpi.l #abi.PRVM_LOOP_WHILE, d0
	bne.w skipOrdinaryKind
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a1, d1.w)
	beq.w skipMatched
skipOrdinaryKind
	cmp.l Slot.Kind(a1, d1.w), d0
	bne.w invalid
skipMatched
	cmp.w State.Depth(a3), d7
	beq.w skipped
skipNext
	movea.l a4, a0
	bra.w skipLine
skipped
	movea.l a4, a0
	bra.w consumed
closingFor
	moveq #abi.PRVM_LOOP_MATCHED, d0
	bra.w closing
closingWhile
	moveq #abi.PRVM_LOOP_WHILE, d0
closing
	move.w State.Depth(a3), d7
	beq.w invalid
	subq.w #1, d7
	mulu.w #SLOT_BYTES, d7
	lea SLOTS(a3), a1
	cmpi.l #abi.PRVM_LOOP_MATCHED, d0
	bne.w closingKind
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a1, d7.w)
	bne.w closingKind
	moveq #abi.PRVM_LOOP_BFOR, d0
closingKind
	cmpi.l #abi.PRVM_LOOP_WHILE, d0
	bne.w closingOrdinaryKind
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a1, d7.w)
	bne.w closingOrdinaryKind
	moveq #abi.PRVM_LOOP_BWHILE, d0
closingOrdinaryKind
	cmp.l Slot.Kind(a1, d7.w), d0
	bne.w invalid
	cmpi.l #abi.PRVM_LOOP_WHILE, d0
	beq.w whileClosing
	cmpi.l #abi.PRVM_LOOP_BWHILE, d0
	beq.w whileClosing
	movea.l a1, a5
	adda.w d7, a5
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a5)
	bne.w captured
	bsr.w captureScope
	bne.w invalid
captured
	move.l Slot.Remaining(a1, d7.w), d1
	subq.l #1, d1
	beq.w exhausted
	move.l d1, Slot.Remaining(a1, d7.w)
	movea.l a1, a5
	adda.w d7, a5
	addq.l #1, Slot.Index(a5)
	addq.l #1, Slot.Iterations(a5)
	tst.l Slot.Iterable(a5)
	beq.w nextScope
	bsr.w bindElement
	bne.w invalid
nextScope
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a5)
	bne.w repeatBody
	bsr.w enterScope
	bne.w invalid
	bra.w repeatBody
repeatBody
	movea.l Slot.Body(a5), a0
	bra.w consumed
whileClosing
	movea.l a1, a5
	adda.w d7, a5
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a5)
	bne.w whileGuard
	bsr.w captureScope
	bne.w invalid
	; Guards see only enclosing cells, after capturing the completed iteration.
	bsr.w restoreScope
	bne.w invalid
whileGuard
	movea.l Slot.Header(a5), a0
	bsr.w evaluateWhile
	bne.w invalid
	tst.l d1
	beq.w finishWhile
	cmpi.l #MAX_ITERATIONS, Slot.Iterations(a5)
	bhs.w invalid
	addq.l #1, Slot.Index(a5)
	addq.l #1, Slot.Iterations(a5)
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a5)
	bne.w repeatBody
	bsr.w enterScope
	bne.w invalid
	bra.w repeatBody
finishWhile
	bsr.w closeObservation
	bne.w invalid
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a5)
	bne.w popLoop
	bsr.w closeScope
	bne.w invalid
	bra.w popLoop
exhausted
	movea.l a1, a5
	adda.w d7, a5
	bsr.w closeObservation
	bne.w invalid
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a5)
	bne.w ordinaryExhausted
	bsr.w closeScope
	bne.w invalid
ordinaryExhausted
	tst.l Slot.Iterable(a5)
	beq.w popLoop
	movea.l package.Context.Owner(a2), a0
	move.l Slot.Variable(a5), d1
	move.l Slot.OldKind(a5), d2
	jsr values.setKind
	bne.w invalid
	move.l d1, d6
	lsl.l #3, d1
	movea.l package.Context.Values(a2), a1
	move.l Slot.OldLow(a5), 0(a1, d1.l)
	move.l Slot.OldHigh(a5), 4(a1, d1.l)
	movea.l package.Context.Defined(a2), a1
	move.b Slot.OldDefined(a5), 0(a1, d6.l)
	movea.l package.Context.SectionIds(a2), a1
	move.b Slot.OldSection(a5), 0(a1, d6.l)
popLoop
	subq.w #1, State.Depth(a3)
	movea.l a4, a0
consumed
	moveq #1, d0
	bra.w done
ordinaryExtent
	move.l a4, d0
	sub.l a5, d0
ordinary
	; Rust's unscoped loops reject declarations/labels in its body. A
	; column-one name is a label; explicit ':' and '=' are labels at any indent.
	tst.w State.Depth(a3)
	beq.w ordinaryReady
	moveq #0, d1
	move.w State.Depth(a3), d1
	subq.w #1, d1
	mulu.w #SLOT_BYTES, d1
	lea SLOTS(a3), a1
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a1, d1.w)
	beq.w ordinaryReady
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a1, d1.w)
	beq.w ordinaryReady
	cmpi.w #9, d0
	blo.w ordinaryReady
	cmpi.b #1, 4(a5)
	bhi.w ordinaryReady
	btst #0, 1(a5)
	beq.w invalid
	cmpi.w #9, d0
	blo.w ordinaryReady
	cmpi.b #5, 8(a5)
	beq.w invalid
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, 8(a5)
	beq.w invalid
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 8(a5)
	blo.w ordinaryReady
	cmpi.b #source.TOKEN_UPDATE_LAST, 8(a5)
	bls.w invalid
ordinaryReady
	movea.l a5, a0
	moveq #0, d0
	bra.w done
invalid
	moveq #2, d0
done
	movem.l (sp)+, d1-d7/a1-a6
	tst.l d0
	rts
	.bend  ; step
	.pub
; A0=loop state. D0/CCR=zero when an enclosing scoped repetition owns live cells.
; Other registers preserved. Ordinary loops may reference enclosing scoped names.
scoped	.block
	movem.l d1/a1, -(sp)
	moveq #0, d1
	move.w State.Depth(a0), d1
	beq.w absent
	lea SLOTS(a0), a1
next
	subq.w #1, d1
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a1)
	beq.w found
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a1)
	beq.w found
	adda.w #SLOT_BYTES, a1
	tst.w d1
	bne.w next
absent
	moveq #1, d0
	bra.w done
found
	moveq #0, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; scoped

; A0=selected branch record,A3=loop state. Activate the innermost scoped
; owner's immutable declarations through shared dependency resolution.
; D0/CCR=status; other registers kept. Ordinary loops have no local roots.
activateBranch	.block
	movem.l d1-d2/a0/a5, -(sp)
	moveq #0, d2
	move.w State.Depth(a3), d2
	beq.w good
	subq.w #1, d2
	mulu.w #SLOT_BYTES, d2
	lea SLOTS(a3), a5
	adda.w d2, a5
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a5)
	beq.w activate
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a5)
	bne.w good
activate
	move.l a0, d1
	sub.l State.SourceBase(a3), d1
	move.l Slot.ScopeGroup(a5), d0
	lea State.Scopes(a3), a0
	jsr frames.activate
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d2/a0/a5
	tst.l d0
	rts
	.bend  ; activateBranch

; A0=record,A2=context,A3=loop state. Record Rust's previous regular-line
; start PC, including conditional/empty lines. Driver loop controls do not set it.
; D0/CCR=status; others preserved. No text or CPU grammar enters this boundary.
observe	.block
	cmpi.b #8, (a0)
	blo.w regular
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, abi.LoopRecord.HeadDot(a0)
	beq.w directive
regular
	move.l package.Context.Pc(a2), State.ConditionPc(a3)
	moveq #0, d0
	rts
directive
	movem.l d1-d2/a0-a1, -(sp)
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	movea.l package.Context.Package(a2), a1
	move.l package.Header.LoopPlanBytes(a1), d1
	adda.l package.Header.LoopPlan(a1), a1
	jsr parser.classify
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w bad
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	bne.w ready
	move.l package.Context.Pc(a2), State.ConditionPc(a3)
ready
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; observe

; A0=state,D0=assembly pass. D0/CCR=status; other registers preserved.
beginPass	.block
	clr.l State.ConditionPc(a0)
	move.l a0, -(sp)
	move.l d0, -(sp)
	lea State.Guard(a0), a0
	jsr guards.beginPass
	move.l (sp)+, d0
	movea.l (sp)+, a0
	move.l a0, -(sp)
	lea State.Trace(a0), a0
	jsr trace.beginPass
	movea.l (sp)+, a0
	bne.w failed
	move.l a0, -(sp)
	moveq #0, d0
	move.w State.Trace+trace.State.Mode(a0), d0
	lea State.Scopes(a0), a0
	jsr frames.beginPass
	movea.l (sp)+, a0
failed
	rts
	.bend  ; beginPass

; A0=state. Require the complete captured pass traversal. D0/CCR=status.
endPass	.block
	move.l a0, -(sp)
	lea State.Trace(a0), a0
	jsr trace.endPass
	movea.l (sp)+, a0
	rts
	.bend  ; endPass

; A0=state. Release transient observations on all assembly exits.
; All registers preserved; CCR unspecified.
release	.block
	movem.l d0-d3/a0-a3/a5, -(sp)
	movea.l a0, a3
	moveq #0, d3
	move.w State.Depth(a3), d3
restore
	tst.w d3
	beq.w restored
	subq.w #1, d3
	move.l d3, d0
	mulu.w #SLOT_BYTES, d0
	lea SLOTS(a3), a5
	adda.w d0, a5
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a5)
	beq.w restoreScoped
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a5)
	bne.w restore
restoreScoped
	bsr.w restoreScope
	bra.w restore
restored
	moveq #LIMIT-1, d3
	lea SLOTS(a3), a5
releaseCells
	lea Slot.RecordCells(a5), a0
	jsr memory.release
	clr.l Slot.RecordCells+memory.Block.Used(a5)
	adda.w #SLOT_BYTES, a5
	dbra d3, releaseCells
	movea.l a3, a0
	clr.w State.Depth(a0)
	move.l a0, -(sp)
	lea State.Guard(a0), a0
	jsr guards.release
	movea.l (sp), a0
	lea State.Trace(a0), a0
	jsr trace.release
	movea.l (sp), a0
	lea State.Snapshots(a0), a0
	jsr snapshots.release
	movea.l (sp), a0
	lea State.Scopes(a0), a0
	jsr frames.release
	movea.l (sp)+, a0
	movem.l (sp)+, d0-d3/a0-a3/a5
	rts
	.bend  ; release

; A0=bounded records,D0=bytes,A2=validated package,D1=section mode.
; While requires source-order PC replay, absent in filtered mapped layout modes 2/4.
; D0=0 supported,UNSUPPORTED_LAYOUT present while,1 invalid; D1=offset on error.
; Other registers preserved. Does not reject filtered layouts without while.
validateLayout	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	cmpi.l #FILTERED_ONE_MAP_MODE, d1
	beq.w scan
	cmpi.l #FILTERED_TWO_MAP_MODE, d1
	bne.w good
scan
	move.l a0, d2
	add.l d0, d2
	bcs.w bad
	movea.l d2, a6
line
	cmpa.l a6, a0
	beq.w good
	bhi.w bad
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	cmpi.w #4, d0
	blo.w bad
	movea.l a0, a4
	adda.l d0, a4
	cmpa.l a6, a4
	bhi.w bad
	movea.l a2, a1
	move.l package.Header.LoopPlanBytes(a2), d1
	adda.l package.Header.LoopPlan(a2), a1
	jsr parser.classify
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w bad
	cmpi.l #abi.PRVM_LOOP_WHILE, d0
	beq.w unsupported
	cmpi.l #abi.PRVM_LOOP_BFOR, d0
	beq.w unsupported
	cmpi.l #abi.PRVM_LOOP_BWHILE, d0
	beq.w unsupported
	movea.l a4, a0
	bra.w line
unsupported
	move.l a0, d1
	sub.l a5, d1
	moveq #UNSUPPORTED_LAYOUT, d0
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	move.l a0, d1
	sub.l a5, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validateLayout

	.priv
; A0=while header,A2=context,A3=loop state. Evaluate at the previous
; regular statement's start PC, matching Rust; restore actual emission PC.
; D0/CCR=status,D1=truth,D2 scratch; other registers preserved.
evaluateWhile	.block
	movem.l d3/a0-a1, -(sp)
	move.l a0, -(sp)
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	movea.l package.Context.Package(a2), a1
	move.l package.Header.LoopPlanBytes(a1), d1
	adda.l package.Header.LoopPlan(a1), a1
	jsr parser.classify
	cmpi.l #abi.PRVM_LOOP_WHILE, d0
	beq.w classified
	cmpi.l #abi.PRVM_LOOP_BWHILE, d0
	bne.w classifyFailed
classified
	move.l d2, d3
	lea State.Guard(a3), a0
	jsr guards.enter
	movea.l (sp)+, a0
	bne.w enterFailed
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	movea.l a0, a1
	adda.l d0, a1
	adda.l d3, a0
	move.l package.Context.Pc(a2), -(sp)
	move.l State.ConditionPc(a3), package.Context.Pc(a2)
	jsr expression.evaluate
	tst.l d0
	bne.w failed
	cmpa.l a1, a0
	bne.w failed
	tst.l d2
	beq.w valueReady
	cmpi.w #1, package.Context.Pass(a2)
	bne.w failed
valueReady
	or.l package.Context.High(a2), d1
	moveq #0, d0
	bra.w restore
failed
	moveq #1, d0
restore
	move.l (sp)+, package.Context.Pc(a2)
	lea State.Guard(a3), a0
	jsr guards.leave
	bra.w finished
classifyFailed
	movea.l (sp)+, a0
enterFailed
	moveq #1, d0
finished
	movem.l (sp)+, d3/a0-a1
	tst.l d0
	rts
	.bend  ; evaluateWhile

; A1=new loop slot,A3=state. Initialize replay/path identity; others kept.
initializeSlot	.block
	move.l State.PendingHeader(a3), Slot.Header(a1)
	move.l State.PendingKind(a3), Slot.Kind(a1)
	move.l State.PendingTrace(a3), Slot.Trace(a1)
	move.l #1, Slot.Iterations(a1)
	clr.l Slot.Index(a1)
	clr.l Slot.ScopeGroup(a1)
	moveq #-1, d0
	move.l d0, Slot.Label(a1)
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a1)
	beq.w scopedSlot
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a1)
	bne.w ready
scopedSlot
	clr.l Slot.RecordCells+memory.Block.Used(a1)
	movem.l d1-d2/a0, -(sp)
	movea.l Slot.Header(a1), a0
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, inventory.PackedRecord.NameKind(a0)
	bhi.w anonymous
	moveq #0, d1
	move.w inventory.PackedRecord.NameId(a0), d1
	move.l d1, Slot.Label(a1)
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a1)
	beq.w anonymous
	movea.l package.Context.Owner(a2), a0
	move.l Slot.Remaining(a1), d0
	jsr records.createList
	move.l d2, Slot.Records(a1)
	bra.w allocated
anonymous
	moveq #0, d0
allocated
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	rts
ready
	moveq #0, d0
	rts
	.bend  ; initializeSlot

; A1=new scoped slot,A3=replay state. D0/CCR=status; other registers kept.
openScope	.block
	movem.l d1-d3/a0/a5, -(sp)
	movea.l a1, a5
	move.l Slot.Trace(a5), d0
	move.l Slot.Header(a5), d1
	sub.l State.SourceBase(a3), d1
	move.l Slot.Remaining(a5), d2
	lea State.Scopes(a3), a0
	jsr frames.open
	bne.w done
	move.l d3, Slot.ScopeGroup(a5)
	bsr.w enterScope
done
	movem.l (sp)+, d1-d3/a0/a5
	tst.l d0
	rts
	.bend  ; openScope

; A5=scoped slot,A3=replay state. D0/CCR=status; other registers kept.
enterScope	.block
	move.l package.Context.Pc(a2), Slot.StartPc(a5)
	moveq #0, d0
	move.w package.Context.CurrentSection(a2), d0
	move.l d0, Slot.StartSection(a5)
	movem.l d1/a0, -(sp)
	move.l Slot.ScopeGroup(a5), d0
	move.l Slot.Index(a5), d1
	lea State.Scopes(a3), a0
	jsr frames.enter
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; enterScope

; A5=scoped slot,A3=replay state. D0/CCR=status; other registers kept.
captureScope	.block
	movem.l d1-d5/a0-a1, -(sp)
	move.l Slot.ScopeGroup(a5), d0
	move.l Slot.Index(a5), d1
	lea State.Scopes(a3), a0
	jsr frames.capture
	bne.w done
	tst.l Slot.Label(a5)
	bmi.w done
	move.l Slot.Header(a5), d0
	sub.l State.SourceBase(a3), d0
	move.l Slot.StartPc(a5), d1
	move.l Slot.StartSection(a5), d2
	movea.l State.Scopes+frames.State.Inventory(a3), a1
	lea State.Snapshots(a3), a0
	jsr snapshots.capture
	bne.w done
	suba.w #values.CELL_BYTES, sp
	move.l d3, values.Cell.Low(sp)
	clr.l values.Cell.High(sp)
	move.l #records.ADDRESS_RECORD, values.Cell.Kind(sp)
	clr.l values.Cell.Provenance(sp)
	movea.l sp, a1
	movea.l package.Context.Owner(a2), a0
	move.l Slot.Records(a5), d0
	move.l Slot.Index(a5), d1
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a5)
	beq.w dynamicRecord
	jsr records.set
	bra.w recordStored
dynamicRecord
	bsr.w appendRecordCell
recordStored
	adda.w #values.CELL_BYTES, sp
done
	movem.l (sp)+, d1-d5/a0-a1
	tst.l d0
	rts
	.bend  ; captureScope

; A5=scoped slot,A3=replay state. D0/CCR=status; other registers kept.
closeScope	.block
	move.l a0, -(sp)
	bsr.w restoreScope
	bne.w done
	tst.l Slot.Label(a5)
	bmi.w done
	movem.l d1-d2/a1, -(sp)
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a5)
	bne.w collectionReady
	movea.l Slot.RecordCells+memory.Block.Pointer(a5), a1
	move.l Slot.RecordCells+memory.Block.Used(a5), d1
	move.l Slot.Iterations(a5), d0
	movea.l package.Context.Owner(a2), a0
	jsr records.appendList
	bne.w collectionDone
	move.l d2, Slot.Records(a5)
collectionReady
	move.l Slot.Label(a5), d1
	move.l Slot.Records(a5), d2
	bsr.w publishCollection
collectionDone
	movem.l (sp)+, d1-d2/a1
done
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; closeScope

; A5=scoped slot,A3=state. Restore only enclosing cells; publication waits
; until the final guard is false. D0/CCR=status; other registers preserved.
restoreScope	.block
	move.l a0, -(sp)
	move.l Slot.ScopeGroup(a5), d0
	lea State.Scopes(a3), a0
	jsr frames.close
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; restoreScope

; A5=dynamic slot,A1=external typed Cell. Append a provisional record cell.
; D0/CCR=status; other registers preserved. No arena descriptor is published.
appendRecordCell	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a1, a2
	lea Slot.RecordCells(a5), a0
	move.l memory.Block.Used(a0), d2
	move.l d2, d0
	addi.l #values.CELL_BYTES, d0
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	move.l (a2)+, (a1)+
	move.l (a2)+, (a1)+
	move.l (a2)+, (a1)+
	move.l (a2)+, (a1)+
	addi.l #values.CELL_BYTES, d2
	move.l d2, memory.Block.Used(a0)
	moveq #0, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; appendRecordCell

; Publish completed collection D1=id,D2=descriptor. A2=context.
; D0/CCR=status; other registers preserved. No body cell escapes its owner.
publishCollection	.block
	movem.l d1-d4/a0-a1, -(sp)
	cmp.l package.Context.Count(a2), d1
	bhs.w bad
	move.l d1, d3
	move.l d2, d4
	movea.l package.Context.Owner(a2), a0
	moveq #records.TYPED_LIST, d2
	jsr values.setKind
	bne.w done
	move.l d3, d0
	lsl.l #3, d0
	movea.l package.Context.Values(a2), a0
	move.l d4, 0(a0, d0.l)
	clr.l 4(a0, d0.l)
	movea.l package.Context.Defined(a2), a0
	move.b #SNAPSHOT_ABSOLUTE+(1<<expression.COMPOUND_BIT), 0(a0, d3.l)
	movea.l package.Context.SectionIds(a2), a0
	clr.b 0(a0, d3.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a1
	tst.l d0
	rts
	.bend  ; publishCollection

; A3=pending empty loop. Publish an empty typed list only for a labeled scoped loop.
; D0/CCR=status; all other registers preserved.
emptyCollection	.block
	movem.l d1-d3/a0, -(sp)
	cmpi.l #abi.PRVM_LOOP_BFOR, State.PendingKind(a3)
	beq.w scopedEmpty
	cmpi.l #abi.PRVM_LOOP_BWHILE, State.PendingKind(a3)
	bne.w ordinary
scopedEmpty
	movea.l State.PendingHeader(a3), a0
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, inventory.PackedRecord.NameKind(a0)
	bhi.w ordinary
	moveq #0, d3
	move.w inventory.PackedRecord.NameId(a0), d3
	movea.l package.Context.Owner(a2), a0
	moveq #0, d0
	jsr records.createList
	bne.w done
	move.l d3, d1
	bsr.w publishCollection
	bra.w done
ordinary
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0
	tst.l d0
	rts
	.bend  ; emptyCollection

; A3=state with PendingHeader/Kind. Capture or match numeric ancestry.
; D0/CCR=status; other registers kept. Root parents carry the sweep identity.
openObservation	.block
	movem.l d1-d7/a0-a2, -(sp)
	move.l State.PendingHeader(a3), d0
	sub.l State.SourceBase(a3), d0
	moveq #0, d2
	move.l State.Sweep(a3), d1
	ori.l #$ffffff00, d1
	move.w State.Depth(a3), d3
	beq.w observe
	subq.w #1, d3
	mulu.w #SLOT_BYTES, d3
	lea SLOTS(a3), a1
	move.l Slot.Trace(a1, d3.w), d1
	move.l Slot.Index(a1, d3.w), d2
observe
	lea State.Trace(a3), a0
	jsr trace.open
	tst.l d0
	bne.w done
	move.l d1, State.PendingTrace(a3)
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; openObservation

; A5=completed loop slot,A3=state. D0/CCR=status; other registers kept.
closeObservation	.block
	movem.l d1-d2, -(sp)
	move.l Slot.Iterations(a5), d1
	move.l Slot.Trace(a5), d2
	bsr.w finishObservation
	movem.l (sp)+, d1-d2
	rts
	.bend  ; closeObservation

; A3=state,D1=count,D2=observation offset. D0/CCR=status; others kept.
finishObservation	.block
	movem.l d1-d2/a0, -(sp)
	move.l d1, d0
	move.l d2, d1
	lea State.Trace(a3), a0
	jsr trace.finish
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; finishObservation

; A0=Owner,D0=known descriptor kind,D1=descriptor,D2/D3=index low/high.
; D0/CCR=status,D1/D2=scalar low/high; others preserved. Iterator bindings
; discard shape and section identity, matching the assembler value iterator.
element	.block
	cmpi.l #values.TYPED_LIST, d0
	beq.w typed
	; Legacy descriptors keep their single validated view inside compoundGet.
	jmp values.compoundGet
typed
	movem.l d3-d6/a1, -(sp)
	move.l d1, d6
	tst.l d3
	bne.w bad
	move.l d2, d1
	move.l d6, d0
	jsr records.get
	bne.w done
	moveq #expression.MAX_DEPTH, d6
project
	tst.l d4
	beq.w scalar
	cmpi.l #records.ADDRESS_RECORD, d4
	bne.w bad
	subq.l #1, d6
	beq.w bad
	move.l d2, d0
	jsr records.project
	bne.w done
	bra.w project
scalar
	move.l d2, d1
	move.l d3, d2
	moveq #0, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d3-d6/a1
	tst.l d0
	rts
	.bend  ; element

; A5=iterable slot,A2=context. D0/CCR=status; clobbers D1-D3/D6/A0-A1.
; The descriptor offset survives arena relocation and body mutations.
; Elements project to absolute scalar bindings at the assembler iterator boundary.
bindElement	.block
	movea.l package.Context.Owner(a2), a0
	move.l Slot.Descriptor(a5), d1
	move.l Slot.Index(a5), d2
	moveq #0, d3
	move.l Slot.Iterable(a5), d0
	bsr.w element
	bne.w done
	move.l d1, d3
	move.l d2, d6
	move.l Slot.Variable(a5), d1
	moveq #values.SCALAR, d2
	jsr values.setKind
	bne.w done
	move.l d1, d2
	lsl.l #3, d1
	movea.l package.Context.Values(a2), a1
	move.l d3, 0(a1, d1.l)
	move.l d6, 4(a1, d1.l)
	movea.l package.Context.Defined(a2), a1
	move.b #SNAPSHOT_ABSOLUTE, 0(a1, d2.l)
	movea.l package.Context.SectionIds(a2), a1
	clr.b 0(a1, d2.l)
	.MEMORY_COUNTER_INC IterableBindings
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; bindElement
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
	.section bss, kind=bss
IterableScopes	.res long, 1
IterableBindings	.res long, 1
	.endsection
.endif
.endif
	.endmodule
