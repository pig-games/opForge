; Counted and iterable preparation-time repetition over packed, compiled records.
; The source text and package dictionary are absent during both assembly passes.
; @opforge-owner: experimental.amigaos.binary_repetition
	.module experimental.amigaos.binary_repetition
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use opasm.amigaos.binary_expression as expression
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_source as source
	.pub
LIMIT = 16
MAX_ITERATIONS = 65536
SNAPSHOT_ABSOLUTE = 4
Slot	.struct
Body	.long ?
Remaining	.long ?
Iterable	.long ?
Descriptor	.long ?
Index	.long ?
Variable	.long ?
OldLow	.long ?
OldHigh	.long ?
OldKind	.long ?
OldDefined	.byte ?
OldSection	.byte ?
Padding	.word ?
	.endstruct
State	.struct
Depth	.word ?
	.endstruct
SLOTS = State.Depth+2
SLOT_BYTES = Slot.OldSection+3
STATE_BYTES = SLOTS+LIMIT*SLOT_BYTES
	.section code, kind=code

; A0=state. Reset the loop stack for one complete source sweep.
begin	.block
	clr.w State.Depth(a0)
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
	cmpi.b #7, 4(a5)
	bne.w ordinary
	cmpi.b #1, 5(a5)
	bhi.w ordinary
	tst.b 8(a5)
	bne.w ordinary
	movea.l package.Context.Package(a2), a1
	move.w 6(a5), d1
	cmp.w package.Header.ForDirective(a1), d1
	beq.w opening
	cmp.w package.Header.EndforDirective(a1), d1
	beq.w closing
	bra.w ordinary
opening
	cmpi.w #12, d0
	blo.w invalid
	move.w State.Depth(a3), d7
	cmpi.w #LIMIT, d7
	bhs.w invalid
	cmpi.b #source.TOKEN_ITERATOR_BINDING, 9(a5)
	beq.w iterableOpening
	lea 9(a5), a0
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
	addq.w #1, State.Depth(a3)
	movea.l a4, a0
	bra.w consumed
iterableOpening
	cmpi.w #15, d0
	blo.w invalid
	moveq #0, d6
	move.w 10(a5), d6
	cmp.l package.Context.Count(a2), d6
	bhs.w invalid
	lea 12(a5), a0
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
	cmpi.l #values.RANGE, package.Context.Kind(a2)
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
	jsr values.compoundGet
	move.l (sp)+, d3
	tst.l d0
	bne.w invalid
	tst.l d2
	bne.w invalid
	addq.l #1, d3
	cmp.l d4, d3
	blo.w validateElement
	mulu.w #SLOT_BYTES, d7
	lea SLOTS(a3), a5
	adda.w d7, a5
	move.l a4, Slot.Body(a5)
	move.l d4, Slot.Remaining(a5)
	move.l #1, Slot.Iterable(a5)
	move.l d5, Slot.Descriptor(a5)
	clr.l Slot.Index(a5)
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
	addq.w #1, State.Depth(a3)
	movea.l Slot.Body(a5), a0
	bra.w consumed
skipBody
	moveq #1, d7
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
	cmpi.w #9, d0
	blo.w skipNext
	cmpi.b #7, 4(a0)
	bne.w skipNext
	cmpi.b #1, 5(a0)
	bhi.w skipNext
	tst.b 8(a0)
	bne.w skipNext
	movea.l package.Context.Package(a2), a1
	move.w 6(a0), d0
	cmp.w package.Header.ForDirective(a1), d0
	bne.w skipClose
	addq.w #1, d7
	bra.w skipNext
skipClose
	cmp.w package.Header.EndforDirective(a1), d0
	bne.w skipNext
	subq.w #1, d7
	beq.w skipped
skipNext
	movea.l a4, a0
	bra.w skipLine
skipped
	movea.l a4, a0
	bra.w consumed
closing
	cmpi.w #9, d0
	bne.w invalid
	move.w State.Depth(a3), d7
	beq.w invalid
	subq.w #1, d7
	mulu.w #SLOT_BYTES, d7
	lea SLOTS(a3), a1
	move.l Slot.Remaining(a1, d7.w), d1
	subq.l #1, d1
	beq.w exhausted
	move.l d1, Slot.Remaining(a1, d7.w)
	movea.l a1, a5
	adda.w d7, a5
	tst.l Slot.Iterable(a5)
	beq.w repeatBody
	addq.l #1, Slot.Index(a5)
	bsr.w bindElement
	bne.w invalid
repeatBody
	movea.l Slot.Body(a5), a0
	bra.w consumed
exhausted
	movea.l a1, a5
	adda.w d7, a5
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
ordinary
	; Rust's unscoped .for rejects declarations/labels in its body. A
	; column-one name is a label; explicit ':' and '=' are labels at any indent.
	tst.w State.Depth(a3)
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
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, 8(a5)
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
	.priv
; A5=iterable slot,A2=context. D0/CCR=status; clobbers D1-D3/D6/A0-A1.
; The descriptor offset survives arena relocation and body mutations.
bindElement	.block
	movea.l package.Context.Owner(a2), a0
	move.l Slot.Descriptor(a5), d1
	move.l Slot.Index(a5), d2
	moveq #0, d3
	jsr values.compoundGet
	bne.w done
	tst.l d2
	bne.w bad
	move.l d1, d3
	move.l Slot.Variable(a5), d1
	moveq #values.SCALAR, d2
	jsr values.setKind
	bne.w done
	move.l d1, d6
	lsl.l #3, d1
	movea.l package.Context.Values(a2), a1
	move.l d3, 0(a1, d1.l)
	clr.l 4(a1, d1.l)
	movea.l package.Context.Defined(a2), a1
	move.b #SNAPSHOT_ABSOLUTE, 0(a1, d6.l)
	movea.l package.Context.SectionIds(a2), a1
	clr.b 0(a1, d6.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; bindElement
	.endsection
	.endmodule
