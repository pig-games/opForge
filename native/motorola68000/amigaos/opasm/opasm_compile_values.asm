; Native opasm compile-time sequence storage.

	.module opasm.amigaos.compile_values
	.cpu 68020

SEQUENCE_CAPACITY = 8
SEQUENCE_NAME_CAPACITY = 32
SEQUENCE_ELEMENT_CAPACITY = 16
BINDING_CAPACITY = 8

	.section code, kind=code
	.pub

; Reset all session-local compile-time sequences.
; Outputs: D0 = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resetV1	.block
	clr.w SequenceCount
	clr.w BindingDepth
	moveq #0, d0
	rts
	.bend  ; resetV1

; Reset loop-variable bindings at one assembly-pass boundary.
; Outputs: D0 = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resetBindingsV1	.block
	clr.w BindingDepth
	moveq #0, d0
	rts
	.bend  ; resetBindingsV1

; Capture `name = {number, ...}` from one complete source line.
; Inputs: A0 = source text; D0 = source length.
; Outputs: D0 = 0 on success, 1 on malformed input.
; Clobbers: D0-D3/A0-A3/CCR.
; CCR: reflects D0 on return.
captureSourceListAssignmentV1	.block
	movem.l d1-d3/a0-a3, -(sp)
	move.l d0, d3
	movea.l a0, a2
	clr.w d2
nameLoop
	tst.l d3
	beq.s fail
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s nameDone
	cmpi.b #9, d1
	beq.s nameDone
	cmpi.b #'=', d1
	beq.s nameDone
	addq.l #1, a0
	subq.l #1, d3
	addq.w #1, d2
	bra.s nameLoop
nameDone
	tst.w d2
	beq.s fail
skip
	tst.l d3
	beq.s fail
	cmpi.b #' ', (a0)
	beq.s skipOne
	cmpi.b #9, (a0)
	bne.s equals
skipOne
	addq.l #1, a0
	subq.l #1, d3
	bra.s skip
equals
	cmpi.b #'=', (a0)
	bne.s fail
	addq.l #1, a0
	subq.l #1, d3
	movea.l a0, a1
	move.l d3, d1
	movea.l a2, a0
	moveq #0, d0
	move.w d2, d0
	bsr.w captureListAssignmentV1
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d3/a0-a3
	rts
	.bend  ; captureSourceListAssignmentV1

; Capture `name = {number, ...}` from parsed statement fields.
; Inputs: A0/D0 = label text/length; A1/D1 = list operand text/length.
; Outputs: D0 = 0 on success, 1 on malformed input or capacity failure.
; Clobbers: D0-D7/A0-A4/CCR.
; CCR: reflects D0 on return.
captureListAssignmentV1	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a3
	move.w d0, d6
	beq.w fail
	cmpi.w #SEQUENCE_NAME_CAPACITY, d6
	bhs.w fail
	movea.l a1, a0
	move.l d1, d7
	bsr.w skipWhitespace
	tst.l d7
	beq.w fail
	cmpi.b #'{', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d7
	moveq #0, d4
	move.w SequenceCount, d4
	cmpi.w #SEQUENCE_CAPACITY, d4
	bhs.w fail
	move.l d4, d5
	lsl.l #5, d5
	lea SequenceNames, a1
	adda.l d5, a1
	movea.l a3, a4
	move.w d6, d5

copyName
	move.b (a4)+, (a1)+
	subq.w #1, d5
	bne.s copyName
	clr.b (a1)
	move.l d4, d5
	lsl.l #6, d5
	lea SequenceValues, a2
	adda.l d5, a2
	clr.w d6

elementLoop
	bsr.w skipWhitespace
	tst.l d7
	beq.w fail
	cmpi.b #'}', (a0)
	beq.s listDone
	cmpi.w #SEQUENCE_ELEMENT_CAPACITY, d6
	bhs.w fail
	bsr.w parseNumber
	bne.w fail
	move.l d3, (a2)+
	addq.w #1, d6
	bsr.w skipWhitespace
	tst.l d7
	beq.w fail
	cmpi.b #',', (a0)
	beq.s comma
	cmpi.b #'}', (a0)
	beq.s listDone
	bra.w fail

comma
	addq.l #1, a0
	subq.l #1, d7
	bra.s elementLoop

listDone
	addq.l #1, a0
	subq.l #1, d7
	bsr.w skipWhitespace
	tst.l d7
	beq.s commit
	cmpi.b #';', (a0)
	bne.w fail

commit
	moveq #0, d5
	move.w SequenceCount, d5
	add.w d5, d5
	lea SequenceLengths, a1
	move.w d6, 0(a1, d5.l)
	move.w SequenceCount, d5
	addq.w #1, d5
	move.w d5, SequenceCount
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a4
	rts
	.bend  ; captureListAssignmentV1

; Find one exact sequence name.
; Inputs: A0 = name text; D0 = name length.
; Outputs: D0 = 0 on success, 1 when absent; D1 = index; D2 = element count; A1 = values.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
findSequenceV1	.block
	movem.l d3-d5/a0/a2, -(sp)
	movea.l a0, a2
	move.l d0, d5
	clr.w d1

scan
	cmp.w SequenceCount, d1
	bhs.s notFound
	move.l d1, d3
	lsl.l #5, d3
	lea SequenceNames, a1
	adda.l d3, a1
	movea.l a2, a0
	move.l d5, d0
	bsr.w nameEquals
	bne.s found
	addq.w #1, d1
	bra.s scan

found
	move.l d1, d3
	add.w d3, d3
	lea SequenceLengths, a0
	moveq #0, d2
	move.w 0(a0, d3.l), d2
	move.l d1, d3
	lsl.l #6, d3
	lea SequenceValues, a1
	adda.l d3, a1
	moveq #0, d0
	bra.s return

notFound
	moveq #1, d0

return
	movem.l (sp)+, d3-d5/a0/a2
	rts
	.bend  ; findSequenceV1

; Resolve `.for <name> in <sequence>` into a bounded iterable plan.
; Inputs: A0/D0 = operand text/length.
; Outputs: D0 = status; A0/D4 = variable text/length; A1 = values;
;          D1 = element count; D2 = first value; D3 = 0 for list.
; Clobbers: D0-D7/A0-A4/CCR.
; CCR: reflects D0 on return.
planListForOperandV1	.block
	movem.l d6-d7/a2-a4, -(sp)
	move.l d0, d7
	bsr.w skipWhitespace
	movea.l a0, a2
	clr.l d6
variableLoop
	tst.l d7
	beq.w fail
	cmpi.b #' ', (a0)
	beq.s variableDone
	cmpi.b #9, (a0)
	beq.s variableDone
	addq.l #1, a0
	subq.l #1, d7
	addq.l #1, d6
	cmpi.l #SEQUENCE_NAME_CAPACITY, d6
	bhs.w fail
	bra.s variableLoop
variableDone
	tst.l d6
	beq.w fail
	bsr.w skipWhitespace
	cmpi.l #2, d7
	bcs.w fail
	cmpi.b #'i', (a0)
	bne.w fail
	cmpi.b #'n', 1(a0)
	bne.w fail
	addq.l #2, a0
	subq.l #2, d7
	tst.l d7
	beq.w fail
	cmpi.b #' ', (a0)
	beq.s iterable
	cmpi.b #9, (a0)
	bne.w fail
iterable
	bsr.w skipWhitespace
	movea.l a0, a3
	move.l d7, d5
iterableEnd
	tst.l d5
	beq.s lookup
	cmpi.b #' ', (a0)
	beq.s lookup
	cmpi.b #9, (a0)
	beq.s lookup
	cmpi.b #';', (a0)
	beq.s lookup
	addq.l #1, a0
	subq.l #1, d5
	bra.s iterableEnd
lookup
	move.l d7, d0
	sub.l d5, d0
	beq.s fail
	movea.l a3, a0
	bsr.w findSequenceV1
	bne.s fail
	move.l d2, d1
	clr.l d2
	tst.l d1
	beq.s planned
	move.l (a1), d2
planned
	clr.l d3
	movea.l a2, a0
	move.l d6, d4
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d6-d7/a2-a4
	rts
	.bend  ; planListForOperandV1

; Resolve `.for <name> in <start>..<end>[:step]` into an ascending range plan.
; Inputs: A0/D0 = operand text/length.
; Outputs: D0 = status; A0/D4 = variable text/length; A1 = 0;
;          D1 = element count; D2 = first value; D3 = positive step.
; Clobbers: D0-D7/A0-A4/CCR.
; CCR: reflects D0 on return.
planRangeForOperandV1	.block
	movem.l d6-d7/a2-a4, -(sp)
	move.l d0, d7
	bsr.w skipWhitespace
	movea.l a0, a2
	clr.l d4
variableLoop
	tst.l d7
	beq.w fail
	cmpi.b #' ', (a0)
	beq.s variableDone
	cmpi.b #9, (a0)
	beq.s variableDone
	addq.l #1, a0
	subq.l #1, d7
	addq.l #1, d4
	cmpi.l #SEQUENCE_NAME_CAPACITY, d4
	bhs.w fail
	bra.s variableLoop
variableDone
	tst.l d4
	beq.w fail
	bsr.w skipWhitespace
	cmpi.l #2, d7
	bcs.w fail
	cmpi.b #'i', (a0)
	bne.w fail
	cmpi.b #'n', 1(a0)
	bne.w fail
	addq.l #2, a0
	subq.l #2, d7
	tst.l d7
	beq.w fail
	cmpi.b #' ', (a0)
	beq.s range
	cmpi.b #9, (a0)
	bne.w fail
range
	bsr.w skipWhitespace
	movea.l a0, a4
	bsr.w parseNumber
	bne.w fail
	cmpa.l a4, a0
	beq.w fail
	move.l d3, d2
	cmpi.l #2, d7
	bcs.w fail
	cmpi.b #'.', (a0)
	bne.w fail
	cmpi.b #'.', 1(a0)
	bne.w fail
	addq.l #2, a0
	subq.l #2, d7
	moveq #0, d1
	tst.l d7
	beq.w fail
	cmpi.b #'=', (a0)
	bne.s rangeEnd
	moveq #1, d1
	addq.l #1, a0
	subq.l #1, d7
rangeEnd
	movea.l a0, a4
	bsr.w parseNumber
	bne.w fail
	cmpa.l a4, a0
	beq.w fail
	move.l d3, d5
	moveq #1, d6
	tst.l d7
	beq.s count
	cmpi.b #':', (a0)
	bne.s trailing
	addq.l #1, a0
	subq.l #1, d7
	movea.l a0, a4
	bsr.w parseNumber
	bne.w fail
	cmpa.l a4, a0
	beq.w fail
	tst.l d3
	beq.w fail
	move.l d3, d6
trailing
	bsr.w skipWhitespace
	tst.l d7
	beq.s count
	cmpi.b #';', (a0)
	bne.w fail
count
	cmp.l d2, d5
	blo.w fail
	tst.l d1
	bne.s inclusiveCount
	move.l d2, d0
	clr.l d1
exclusiveLoop
	cmp.l d5, d0
	bhs.s planned
	addq.l #1, d1
	cmpi.l #1024, d1
	bhi.w fail
	add.l d6, d0
	bcs.w fail
	bra.s exclusiveLoop
inclusiveCount
	move.l d2, d0
	clr.l d1
inclusiveLoop
	cmp.l d5, d0
	bhi.s planned
	addq.l #1, d1
	cmpi.l #1024, d1
	bhi.w fail
	add.l d6, d0
	bcs.w fail
	bra.s inclusiveLoop
planned
	move.l d6, d3
	movea.l a2, a0
	suba.l a1, a1
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d6-d7/a2-a4
	rts
	.bend  ; planRangeForOperandV1

; Push a loop-variable binding.
; Inputs: A0/D0 = name text/length; D1 = value.
; Outputs: D0 = status.
; Clobbers: D0-D4/A0-A2/CCR.
; CCR: reflects D0 on return.
pushBindingV1	.block
	moveq #0, d2
	move.w BindingDepth, d2
	cmpi.w #BINDING_CAPACITY, d2
	bhs.s fail
	tst.l d0
	beq.s fail
	cmpi.l #SEQUENCE_NAME_CAPACITY, d0
	bhs.s fail
	move.l d2, d3
	lsl.l #5, d3
	lea BindingNames, a1
	adda.l d3, a1
	move.l d0, d4
copy
	move.b (a0)+, (a1)+
	subq.l #1, d4
	bne.s copy
	clr.b (a1)
	lsl.l #2, d2
	lea BindingValues, a2
	move.l d1, 0(a2, d2.l)
	move.w BindingDepth, d2
	addq.w #1, d2
	move.w d2, BindingDepth
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; pushBindingV1

; Store a named binding or replace its innermost existing value.
; Inputs: A0/D0 = name text/length; D1 = value.
; Outputs: D0 = status.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
upsertBindingV1	.block
	movea.l a0, a2
	move.l d0, d5
	moveq #0, d2
	move.w BindingDepth, d2
scan
	tst.w d2
	beq.s push
	subq.w #1, d2
	move.l d2, d3
	lsl.l #5, d3
	lea BindingNames, a1
	adda.l d3, a1
	movea.l a2, a0
	move.l d5, d0
	bsr.w nameEquals
	beq.s scan
	move.l d2, d3
	lsl.l #2, d3
	lea BindingValues, a0
	move.l d1, 0(a0, d3.l)
	moveq #0, d0
	rts
push
	movea.l a2, a0
	move.l d5, d0
	bra.w pushBindingV1
	.bend  ; upsertBindingV1

; Update the innermost loop-variable binding.
; Inputs: D1 = value.
; Outputs: D0 = status.
; Clobbers: D0-D2/A0/CCR.
; CCR: reflects D0 on return.
updateTopBindingV1	.block
	moveq #0, d2
	move.w BindingDepth, d2
	beq.s fail
	subq.w #1, d2
	lsl.l #2, d2
	lea BindingValues, a0
	move.l d1, 0(a0, d2.l)
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; updateTopBindingV1

; Pop the innermost loop-variable binding.
; Outputs: D0 = status.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
popBindingV1	.block
	tst.w BindingDepth
	beq.s fail
	move.w BindingDepth, d0
	subq.w #1, d0
	move.w d0, BindingDepth
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; popBindingV1

; Resolve the innermost exact loop-variable binding.
; Inputs: A0/D0 = name text/length.
; Outputs: D0 = status; D3 = value.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
resolveBindingV1	.block
	movem.l d1-d2/d4-d5/a0-a2, -(sp)
	movea.l a0, a2
	move.l d0, d5
	moveq #0, d1
	move.w BindingDepth, d1
scanBinding
	tst.w d1
	beq.s notFound
	subq.w #1, d1
	move.l d1, d2
	lsl.l #5, d2
	lea BindingNames, a1
	adda.l d2, a1
	movea.l a2, a0
	move.l d5, d0
	bsr.w nameEquals
	beq.s scanBinding
	move.l d1, d2
	lsl.l #2, d2
	lea BindingValues, a0
	move.l 0(a0, d2.l), d3
	moveq #0, d0
	bra.s resolveReturn
notFound
	moveq #1, d0
resolveReturn
	movem.l (sp)+, d1-d2/d4-d5/a0-a2
	rts
	.bend  ; resolveBindingV1

; Resolve `.len(sequence)` or `sequence[index]`.
; Inputs: A0/D0 = trimmed expression text/length.
; Outputs: D0 = status; D3 = scalar value.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
resolveSequenceExpressionV1	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	move.l d0, d7
	cmpi.l #6, d7
	bcs.w indexExpression
	cmpi.b #'.', (a0)
	bne.w indexExpression
	cmpi.b #'l', 1(a0)
	bne.w indexExpression
	cmpi.b #'e', 2(a0)
	bne.w indexExpression
	cmpi.b #'n', 3(a0)
	bne.w indexExpression
	cmpi.b #'(', 4(a0)
	bne.w indexExpression
	addq.l #5, a0
	subq.l #5, d7
	movea.l a0, a2
	clr.l d5
lenName
	tst.l d7
	beq.w fail
	cmpi.b #')', (a0)
	beq.s lenLookup
	addq.l #1, a0
	subq.l #1, d7
	addq.l #1, d5
	bra.s lenName
lenLookup
	tst.l d5
	beq.w fail
	cmpi.l #1, d7
	bne.w fail
	movea.l a2, a0
	move.l d5, d0
	bsr.w findSequenceV1
	bne.w fail
	move.l d2, d3
	bra.w ok

indexExpression
	movea.l a0, a2
	clr.l d5
findBracket
	tst.l d7
	beq.w fail
	cmpi.b #'[', (a0)
	beq.s indexStart
	addq.l #1, a0
	subq.l #1, d7
	addq.l #1, d5
	bra.s findBracket
indexStart
	tst.l d5
	beq.w fail
	addq.l #1, a0
	subq.l #1, d7
	movea.l a0, a3
	bsr.w parseNumber
	bne.w fail
	cmpa.l a3, a0
	beq.w fail
	move.l d3, d6
	cmpi.l #1, d7
	bne.w fail
	cmpi.b #']', (a0)
	bne.w fail
	movea.l a2, a0
	move.l d5, d0
	bsr.w findSequenceV1
	bne.w fail
	cmp.l d2, d6
	bhs.w fail
	lsl.l #2, d6
	move.l 0(a1, d6.l), d3
ok
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; resolveSequenceExpressionV1

	.priv

skipWhitespace	.block
loop
	tst.l d7
	beq.s done
	cmpi.b #' ', (a0)
	beq.s skip
	cmpi.b #9, (a0)
	bne.s done
skip
	addq.l #1, a0
	subq.l #1, d7
	bra.s loop
done
	rts
	.bend  ; skipWhitespace

parseNumber	.block
	clr.l d3
	tst.l d7
	beq.s fail
	cmpi.b #'$', (a0)
	beq.s hexStart

decimalLoop
	tst.l d7
	beq.s ok
	moveq #0, d1
	move.b (a0), d1
	cmpi.b #'0', d1
	blo.s ok
	cmpi.b #'9', d1
	bhi.s ok
	subi.b #'0', d1
	move.l d3, d2
	lsl.l #3, d3
	add.l d2, d3
	add.l d2, d3
	add.l d1, d3
	addq.l #1, a0
	subq.l #1, d7
	bra.s decimalLoop

hexStart
	addq.l #1, a0
	subq.l #1, d7
	tst.l d7
	beq.s fail

hexLoop
	tst.l d7
	beq.s ok
	moveq #0, d1
	move.b (a0), d1
	cmpi.b #'0', d1
	blo.s ok
	cmpi.b #'9', d1
	bls.s hexDigit
	ori.b #32, d1
	cmpi.b #'a', d1
	blo.s ok
	cmpi.b #'f', d1
	bhi.s ok
	subi.b #'a' - 10, d1
	bra.s appendHex
hexDigit
	subi.b #'0', d1
appendHex
	lsl.l #4, d3
	or.l d1, d3
	addq.l #1, a0
	subq.l #1, d7
	bra.s hexLoop

ok
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; parseNumber

nameEquals	.block
	tst.l d0
	beq.s checkEnd
loop
	move.b (a0)+, d3
	move.b (a1)+, d4
	cmpi.b #'A', d3
	blo.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	ori.b #32, d3
compare
	cmpi.b #'A', d4
	bcs.s compareNormalized
	cmpi.b #'Z', d4
	bhi.s compareNormalized
	ori.b #32, d4
compareNormalized
	cmp.b d4, d3
	bne.s no
	subq.l #1, d0
	bne.s loop
checkEnd
	tst.b (a1)
	bne.s no
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; nameEquals

	.endsection

	.section bss, kind=bss
SequenceCount
	.res word, 1
SequenceLengths
	.res word, SEQUENCE_CAPACITY
SequenceNames
	.res byte, SEQUENCE_CAPACITY * SEQUENCE_NAME_CAPACITY
SequenceValues
	.res long, SEQUENCE_CAPACITY * SEQUENCE_ELEMENT_CAPACITY
BindingDepth
	.res word, 1
BindingNames
	.res byte, BINDING_CAPACITY * SEQUENCE_NAME_CAPACITY
BindingValues
	.res long, BINDING_CAPACITY
	.endsection
	.endmodule
