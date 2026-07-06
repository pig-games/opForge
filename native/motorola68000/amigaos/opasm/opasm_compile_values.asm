; Native opasm compile-time sequence storage.

	.module opasm.amigaos.compile_values
	.cpu 68020

SEQUENCE_CAPACITY = 8
SEQUENCE_NAME_CAPACITY = 32
SEQUENCE_ELEMENT_CAPACITY = 16

	.section code, kind=code
	.pub

; Reset all session-local compile-time sequences.
; Outputs: D0 = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resetV1	.block
	clr.w SequenceCount
	moveq #0, d0
	rts
	.bend  ; resetV1

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
	.endsection
	.endmodule
