; Package-selected composed names retain lexical records and attach packed recipes.
ComposeFrame	.struct
Remaining	.long ?
Capacity	.long ?
NextPc	.long ?
Marker	.long ?
Minimum	.long ?
Maximum	.long ?
SuffixCount	.long ?
SuffixBytes	.long ?
Head	.long ?
RunCount	.long ?
RunEnd	.long ?
IteratorRecord	.long ?
IteratorCount	.long ?
IteratorBytes	.long ?
IteratorPointer	.long ?
PayloadLength	.long ?
FragmentCount	.long ?
LiteralLength	.long ?
PositionalCount	.long ?
LiteralCount	.long ?
SuffixBitmap	.res 32
Payload	.res 256
	.endstruct
COMPOSE_FRAME_BYTES = ComposeFrame.Payload+256
COMPOSE_SAVED_CURSOR = COMPOSE_FRAME_BYTES+4
COMPOSE_PAYLOAD_LIMIT = 255
COMPOSE_FRAGMENT_LITERAL = 0

; Interpreter inputs/outputs match normalizeNumbers. Preserve all other registers;
; D2 reports the failing head's zero-based source cursor on scratch overflow.
composeNames	.block
	.TOKEN_SCOPE_BEGIN #0
	movem.l d1-d2/d4-d7/a1-a5, -(sp)
	suba.w #COMPOSE_FRAME_BYTES, sp
	movea.l sp, a2
	move.l d1, ComposeFrame.Remaining(a2)
	move.l d6, ComposeFrame.Capacity(a2)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w badProgram
	moveq #0, d0
	move.b (a0)+, d0
	move.l d0, ComposeFrame.Marker(a2)
	cmpi.b #33, d0
	blo.w badProgram
	cmpi.b #126, d0
	bhi.w badProgram
	cmpi.b #'0', d0
	blo.w markerValid
	cmpi.b #'9', d0
	bls.w badProgram
	cmpi.b #'A', d0
	blo.w markerValid
	cmpi.b #'Z', d0
	bls.w badProgram
	cmpi.b #'a', d0
	blo.w markerValid
	cmpi.b #'z', d0
	bls.w badProgram
markerValid
	moveq #0, d0
	move.b (a0)+, d0
	cmpi.b #1, d0
	blo.w badProgram
	cmpi.b #9, d0
	bhi.w badProgram
	move.l d0, ComposeFrame.Minimum(a2)
	moveq #0, d0
	move.b (a0)+, d0
	cmp.l ComposeFrame.Minimum(a2), d0
	blo.w badProgram
	cmpi.b #9, d0
	bhi.w badProgram
	move.l d0, ComposeFrame.Maximum(a2)
	moveq #0, d0
	move.b (a0)+, d0
	tst.l d0
	beq.w badProgram
	move.l d0, ComposeFrame.SuffixCount(a2)
	move.l a0, ComposeFrame.SuffixBytes(a2)
	move.l a1, d1
	sub.l a0, d1
	cmp.l d1, d0
	bhi.w badProgram
	move.l d0, d4
	; Validate membership and uniqueness once in a bounded byte bitmap.
	lea ComposeFrame.SuffixBitmap(a2), a3
	moveq #7, d1
clearSuffixBitmap
	clr.l (a3)+
	dbra d1, clearSuffixBitmap
validateSuffix
	tst.l d4
	beq.w validated
	moveq #0, d0
	move.b (a0)+, d0
	cmpi.b #33, d0
	blo.w badProgram
	cmpi.b #126, d0
	bhi.w badProgram
	cmp.l ComposeFrame.Marker(a2), d0
	beq.w badProgram
	move.l d0, d1
	lsr.l #3, d1
	lea ComposeFrame.SuffixBitmap(a2), a3
	bset d0, 0(a3, d1.w)
	bne.w badProgram
	subq.l #1, d4
	bra.w validateSuffix
validated
	move.l a0, ComposeFrame.NextPc(a2)
	; A later policy operation replaces recipe annotations, preserving numbers.
	movea.l a5, a3
	move.l ComposeFrame.Remaining(a2), d4
clearAnnotations
	tst.l d4
	beq.w headLoop
	andi.w #TOKEN_RECIPE_LOW_MASK, 2(a3)
	adda.w #TOKEN_RECORD_SIZE, a3
	subq.l #1, d4
	bra.w clearAnnotations
headLoop
	tst.l ComposeFrame.Remaining(a2)
	beq.w success
	moveq #0, d0
	move.w (a5), d0
	cmpi.w #TK_KIND_IDENTIFIER+1, d0
	bls.w candidate
	movea.l a5, a0
	bsr.w markerRecord
	tst.l d0
	beq.w nextHead
candidate
	move.l a5, ComposeFrame.Head(a2)
	movea.l a5, a3
	moveq #1, d4
	move.l 8(a3), d5
collectRun
	cmp.l ComposeFrame.Remaining(a2), d4
	bcc.w runReady
	lea TOKEN_RECORD_SIZE(a3), a0
	cmp.l 4(a0), d5
	bne.w runReady
	moveq #0, d0
	move.w TOKEN_RECORD_SIZE(a3), d0
	cmpi.w #TK_KIND_NUMBER, d0
	bls.w includeRecord
	lea TOKEN_RECORD_SIZE(a3), a0
	bsr.w markerRecord
	tst.l d0
	beq.w runReady
includeRecord
	adda.w #TOKEN_RECORD_SIZE, a3
	move.l 8(a3), d5
	addq.l #1, d4
	bra.w collectRun
runReady
	move.l d4, ComposeFrame.RunCount(a2)
	move.l d5, ComposeFrame.RunEnd(a2)
	bsr.w resetIterator
	moveq #0, d7
findAttempt
	bsr.w nextByte
	tst.l d0
	bmi.w skipRun
	tst.l d7
	beq.w rememberMarker
	cmpi.b #'0', d0
	blo.w rememberMarker
	cmpi.b #'9', d0
	bls.w buildRecipe
rememberMarker
	moveq #0, d7
	cmp.l ComposeFrame.Marker(a2), d0
	seq d7
	bra.w findAttempt
buildRecipe
	bsr.w resetIterator
	moveq #2, d0
	move.l d0, ComposeFrame.PayloadLength(a2)
	clr.l ComposeFrame.FragmentCount(a2)
	clr.l ComposeFrame.LiteralLength(a2)
	clr.l ComposeFrame.PositionalCount(a2)
	clr.l ComposeFrame.LiteralCount(a2)
	moveq #0, d0
	move.w (a5), d0
	cmpi.w #1, d0
	bls.w logicalReady
	moveq #0, d0
logicalReady
	move.b d0, ComposeFrame.Payload(a2)
parseLoop
	bsr.w nextByte
	tst.l d0
	bmi.w recipeReady
	cmp.l ComposeFrame.Marker(a2), d0
	beq.w positional
	tst.l ComposeFrame.PositionalCount(a2)
	bne.w checkSuffix
	cmpi.w #1, (a5)
	bls.w literal
checkSuffix
	move.l d0, d1
	lsr.l #3, d1
	lea ComposeFrame.SuffixBitmap(a2), a0
	btst d0, 0(a0, d1.w)
	beq.w invalidRecipe
literal
	move.l ComposeFrame.LiteralLength(a2), d1
	bne.w appendLiteral
	move.l ComposeFrame.PayloadLength(a2), d1
	cmpi.l #COMPOSE_PAYLOAD_LIMIT-2, d1
	bcc.w invalidRecipe
	lea ComposeFrame.Payload(a2), a0
	adda.l d1, a0
	clr.b (a0)+
	move.l a0, ComposeFrame.LiteralLength(a2)
	clr.b (a0)
	addq.l #2, ComposeFrame.PayloadLength(a2)
	addq.l #1, ComposeFrame.FragmentCount(a2)
appendLiteral
	move.l ComposeFrame.PayloadLength(a2), d1
	cmpi.l #COMPOSE_PAYLOAD_LIMIT, d1
	bcc.w invalidRecipe
	lea ComposeFrame.Payload(a2), a0
	adda.l d1, a0
	move.b d0, (a0)
	addq.l #1, ComposeFrame.PayloadLength(a2)
	movea.l ComposeFrame.LiteralLength(a2), a0
	addq.b #1, (a0)
	bcs.w invalidRecipe
	addq.l #1, ComposeFrame.LiteralCount(a2)
	bra.w parseLoop
positional
	clr.l ComposeFrame.LiteralLength(a2)
	bsr.w nextByte
	tst.l d0
	bmi.w invalidRecipe
	subi.l #'0', d0
	cmp.l ComposeFrame.Minimum(a2), d0
	blo.w invalidRecipe
	cmp.l ComposeFrame.Maximum(a2), d0
	bhi.w invalidRecipe
	move.l ComposeFrame.PayloadLength(a2), d1
	cmpi.l #COMPOSE_PAYLOAD_LIMIT, d1
	bcc.w invalidRecipe
	lea ComposeFrame.Payload(a2), a0
	adda.l d1, a0
	move.b d0, (a0)
	addq.l #1, ComposeFrame.PayloadLength(a2)
	addq.l #1, ComposeFrame.FragmentCount(a2)
	addq.l #1, ComposeFrame.PositionalCount(a2)
	bra.w parseLoop
recipeReady
	tst.l ComposeFrame.PositionalCount(a2)
	beq.w skipRun
	tst.l ComposeFrame.LiteralCount(a2)
	beq.w skipRun
	move.l ComposeFrame.FragmentCount(a2), d0
	cmpi.l #255, d0
	bhi.w invalidRecipe
	move.b d0, ComposeFrame.Payload+1(a2)
	move.l ComposeFrame.RunCount(a2), d0
	cmpi.l #255, d0
	bhi.w invalidRecipe
	move.l d3, d0
	add.l 16(a5), d0
	bcs.w scratchFull
	addq.l #2, d0
	bcs.w scratchFull
	add.l ComposeFrame.PayloadLength(a2), d0
	bcs.w scratchFull
	cmp.l ComposeFrame.Capacity(a2), d0
	bhi.w scratchFull
	move.l d0, d7
	lea 0(a6, d3.l), a1
	move.l 12(a5), d0
	lea 0(a6, d0.l), a0
	move.l 16(a5), d1
copySpelling
	tst.l d1
	beq.w copyMetadata
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bra.w copySpelling
copyMetadata
	move.l ComposeFrame.RunCount(a2), d0
	move.b d0, (a1)+
	move.l ComposeFrame.PayloadLength(a2), d1
	move.b d1, (a1)+
	lea ComposeFrame.Payload(a2), a0
copyPayload
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w copyPayload
	move.l d3, 12(a5)
	move.l d7, d3
	ori.w #TOKEN_RECIPE_VALID, 2(a5)
	bra.w skipRun
invalidRecipe
	ori.w #TOKEN_RECIPE_INVALID, 2(a5)
skipRun
	move.l ComposeFrame.RunCount(a2), d0
	sub.l d0, ComposeFrame.Remaining(a2)
	mulu.w #TOKEN_RECORD_SIZE, d0
	adda.l d0, a5
	bra.w headLoop
nextHead
	adda.w #TOKEN_RECORD_SIZE, a5
	subq.l #1, ComposeFrame.Remaining(a2)
	bra.w headLoop
success
	movea.l ComposeFrame.NextPc(a2), a0
	moveq #TK_STATUS_SUCCESS, d0
	bra.w done
badProgram
	moveq #TK_STATUS_INVALID_PROGRAM, d0
	bra.w done
scratchFull
	move.l 4(a5), d0
	subq.l #1, d0
	move.l d0, COMPOSE_SAVED_CURSOR(a2)
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
done
	.TOKEN_SCOPE_END #0
	adda.w #COMPOSE_FRAME_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a1-a5
	rts
	.bend  ; composeNames

; Reset the lexeme iterator over the selected contiguous run. Clobbers D0/A0.
resetIterator	.block
	move.l ComposeFrame.Head(a2), ComposeFrame.IteratorRecord(a2)
	move.l ComposeFrame.RunCount(a2), ComposeFrame.IteratorCount(a2)
	clr.l ComposeFrame.IteratorBytes(a2)
	rts
	.bend  ; resetIterator

; Return next normalized lexeme byte in D0, or -1 at end. Clobbers D1/A0.
; Lexical record offsets were committed by the VM; spelling length excludes metadata.
nextByte	.block
	tst.l ComposeFrame.IteratorBytes(a2)
	bne.w read
	tst.l ComposeFrame.IteratorCount(a2)
	beq.w eof
	movea.l ComposeFrame.IteratorRecord(a2), a0
	move.l 12(a0), d0
	lea 0(a6, d0.l), a1
	move.l a1, ComposeFrame.IteratorPointer(a2)
	move.l 16(a0), ComposeFrame.IteratorBytes(a2)
	addi.l #TOKEN_RECORD_SIZE, ComposeFrame.IteratorRecord(a2)
	subq.l #1, ComposeFrame.IteratorCount(a2)
	bra.w nextByte
read
	movea.l ComposeFrame.IteratorPointer(a2), a0
	moveq #0, d0
	move.b (a0)+, d0
	move.l a0, ComposeFrame.IteratorPointer(a2)
	subq.l #1, ComposeFrame.IteratorBytes(a2)
	rts
eof
	moveq #-1, d0
	rts
	.bend  ; nextByte

; D0=1 when the record spelling is exactly the selected standalone marker.
; A0=record, clobbers D0/D1/A1; CCR unspecified.
markerRecord	.block
	moveq #0, d0
	cmpi.w #TK_KIND_AT, (a0)
	bne.w done
	cmpi.l #1, 16(a0)
	bne.w done
	move.l 12(a0), d1
	lea 0(a6, d1.l), a1
	move.b (a1), d0
	cmp.l ComposeFrame.Marker(a2), d0
	seq d0
	andi.l #1, d0
done
	rts
	.bend  ; markerRecord
