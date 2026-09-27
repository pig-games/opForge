NumberFrame	.struct
Count	.long ?
Capacity	.long ?
Rules	.long ?
Flags	.long ?
RuleCount	.long ?
NextPc	.long ?
Spelling	.long ?
Length	.long ?
Radix	.long ?
NextRule	.long ?
Body	.long ?
BodyLength	.long ?
RulesRemaining	.long ?
RawEnd	.long ?
RuleFlags	.long ?
	.endstruct
NUMBER_FRAME_SIZE = NumberFrame.RuleFlags+4
; The flags word is zero extended into a big-endian long frame slot.
FLAGS_BYTE = NumberFrame.Flags+3
SAVED_SOURCE_CURSOR = NUMBER_FRAME_SIZE+4
RULE_FLAG_TERMINAL = 1

; Package-owned numeric rules normalize only number records after scanning.
; Inputs: interpreter A0=PC, A1=end, A5=records, A6=scratch; D1=count,
; D3=used, D6=capacity. Outputs: A0=next PC, D3=used, D0=status/CCR.
; Other interpreter registers are preserved. Values are stored big endian.
normalizeNumbers	.block
	.TOKEN_SCOPE_BEGIN #0
	movem.l d1-d2/d4-d7/a2-a5, -(sp)
	suba.w #NUMBER_FRAME_SIZE, sp
	movea.l sp, a2
	move.l d1, NumberFrame.Count(a2)
	move.l d6, NumberFrame.Capacity(a2)
	move.l a0, NumberFrame.Rules(a2)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w badProgram
	moveq #0, d7
	move.b (a0)+, d7
	move.l d7, NumberFrame.Flags(a2)
	andi.b #$fc, d7
	bne.w badProgram
	moveq #0, d7
	move.b (a0)+, d7
	beq.w badProgram
	move.l d7, NumberFrame.RuleCount(a2)
	move.l a0, NumberFrame.Rules(a2)
validateRule
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w badProgram
	moveq #0, d4
	move.b (a0)+, d4
	moveq #0, d5
	move.b (a0)+, d5
	moveq #0, d6
	move.b (a0)+, d6
	cmpi.b #2, d6
	blo.w badProgram
	cmpi.b #36, d6
	bhi.w badProgram
	move.b (a0)+, d0
	andi.b #$fe, d0
	bne.w badProgram
	add.l d5, d4
	move.l a1, d0
	sub.l a0, d0
	cmp.l d0, d4
	bhi.w badProgram
	adda.l d4, a0
	subq.l #1, d7
	bne.w validateRule
	; Every policy terminates in a decimal fallback with no markers.
	tst.l d4
	bne.w badProgram
	cmpi.b #10, d6
	bne.w badProgram
	move.l a0, NumberFrame.NextPc(a2)
recordLoop
	tst.l NumberFrame.Count(a2)
	beq.w success
	cmpi.w #TK_KIND_NUMBER, (a5)
	bne.w nextRecord
	move.l 12(a5), d0
	cmp.l d3, d0
	bhi.w badProgram
	move.l 16(a5), d1
	move.l d3, d2
	sub.l d0, d2
	cmp.l d2, d1
	bhi.w badProgram
	lea 0(a6, d0.l), a3
	move.l a3, NumberFrame.Spelling(a2)
	move.l d1, NumberFrame.Length(a2)
	movea.l NumberFrame.Rules(a2), a4
	move.l NumberFrame.RuleCount(a2), d7
ruleLoop
	move.l d7, NumberFrame.RulesRemaining(a2)
	moveq #0, d4
	move.b (a4)+, d4
	moveq #0, d5
	move.b (a4)+, d5
	moveq #0, d6
	move.b (a4)+, d6
	move.l d6, NumberFrame.Radix(a2)
	moveq #0, d0
	move.b (a4)+, d0
	move.l d0, NumberFrame.RuleFlags(a2)
	move.l d4, d0
	add.l d5, d0
	lea 0(a4, d0.l), a0
	move.l a0, NumberFrame.NextRule(a2)
	movea.l NumberFrame.Spelling(a2), a3
	move.l a3, d2
	add.l NumberFrame.Length(a2), d2
	move.l d2, NumberFrame.RawEnd(a2)
	move.l d4, d6
prefixLoop
	tst.l d6
	beq.w suffixStart
prefixByte
	cmpa.l d2, a3
	bcc.w nextRule
	move.b (a3)+, d0
	btst #0, FLAGS_BYTE(a2)
	beq.w comparePrefix
	cmpi.b #'_', d0
	beq.w prefixByte
comparePrefix
	move.b (a4)+, d1
	bsr.w markerEqual
	bne.w nextRule
	subq.l #1, d6
	bra.w prefixLoop
suffixStart
	move.l a3, NumberFrame.Body(a2)
	movea.l NumberFrame.RawEnd(a2), a3
	adda.l d5, a4
suffixLoop
	tst.l d5
	beq.w matchedBody
suffixByte
	cmpa.l NumberFrame.Body(a2), a3
	bls.w nextRule
	move.b -(a3), d0
	btst #0, FLAGS_BYTE(a2)
	beq.w compareSuffix
	cmpi.b #'_', d0
	beq.w suffixByte
compareSuffix
	move.b -(a4), d1
	bsr.w markerEqual
	bne.w nextRule
	subq.l #1, d5
	bra.w suffixLoop
matchedBody
	move.l a3, d0
	sub.l NumberFrame.Body(a2), d0
	move.l d0, NumberFrame.BodyLength(a2)
	bra.w parseBody
nextRule
	move.l NumberFrame.RulesRemaining(a2), d7
	movea.l NumberFrame.NextRule(a2), a4
	subq.l #1, d7
	bne.w ruleLoop
	move.w #NUMBER_MALFORMED, 2(a5)
	bra.w nextRecord
parseBody
	movea.l NumberFrame.Body(a2), a3
	move.l NumberFrame.BodyLength(a2), d7
	moveq #0, d4  ; value high
	moveq #0, d5  ; value low
	moveq #0, d6  ; digit seen
bodyLoop
	tst.l d7
	beq.w bodyDone
	subq.l #1, d7
	moveq #0, d0
	move.b (a3)+, d0
	cmpi.b #'_', d0
	bne.w digit
	btst #0, FLAGS_BYTE(a2)
	beq.w bodyInvalid
	bra.w bodyLoop
digit
	cmpi.b #'0', d0
	blo.w bodyInvalid
	cmpi.b #'9', d0
	bls.w decimalDigit
	ori.b #$20, d0
	subi.b #'a', d0
	cmpi.b #25, d0
	bhi.w bodyInvalid
	addi.l #10, d0
	bra.w checkDigit
decimalDigit
	subi.b #'0', d0
checkDigit
	cmp.l NumberFrame.Radix(a2), d0
	bcc.w bodyInvalid
	; Multiply by repeated checked additions (radix <=36), retaining u64.
	move.l d4, d1
	move.l d5, d2
	move.l NumberFrame.Radix(a2), d6
	subq.l #1, d6
multiplyLoop
	add.l d2, d5
	addx.l d1, d4
	bcs.w overflow
	subq.l #1, d6
	bne.w multiplyLoop
	add.l d0, d5
	moveq #0, d1
	addx.l d1, d4
	bcs.w overflow
	moveq #1, d6
	bra.w bodyLoop
bodyInvalid
	tst.l NumberFrame.RuleFlags(a2)
	beq.w nextRule
	move.w #NUMBER_MALFORMED, 2(a5)
	bra.w nextRecord
overflow
	move.w #NUMBER_OVERFLOW, 2(a5)
	bra.w nextRecord
bodyDone
	tst.l d6
	beq.w bodyInvalid
	move.l NumberFrame.Length(a2), d0
	addq.l #8, d0
	add.l d3, d0
	bcs.w scratchFull
	cmp.l NumberFrame.Capacity(a2), d0
	bhi.w scratchFull
	movea.l NumberFrame.Spelling(a2), a3
	lea 0(a6, d3.l), a4
	move.l d3, 12(a5)
	move.l NumberFrame.Length(a2), d1
copySpelling
	tst.l d1
	beq.w storeValue
	move.b (a3)+, (a4)+
	subq.l #1, d1
	bra.w copySpelling
storeValue
	move.l d4, (a4)+
	move.l d5, (a4)
	move.l d0, d3
	move.w #NUMBER_VALID, 2(a5)
nextRecord
	adda.w #TOKEN_RECORD_SIZE, a5
	subq.l #1, NumberFrame.Count(a2)
	bra.w recordLoop
success
	movea.l NumberFrame.NextPc(a2), a0
	moveq #TK_STATUS_SUCCESS, d0
	bra.w done
badProgram
	moveq #TK_STATUS_INVALID_PROGRAM, d0
	bra.w done
scratchFull
	move.l 4(a5), d0
	subq.l #1, d0
	move.l d0, SAVED_SOURCE_CURSOR(a2)
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
done
	.TOKEN_SCOPE_END #0
	adda.w #NUMBER_FRAME_SIZE, sp
	movem.l (sp)+, d1-d2/d4-d7/a2-a5
	rts
	.bend  ; normalizeNumbers

; Compare marker bytes in D0/D1 using only the package's ASCII folding flag.
; Clobbers D0/D1; CCR is zero for equality.
markerEqual	.block
	btst #1, FLAGS_BYTE(a2)
	beq.w compare
	cmpi.b #'A', d0
	blo.w foldSecond
	cmpi.b #'Z', d0
	bhi.w foldSecond
	ori.b #$20, d0
foldSecond
	cmpi.b #'A', d1
	blo.w compare
	cmpi.b #'Z', d1
	bhi.w compare
	ori.b #$20, d1
compare
	cmp.b d1, d0
	rts
	.bend  ; markerEqual
