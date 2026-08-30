; Native opasm statement-navigation helpers.

	.module opasm.amigaos.flow_navigation
	.cpu 68020

	.use opasm.amigaos.engine as eng

	.section code, kind=code
	.pub

; Initialize ordinary forward statement flow for one callback invocation.
; Inputs: D0.L = current statement index.
; Outputs: D0 = 0; D1 = 0 to process; D2.L = next statement index.
; Clobbers: D0-D2/CCR.
; CCR: reflects D0 on return.
initializeStatementFlowV1	.block
	move.l d0, d2
	addq.l #1, d2
	clr.w d1
	moveq #0, d0
	rts
	.bend  ; initializeStatementFlowV1

; Find the next same-level `.elseif`, `.else`, or `.endif` after an `.if` branch.
; Inputs: D7.L = current conditional statement index.
; Outputs: D0 = status; D1 = 1 elseif, 2 else, 3 endif; D2.L = statement index.
; Clobbers: D0-D6/A0-A1/CCR.
; CCR: reflects D0 on return.
findNextIfBranchV1	.block
	movem.l d3-d6/a0-a1, -(sp)
	move.l d7, d2
	moveq #1, d6
scan
	addq.l #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.l d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	move.l d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.l #2, d4
	beq.s maybeIf
	cmpi.l #3, d4
	bne.s maybeEndif
maybeIf
	moveq #0, d0
	move.w d4, d0
	lea FlowIfText, a1
	moveq #2, d1
	bsr.w flowMnemonicEquals
	beq.s maybeEndif
	addq.w #1, d6
	bra.w next
maybeEndif
	cmpi.l #5, d4
	beq.s compareEndif
	cmpi.l #6, d4
	bne.s outerBranch
compareEndif
	moveq #0, d0
	move.w d4, d0
	lea FlowEndifText, a1
	moveq #5, d1
	bsr.w flowMnemonicEquals
	beq.s outerBranch
	subq.w #1, d6
	beq.w foundEndif
outerBranch
	cmpi.w #1, d6
	bne.w next
	cmpi.l #6, d4
	beq.s compareElseif
	cmpi.l #7, d4
	bne.s maybeElse
compareElseif
	moveq #0, d0
	move.w d4, d0
	lea FlowElseifText, a1
	moveq #6, d1
	bsr.w flowMnemonicEquals
	beq.s maybeElse
	moveq #1, d1
	bra.w found
maybeElse
	cmpi.l #4, d4
	beq.s compareElse
	cmpi.l #5, d4
	bne.w next
compareElse
	moveq #0, d0
	move.w d4, d0
	lea FlowElseText, a1
	moveq #4, d1
	bsr.w flowMnemonicEquals
	beq.w next
	moveq #2, d1
	bra.w found
foundEndif
	moveq #3, d1
found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return
next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan
fail
	moveq #1, d0
return
	movem.l (sp)+, d3-d6/a0-a1
	rts
	.bend  ; findNextIfBranchV1

; Find a matching same-level `.endif` after one selected branch marker.
; Inputs: D7.L = current `.elseif` or `.else` statement index.
; Outputs: D0 = status; D2.L = matching endif index.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
findMatchingEndifV1	.block
	lea FlowIfText, a0
	moveq #2, d0
	lea FlowEndifText, a1
	moveq #5, d1
	bsr.w findMatchingPairV1
	rts
	.bend  ; findMatchingEndifV1

; Find the first same-level matching `.case`, otherwise `.default`, for `.match`.
; Inputs: A0 = callback that returns zero for a matching D7.L `.case`; D7.L = `.match` statement index.
; Outputs: D0 = status; D2.L = selected case/default/endmatch statement index.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
findSelectedMatchBranchV1	.block
	movem.l d1/d3-d6/a0-a2, -(sp)
	move.l a0, FlowCaseMatcher
	moveq #-1, d3
	move.l d7, d2
	moveq #1, d6
scan
	addq.l #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.l d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	move.l d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.w d4, d0
	lea FlowMatchText, a1
	moveq #5, d1
	bsr.w flowMnemonicEquals
	beq.s maybeEndmatch
	addq.w #1, d6
	bra.w next
maybeEndmatch
	moveq #0, d0
	move.w d4, d0
	lea FlowEndmatchText, a1
	moveq #8, d1
	bsr.w flowMnemonicEquals
	beq.s outerBranch
	subq.w #1, d6
	beq.w chooseDefault
outerBranch
	cmpi.w #1, d6
	bne.w next
	moveq #0, d0
	move.w d4, d0
	lea FlowCaseText, a1
	moveq #4, d1
	bsr.w flowMnemonicEquals
	beq.s maybeDefault
	move.l d2, d7
	movea.l FlowCaseMatcher, a2
	move.l d2, -(sp)
	jsr (a2)
	move.l (sp)+, d2
	tst.l d0
	bne.w next
	bra.w found
maybeDefault
	moveq #0, d0
	move.w d4, d0
	lea FlowDefaultText, a1
	moveq #7, d1
	bsr.w flowMnemonicEquals
	beq.w next
	move.l d2, d3
	bra.w next
chooseDefault
	cmpi.l #$ffffffff, d3
	beq.s found
	move.l d3, d2
found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return
next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan
fail
	moveq #1, d0
return
	movem.l (sp)+, d1/d3-d6/a0-a2
	rts
	.bend  ; findSelectedMatchBranchV1

; Find a matching same-level `.endmatch` after a selected case/default.
; Inputs: D7.L = current case/default statement index.
; Outputs: D0 = status; D2.L = matching end statement.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
findMatchingEndmatchV1	.block
	lea FlowMatchText, a0
	moveq #5, d0
	lea FlowEndmatchText, a1
	moveq #8, d1
	bsr.w findMatchingPairV1
	rts
	.bend  ; findMatchingEndmatchV1

; Find the matching `.endfor` for a zero-count `.for`.
; Inputs: D7.L = opening statement index.
; Outputs: D0 = status; D2.L = matching end statement.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
findMatchingEndforV1	.block
	lea FlowForText, a0
	moveq #3, d0
	lea FlowEndforText, a1
	moveq #6, d1
	bsr.w findMatchingPairV1
	rts
	.bend  ; findMatchingEndforV1

; Find the matching `.endwhile` for a false-first `.while`.
; Inputs: D7.L = opening statement index.
; Outputs: D0 = status; D2.L = matching end statement.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
findMatchingEndwhileV1	.block
	lea FlowWhileText, a0
	moveq #5, d0
	lea FlowEndwhileText, a1
	moveq #8, d1
	bsr.w findMatchingPairV1
	rts
	.bend  ; findMatchingEndwhileV1

; Find an end token at the same nesting level as the supplied opening token.
; Inputs: D7.L = opening index; A0/D0 = opening token; A1/D1 = ending token.
; Outputs: D0 = status; D2.L = matching end index on success.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
findMatchingPairV1	.block
	movem.l d3-d7/a0-a3, -(sp)
	movea.l a0, a2
	move.l d0, d5
	movea.l a1, a3
	move.l d1, d6
	move.l d7, d2
	moveq #1, d7
scan
	addq.l #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.l d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	move.l d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.w d4, d0
	movea.l a2, a1
	move.l d5, d1
	bsr.w flowMnemonicEquals
	beq.s maybeEnd
	addq.w #1, d7
	bra.s next
maybeEnd
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.w d4, d0
	movea.l a3, a1
	move.l d6, d1
	bsr.w flowMnemonicEquals
	beq.s next
	subq.w #1, d7
	beq.s found
next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan
found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d3-d7/a0-a3
	rts
	.bend  ; findMatchingPairV1

; Compare a mnemonic against a lower-case structural-flow token.
; Inputs: A0/D0 = mnemonic; A1/D1 = token and length.
; Outputs: D0 = 1 match, 0 otherwise.
; Clobbers: D0-D4/A0-A3/CCR.
; CCR: reflects D0 on return.
flowMnemonicEquals	.block
	movem.l d2-d4/a0-a3, -(sp)
	tst.l d0
	beq.s no
	cmpi.b #'.', (a0)
	bne.s compareStart
	addq.l #1, a0
	subq.l #1, d0
compareStart
	cmp.l d1, d0
	bcs.s no
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d2
	beq.s boundary
	subq.l #1, d2
loop
	move.b (a2)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	addi.b #32, d3
compare
	cmp.b d4, d3
	bne.s no
	dbra d2, loop
boundary
	cmp.l d1, d0
	beq.s yes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.s yes
	cmpi.b #9, d3
	beq.s yes
	cmpi.b #';', d3
	beq.s yes
no
	movem.l (sp)+, d2-d4/a0-a3
	moveq #0, d0
	rts
yes
	movem.l (sp)+, d2-d4/a0-a3
	moveq #1, d0
	rts
	.bend  ; flowMnemonicEquals

	.endsection

	.section data, kind=data

FlowIfText
	.byte "if", 0
FlowElseifText
	.byte "elseif", 0
FlowElseText
	.byte "else", 0
FlowEndifText
	.byte "endif", 0
FlowMatchText
	.byte "match", 0
FlowCaseText
	.byte "case", 0
FlowDefaultText
	.byte "default", 0
FlowEndmatchText
	.byte "endmatch", 0
FlowForText
	.byte "for", 0
FlowEndforText
	.byte "endfor", 0
FlowWhileText
	.byte "while", 0
FlowEndwhileText
	.byte "endwhile", 0

	.endsection

	.section bss, kind=bss

FlowCaseMatcher
	.res long, 1

	.endsection
	.endmodule
