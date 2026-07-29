; Native opasm non-structural directive routing.

	.module opasm.amigaos.directive_router
	.cpu 68020
	.pub

OPASM_DIRECTIVE_NONE = 0
OPASM_DIRECTIVE_ORG = 1
OPASM_DIRECTIVE_CPU = 2
OPASM_DIRECTIVE_CONST = 3
OPASM_DIRECTIVE_VAR = 4
OPASM_DIRECTIVE_SET = 5
OPASM_DIRECTIVE_END = 6
OPASM_DIRECTIVE_REGION = 7
OPASM_DIRECTIVE_SECTION = 8
OPASM_DIRECTIVE_ENDSECTION = 9
OPASM_DIRECTIVE_PLACE = 10
OPASM_DIRECTIVE_ALIGN = 11
OPASM_DIRECTIVE_DS = 12
OPASM_DIRECTIVE_RES = 13
OPASM_DIRECTIVE_FILL = 14
OPASM_DIRECTIVE_BYTE = 15
OPASM_DIRECTIVE_WORD = 16
OPASM_DIRECTIVE_LONG = 17
OPASM_DIRECTIVE_TEXT = 18
OPASM_DIRECTIVE_NULL = 19
OPASM_DIRECTIVE_PTEXT = 20
	.section code, kind=code

; Classify one non-structural assembly directive.
;
; Inputs:
; - A0: mnemonic text.
; - D0: mnemonic byte length.
;
; Outputs:
; - D0: zero.
; - D3.W: OPASM_DIRECTIVE_* classification, or NONE.
;
; Clobbers:
; - D0-D4/A0-A2/CCR.
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
classifyV1	.block
	movem.l d5-d6, -(sp)
	movea.l a0, a2
	move.l d0, d6
	lea DirectiveOrgText, a2
	moveq #3, d1
	bsr.w directiveTry
	bne.w classifyOrg
	lea DirectiveCpuText, a2
	moveq #3, d1
	bsr.w directiveTry
	bne.w classifyCpu
	lea DirectiveConstText, a2
	moveq #5, d1
	bsr.w directiveTry
	bne.w classifyConst
	lea DirectiveVarText, a2
	moveq #3, d1
	bsr.w directiveTry
	bne.w classifyVar
	lea DirectiveSetText, a2
	moveq #3, d1
	bsr.w directiveTry
	bne.w classifySet
	lea DirectiveEndText, a2
	moveq #3, d1
	bsr.w directiveTry
	bne.w classifyEnd
	lea DirectiveRegionText, a2
	moveq #6, d1
	bsr.w directiveTry
	bne.w classifyRegion
	lea DirectiveSectionText, a2
	moveq #7, d1
	bsr.w directiveTry
	bne.w classifySection
	lea DirectiveEndsectionText, a2
	moveq #10, d1
	bsr.w directiveTry
	bne.w classifyEndsection
	lea DirectivePlaceText, a2
	moveq #5, d1
	bsr.w directiveTry
	bne.w classifyPlace
	lea DirectiveAlignText, a2
	moveq #5, d1
	bsr.w directiveTry
	bne.w classifyAlign
	lea DirectiveDsText, a2
	moveq #2, d1
	bsr.w directiveTry
	bne.w classifyDs
	lea DirectiveResText, a2
	moveq #3, d1
	bsr.w directiveTry
	bne.w classifyRes
	lea DirectiveFillText, a2
	moveq #4, d1
	bsr.w directiveTry
	bne.w classifyFill
	lea DirectiveByteText, a2
	moveq #4, d1
	bsr.w directiveTry
	bne.w classifyByte
	lea DirectiveDbText, a2
	moveq #2, d1
	bsr.w directiveTry
	bne.w classifyByte
	lea DirectiveWordText, a2
	moveq #4, d1
	bsr.w directiveTry
	bne.w classifyWord
	lea DirectiveDwText, a2
	moveq #2, d1
	bsr.w directiveTry
	bne.w classifyWord
	lea DirectiveLongText, a2
	moveq #4, d1
	bsr.w directiveTry
	bne.w classifyLong
	lea DirectiveTextText, a2
	moveq #4, d1
	bsr.w directiveTry
	bne.w classifyText
	lea DirectiveNullText, a2
	moveq #4, d1
	bsr.w directiveTry
	bne.w classifyNull
	lea DirectivePtextText, a2
	moveq #5, d1
	bsr.w directiveTry
	bne.w classifyPtext
classifyNone
	clr.w d3
	moveq #0, d0
	movem.l (sp)+, d5-d6
	rts
classifyOrg
	moveq #OPASM_DIRECTIVE_ORG, d3
	bra.w classifyDone
classifyCpu
	moveq #OPASM_DIRECTIVE_CPU, d3
	bra.w classifyDone
classifyConst
	moveq #OPASM_DIRECTIVE_CONST, d3
	bra.w classifyDone
classifyVar
	moveq #OPASM_DIRECTIVE_VAR, d3
	bra.w classifyDone
classifySet
	moveq #OPASM_DIRECTIVE_SET, d3
	bra.w classifyDone
classifyEnd
	moveq #OPASM_DIRECTIVE_END, d3
	bra.w classifyDone
classifyRegion
	moveq #OPASM_DIRECTIVE_REGION, d3
	bra.w classifyDone
classifySection
	moveq #OPASM_DIRECTIVE_SECTION, d3
	bra.w classifyDone
classifyEndsection
	moveq #OPASM_DIRECTIVE_ENDSECTION, d3
	bra.w classifyDone
classifyPlace
	moveq #OPASM_DIRECTIVE_PLACE, d3
	bra.w classifyDone
classifyAlign
	moveq #OPASM_DIRECTIVE_ALIGN, d3
	bra.w classifyDone
classifyDs
	moveq #OPASM_DIRECTIVE_DS, d3
	bra.w classifyDone
classifyRes
	moveq #OPASM_DIRECTIVE_RES, d3
	bra.w classifyDone
classifyFill
	moveq #OPASM_DIRECTIVE_FILL, d3
	bra.w classifyDone
classifyByte
	moveq #OPASM_DIRECTIVE_BYTE, d3
	bra.w classifyDone
classifyWord
	moveq #OPASM_DIRECTIVE_WORD, d3
	bra.w classifyDone
classifyLong
	moveq #OPASM_DIRECTIVE_LONG, d3
	bra.w classifyDone
classifyText
	moveq #OPASM_DIRECTIVE_TEXT, d3
	bra.w classifyDone
classifyNull
	moveq #OPASM_DIRECTIVE_NULL, d3
	bra.w classifyDone
classifyPtext
	moveq #OPASM_DIRECTIVE_PTEXT, d3
classifyDone
	moveq #0, d0
	movem.l (sp)+, d5-d6
	rts
	.bend  ; classifyV1

; Restore the saved mnemonic length before one token comparison.
; Inputs: D6 = mnemonic length; A0/A2/D1 as directiveLineStartsWith.
; Outputs: D0 = match status.
; Clobbers: D0-D4/A0-A3/CCR.
; CCR: reflects D0 on return.
directiveTry	.block
	move.l d6, d0
	bra.w directiveLineStartsWith
	.bend  ; directiveTry

; Compare one mnemonic to one lower-case directive token.
; Inputs: A0/D0 = mnemonic; A2 = token; D1 = token length.
; Outputs: D0 = 1 match, 0 otherwise.
; Clobbers: D2-D4/A0-A3/CCR.
; CCR: reflects D0 on return.
directiveLineStartsWith	.block
	movem.l d2-d4/a0-a3, -(sp)
	tst.l d0
	beq.w directiveNo
	cmpi.b #'.', (a0)
	bne.w directiveCompareStart
	addq.l #1, a0
	subq.l #1, d0
directiveCompareStart
	cmp.l d1, d0
	bcs.w directiveNo
	movea.l a0, a1
	movea.l a2, a3
	move.l d1, d2
	beq.w directiveBoundary
	subq.l #1, d2
directiveLoop
	move.b (a1)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.w directiveCompare
	cmpi.b #'Z', d3
	bhi.w directiveCompare
	addi.b #32, d3
directiveCompare
	cmp.b d4, d3
	bne.w directiveNo
	dbra d2, directiveLoop
directiveBoundary
	cmp.l d1, d0
	beq.w directiveYes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.w directiveYes
	cmpi.b #9, d3
	beq.w directiveYes
	cmpi.b #';', d3
	beq.w directiveYes
directiveNo
	movem.l (sp)+, d2-d4/a0-a3
	moveq #0, d0
	rts
directiveYes
	movem.l (sp)+, d2-d4/a0-a3
	moveq #1, d0
	rts
	.bend  ; directiveLineStartsWith

	.endsection
	.section data, kind=data

DirectiveOrgText
	.byte "org", 0
DirectiveCpuText
	.byte "cpu", 0
DirectiveConstText
	.byte "const", 0
DirectiveVarText
	.byte "var", 0
DirectiveSetText
	.byte "set", 0
DirectiveEndText
	.byte "end", 0
DirectiveRegionText
	.byte "region", 0
DirectiveSectionText
	.byte "section", 0
DirectiveEndsectionText
	.byte "endsection", 0
DirectivePlaceText
	.byte "place", 0
DirectiveAlignText
	.byte "align", 0
DirectiveDsText
	.byte "ds", 0
DirectiveResText
	.byte "res", 0
DirectiveFillText
	.byte "fill", 0
DirectiveByteText
	.byte "byte", 0
DirectiveDbText
	.byte "db", 0
DirectiveWordText
	.byte "word", 0
DirectiveDwText
	.byte "dw", 0
DirectiveLongText
	.byte "long", 0
DirectiveTextText
	.byte "text", 0
DirectiveNullText
	.byte "null", 0
DirectivePtextText
	.byte "ptext", 0

	.endsection
	.endmodule
