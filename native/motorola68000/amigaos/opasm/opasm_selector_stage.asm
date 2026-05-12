; Native opasm selector/request staging for the initial AmigaOS CLI slice.

	.module opasm.amigaos.selector_stage
	.cpu 68020
	.pub
OPASM_SELECTOR_STATUS_OK                  = 0
OPASM_SELECTOR_STATUS_NO_OUTPUT           = 1
OPASM_SELECTOR_STATUS_UNKNOWN_MNEMONIC    = 2
OPASM_SELECTOR_STATUS_UNSUPPORTED_ADDRESS = 3
OPASM_SELECTOR_STATUS_OPERAND_ERROR       = 4

	.section code, kind=code

	.pub

; ---------------------------------------------------------------------------
; Build the compact tkpkg encode-request payload for one parsed statement.
;
; This is a transitional native opasm entry: it stands where the full selector
; will eventually own CPU/addressing candidate selection. For the current smoke
; path it accepts the small 6502 subset and emits the package-service request
; envelope consumed by ENTRY_ORD_ENCODE_INSTRUCTION.
;
; Inputs:
; - A0/D0: mnemonic pointer and byte length.
; - A1/D1: operand text pointer and byte length.
; - A4: selector context pointer containing:
;   - long 0: output request buffer pointer.
;   - long 1: operand-evaluation callback pointer returning D3=value and D5=1
;     when the operand depended on unstable pass-1 symbols.
;
; Outputs:
; - D0: OPASM_SELECTOR_STATUS_*.
; - D1: request byte length when D0 is OPASM_SELECTOR_STATUS_OK.
; - request buffer contains the compact package encode envelope.
; ---------------------------------------------------------------------------
	
opasmSelectorStageBuildEncodeRequestV1	.block
	movem.l d3-d7/a2-a6, -(sp)
	movea.l a0, a5  ; mnemonic text base survives helper calls in A5
	moveq #0, d6
	move.w d0, d6  ; D6 is the mnemonic length used in string compare/copy
	movea.l a1, a6  ; operand text base; later reused by operand evaluators
	moveq #0, d7
	move.w d1, d7  ; D7 is the operand text length
	movea.l a4, a0  ; unpack the selector context supplied by the CLI/session
	movea.l (a0)+, a4  ; A4 becomes the output request buffer
	movea.l (a0), a3  ; A3 is the caller-supplied operand-evaluation callback
	movea.l a4, a0  ; restore output-buffer base for later payload construction
	tst.w d6
	beq.w noOutput
	movea.l a5, a0
	move.w d6, d0
	lea OpasmSelectorLdaText, a1
	moveq #3, d1
	bsr.w textEquals
	tst.l d0
	bne.w buildLda
	movea.l a5, a0
	move.w d6, d0
	lea OpasmSelectorStaText, a1
	moveq #3, d1
	bsr.w textEquals
	tst.l d0
	bne.w buildSta
	movea.l a5, a0
	move.w d6, d0
	lea OpasmSelectorJmpText, a1
	moveq #3, d1
	bsr.w textEquals
	tst.l d0
	bne.w buildJmp
	moveq #OPASM_SELECTOR_STATUS_UNKNOWN_MNEMONIC, d0
	bra.w return

buildLda
	movea.l a6, a0
	move.l d7, d0
	bsr.w operandHasImmediatePrefix
	tst.l d0
	bne.w buildImmediate
	moveq #0, d2
	bra.w buildMemory

buildSta
	moveq #1, d2
	bra.w buildMemory

buildJmp
	moveq #2, d2
	bra.w buildMemory

buildImmediate
	movea.l a6, a0
	move.l d7, d0
	bsr.w readOperandValue
	tst.l d0
	bne.w operandError
	cmpi.l #$000000FF, d3
	bhi.w operandError
	moveq #1, d5
	moveq #9, d4
	lea OpasmSelectorImmediateText, a6
	bra.w buildPayload

buildMemory
	move.l d2, -(sp)
	movea.l a6, a0
	move.l d7, d0
	bsr.w splitIndexedOperand
	tst.l d2
	bne.w buildMemorySplitFail
	move.l (sp)+, d2
	bsr.w readOperandValue
	tst.l d0
	bne.w operandError
	tst.l d1
	beq.w buildUnindexed
	cmpi.l #1, d1
	beq.w buildIndexedX
	cmpi.l #2, d1
	beq.w buildIndexedY
	moveq #OPASM_SELECTOR_STATUS_UNSUPPORTED_ADDRESS, d0
	bra.w return

buildMemorySplitFail
	addq.l #4, sp
	bra.w unsupportedAddress

buildUnindexed
	cmpi.l #2, d2
	beq.w buildAbsoluteMode
	tst.l d5
	bne.w buildAbsoluteMode
	cmpi.l #$000000FF, d3
	bhi.w buildAbsoluteMode
	moveq #1, d5
	moveq #8, d4
	lea OpasmSelectorZeroPageText, a6
	bra.w buildPayload

buildIndexedX
	cmpi.l #2, d2
	beq.w unsupportedAddress
	tst.l d5
	bne.w buildAbsoluteXMode
	cmpi.l #$000000FF, d3
	bhi.w buildAbsoluteXMode
	moveq #1, d5
	moveq #9, d4
	lea OpasmSelectorZeroPageXText, a6
	bra.w buildPayload

buildIndexedY
	cmpi.l #2, d2
	beq.w unsupportedAddress
	moveq #2, d5
	moveq #9, d4
	lea OpasmSelectorAbsoluteYText, a6
	bra.w buildPayload

buildAbsoluteMode
	moveq #2, d5
	moveq #8, d4
	lea OpasmSelectorAbsoluteText, a6
	bra.s buildPayload

buildAbsoluteXMode
	moveq #2, d5
	moveq #9, d4
	lea OpasmSelectorAbsoluteXText, a6
	bra.s buildPayload

unsupportedAddress
	moveq #OPASM_SELECTOR_STATUS_UNSUPPORTED_ADDRESS, d0
	bra.w return

buildPayload
	movea.l a4, a2
	move.b d6, (a2)+
	movea.l a5, a0
	movea.l a2, a1
	move.w d6, d0
	bsr.w copyFixedString
	movea.l a1, a2
	move.b #1, (a2)+
	move.b d4, (a2)+
	movea.l a6, a0
	movea.l a2, a1
	move.w d4, d0
	bsr.w copyFixedString
	movea.l a1, a2
	move.b #1, (a2)+
	move.b d5, (a2)+
	move.b d3, (a2)+
	cmpi.b #2, d5
	bne.s payloadLenDone
	move.l d3, d0
	lsr.l #8, d0
	move.b d0, (a2)+

payloadLenDone
	move.l a2, d1
	sub.l a4, d1
	moveq #OPASM_SELECTOR_STATUS_OK, d0
	bra.s return

noOutput
	moveq #0, d1
	moveq #OPASM_SELECTOR_STATUS_NO_OUTPUT, d0
	bra.s return

operandError
	moveq #OPASM_SELECTOR_STATUS_OPERAND_ERROR, d0

return
	movem.l (sp)+, d3-d7/a2-a6
	rts
	.bend  ; opasmSelectorStageBuildEncodeRequestV1

; ---------------------------------------------------------------------------
; Return the byte length for one currently supported statement mnemonic.
;
; This is pass-1 sizing support for the same smoke subset as
; opasmSelectorStageBuildEncodeRequestV1. Unknown or non-output
; statements return size 0 so the caller can leave PC unchanged.
;
; Inputs:
; - A0/D0: mnemonic pointer and byte length.
;
; Outputs:
; - D0: OPASM_SELECTOR_STATUS_OK.
; - D1: instruction size in bytes, or 0 for no output/unknown.
; ---------------------------------------------------------------------------

opasmSelectorStageInstructionSizeV1	.block
	movem.l d2/a0-a2, -(sp)
	moveq #0, d2
	move.w d0, d2  ; preserve caller's mnemonic length for repeated compares
	movea.l a0, a2  ; preserve caller's mnemonic pointer for repeated compares
	moveq #0, d1
	tst.w d2
	beq.s done
	movea.l a2, a0
	move.w d2, d0
	lea OpasmSelectorLdaText, a1
	moveq #3, d1
	bsr.w textEquals
	tst.l d0
	bne.s sizeTwo
	movea.l a2, a0
	move.w d2, d0
	lea OpasmSelectorNopText, a1
	moveq #3, d1
	bsr.w textEquals
	tst.l d0
	bne.s sizeOne
	movea.l a2, a0
	move.w d2, d0
	lea OpasmSelectorStaText, a1
	moveq #3, d1
	bsr.w textEquals
	tst.l d0
	bne.s sizeThree
	movea.l a2, a0
	move.w d2, d0
	lea OpasmSelectorJmpText, a1
	moveq #3, d1
	bsr.w textEquals
	tst.l d0
	bne.s sizeThree
	bra.s done

sizeOne
	moveq #1, d1
	bra.s done

sizeTwo
	moveq #2, d1
	bra.s done

sizeThree
	moveq #3, d1

done
	moveq #OPASM_SELECTOR_STATUS_OK, d0
	movem.l (sp)+, d2/a0-a2
	rts
	.bend  ; opasmSelectorStageInstructionSizeV1
	
	.priv

readOperandValue	.block
	movem.l d1-d2/d4/d6-d7/a0-a2, -(sp)
	moveq #0, d5
	bsr.w resolveLabelOperand
	tst.l d0
	beq.s haveValue
	bra.s fail

haveValue
	moveq #0, d0
	bra.s return

fail
	moveq #0, d5
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4/d6-d7/a0-a2
	rts
	.bend  ; readOperandValue

resolveLabelOperand	.block
	movem.l d1/a0, -(sp)
	bsr.w skipWhitespace
	tst.l d0
	beq.s fail
	cmpi.b #'#', (a0)
	bne.s noImmediatePrefix
	addq.l #1, a0
	subq.l #1, d0
	bsr.w skipWhitespace
	tst.l d0
	beq.s fail

noImmediatePrefix
	move.l a3, d1
	tst.l d1
	beq.s fail
	jsr (a3)
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1/a0
	rts
	.bend  ; resolveLabelOperand

operandHasImmediatePrefix	.block
	bsr.w skipWhitespace
	tst.l d0
	beq.s no
	cmpi.b #'#', (a0)
	bne.s no
	moveq #1, d0
	rts

no
	moveq #0, d0
	rts
	.bend  ; operandHasImmediatePrefix

splitIndexedOperand	.block
	movem.l d3-d5/a1-a2, -(sp)
	bsr.w skipWhitespace
	tst.l d0
	beq.w fail
	movea.l a0, a1
	move.l d0, d4
	move.l d4, d5

trimTailLoop
	tst.l d5
	beq.s none
	movea.l a1, a2
	adda.l d5, a2
	subq.l #1, a2
	moveq #0, d3
	move.b (a2), d3
	tst.b d3
	beq.s trimTailOne
	cmpi.b #' ', d3
	beq.s trimTailOne
	cmpi.b #9, d3
	bne.s tailReady

trimTailOne
	subq.l #1, d5
	bra.s trimTailLoop

tailReady
	cmpi.b #'A', d3
	bcs.s suffixCaseOk
	cmpi.b #'Z', d3
	bhi.s suffixCaseOk
	addi.b #'a' - 'A', d3

suffixCaseOk
	cmpi.b #'x', d3
	beq.s haveX
	cmpi.b #'y', d3
	beq.s haveY
	bra.s none

haveX
	moveq #1, d1
	bra.s findComma

haveY
	moveq #2, d1

findComma
	subq.l #1, d5

findCommaLoop
	tst.l d5
	beq.s fail
	movea.l a1, a2
	adda.l d5, a2
	subq.l #1, a2
	moveq #0, d3
	move.b (a2), d3
	cmpi.b #' ', d3
	beq.s findCommaTrim
	cmpi.b #9, d3
	beq.s findCommaTrim
	cmpi.b #',', d3
	beq.s found
	bra.s fail

findCommaTrim
	subq.l #1, d5
	bra.s findCommaLoop

none
	movea.l a1, a0
	move.l d4, d0
	moveq #0, d1
	moveq #0, d2
	bra.s return

found
	subq.l #1, d5
	move.l d5, d0
	beq.s fail

trimLoop
	tst.l d0
	beq.s fail
	movea.l a1, a0
	adda.l d0, a0
	subq.l #1, a0
	cmpi.b #' ', (a0)
	beq.s trimOne
	cmpi.b #9, (a0)
	bne.s afterExpr

trimOne
	subq.l #1, d0
	bra.s trimLoop

afterExpr
	movea.l a1, a0
	moveq #0, d2
	bra.s return

fail
	moveq #1, d2

return
	movem.l (sp)+, d3-d5/a1-a2
	rts
	.bend  ; splitIndexedOperand

textEquals	.block
	movem.l d2-d3, -(sp)
	cmp.l d1, d0
	bne.s notEqual
	tst.l d1
	beq.s equal

loop
	move.b (a0)+, d2
	move.b (a1)+, d3
	cmpi.b #'A', d2
	bcs.s sourceCaseOk
	cmpi.b #'Z', d2
	bhi.s sourceCaseOk
	addi.b #'a' - 'A', d2

sourceCaseOk
	cmpi.b #'A', d3
	bcs.s needleCaseOk
	cmpi.b #'Z', d3
	bhi.s needleCaseOk
	addi.b #'a' - 'A', d3

needleCaseOk
	cmp.b d3, d2
	bne.s notEqual
	subq.l #1, d1
	bne.s loop

equal
	moveq #1, d0
	bra.s return

notEqual
	moveq #0, d0

return
	movem.l (sp)+, d2-d3
	rts
	.bend  ; textEquals

skipWhitespace	.block
loop
	tst.l d0
	beq.s done
	moveq #0, d1
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s one
	cmpi.b #9, d1
	bne.s done

one
	addq.l #1, a0
	subq.l #1, d0
	bra.s loop

done
	rts
	.bend  ; skipWhitespace

copyFixedString	.block
	tst.w d0
	beq.s done

loop
	move.b (a0)+, (a1)+
	subq.w #1, d0
	bne.s loop

done
	rts
	.bend  ; copyFixedString

	.endsection

	.section data, kind=data

OpasmSelectorStageMarker
	.byte "OPASM-SELECTOR-STAGE-V1", 0

OpasmSelectorLdaText
	.byte "lda"
OpasmSelectorStaText
	.byte "sta"
OpasmSelectorJmpText
	.byte "jmp"
OpasmSelectorNopText
	.byte "nop"
OpasmSelectorImmediateText
	.byte "immediate"
OpasmSelectorZeroPageText
	.byte "zeropage"
OpasmSelectorZeroPageXText
	.byte "zeropagex"
OpasmSelectorAbsoluteText
	.byte "absolute"
OpasmSelectorAbsoluteXText
	.byte "absolutex"
OpasmSelectorAbsoluteYText
	.byte "absolutey"

	.endsection
	.endmodule
