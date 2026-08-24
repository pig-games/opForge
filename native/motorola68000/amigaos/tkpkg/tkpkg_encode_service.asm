; Package-table encoding and selected-envelope output construction.
; Ownership extraction only: package lookup, program execution, and bytes remain unchanged.

	.module tkpkg.amigaos.encode_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.selection_service as selection
	.use tkpkg.amigaos.compact_table as compact

ENCODE_ENVELOPE_MALFORMED_TEXT_LEN = 33
ENCODE_TABLE_MALFORMED_TEXT_LEN = 30
EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN = 45

	.section data, kind=data
	.priv

EvaluateExprNeedsPipelineText
	.byte "OTR001: evaluate_expression requires pipeline", 0

EncodeEnvelopeMalformedText
	.byte "OTR901: encode envelope malformed", 0

EncodeTableMalformedText
	.byte "OTR901: encode table malformed", 0

	.endsection

	.section code, kind=code
	.pub

encodeSelectedInstructionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	btst #1, buffers.PackageStateFlags
	bne.s havePipeline
	lea EvaluateExprNeedsPipelineText, a1
	moveq #EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

havePipeline
	moveq #0, d0
	movea.l d0, a1
	jsr compact.findFixedProgramFromRequestV1
	bne.s return
	tst.w d1
	beq.s useSelectedEnvelope
	moveq #0, d5
	moveq #0, d6
	movea.l d6, a3
	bsr.w tkpkgEncodeExecuteProgram
	bra.s return

useSelectedEnvelope
	jsr selection.buildSelectedEnvelopeV1
	bne.s return
	tst.w d1
	bne.s haveEnvelope
	jsr selection.noOutputErrorV1
	bra.s return

haveEnvelope
	lea buffers.TokenScratchBuffer, a4
	move.w d1, d7
	bsr.w tkpkgEncodeInstructionEnvelopeV1
	tst.b d0
	bne.s return
	tst.w d1
	bne.s return
	moveq #1, d2
	jsr selection.noOutputErrorV1

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; encodeSelectedInstructionV1

; Inputs:
; - A0: selected-instruction service request control block.
;
; Outputs:
; - D0: 0 on success, nonzero ABI/runtime status on failure.
; - D1: 1 when a selectable instruction exists, 0 when no output is available.
;
; Clobbers:
; - D0-D1/D2-D7/A1-A6/CCR
;
; CCR:
; - Reflects D0 on return.

writeCandidateOutputV1	.block
	movem.l d2-d7/a2-a4, -(sp)
	lea buffers.TokenScratchBuffer, a4
	moveq #0, d4
	move.w d7, d4
	cmpi.w #4, d4
	bcs.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d4
	cmp.w d4, d5
	bhi.w fail
	adda.w d5, a4
	sub.w d5, d4
	beq.w fail
	moveq #0, d3
	move.b (a4)+, d3
	subq.w #1, d4
	beq.w noOutput
	tst.w d3
	beq.w noOutput
	lea buffers.LastErrorBuffer, a2
	moveq #0, d1

loop
	tst.w d4
	beq.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d4
	cmp.w d4, d5
	bhi.w fail

modeLoop
	tst.w d5
	beq.s modeDone
	move.b (a4)+, (a2)+
	addq.w #1, d1
	subq.w #1, d4
	subq.w #1, d5
	bne.s modeLoop

modeDone
	tst.w d4
	beq.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d4

operandLoop
	tst.w d5
	beq.s newline
	tst.w d4
	beq.w fail
	move.b #' ', (a2)+
	addq.w #1, d1
	moveq #0, d6
	move.b (a4)+, d6
	subq.w #1, d4
	cmp.w d4, d6
	bhi.w fail

operandBytesLoop
	tst.w d6
	beq.s nextOperand
	moveq #0, d7
	move.b (a4)+, d7
	subq.w #1, d4
	bsr.w appendHexByteV1
	subq.w #1, d6
	bne.s operandBytesLoop

nextOperand
	subq.w #1, d5
	bne.s operandLoop

newline
	move.b #10, (a2)+
	addq.w #1, d1
	subq.w #1, d3
	bne.s loop
	moveq #0, d0
	bra.s return

noOutput
	moveq #0, d1
	moveq #0, d0
	bra.s return

fail
	lea buffers.RuntimeErrorText, a1
	moveq #buffers.RUNTIME_ERROR_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0

return
	movem.l (sp)+, d2-d7/a2-a4
	rts
	.bend  ; writeCandidateOutputV1

appendHexByteV1	.block
	moveq #0, d2
	move.b d7, d2
	move.b d2, d6
	lsr.b #4, d6
	bsr.s hexDigitFromNibbleV1
	move.b d0, (a2)+
	addq.w #1, d1
	move.b d2, d6
	andi.b #$0f, d6
	bsr.s hexDigitFromNibbleV1
	move.b d0, (a2)+
	addq.w #1, d1
	rts
	.bend  ; appendHexByteV1

hexDigitFromNibbleV1	.block
	moveq #0, d0
	move.b d6, d0
	cmpi.b #9, d0
	ble.s decimal
	addi.b #'A' - 10, d0
	rts

decimal
	addi.b #'0', d0
	rts
	.bend  ; hexDigitFromNibbleV1

encodeInstructionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0  ; low byte of CB-relative encode-request offset
	moveq #0, d1
	move.b 17(a0), d1  ; high byte of CB_INPUT_PTR
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.W), a4  ; A4 walks the request envelope in-place
	moveq #0, d7
	move.b abi.CB_INPUT_LEN(a0), d7  ; D7 tracks remaining request bytes as fields are consumed
	moveq #0, d0
	move.b 19(a0), d0
	lsl.w #8, d0
	or.w d0, d7
	bsr.w tkpkgEncodeInstructionEnvelopeV1

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; encodeInstructionV1

tkpkgEncodeInstructionEnvelopeV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	cmpi.w #4, d7
	bcs.w fail
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d7
	tst.w d2
	beq.w fail
	cmp.w d7, d2
	bhi.w fail
	movea.l a4, a5
	adda.w d2, a4
	sub.w d2, d7
	beq.w fail
	moveq #0, d3
	move.b (a4)+, d3
	subq.w #1, d7
	tst.w d3
	beq.w noMatch
	tst.w d7
	beq.w fail
	moveq #0, d4
	move.b (a4)+, d4
	subq.w #1, d7
	tst.w d4
	beq.w fail
	cmp.w d7, d4
	bhi.w fail
	movea.l a4, a6
	adda.w d4, a4
	sub.w d4, d7
	beq.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d7
	tst.w d5
	beq.s noOperandRecord
	tst.w d7
	beq.w fail
	moveq #0, d6
	move.b (a4)+, d6
	subq.w #1, d7
	cmp.w d7, d6
	bhi.w fail
	movea.l a4, a3
	move.w d6, -(sp)
	move.w d5, d0
	move.w d7, d1
	movea.l a4, a2

validateOperandRecord
	cmp.w d1, d6
	bhi.s validateOperandFail
	adda.w d6, a2
	sub.w d6, d1
	subq.w #1, d0
	beq.s validateOperandDone
	tst.w d1
	beq.s validateOperandFail
	moveq #0, d6
	move.b (a2)+, d6
	subq.w #1, d1
	bra.s validateOperandRecord

validateOperandFail
	addq.l #2, sp
	bra.w fail

validateOperandDone
	move.w (sp)+, d6
	bra.w encodeCandidate

noOperandRecord
	moveq #0, d6
	movea.l a4, a3

encodeCandidate
	bsr.w tkpkgEncodeFindAndExecuteTableProgram
	bra.s return

noMatch
	moveq #0, d1
	moveq #2, d0
	bra.w return

fail
	lea EncodeEnvelopeMalformedText, a1
	moveq #ENCODE_ENVELOPE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; tkpkgEncodeInstructionEnvelopeV1

tkpkgEncodeFindAndExecuteTableProgram	.block
	movem.l d2-d7/a0/a2-a6, -(sp)
	movea.l a6, a1
	moveq #0, d0
	move.w d4, d0
	movem.l d5-d6/a3, -(sp)
	jsr compact.findFixedProgramFromRequestV1
	movem.l (sp)+, d5-d6/a3
	tst.l d0
	bne.w return
	tst.w d1
	beq.s legacyTableLookup
	bsr.w tkpkgEncodeExecuteProgram
	bra.w return

legacyTableLookup
	moveq #0, d0
	move.b buffers.TablChunkOffsetLo, d0
	moveq #0, d1
	move.b buffers.TablChunkOffsetMidLo, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b buffers.TablChunkOffsetMidHi, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b buffers.TablChunkOffsetHi, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	beq.w fail
	lea buffers.PackageStorage, a0
	adda.l d0, a0
	bsr.w tkpkgEncodeReadU32Low16
	beq.w noMatch
	move.w d0, d7
	subq.w #1, d7

loop
	move.b (a0)+, d0
	move.w d6, -(sp)
	move.b d0, d6
	bsr.w tkpkgEncodeReadU32Low16
	movea.l a0, a1
	move.l a1, -(sp)
	move.w d0, -(sp)
	adda.w d0, a0
	movea.l a0, a1
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a2
	adda.w d1, a0
	movea.l a5, a1
	move.w d2, d0
	bsr.w tkpkgEncodeStringEqIgnoreCase
	beq.s skipEntryFromMode
	movea.l a0, a1
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a2
	adda.w d1, a0
	movea.l a6, a1
	move.w d4, d0
	bsr.w tkpkgEncodeStringEqIgnoreCase
	beq.s skipEntryProgram
	move.w (sp), d0
	movea.l 2(sp), a1
	jsr selection.tkpkgSelectedMselOwnerMatchesV1
	move.b d0, d3
	move.w 6(sp), d6
	adda.l #8, sp
	tst.b d3
	beq.s skipProgram
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a1
	bsr.w tkpkgEncodeExecuteProgram
	bra.s return

skipEntryFromMode
	move.w 6(sp), d6
	adda.l #8, sp
	bsr.w tkpkgEncodeSkipString

skipProgram
	bsr.w tkpkgEncodeSkipBytes
	dbra d7, loop
	bra.s noMatch

skipEntryProgram
	move.w 6(sp), d6
	adda.l #8, sp
	bsr.w tkpkgEncodeSkipBytes
	dbra d7, loop

noMatch
	moveq #0, d1
	moveq #0, d0
	bra.s return

fail
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0/a2-a6
	rts
	.bend  ; tkpkgEncodeFindAndExecuteTableProgram

tkpkgEncodeExecuteProgram	.block
	movem.l d2-d7/a0/a2-a4, -(sp)
	movea.l a1, a0
	move.w d1, d7
	lea buffers.LastErrorBuffer, a2
	clr.w d1

loop
	tst.w d7
	beq.s fail
	move.b (a0)+, d0
	subq.w #1, d7
	cmpi.b #$FF, d0
	beq.s endProgram
	cmpi.b #$01, d0
	beq.s emitU8
	cmpi.b #$02, d0
	beq.s emitOperand
	bra.w fail

emitU8
	tst.w d7
	beq.s fail
	cmpi.w #buffers.LAST_ERROR_BUFFER_CAPACITY, d1
	bhs.s fail
	move.b (a0)+, (a2)+
	subq.w #1, d7
	addq.w #1, d1
	bra.s loop

emitOperand
	tst.w d7
	beq.s fail
	moveq #0, d3
	move.b (a0)+, d3
	subq.w #1, d7
	cmp.w d5, d3
	bhs.s fail
	movea.l a3, a4
	move.w d6, d2
	tst.w d3
	beq.s operandCopyStart

operandSelectLoop
	adda.w d2, a4
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d3
	bne.s operandSelectLoop

operandCopyStart
	move.w d2, d0
	beq.s loop

operandLoop
	move.b (a4)+, (a2)+
	addq.w #1, d1
	subq.w #1, d0
	bne.s operandLoop
	bra.s loop

endProgram
	tst.w d7
	bne.s fail

ok
	moveq #0, d0
	bra.s return

fail
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0/a2-a4
	rts
	.bend  ; tkpkgEncodeExecuteProgram

; Inputs:
; - A0: points at a 32-bit little-endian table field whose low 16 bits are consumed.
;
; Outputs:
; - D0.W: decoded low 16-bit value.
; - A0: advanced past the 4-byte field.
;
; Clobbers:
; - D0-D1/A0/CCR
;
; CCR:
; - Reflects D0.W on return.
tkpkgEncodeReadU32Low16	.block
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d1
	move.b (a0)+, d1
	lsl.w #8, d1
	or.w d1, d0
	addq.l #2, a0
	rts
	.bend  ; tkpkgEncodeReadU32Low16

tkpkgEncodeSkipString	.block
	bsr.w tkpkgEncodeReadU32Low16
	adda.w d0, a0
	rts
	.bend  ; tkpkgEncodeSkipString

tkpkgEncodeSkipBytes	.block
	bsr.w tkpkgEncodeReadU32Low16
	adda.w d0, a0
	rts
	.bend  ; tkpkgEncodeSkipBytes

tkpkgEncodeStringEqIgnoreCase	.block
	movem.l d1-d4/a1-a2, -(sp)
	cmp.w d1, d0
	bne.s no
	tst.w d0
	beq.s yes
	move.w d0, d4
	subq.w #1, d4

loop
	move.b (a1)+, d2
	move.b (a2)+, d3
	cmpi.b #'A', d2
	bcs.s leftOk
	cmpi.b #'Z', d2
	bhi.s leftOk
	addi.b #32, d2

leftOk
	cmpi.b #'A', d3
	bcs.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	addi.b #32, d3

compare
	cmp.b d3, d2
	bne.s no
	dbra d4, loop

yes
	moveq #1, d0
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d4/a1-a2
	rts
	.bend  ; tkpkgEncodeStringEqIgnoreCase

	.endsection
	.endmodule
