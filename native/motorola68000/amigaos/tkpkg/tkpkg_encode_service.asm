; Package-table encoding and selected-envelope output construction.
; Ownership extraction only: package lookup, program execution, and bytes remain unchanged.

	.module tkpkg.amigaos.encode_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.selection_service as selection
	.use tkpkg.amigaos.selection_state as state
	.use tkpkg.amigaos.compact_table as compact

ENCODE_ENVELOPE_MALFORMED_TEXT_LEN = 33
ENCODE_TABLE_MALFORMED_TEXT_LEN = 30
EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN = 45
BRANCH_DISPLACEMENT_RANGE_SUFFIX_LEN = 33

	.section data, kind=data
	.priv

EvaluateExprNeedsPipelineText
	.byte "OTR001: evaluate_expression requires pipeline", 0

EncodeEnvelopeMalformedText
	.byte "OTR901: encode envelope malformed", 0

EncodeTableMalformedText
	.byte "OTR901: encode table malformed", 0

BranchDisplacementRangeSuffix
	.byte " branch displacement out of range"

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
	cmpi.b #4, state.EncodeSelectedSemanticPlanKind
	beq.w encodeSemanticSequenceCandidate
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
	move.l a3, buffers.SemanticInputRecordPtr
	move.w d5, buffers.SemanticInputRecordCount
	move.w d6, buffers.SemanticFirstInputLen
	tst.b state.EncodeSelectedSemanticPlanKind
	beq.s encodeLegacyCandidate
	clr.w buffers.SemanticOutputWriteOffset
	bsr.w tkpkgEncodeFindAndExecuteSemanticProgramV2
	tst.l d0
	beq.s semanticProgramExecuted
	cmpi.w #$FFFF, state.EncodeSelectedSemanticDiagnosticIndex
	beq.w return
	jsr selection.tkpkgRenderSelectedSemanticRejectV1
	tst.l d0
	bne.s semanticDiagnosticFail
	moveq #1, d0
	bra.w return
semanticDiagnosticFail
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0
	bra.w return
semanticProgramExecuted
	tst.b d3
	bne.w return
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0
	bra.w return
encodeLegacyCandidate
	bsr.w tkpkgEncodeFindAndExecuteTableProgram
	bra.w return

; Execute the encode-step records emitted by the neutral CMSE sequence
; selector. Match steps have already projected successfully and therefore
; contribute no bytes, exactly as in Rust selector_encoding.rs.
encodeSemanticSequenceCandidate
	clr.w buffers.SemanticOutputWriteOffset
	move.w d3, d2

semanticSequenceStepLoop
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
	tst.w d7
	beq.w fail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d7
	tst.w d5
	beq.w fail
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

semanticSequenceRecordLoop
	cmp.w d1, d6
	bhi.s semanticSequenceRecordFail
	adda.w d6, a2
	sub.w d6, d1
	subq.w #1, d0
	beq.s semanticSequenceRecordsReady
	tst.w d1
	beq.s semanticSequenceRecordFail
	moveq #0, d6
	move.b (a2)+, d6
	subq.w #1, d1
	bra.s semanticSequenceRecordLoop

semanticSequenceRecordFail
	addq.l #2, sp
	bra.w fail

semanticSequenceRecordsReady
	move.w (sp)+, d6
	movea.l a2, a4
	move.w d1, d7
	move.l a3, buffers.SemanticInputRecordPtr
	move.w d5, buffers.SemanticInputRecordCount
	move.w d6, buffers.SemanticFirstInputLen
	bsr.w tkpkgEncodeFindAndExecuteSemanticProgramV2
	tst.l d0
	bne.s return
	tst.b d3
	beq.w fail
	move.w d1, buffers.SemanticOutputWriteOffset
	subq.w #1, d2
	bne.w semanticSequenceStepLoop
	tst.w d7
	bne.w fail
	tst.w d1
	beq.w fail
	moveq #0, d0
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

	.priv
; Resolve an opaque selected mode as a CSEM program using Rust's
; dialect/cpu/family precedence, then execute encoding v2/v6, branch v5, or
; fixup v4/v7.
; Inputs are the same selected-envelope registers used by the table path.
; Outputs: D0 status, D1 encoded length, D3.B found flag.
tkpkgEncodeFindAndExecuteSemanticProgramV2	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	lea -22(sp), sp
	clr.w (sp)
	clr.w 2(sp)
	clr.w 4(sp)
	clr.l 6(sp)
	move.w d4, 10(sp)
	move.l a6, 12(sp)
	move.w #$FFFF, 16(sp)
	move.w #$FFFF, 18(sp)
	move.w #$FFFF, 20(sp)
	lea buffers.CsemChunkOffsetLo, a3
	jsr selection.tkpkgServiceChunkPtrFromLocatorV1
	bne.w semanticNotFound
	jsr selection.tkpkgServiceReadU16LeV1
	bne.w semanticMalformed
	cmpi.w #1, d0
	bne.w semanticMalformed
	jsr selection.tkpkgServiceReadU16LeV1
	bne.w semanticMalformed
	tst.w d0
	beq.w semanticMalformed
	move.w d0, d7
	subq.w #1, d7
	moveq #0, d5

semanticOwnerLoop
	moveq #1, d0
	jsr selection.tkpkgServiceRequireBytesV1
	bne.w semanticMalformed
	moveq #0, d6
	move.b (a2)+, d6
	cmpi.b #2, d6
	bhi.w semanticMalformed
	jsr selection.tkpkgServiceLocateStringV1
	bne.w semanticMalformed
	move.l a2, -(sp)
	jsr selection.tkpkgSelectedMselOwnerMatchesV1
	movea.l (sp)+, a2
	tst.b d0
	beq.s semanticOwnerNext
	tst.b d6
	beq.s semanticOwnerFamily
	cmpi.b #1, d6
	beq.s semanticOwnerCpu
	move.w d5, 20(sp)
	bra.s semanticOwnerNext
semanticOwnerCpu
	move.w d5, 18(sp)
	bra.s semanticOwnerNext
semanticOwnerFamily
	move.w d5, 16(sp)
semanticOwnerNext
	addq.w #1, d5
	dbf d7, semanticOwnerLoop

	jsr selection.tkpkgServiceReadU32LeLow16V1
	bne.w semanticMalformed
	addq.l #4, a2
	tst.w d0
	beq.w semanticMalformed
	move.w d0, d7
	subq.w #1, d7

semanticProgramLoop
	jsr selection.tkpkgServiceReadU16LeV1
	bne.w semanticMalformed
	move.w d0, d6
	jsr selection.tkpkgServiceLocateStringV1
	bne.w semanticMalformed
	move.l a2, -(sp)
	move.w 14(sp), d1
	movea.l 16(sp), a2
	jsr selection.tkpkgServiceStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	move.w d0, d5
	jsr selection.tkpkgServiceReadU16LeV1
	bne.w semanticMalformed
	move.w d0, d4
	jsr selection.tkpkgServiceReadU32LeLow16V1
	bne.w semanticMalformed
	addq.l #4, a2
	move.w d0, d1
	movea.l a2, a0
	moveq #0, d0
	move.w d1, d0
	jsr selection.tkpkgServiceRequireBytesV1
	bne.w semanticMalformed
	move.w d0, d1
	adda.w d0, a2
	tst.w d5
	beq.s semanticProgramNext
	moveq #0, d5
	cmp.w 16(sp), d6
	bne.s semanticProgramCpu
	moveq #1, d5
semanticProgramCpu
	cmp.w 18(sp), d6
	bne.s semanticProgramDialect
	moveq #2, d5
semanticProgramDialect
	cmp.w 20(sp), d6
	bne.s semanticProgramRanked
	moveq #3, d5
semanticProgramRanked
	tst.w d5
	beq.s semanticProgramNext
	cmp.w (sp), d5
	bls.s semanticProgramNext
	move.w d5, (sp)
	move.w d4, 2(sp)
	move.w d1, 4(sp)
	move.l a0, 6(sp)
semanticProgramNext
	dbf d7, semanticProgramLoop
	cmpa.l a6, a2
	bne.s semanticMalformed
	tst.w (sp)
	beq.s semanticNotFound
	move.w 2(sp), d4
	cmpi.w #2, d4
	beq.s semanticEncodingReady
	cmpi.w #6, d4
	beq.s semanticEncodingReady
	cmpi.w #5, d4
	beq.s semanticBranchReady
	cmpi.w #7, d4
	beq.s semanticFixupReady
	cmpi.w #4, d4
	bne.s semanticMalformed
semanticFixupReady
	movea.l 6(sp), a1
	move.w 4(sp), d1
	bsr.w tkpkgEncodeExecuteFixupProgramV4
	moveq #1, d3
	bra.s semanticReturn
semanticBranchReady
	movea.l 6(sp), a1
	move.w 4(sp), d1
	bsr.w tkpkgEncodeExecuteBranchProgramV5
	moveq #1, d3
	bra.s semanticReturn
semanticEncodingReady
	movea.l 6(sp), a1
	move.w 4(sp), d1
	bsr.w tkpkgEncodeExecuteSemanticProgramV2
	moveq #1, d3
	bra.s semanticReturn

semanticNotFound
	moveq #0, d0
	moveq #0, d1
	moveq #0, d3
	bra.s semanticReturn
semanticMalformed
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0
	moveq #1, d3
semanticReturn
	lea 22(sp), sp
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	rts
	.bend  ; tkpkgEncodeFindAndExecuteSemanticProgramV2

; Direct Rust branch_vm::execute_branch_program port for SEMV/CSEM v5.  The
; selected envelope supplies opcode, target, requested candidate (`-1` means
; auto), and automatic class.  Candidate widths, suffixes, endian, position
; adjustment, unresolved placeholder, and reserved values remain package data.
; Inputs: A1/D1 = program; four scalar records in SemanticInputRecordPtr.
; Outputs: D0 status; D1 total output length in LastErrorBuffer.
tkpkgEncodeExecuteBranchProgramV5	.block
	movem.l d2-d7/a0/a2-a6, -(sp)
	lea -60(sp), sp
	movea.l a1, a0
	moveq #0, d0
	move.w d1, d0
	movea.l a1, a5
	adda.l d0, a5
	move.l a5, (sp)
	clr.l 4(sp)
	clr.l 8(sp)

	moveq #0, d0
	bsr.w tkpkgSemanticLoadInputV2
	bne.w branchFail
	cmpi.l #255, d3
	bhi.w branchFail
	move.l d3, 12(sp)
	moveq #1, d0
	bsr.w tkpkgSemanticLoadInputV2
	bne.w branchFail
	move.l d3, 16(sp)
	moveq #2, d0
	bsr.w tkpkgSemanticLoadInputV2
	bne.w branchFail
	cmpi.l #-1, d3
	beq.s branchRequestedReady
	cmpi.l #255, d3
	bhi.w branchFail
branchRequestedReady
	move.l d3, 20(sp)
	moveq #3, d0
	bsr.w tkpkgSemanticLoadInputV2
	bne.w branchFail
	cmpi.l #7, d3
	bhi.w branchFail
	move.w d3, d2
	move.w d2, 30(sp)
	moveq #1, d0
	lsl.b d2, d0
	move.w d0, 32(sp)
	move.l state.EncodeSelectedCurrentPc, 24(sp)
	clr.w 34(sp)
	tst.b state.EncodeSelectedMselUnstable
	beq.s branchUnresolvedReady
	cmpi.w #1, state.EncodeSelectedSessionPass
	bne.s branchUnresolvedReady
	move.w #1, 34(sp)
branchUnresolvedReady

	moveq #5, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w branchFail
	cmpi.b #$01, (a0)+
	bne.w branchFail
	tst.b (a0)+
	bne.w branchFail
	tst.b (a0)+
	bne.w branchFail
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 28(sp)
	moveq #0, d7
	move.b (a0)+, d7
	tst.w d7
	beq.w branchFail
	cmpi.w #16, d7
	bhi.w branchFail
	move.w d7, 30(sp)

branchCandidateLoop
	movea.l a0, a3
	moveq #3, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w branchFail
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 36(sp)
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 38(sp)
	moveq #0, d4
	move.b (a0)+, d4
	cmpi.w #8, d4
	bhi.w branchFail
	moveq #11, d0
	add.w d4, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w branchFail
	adda.w d4, a0
	moveq #0, d5
	move.b (a0)+, d5
	cmpi.w #1, d5
	beq.s branchWidthReady
	cmpi.w #2, d5
	beq.s branchWidthReady
	cmpi.w #4, d5
	bne.w branchFail
branchWidthReady
	moveq #0, d6
	move.b (a0)+, d6
	cmpi.w #1, d6
	bhi.w branchFail
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w branchFail
	move.l d0, 44(sp)
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w branchFail
	move.l d0, 48(sp)
	move.w d5, d2
	bsr.w tkpkgBranchValueFitsSignedWidthV5
	tst.l d0
	bne.w branchFail
	moveq #0, d4
	move.b (a0)+, d4
	cmpi.w #8, d4
	bhi.w branchFail
	move.w d4, d0
	lsl.w #2, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w branchFail

	clr.w 40(sp)
	clr.w 42(sp)
	move.l 20(sp), d0
	cmpi.l #-1, d0
	beq.s branchAutomaticCandidate
	cmp.w 36(sp), d0
	bne.s branchCandidateValueReady
	move.w #1, 40(sp)
	bra.s branchComputeCandidateValue

branchAutomaticCandidate
	tst.l 4(sp)
	bne.s branchCandidateValueReady
	move.w 38(sp), d0
	and.w 32(sp), d0
	beq.s branchCandidateValueReady
	tst.w 34(sp)
	beq.s branchAutomaticResolved
	move.w 36(sp), d0
	cmp.w 28(sp), d0
	bne.s branchCandidateValueReady
branchAutomaticResolved
	move.w #1, 40(sp)

branchComputeCandidateValue
	tst.w 34(sp)
	beq.s branchProjectCandidateValue
	move.l 48(sp), d3
	bra.s branchValidateCandidateValue
branchProjectCandidateValue
	move.l 24(sp), d0
	add.l 44(sp), d0
	bvs.w branchFail
	move.l 16(sp), d3
	sub.l d0, d3
	bvs.w branchFail
branchValidateCandidateValue
	move.l d3, 52(sp)
	move.l d3, d0
	move.w d5, d2
	bsr.w tkpkgBranchValueFitsSignedWidthV5
	tst.l d0
	beq.s branchCandidateValueReady
	cmpi.l #-1, 20(sp)
	bne.w branchRangeFail
	clr.w 40(sp)

branchCandidateValueReady
	clr.l 56(sp)
	moveq #0, d6
	movea.l d4, a4
	bra.s branchReservedCheck

branchReservedLoop
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w branchFail
	move.l d0, d3
	move.w d5, d2
	bsr.w tkpkgBranchValueFitsSignedWidthV5
	tst.l d0
	bne.w branchFail
	tst.w d6
	beq.s branchReservedAscending
	move.l 56(sp), d0
	cmp.l d3, d0
	bge.w branchFail
branchReservedAscending
	move.l d3, 56(sp)
	moveq #1, d6
	cmp.l 48(sp), d3
	beq.w branchFail
	tst.w 40(sp)
	beq.s branchReservedNext
	cmp.l 52(sp), d3
	bne.s branchReservedNext
	move.w #1, 42(sp)
branchReservedNext
	subq.l #1, a4

branchReservedCheck
	move.l a4, d0
	bne.s branchReservedLoop
	tst.w 40(sp)
	beq.s branchCandidateNext
	tst.w 42(sp)
	beq.s branchSelectCandidate
	cmpi.l #-1, 20(sp)
	bne.w branchFail
	bra.s branchCandidateNext

branchSelectCandidate
	tst.l 4(sp)
	beq.s branchStoreCandidate
	cmpi.l #-1, 20(sp)
	bne.w branchFail
	bra.s branchCandidateNext
branchStoreCandidate
	move.l a3, 4(sp)
	move.l 52(sp), d0
	move.l d0, 8(sp)

branchCandidateNext
	subq.w #1, d7
	bne.w branchCandidateLoop
	moveq #1, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w branchFail
	cmpi.b #$FF, (a0)+
	bne.w branchFail
	cmpa.l (sp), a0
	bne.w branchFail
	tst.l 4(sp)
	beq.w branchFail

	movea.l 4(sp), a0
	addq.l #2, a0
	moveq #0, d7
	move.b (a0)+, d7
	cmpi.w #8, d7
	bhi.w branchFail
	moveq #0, d0
	move.w d7, d0
	addq.w #3, d0
	move.l a0, d2
	add.l d0, d2
	cmp.l (sp), d2
	bhi.w branchFail
	lea buffers.LastErrorBuffer, a2
	moveq #0, d0
	move.w buffers.SemanticOutputWriteOffset, d0
	adda.w d0, a2
	move.l a2, d3
	lea buffers.LastErrorBuffer, a1
	sub.l a1, d3
	addq.l #1, d3
	add.l d7, d3
	cmpi.l #buffers.LAST_ERROR_BUFFER_CAPACITY, d3
	bhi.w branchFail
	move.l 12(sp), d0
	move.b d0, (a2)+
	tst.w d7
	beq.s branchSuffixReady
branchSuffixLoop
	move.b (a0)+, (a2)+
	subq.w #1, d7
	bne.s branchSuffixLoop
branchSuffixReady
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d4
	move.b (a0)+, d4
	move.l 8(sp), d0
	bsr.w tkpkgSemanticEmitUnitV2
	tst.l d3
	bne.w branchFail
	move.l a2, d1
	lea buffers.LastErrorBuffer, a1
	sub.l a1, d1
	cmp.w buffers.SemanticOutputWriteOffset, d1
	bls.w branchFail
	moveq #0, d0
	bra.s branchReturn

branchRangeFail
	moveq #0, d1
	move.w state.EncodeSelectedMselMnemonicLen, d1
	move.l d1, d2
	addi.l #BRANCH_DISPLACEMENT_RANGE_SUFFIX_LEN + 1, d2
	cmpi.l #buffers.LAST_ERROR_BUFFER_CAPACITY, d2
	bhi.s branchFail
	lea buffers.LastErrorBuffer, a2
	lea buffers.CompactSelectorMnemonicText, a3
	move.l d1, d2
branchRangeMnemonicLoop
	tst.l d2
	beq.s branchRangeSuffixReady
	move.b (a3)+, (a2)+
	subq.l #1, d2
	bra.s branchRangeMnemonicLoop
branchRangeSuffixReady
	lea BranchDisplacementRangeSuffix, a3
	moveq #BRANCH_DISPLACEMENT_RANGE_SUFFIX_LEN, d2
branchRangeSuffixLoop
	move.b (a3)+, (a2)+
	subq.l #1, d2
	bne.s branchRangeSuffixLoop
	clr.b (a2)
	addi.w #BRANCH_DISPLACEMENT_RANGE_SUFFIX_LEN, d1
	lea buffers.LastErrorBuffer, a1
	moveq #1, d0
	bra.s branchReturn

branchFail
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0
branchReturn
	lea 60(sp), sp
	movem.l (sp)+, d2-d7/a0/a2-a6
	rts
	.bend  ; tkpkgEncodeExecuteBranchProgramV5

; Inputs: D0 signed value, D2.W width (1/2/4). Output D0=0 fits, 1 fails.
tkpkgBranchValueFitsSignedWidthV5	.block
	cmpi.w #1, d2
	beq.s branchFitsByte
	cmpi.w #2, d2
	beq.s branchFitsWord
	cmpi.w #4, d2
	bne.s branchDoesNotFit
	moveq #0, d0
	rts
branchFitsByte
	cmpi.l #-128, d0
	blt.s branchDoesNotFit
	cmpi.l #127, d0
	bgt.s branchDoesNotFit
	moveq #0, d0
	rts
branchFitsWord
	cmpi.l #-32768, d0
	blt.s branchDoesNotFit
	cmpi.l #32767, d0
	bgt.s branchDoesNotFit
	moveq #0, d0
	rts
branchDoesNotFit
	moveq #1, d0
	rts
	.bend  ; tkpkgBranchValueFitsSignedWidthV5

; Direct Rust fixup_vm::execute_fixup_program v4/v7 port over the native
; signed-32 scalar transport.  Fixup inputs use a five-byte record: flags then
; little-endian u32.  Bit zero carries Rust's target_reference property; bit
; one is unresolved.  V7 transforms remain package data and are interpreted
; generically; signed i64 transform values outside native i32 transport fail
; closed.
; Inputs: A1/D1 = program; D4.W = opcode version; D5/D6/A3 = input records.
; Outputs: D0 status; D1 total output length in LastErrorBuffer.
tkpkgEncodeExecuteFixupProgramV4	.block
	movem.l d2-d7/a0/a2-a5, -(sp)
	move.w d4, d7
	cmpi.w #4, d7
	beq.s fixupVersionReady
	cmpi.w #7, d7
	bne.w fixupFail
fixupVersionReady
	movea.l a1, a0
	moveq #0, d0
	move.w d1, d0
	lea 0(a0, d0.l), a5
	lea buffers.LastErrorBuffer, a2
	moveq #0, d0
	move.w buffers.SemanticOutputWriteOffset, d0
	adda.w d0, a2

fixupLoop
	cmpa.l a5, a0
	bhs.w fixupFail
	moveq #0, d0
	move.b (a0)+, d0
	cmpi.b #$FF, d0
	beq.w fixupEnd
	cmpi.b #$01, d0
	bne.w fixupFail
	moveq #15, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w fixupFail
	lea -22(sp), sp
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, (sp)
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 2(sp)
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 4(sp)
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 6(sp)
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w fixupFrameFail
	move.l d0, 8(sp)
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 12(sp)
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w fixupFrameFail
	move.l d0, 14(sp)
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 18(sp)
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 20(sp)

	move.w 2(sp), d2
	cmpi.w #1, d2
	beq.s fixupWidthReady
	cmpi.w #2, d2
	beq.s fixupWidthReady
	cmpi.w #4, d2
	bne.w fixupFrameFail
fixupWidthReady
	cmpi.w #1, 4(sp)
	bhi.w fixupFrameFail
	cmpi.w #2, 6(sp)
	bhi.w fixupFrameFail
	cmpi.w #1, 12(sp)
	bhi.w fixupFrameFail
	cmpi.w #2, 18(sp)
	bhi.w fixupFrameFail
	cmpi.w #1, 20(sp)
	bhi.w fixupFrameFail

	move.w (sp), d0
	bsr.w tkpkgSemanticLoadFixupInputV4
	bne.w fixupFrameFail
	btst #1, d6
	beq.s fixupResolved
	tst.w 12(sp)
	beq.w fixupFrameFail
	move.l 14(sp), d3
	bra.s fixupProjected

fixupResolved
	move.w 6(sp), d0
	beq.s fixupProjected
	cmpi.w #2, d0
	bne.s fixupApplyPosition
	btst #0, d6
	beq.s fixupProjected
fixupApplyPosition
	move.l state.EncodeSelectedCurrentPc, d0
	add.l 8(sp), d0
	bvs.w fixupFrameFail
	sub.l d0, d3
	bvs.w fixupFrameFail

fixupProjected
	cmpi.w #7, d7
	bne.s fixupTransformReady
	bsr.w tkpkgSemanticApplyFixupTransformV7
	bne.w fixupFrameFail
fixupTransformReady
	move.w 18(sp), d0
	move.w 2(sp), d2
	bsr.w tkpkgSemanticValidateFixupRangeV4
	bne.w fixupFrameFail
	move.l d3, d0
	move.w 4(sp), d4
	bsr.w tkpkgSemanticEmitUnitV2
	tst.l d3
	bne.w fixupFrameFail
	lea 22(sp), sp
	bra.w fixupLoop

fixupFrameFail
	lea 22(sp), sp
fixupFail
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0
	bra.s fixupReturn
fixupEnd
	cmpa.l a5, a0
	bne.s fixupFail
	move.l a2, d1
	lea buffers.LastErrorBuffer, a1
	sub.l a1, d1
	cmp.w buffers.SemanticOutputWriteOffset, d1
	bls.s fixupFail
	moveq #0, d0
fixupReturn
	movem.l (sp)+, d2-d7/a0/a2-a5
	rts
	.bend  ; tkpkgEncodeExecuteFixupProgramV4

; Apply one Rust FixupTransform carried by SEMV v7.  The native expression
; boundary transports signed i32 scalars, so range-map i64 fields must be exact
; sign extensions of i32 values.  This is the only representation difference
; from Rust; ordering, alignment, mapping, and overflow behavior are identical.
; Inputs: A0/A5 = transform cursor/program end; D2.W = output width;
;         D3.L = projected value; D6.W bit one = unresolved.
; Outputs: D0 = 0/1; D3.L transformed value; A0 advanced.
tkpkgSemanticApplyFixupTransformV7	.block
	movem.l d1-d2/d4-d7/a1-a4, -(sp)
	lea -32(sp), sp
	move.l d3, (sp)
	move.w d2, 4(sp)
	clr.w 6(sp)
	clr.w 8(sp)
	clr.l 10(sp)
	clr.l 14(sp)
	clr.l 18(sp)
	clr.l 22(sp)
	clr.l 26(sp)
	move.w d6, 30(sp)

	moveq #1, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w transformFail
	moveq #0, d0
	move.b (a0)+, d0
	tst.b d0
	bne.s transformCheckKind
	bra.w transformIdentity
transformCheckKind
	cmpi.b #1, d0
	bne.s transformCheckRangeMap
	bra.w transformAlignedBitOr
transformCheckRangeMap
	cmpi.b #2, d0
	bne.w transformFail

transformRangeMap
	moveq #5, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w transformFail
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w transformFail
	move.l d0, 22(sp)
	bsr.w tkpkgSemanticValidateAlignmentV7
	bne.w transformFail
	moveq #0, d7
	move.b (a0)+, d7
	beq.w transformFail
	move.w 30(sp), d0
	btst #1, d0
	bne.s transformRangeLoop
	move.l 22(sp), d4
	subq.l #1, d4
	move.l (sp), d0
	and.l d4, d0
	bne.w transformFail

transformRangeLoop
	moveq #24, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w transformFail
	bsr.w tkpkgSemanticReadI64LeV7
	bsr.w tkpkgSemanticRequireI32V7
	bne.w transformFail
	move.l d1, 14(sp)
	bsr.w tkpkgSemanticReadI64LeV7
	bsr.w tkpkgSemanticRequireI32V7
	bne.w transformFail
	move.l d1, 18(sp)
	bsr.w tkpkgSemanticReadI64LeV7
	bsr.w tkpkgSemanticRequireI32V7
	bne.w transformFail
	move.l d1, 26(sp)

	move.l 14(sp), d4
	cmp.l 18(sp), d4
	bgt.w transformFail
	tst.w 6(sp)
	beq.s transformRangeOrdered
	move.l 10(sp), d5
	cmp.l d4, d5
	bge.w transformFail
transformRangeOrdered
	move.l 18(sp), 10(sp)
	move.w #1, 6(sp)
	move.l d4, d0
	add.l 26(sp), d0
	bvs.w transformFail
	move.l 18(sp), d0
	add.l 26(sp), d0
	bvs.w transformFail

	move.w 30(sp), d0
	btst #1, d0
	bne.s transformRangeNext
	move.l (sp), d5
	cmp.l 14(sp), d5
	blt.s transformRangeNext
	cmp.l 18(sp), d5
	bgt.s transformRangeNext
	tst.w 8(sp)
	bne.w transformFail
	add.l 26(sp), d5
	bvs.w transformFail
	move.l d5, (sp)
	move.w #1, 8(sp)
transformRangeNext
	subq.w #1, d7
	beq.s transformRangeDone
	bra.w transformRangeLoop
transformRangeDone
	move.w 30(sp), d0
	btst #1, d0
	beq.s transformRangeResolved
	bra.w transformIdentity
transformRangeResolved
	tst.w 8(sp)
	beq.w transformFail
	move.l (sp), d3
	bra.s transformOk

transformAlignedBitOr
	moveq #8, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w transformFail
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w transformFail
	move.l d0, 22(sp)
	bsr.w tkpkgSemanticValidateAlignmentV7
	bne.w transformFail
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w transformFail
	move.l d0, 26(sp)
	move.w 4(sp), d2
	cmpi.w #1, d2
	bne.s transformMaskWord
	cmpi.l #$ff, d0
	bhi.w transformFail
	bra.s transformMaskReady
transformMaskWord
	cmpi.w #2, d2
	bne.s transformMaskLong
	cmpi.l #$ffff, d0
	bhi.w transformFail
	bra.s transformMaskReady
transformMaskLong
	cmpi.w #4, d2
	bne.w transformFail
transformMaskReady
	move.w 30(sp), d0
	btst #1, d0
	bne.s transformIdentity
	move.l 22(sp), d4
	subq.l #1, d4
	move.l (sp), d0
	and.l d4, d0
	bne.w transformFail
	move.l (sp), d3
	or.l 26(sp), d3
	bra.s transformOk

transformIdentity
	move.l (sp), d3
transformOk
	moveq #0, d0
	bra.s transformReturn
transformFail
	moveq #1, d0
transformReturn
	lea 32(sp), sp
	movem.l (sp)+, d1-d2/d4-d7/a1-a4
	tst.l d0
	rts
	.bend  ; tkpkgSemanticApplyFixupTransformV7

; Validate a package u32 alignment as Rust's nonzero power-of-two contract.
; Input: D0.L alignment. Output: D0=0/1.
tkpkgSemanticValidateAlignmentV7	.block
	move.l d1, -(sp)
	tst.l d0
	beq.s alignmentFail
	move.l d0, d1
	subq.l #1, d1
	and.l d0, d1
	bne.s alignmentFail
	moveq #0, d0
	bra.s alignmentReturn
alignmentFail
	moveq #1, d0
alignmentReturn
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; tkpkgSemanticValidateAlignmentV7

; Read one signed package i64 stored little-endian.
; Inputs: A0 = cursor already proven to have eight bytes.
; Outputs: D0=high32, D1=low32, A0+=8.
tkpkgSemanticReadI64LeV7	.block
	move.l (a0)+, d1
	ror.w #8, d1
	swap d1
	ror.w #8, d1
	move.l (a0)+, d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	rts
	.bend  ; tkpkgSemanticReadI64LeV7

; Require one signed i64 pair to be exactly representable by native i32.
; Inputs: D0=high32, D1=low32. Output: D0=0/1.
tkpkgSemanticRequireI32V7	.block
	move.l d2, -(sp)
	moveq #0, d2
	tst.l d1
	bpl.s requireI32HighReady
	moveq #-1, d2
requireI32HighReady
	cmp.l d2, d0
	bne.s requireI32Fail
	moveq #0, d0
	bra.s requireI32Return
requireI32Fail
	moveq #1, d0
requireI32Return
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; tkpkgSemanticRequireI32V7

; Load one fixup record by index. Outputs D3=value, D6=flags, D0=0/1.
tkpkgSemanticLoadFixupInputV4	.block
	cmp.w buffers.SemanticInputRecordCount, d0
	bhs.s fixupInputFail
	move.w d0, d4
	movea.l buffers.SemanticInputRecordPtr, a4
	move.w buffers.SemanticFirstInputLen, d2
	tst.w d4
	beq.s fixupInputReady
fixupInputScan
	adda.w d2, a4
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d4
	bne.s fixupInputScan
fixupInputReady
	cmpi.w #5, d2
	bne.s fixupInputFail
	moveq #0, d6
	move.b (a4)+, d6
	andi.w #3, d6
	moveq #0, d3
	move.b (a4)+, d3
	moveq #0, d4
	move.b (a4)+, d4
	lsl.l #8, d4
	or.l d4, d3
	moveq #0, d4
	move.b (a4)+, d4
	lsl.l #8, d4
	lsl.l #8, d4
	or.l d4, d3
	moveq #0, d4
	move.b (a4)+, d4
	lsl.l #8, d4
	lsl.l #8, d4
	lsl.l #8, d4
	or.l d4, d3
	moveq #0, d0
	rts
fixupInputFail
	moveq #1, d0
	rts
	.bend  ; tkpkgSemanticLoadFixupInputV4

; Apply Rust FixupRange to native i32/u32 scalar transport.
; Inputs: D3=value, D2=width, D0=range tag. Output D0=0/1.
tkpkgSemanticValidateFixupRangeV4	.block
	cmpi.w #4, d2
	beq.s fixupRangeOk
	cmpi.w #1, d2
	beq.s fixupRangeByte
	cmpi.w #2, d2
	bne.s fixupRangeFail
	move.l d3, d1
	cmpi.w #1, d0
	beq.s fixupRangeUnsignedWord
	cmpi.w #2, d0
	beq.s fixupRangePatternWord
	cmpi.l #-32768, d1
	blt.s fixupRangeFail
	cmpi.l #$7fff, d1
	bgt.s fixupRangeFail
	bra.s fixupRangeOk
fixupRangeUnsignedWord
	tst.l d1
	bmi.s fixupRangeFail
fixupRangePatternWord
	cmpi.l #-32768, d1
	blt.s fixupRangeFail
	cmpi.l #$ffff, d1
	bgt.s fixupRangeFail
	bra.s fixupRangeOk
fixupRangeByte
	move.l d3, d1
	cmpi.w #1, d0
	beq.s fixupRangeUnsignedByte
	cmpi.w #2, d0
	beq.s fixupRangePatternByte
	cmpi.l #-128, d1
	blt.s fixupRangeFail
	cmpi.l #$7f, d1
	bgt.s fixupRangeFail
	bra.s fixupRangeOk
fixupRangeUnsignedByte
	tst.l d1
	bmi.s fixupRangeFail
fixupRangePatternByte
	cmpi.l #-128, d1
	blt.s fixupRangeFail
	cmpi.l #$ff, d1
	bgt.s fixupRangeFail
fixupRangeOk
	moveq #0, d0
	rts
fixupRangeFail
	moveq #1, d0
	rts
	.bend  ; tkpkgSemanticValidateFixupRangeV4

; Direct Rust encoding_vm::execute_encoding_program port for SEMV/CSEM v2/v6.
; Inputs: A1/D1 = program; D4.W = opcode version;
;         D5/D6/A3 = scalar record count/first length/data.
; Outputs: D0 status; D1 output length in LastErrorBuffer.
tkpkgEncodeExecuteSemanticProgramV2	.block
	movem.l d2-d7/a0/a2-a5, -(sp)
	move.w d4, -(sp)
	movea.l a1, a0
	moveq #0, d0
	move.w d1, d0
	lea 0(a0, d0.l), a5
	lea buffers.LastErrorBuffer, a2
	moveq #0, d0
	move.w buffers.SemanticOutputWriteOffset, d0
	adda.w d0, a2
	clr.w d1

encodingLoop
	cmpa.l a5, a0
	bhs.w encodingFail
	moveq #0, d0
	move.b (a0)+, d0
	cmpi.b #$FF, d0
	beq.w encodingEnd
	cmpi.b #$01, d0
	beq.w encodingLiteral
	cmpi.b #$02, d0
	beq.w encodingScalar
	cmpi.b #$03, d0
	beq.w encodingFields
	cmpi.b #$04, d0
	beq.w encodingInputFields
	bra.w encodingFail

encodingLiteral
	moveq #6, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w encodingFail
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d4
	move.b (a0)+, d4
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w encodingFail
	bsr.w tkpkgSemanticValidateUnitV2
	tst.l d3
	bne.w encodingFail
	bsr.w tkpkgSemanticEmitUnitV2
	tst.l d3
	bne.w encodingFail
	bra.w encodingLoop

encodingScalar
	moveq #19, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w encodingFail
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d4
	move.b (a0)+, d4
	lea -22(sp), sp
	move.w d0, 16(sp)
	move.w d2, 18(sp)
	move.w d4, 20(sp)
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w encodingScalarFrameFail
	move.l d0, 12(sp)
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w encodingScalarFrameFail
	move.l d0, 8(sp)
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w encodingScalarFrameFail
	move.l d0, 4(sp)
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w encodingScalarFrameFail
	move.l d0, (sp)
	moveq #0, d0
	move.w 16(sp), d0
	bsr.w tkpkgSemanticLoadInputV2
	bne.w encodingScalarFrameFail
	move.l d3, d7
	moveq #0, d6
	tst.l d7
	bpl.s encodingScalarInputHigh
	moveq #-1, d6
encodingScalarInputHigh
	cmp.l 8(sp), d6
	blt.w encodingScalarFrameFail
	bgt.s encodingScalarCheckMax
	cmp.l 12(sp), d7
	blo.w encodingScalarFrameFail
encodingScalarCheckMax
	cmp.l (sp), d6
	bgt.w encodingScalarFrameFail
	blt.s encodingScalarRangeOk
	cmp.l 4(sp), d7
	bhi.w encodingScalarFrameFail
encodingScalarRangeOk
	move.w 18(sp), d2
	move.w 20(sp), d4
	move.l d7, d0
	lea 22(sp), sp
	cmpi.w #1, d2
	beq.s encodingScalarMaskByte
	cmpi.w #2, d2
	beq.s encodingScalarMaskWord
	cmpi.w #4, d2
	bne.w encodingFail
	bra.s encodingScalarUnitReady
encodingScalarMaskByte
	andi.l #$000000FF, d0
	bra.s encodingScalarUnitReady
encodingScalarMaskWord
	andi.l #$0000FFFF, d0
encodingScalarUnitReady
	bsr.w tkpkgSemanticValidateUnitV2
	tst.l d3
	bne.w encodingFail
	bsr.w tkpkgSemanticEmitUnitV2
	tst.l d3
	bne.w encodingFail
	bra.w encodingLoop
encodingScalarFrameFail
	lea 22(sp), sp
	bra.w encodingFail

encodingFields
	moveq #7, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w encodingFail
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d4
	move.b (a0)+, d4
	bsr.w tkpkgSemanticReadU32LeV2
	bne.w encodingFail
	move.l d0, d7
	bsr.w tkpkgSemanticValidateUnitV2
	tst.l d3
	bne.w encodingFail
	moveq #0, d6
	move.b (a0)+, d6
	tst.w d6
	beq.w encodingFail
	bra.s encodingFieldsReady

encodingInputFields
	cmpi.w #6, (sp)
	bne.w encodingFail
	moveq #4, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w encodingFail
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d4
	move.b (a0)+, d4
	moveq #0, d6
	move.b (a0)+, d6
	tst.w d6
	beq.w encodingFail
	move.w d4, -(sp)
	move.w d2, -(sp)
	bsr.w tkpkgSemanticLoadInputV2
	bne.w encodingInputFieldsLoadFail
	move.l d3, d7
	move.w (sp)+, d2
	move.w (sp)+, d4
	move.l d7, d0
	bsr.w tkpkgSemanticValidateUnitV2
	tst.l d3
	bne.w encodingFail

encodingFieldsReady
	move.w d6, d0
	lsl.w #2, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.w encodingFail
	move.w d4, -(sp)
	move.w d2, -(sp)
	clr.l -(sp)
encodingFieldLoop
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d5
	move.b (a0)+, d5
	moveq #0, d3
	move.b (a0)+, d3
	moveq #0, d4
	move.b (a0)+, d4
	tst.b d3
	beq.w encodingFieldStackFail
	move.w d5, d1
	add.w d3, d1
	move.w d2, d0
	lsl.w #3, d0
	cmp.w d0, d1
	bhi.w encodingFieldStackFail
	cmpi.b #1, d4
	bhi.w encodingFieldStackFail
	move.w d0, -(sp)
	move.w d3, -(sp)
	move.w d5, -(sp)
	move.w d4, -(sp)
	moveq #0, d0
	move.b -4(a0), d0
	bsr.w tkpkgSemanticLoadInputV2
	bne.w encodingFieldLocalsFail
	move.w 2(sp), d5
	move.w 4(sp), d4
	moveq #-1, d0
	cmpi.w #32, d4
	beq.s encodingFieldMaskReady
	moveq #32, d1
	sub.w d4, d1
	lsr.l d1, d0
encodingFieldMaskReady
	tst.w (sp)
	bne.s encodingFieldSigned
	tst.l d3
	bmi.w encodingFieldLocalsFail
	cmp.l d0, d3
	bhi.w encodingFieldLocalsFail
	bra.s encodingFieldRangeOk
encodingFieldSigned
	move.l d0, d1
	lsr.l #1, d1
	cmp.l d1, d3
	bgt.w encodingFieldLocalsFail
	not.l d1
	cmp.l d1, d3
	blt.w encodingFieldLocalsFail
encodingFieldRangeOk
	move.l d0, d1
	lsl.l d5, d1
	move.l 8(sp), d4
	and.l d1, d4
	bne.w encodingFieldLocalsFail
	or.l d1, 8(sp)
	and.l d0, d3
	lsl.l d5, d3
	not.l d1
	and.l d1, d7
	or.l d3, d7
	lea 8(sp), sp
	subq.w #1, d6
	bne.w encodingFieldLoop
	move.w 4(sp), d2
	move.w 6(sp), d4
	lea 8(sp), sp
	move.l d7, d0
	bsr.w tkpkgSemanticEmitUnitV2
	tst.l d3
	bne.w encodingFail
	bra.w encodingLoop
encodingInputFieldsLoadFail
	addq.l #4, sp
	bra.w encodingFail
encodingFieldLocalsFail
	lea 8(sp), sp
encodingFieldStackFail
	lea 8(sp), sp
	bra.w encodingFail

encodingEnd
	cmpa.l a5, a0
	bne.s encodingFail
	move.l a2, d1
	lea buffers.LastErrorBuffer, a1
	sub.l a1, d1
	tst.w d1
	beq.s encodingFail
	moveq #0, d0
	bra.s encodingReturn
encodingFail
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0
encodingReturn
	addq.l #2, sp
	movem.l (sp)+, d2-d7/a0/a2-a5
	rts
	.bend  ; tkpkgEncodeExecuteSemanticProgramV2

tkpkgSemanticRequireProgramBytesV2	.block
	movea.l a0, a1
	adda.l d0, a1
	cmpa.l a5, a1
	bhi.s semanticRequireFail
	moveq #0, d1
	rts
semanticRequireFail
	moveq #1, d1
	rts
	.bend  ; tkpkgSemanticRequireProgramBytesV2

tkpkgSemanticReadU32LeV2	.block
	moveq #4, d0
	bsr.w tkpkgSemanticRequireProgramBytesV2
	bne.s semanticReadFail
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d1
	move.b (a0)+, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a0)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a0)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	rts
semanticReadFail
	moveq #1, d1
	rts
	.bend  ; tkpkgSemanticReadU32LeV2

; Validate width/endian and that D0 fits the selected unit. D3 is status.
tkpkgSemanticValidateUnitV2	.block
	cmpi.w #1, d2
	beq.s semanticValidateByte
	cmpi.w #2, d2
	beq.s semanticValidateWord
	cmpi.w #4, d2
	bne.s semanticValidateFail
	bra.s semanticValidateEndian
semanticValidateByte
	cmpi.l #$000000FF, d0
	bhi.s semanticValidateFail
	bra.s semanticValidateEndian
semanticValidateWord
	cmpi.l #$0000FFFF, d0
	bhi.s semanticValidateFail
semanticValidateEndian
	cmpi.w #1, d4
	bhi.s semanticValidateFail
	moveq #0, d3
	rts
semanticValidateFail
	moveq #1, d3
	rts
	.bend  ; tkpkgSemanticValidateUnitV2

; Emit D0 in width D2 and endian D4.
tkpkgSemanticEmitUnitV2	.block
	move.l a2, d3
	lea buffers.LastErrorBuffer, a4
	sub.l a4, d3
	add.l d2, d3
	cmpi.l #buffers.LAST_ERROR_BUFFER_CAPACITY, d3
	bhi.s semanticEmitFail
	tst.b d4
	bne.s semanticEmitLittle
	cmpi.w #4, d2
	beq.s semanticEmitBig4
	cmpi.w #2, d2
	beq.s semanticEmitBig2
	move.b d0, (a2)+
	bra.s semanticEmitDone
semanticEmitBig4
	move.l d0, d3
	lsr.l #8, d3
	lsr.l #8, d3
	lsr.l #8, d3
	move.b d3, (a2)+
	move.l d0, d3
	lsr.l #8, d3
	lsr.l #8, d3
	move.b d3, (a2)+
semanticEmitBig2
	move.l d0, d3
	lsr.l #8, d3
	move.b d3, (a2)+
	move.b d0, (a2)+
	bra.s semanticEmitDone
semanticEmitLittle
	move.l d0, d3
	move.w d2, d5
	move.b d3, (a2)+
	subq.w #1, d5
	beq.s semanticEmitRestoreWidth
semanticEmitLittleLoop
	lsr.l #8, d3
	move.b d3, (a2)+
	subq.w #1, d5
	bne.s semanticEmitLittleLoop
semanticEmitRestoreWidth
semanticEmitDone
	moveq #0, d3
	rts
semanticEmitFail
	moveq #1, d3
	rts
	.bend  ; tkpkgSemanticEmitUnitV2

; Load one four-byte little-endian scalar record by index.
; Inputs: D0.W index; D5/D6/A3 selected operand records.
; Outputs: D0 status; D3 value.
tkpkgSemanticLoadInputV2	.block
	cmp.w buffers.SemanticInputRecordCount, d0
	bhs.s semanticInputFail
	move.w d0, d4
	movea.l buffers.SemanticInputRecordPtr, a4
	move.w buffers.SemanticFirstInputLen, d2
	tst.w d4
	beq.s semanticInputReady
semanticInputScan
	adda.w d2, a4
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d4
	bne.s semanticInputScan
semanticInputReady
	cmpi.w #4, d2
	bne.s semanticInputFail
	moveq #0, d3
	move.b (a4)+, d3
	moveq #0, d4
	move.b (a4)+, d4
	lsl.l #8, d4
	or.l d4, d3
	moveq #0, d4
	move.b (a4)+, d4
	lsl.l #8, d4
	lsl.l #8, d4
	or.l d4, d3
	moveq #0, d4
	move.b (a4)+, d4
	lsl.l #8, d4
	lsl.l #8, d4
	lsl.l #8, d4
	or.l d4, d3
	moveq #0, d0
	rts
semanticInputFail
	moveq #1, d0
	rts
	.bend  ; tkpkgSemanticLoadInputV2

	.pub
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
