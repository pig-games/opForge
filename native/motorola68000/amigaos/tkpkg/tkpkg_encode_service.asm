; Package-table encoding and selected-envelope output construction.
; Ownership extraction only: package lookup, program execution, and bytes remain unchanged.

	.module tkpkg.amigaos.encode_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.encoding_execution as execution
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.selection_service as selection
	.use tkpkg.amigaos.selection_state as state
	.use tkpkg.amigaos.compact_table as compact
	.use tkpkg.amigaos.semantic_bindings as bindings
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	.use debug.amigaos.runtime_profile as runtime_profile
.endif

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

	.section bss, kind=bss
	.priv
Execution	.res byte, execution.Context.FixupTargets+4
BindingId
	.res word, 1
BindingPending
	.res byte, 1
	.align 2
	.endsection

	.section code, kind=code
	.pub

encodeSelectedInstructionV1	.block
	movem.l d2-d7/a2-a6, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_SERVICE_ENCODING, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	movem.l (sp)+, d0-d1
.endif
	; Rust returns a fresh PortableFixupResult for every selected encode.
	; Reset the native side channel at the same request boundary so semantic
	; sequence calls cannot replay a pass-one fixup during pass two.
	clr.w buffers.SemanticOutputFixupCount
	btst #1, buffers.PackageStateFlags
	bne.s havePipeline
	lea EvaluateExprNeedsPipelineText, a1
	moveq #EVAL_EXPR_NEEDS_PIPELINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w return

havePipeline
	moveq #0, d0
	movea.l d0, a1
	jsr compact.find
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
	bsr.w tkpkgNormalizeOutputFixupLengthV1
	bne.s return
	tst.w d1
	bne.s return
	moveq #1, d2
	jsr selection.noOutputErrorV1

return
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	jsr runtime_profile.opforgeRuntimeProfileLeaveServiceV1
.endif
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
	bne.s selectedTextProgram
	tst.b state.EncodeSelectedSemanticPlanKind
	beq.w fail
	cmpi.w #2, d7
	bcs.w fail
	moveq #0, d0
	move.b (a4)+, d0
	moveq #0, d1
	move.b (a4)+, d1
	lsl.w #8, d1
	or.w d1, d0
	move.w d0, state.SemanticProgramId
	subq.w #2, d7
	movea.l a4, a6
	bra.s selectedProgramReady
selectedTextProgram
	cmp.w d7, d4
	bhi.w fail
	movea.l a4, a6
	adda.w d4, a4
	sub.w d4, d7
selectedProgramReady
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
	tst.w d4
	bne.s encodeTextSemanticProgram
	moveq #0, d0
	move.w state.SemanticProgramId, d0
	bsr.w encodeById
	bra.s semanticProgramExecutedStatus
encodeTextSemanticProgram
	bsr.w tkpkgEncodeFindAndExecuteSemanticProgramV2
semanticProgramExecutedStatus
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
	bne.s semanticSequenceTextProgram
	cmpi.w #2, d7
	bcs.w fail
	moveq #0, d0
	move.b (a4)+, d0
	moveq #0, d4
	move.b (a4)+, d4
	lsl.w #8, d4
	or.w d4, d0
	move.w d0, state.SemanticProgramId
	subq.w #2, d7
	moveq #0, d4
	movea.l a4, a6
	bra.s semanticSequenceProgramReady
semanticSequenceTextProgram
	cmp.w d7, d4
	bhi.w fail
	movea.l a4, a6
	adda.w d4, a4
	sub.w d4, d7
semanticSequenceProgramReady
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
	tst.w d4
	beq.s semanticSequenceNumericProgram
	bsr.w tkpkgEncodeFindAndExecuteSemanticProgramV2
	bra.s semanticSequenceProgramExecuted
semanticSequenceNumericProgram
	moveq #0, d0
	move.w state.SemanticProgramId, d0
	bsr.w encodeById
semanticSequenceProgramExecuted
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
	jsr compact.find
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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_ENCODING, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
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

; Execute one caller-selected package semantic program through the same
; dialect/cpu/family precedence and Rust VM interpreter as instruction steps.
; The program id is opaque to tkpkg; directive/output owners may request a
; semantic role without owning its endian, width, transform, or relocation.
; Inputs: A0/D0=program id; A1/D1=input records/first record length;
; D2.W=record count; D3=current PC.
; Outputs: D0=0 success, 1 malformed/execution failure, 2 program absent;
; D1=output length; A0=output bytes on success.
executeNamedSemanticProgramV1	.block
	movem.l d2-d7/a1-a6, -(sp)
	movea.l a0, a6
	move.w d0, d4
	move.l a1, buffers.SemanticInputRecordPtr
	move.w d1, buffers.SemanticFirstInputLen
	move.w d2, buffers.SemanticInputRecordCount
	move.l d3, state.EncodeSelectedCurrentPc
	clr.w buffers.SemanticOutputWriteOffset
	clr.w buffers.SemanticOutputFixupCount
	bsr.w tkpkgEncodeFindAndExecuteSemanticProgramV2
	tst.l d0
	bne.s namedReturn
	tst.b d3
	beq.s namedMissing
	bsr.w tkpkgNormalizeOutputFixupLengthV1
	bne.s namedReturn
	lea buffers.LastErrorBuffer, a0
	moveq #0, d0
	bra.s namedReturn
namedMissing
	moveq #0, d1
	moveq #2, d0
namedReturn
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; executeNamedSemanticProgramV1

	.priv
; Resolve and execute one CMSE numeric semantic-program id. Bindings retain
; package program metadata only; projected inputs and current PC stay fresh.
; Inputs: D0.W=CMSE string id; semantic input/current-PC state already set.
; Outputs: D0 status, D1 encoded length, D3.B found flag.
encodeById	.block
	movem.l d2/d4-d7/a0/a2-a6, -(sp)
	move.w d0, d6
	cmp.w buffers.CompactSelectorStringCount, d6
	bhs.s bindingMalformed
	jsr bindings.find
	tst.l d0
	beq.s bindingHit
	lea buffers.CompactSelectorPlanText, a0
	move.w d6, d0
	jsr selection.resolveCompactSelectorStringV1
	bne.s bindingMalformed
	tst.w d0
	beq.s bindingMalformed
	move.w d6, BindingId
	move.b #1, BindingPending
	move.w d0, d4
	lea buffers.CompactSelectorPlanText, a6
	bsr.w tkpkgEncodeFindAndExecuteSemanticProgramV2
	clr.b BindingPending
	bra.s bindingReturn
bindingHit
	move.w d1, d4
	move.w d2, d1
	bsr.w executeBoundProgram
	moveq #1, d3
	bra.s bindingReturn
bindingMalformed
	lea EncodeTableMalformedText, a1
	moveq #ENCODE_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0
	moveq #1, d3
bindingReturn
	movem.l (sp)+, d2/d4-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; encodeById

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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_ENCODING, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	movem.l (sp)+, d0-d1
.endif
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
	beq.s semanticBindingVersionReady
	cmpi.w #4, d4
	beq.s semanticBindingVersionReady
	cmpi.w #5, d4
	beq.s semanticBindingVersionReady
	cmpi.w #6, d4
	beq.s semanticBindingVersionReady
	cmpi.w #7, d4
	bne.s semanticMalformed
semanticBindingVersionReady
	tst.b BindingPending
	beq.s semanticBindingReady
	move.w BindingId, d0
	move.w d4, d1
	move.w 4(sp), d2
	movea.l 6(sp), a1
	jsr bindings.store
	clr.b BindingPending
semanticBindingReady
	movea.l 6(sp), a1
	move.w 4(sp), d1
	bsr.w executeBoundProgram
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

executeBoundProgram	.block
	move.l a6, -(sp)
	bsr.w prepareExecution
	jsr execution.semantic
	move.w execution.Context.FixupCount(a6), buffers.SemanticOutputFixupCount
	movea.l (sp)+, a6
	tst.l d0
	rts
	.bend  ; executeBoundProgram

tkpkgNormalizeOutputFixupLengthV1	.block
	move.l a6, -(sp)
	bsr.w prepareExecution
	jsr execution.normalizeFixupLength
	movea.l (sp)+, a6
	tst.l d0
	rts
	.bend  ; tkpkgNormalizeOutputFixupLengthV1

; Project service-owned state into one small ephemeral execution context.
; Returns A6=context; preserves all data registers and A0-A5. CCR unspecified.
prepareExecution	.block
	lea Execution, a6
	move.l buffers.SemanticInputRecordPtr, execution.Context.Input(a6)
	move.w buffers.SemanticInputRecordCount, execution.Context.InputCount(a6)
	move.w buffers.SemanticFirstInputLen, execution.Context.FirstInputLen(a6)
	move.w buffers.SemanticOutputWriteOffset, execution.Context.WriteOffset(a6)
	move.w buffers.SemanticOutputFixupCount, execution.Context.FixupCount(a6)
	move.l state.EncodeSelectedCurrentPc, execution.Context.Pc(a6)
	move.w state.EncodeSelectedSessionPass, execution.Context.Pass(a6)
	move.b state.EncodeSelectedMselUnstable, execution.Context.Unstable(a6)
	move.b state.EncodeSelectedDeferUnstableBranchTarget, execution.Context.Defer(a6)
	move.b state.EncodeSelectedMselHasSymbolReference, execution.Context.HasSymbol(a6)
	move.w state.EncodeSelectedMselMnemonicLen, execution.Context.MnemonicLength(a6)
	move.l #buffers.LastErrorBuffer, execution.Context.Output(a6)
	move.l #buffers.SemanticOutputFixupOffsets, execution.Context.FixupOffsets(a6)
	move.l #buffers.SemanticOutputFixupEncodedAddends, execution.Context.FixupAddends(a6)
	move.l #buffers.SemanticOutputFixupWidths, execution.Context.FixupWidths(a6)
	move.l #buffers.SemanticOutputFixupTargetSymbolIndices, execution.Context.FixupTargets(a6)
	move.l #buffers.CompactSelectorMnemonicText, execution.Context.Mnemonic(a6)
	move.l #buffers.LAST_ERROR_BUFFER_CAPACITY, execution.Context.Capacity(a6)
	move.w #buffers.SEMANTIC_OUTPUT_FIXUP_CAPACITY, execution.Context.FixupCapacity(a6)
	rts
	.bend  ; prepareExecution

	.pub
; Return the number of PortableOutputFixup records produced by the most recent
; selected-instruction request.
getOutputFixupCountV1	.block
	moveq #0, d0
	move.w buffers.SemanticOutputFixupCount, d0
	rts
	.bend  ; getOutputFixupCountV1

; Return one PortableOutputFixup side-channel record.
; Inputs: D0.W=index. Outputs: D0=0/1, D1=offset, D2=width,
; D3=opaque target symbol index, D4=encoded scalar.
getOutputFixupV1	.block
	moveq #0, d5
	move.w d0, d5
	cmp.w buffers.SemanticOutputFixupCount, d5
	bhs.s getOutputFixupFail
	move.l d5, d0
	lsl.l #2, d0
	lea buffers.SemanticOutputFixupOffsets, a0
	move.l 0(a0, d0.l), d1
	lea buffers.SemanticOutputFixupEncodedAddends, a0
	move.l 0(a0, d0.l), d4
	move.l d5, d0
	add.w d0, d0
	lea buffers.SemanticOutputFixupWidths, a0
	moveq #0, d2
	move.w 0(a0, d0.w), d2
	lea buffers.SemanticOutputFixupTargetSymbolIndices, a0
	moveq #0, d3
	move.w 0(a0, d0.w), d3
	moveq #0, d0
	rts
getOutputFixupFail
	moveq #1, d0
	rts
	.bend  ; getOutputFixupV1

	.priv
; Adapt the existing table ABI to the same caller-owned execution context.
tkpkgEncodeExecuteProgram	.block
	move.l a6, -(sp)
	bsr.w prepareExecution
	jsr execution.table
	movea.l (sp)+, a6
	tst.l d0
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
