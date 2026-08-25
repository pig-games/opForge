; Operand-plan dispatch and evaluation for the tkpkg selection path.
; This is an ownership extraction only: plan tags and emitted bytes are unchanged.

	.module tkpkg.amigaos.operand_runtime
	.cpu 68020
	.pub
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.selection_state as state
	.use tkpkg.amigaos.runtime_context as context
	.use opcore.amigaos.expr_bridge

TKPKG_SELECTED_STATUS_OK = 0
TKPKG_SELECTED_STATUS_NO_OUTPUT = 1
TKPKG_SELECTED_STATUS_OPERAND_ERROR = 4
TKPKG_MSEL_SURFACE_NONE = 0
TKPKG_MSEL_SURFACE_IMMEDIATE = 1
TKPKG_MSEL_SURFACE_ACCUMULATOR = 2
TKPKG_MSEL_SURFACE_DIRECT_X = 3
TKPKG_MSEL_SURFACE_DIRECT_Y = 4
TKPKG_MSEL_SURFACE_INDIRECT = 5
TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X = 6
TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y = 7

	.section data, kind=data
	.priv

TkpkgMselPlanNoneText
	.byte "none", 0
TkpkgMselPlanU8Text
	.byte "u8", 0
TkpkgMselPlanU16Text
	.byte "u16", 0
TkpkgMselPlanBranch8Text
	.byte "rel8", 0
TkpkgMselPlanPairU8Rel8Text
	.byte "pair_u8_rel8", 0
TkpkgMselShapeImmediateText
	.byte "immediate", 0
TkpkgMselShapeAccumulatorText
	.byte "accumulator", 0
TkpkgMselShapeDirectXText
	.byte "direct_x", 0
TkpkgMselShapeDirectYText
	.byte "direct_y", 0
TkpkgMselShapeIndirectText
	.byte "indirect", 0
TkpkgMselShapeIndexedIndirectXText
	.byte "indexed_indirect_x", 0
TkpkgMselShapeIndirectIndexedYText
	.byte "indirect_indexed_y", 0
TkpkgMselModeIndexedIndirectXText
	.byte "indexedindirectx", 0
TkpkgMselModeIndirectIndexedYText
	.byte "indirectindexedy", 0

	.endsection

	.section code, kind=code
	.pub

tkpkgMselTryBuildCandidateV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea PlanDispatchTable(pc), a4
	moveq #4, d7

dispatchPlanLoop
	movea.l (a4)+, a2
	moveq #0, d1
	move.w (a4)+, d1
	addq.l #2, a4
	bsr.w tkpkgMselPlanEqualsV1
	beq.s dispatchPlanNext
	movea.l (a4), a0
	jmp (a0)

dispatchPlanNext
	adda.w #4, a4
	dbf d7, dispatchPlanLoop
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.w return

tryU8
	bsr.w tkpkgMselEvalOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	tst.b state.EncodeSelectedMselUnstable
	beq.s tryU8Stable
	moveq #1, d6
	bra.w tryUnstablePassOneOperand

tryU8Stable
	move.l state.EncodeSelectedMselValue, d3
	bpl.s tryU8NonNegative
	bra.w operandError

tryU8NonNegative
	cmpi.l #$000000FF, d3
	bls.s tryU8Fits
	bra.w noOutput

tryU8Fits
	moveq #1, d6
	bra.w buildOperand

tryU16
	bsr.w tkpkgMselEvalOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	move.l state.EncodeSelectedMselValue, d3
	bpl.s tryU16NonNegative
	bra.w operandError

tryU16NonNegative
	cmpi.l #$0000FFFF, d3
	bls.s tryU16Fits
	bra.w operandError

tryU16Fits
	moveq #2, d6
	bra.w buildOperand

tryBranchOffset8
	jsr context.getPassV1
	cmpi.w #1, d0
	bne.s tryBranchEvaluate
	moveq #1, d6
	bra.w tryUnstablePassOneOperand

tryBranchEvaluate
	bsr.w tkpkgMselEvalOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	tst.b state.EncodeSelectedMselUnstable
	beq.s tryBranchStable
	moveq #1, d6
	bra.w tryUnstablePassOneOperand

tryBranchStable
	move.l state.EncodeSelectedMselValue, d3
	move.l state.EncodeSelectedCurrentPc, d4
	addq.l #2, d4
	sub.l d4, d3
	cmpi.l #-128, d3
	bge.s tryBranchMinFits
	bra.w operandError

tryBranchMinFits
	cmpi.l #127, d3
	ble.s tryBranchFits
	bra.w operandError

tryBranchFits
	move.l d3, state.EncodeSelectedMselValue
	moveq #1, d6
	bra.w buildOperand

tryPairU8Rel8
	clr.l state.PairAPtr.l
	clr.w state.PairALen.l
	clr.l state.PairBPtr.l
	clr.w state.PairBLen.l
	movea.l state.EncodeSelectedMselExprPtr, a1
	move.w state.EncodeSelectedMselExprLen, d7
	beq.w operandError
	moveq #0, d5
	moveq #0, d6

pairScanLoop
	tst.w d7
	beq.w operandError
	move.b (a1)+, d4
	cmpi.b #'(', d4
	beq.s pairOpenParen
	cmpi.b #')', d4
	beq.s pairCloseParen
	cmpi.b #',', d4
	bne.s pairNextChar
	tst.w d6
	beq.s pairFoundComma
	bra.s pairNextChar

pairOpenParen
	addq.w #1, d6
	bra.s pairNextChar

pairCloseParen
	tst.w d6
	beq.s pairNextChar
	subq.w #1, d6

pairNextChar
	addq.w #1, d5
	subq.w #1, d7
	bra.s pairScanLoop

pairFoundComma
	movea.l state.EncodeSelectedMselExprPtr, a0
	moveq #0, d0
	move.w d5, d0
	move.l d0, d2

pairFirstTrimStartLoop
	tst.l d2
	beq.w operandError
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s pairFirstTrimStartOne
	cmpi.b #9, d3
	bne.s pairFirstTrimEndInit

pairFirstTrimStartOne
	addq.l #1, a0
	subq.l #1, d2
	bra.s pairFirstTrimStartLoop

pairFirstTrimEndInit
	lea 0(a0, d2.l), a1
	subq.l #1, a1

pairFirstTrimEndLoop
	tst.l d2
	beq.w operandError
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s pairFirstTrimEndOne
	cmpi.b #9, d3
	bne.s pairFirstTrimOk

pairFirstTrimEndOne
	subq.l #1, a1
	subq.l #1, d2
	bra.s pairFirstTrimEndLoop

pairFirstTrimOk
	move.l a0, state.PairAPtr.l
	move.w d2, state.PairALen.l
	movea.l state.EncodeSelectedMselExprPtr, a0
	adda.w d5, a0
	addq.l #1, a0
	moveq #0, d0
	move.w state.EncodeSelectedMselExprLen, d0
	sub.w d5, d0
	subq.w #1, d0
	move.l d0, d2

pairSecondTrimStartLoop
	tst.l d2
	beq.w operandError
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s pairSecondTrimStartOne
	cmpi.b #9, d3
	bne.s pairSecondTrimEndInit

pairSecondTrimStartOne
	addq.l #1, a0
	subq.l #1, d2
	bra.s pairSecondTrimStartLoop

pairSecondTrimEndInit
	lea 0(a0, d2.l), a1
	subq.l #1, a1

pairSecondTrimEndLoop
	tst.l d2
	beq.w operandError
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s pairSecondTrimEndOne
	cmpi.b #9, d3
	bne.s pairSecondTrimOk

pairSecondTrimEndOne
	subq.l #1, a1
	subq.l #1, d2
	bra.s pairSecondTrimEndLoop

pairSecondTrimOk
	move.l a0, d0
	move.l d0, state.PairBPtr.l
	move.w d2, state.PairBLen.l
	moveq #0, d6
	jsr context.getPassV1
	cmpi.w #1, d0
	beq.s pairPassCaptured
	moveq #1, d6

pairPassCaptured
	move.w d6, -(sp)
	move.l state.PairAPtr.l, d0
	move.l d0, state.EncodeSelectedMselExprPtr
	move.w state.PairALen.l, d0
	move.w d0, state.EncodeSelectedMselExprLen
	bsr.w tkpkgMselEvalPairPartOperandV1
	move.w (sp)+, d6
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	move.l state.EncodeSelectedMselValue, d3
	bpl.s tryPairFirstNonNegative
	bra.w operandError

tryPairFirstNonNegative
	cmpi.l #$000000FF, d3
	bls.s tryPairFirstFits
	bra.w operandError

tryPairFirstFits
	move.l d3, state.PairAVal.l
	tst.w d6
	bne.s tryPairSecondStable
	clr.l state.PairBVal.l
	bra.w buildPairOperand

tryPairSecondStable
	move.l state.PairBPtr.l, d0
	move.l d0, state.EncodeSelectedMselExprPtr
	move.w state.PairBLen.l, d0
	move.w d0, state.EncodeSelectedMselExprLen
	bsr.w tkpkgMselEvalPairPartOperandV1
	cmpi.l #TKPKG_SELECTED_STATUS_OK, d0
	bne.w return
	move.l state.EncodeSelectedMselValue, d3
	move.l state.EncodeSelectedCurrentPc, d4
	addq.l #3, d4
	sub.l d4, d3
	cmpi.l #-128, d3
	bge.s tryPairSecondMinFits
	bra.w operandError

tryPairSecondMinFits
	cmpi.l #127, d3
	ble.s tryPairSecondFits
	bra.w operandError

tryPairSecondFits
	move.l d3, state.PairBVal.l
	bra.w buildPairOperand

tryUnstablePassOneOperand
	jsr context.getPassV1
	cmpi.w #1, d0
	bne.w noOutput
	clr.l state.EncodeSelectedMselValue
	bra.w buildOperand

buildNone
	tst.w state.EncodeSelectedMselExprLen
	beq.s buildNoneOperand
	bsr.w tkpkgMselCurrentShapeCodeV1
	cmpi.b #TKPKG_MSEL_SURFACE_ACCUMULATOR, d0
	bne.s noOutput
	bsr.w tkpkgMselExprIsAccumulatorAV1
	beq.s noOutput

buildNoneOperand
	moveq #0, d6
	bra.w buildOperand

noOutput
	moveq #TKPKG_SELECTED_STATUS_NO_OUTPUT, d0
	bra.w return

operandError
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	bra.w return

buildOperand
	bsr.w tkpkgMselWriteCandidateEnvelopeV1
	bra.w return

buildPairOperand
	lea buffers.TokenScratchBuffer, a4
	move.w state.EncodeSelectedMselMnemonicLen, d0
	cmpi.w #255, d0
	bhi.w operandError
	move.b d0, (a4)+
	movea.l a5, a0
	bsr.w tkpkgMselCopyBytesV1
	move.b #1, (a4)+
	move.w state.EncodeSelectedMselModeLen, d0
	cmpi.w #255, d0
	bhi.w operandError
	move.b d0, (a4)+
	movea.l state.EncodeSelectedMselModePtr, a0
	bsr.w tkpkgMselCopyBytesV1
	move.b #2, (a4)+
	move.b #1, (a4)+
	move.l state.PairAVal.l, d3
	move.b d3, (a4)+
	move.b #1, (a4)+
	move.l state.PairBVal.l, d3
	move.b d3, (a4)+
	move.l a4, d1
	lea buffers.TokenScratchBuffer, a0
	sub.l a0, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	bra.w return

	.align 2
PlanDispatchTable
	.long TkpkgMselPlanNoneText
	.word 4
	.word 0
	.long buildNone
	.long TkpkgMselPlanU8Text
	.word 2
	.word 0
	.long tryU8
	.long TkpkgMselPlanU16Text
	.word 3
	.word 0
	.long tryU16
	.long TkpkgMselPlanBranch8Text
	.word 4
	.word 0
	.long tryBranchOffset8
	.long TkpkgMselPlanPairU8Rel8Text
	.word 12
	.word 0
	.long tryPairU8Rel8

return
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; tkpkgMselTryBuildCandidateV1

; Compare current selected-plan text against the caller-supplied plan tag.
; Inputs: A2 = expected plan text; D1 = expected plan length.
; Outputs: D0 = 1 when plan matches, 0 otherwise.
; Clobbers: D0/D4/CCR.
; CCR: reflects D0 on return.
tkpkgMselPlanEqualsV1	.block
	movem.l d1/a2, -(sp)
	movea.l state.EncodeSelectedMselPlanPtr, a1
	move.w state.EncodeSelectedMselPlanLen, d0
	bsr.w tkpkgOperandStringEqAsciiCasefoldV1
	movem.l (sp)+, d1/a2
	rts
	.bend  ; tkpkgMselPlanEqualsV1

; Evaluate one operand part from a package-owned pair plan.
; Inputs: state.EncodeSelectedMselExprPtr/Len identify the part to evaluate.
; Outputs: D0 = selected status; state.EncodeSelectedMselValue set on success.
; Clobbers: D0-D1/CCR plus tkpkgMselEvalOperandV1 clobbers.
; CCR: reflects D0 on return.
tkpkgMselEvalPairPartOperandV1	.block
	move.l state.EncodeSelectedMselModePtr, -(sp)
	move.w state.EncodeSelectedMselModeLen, -(sp)
	move.l state.EncodeSelectedCurrentShapePtr, -(sp)
	move.w state.EncodeSelectedCurrentShapeLen, -(sp)
	clr.l state.EncodeSelectedCurrentShapePtr
	clr.w state.EncodeSelectedCurrentShapeLen
	clr.l state.EncodeSelectedMselModePtr
	clr.w state.EncodeSelectedMselModeLen
	bsr.w tkpkgMselEvalOperandV1
	move.w (sp)+, d1
	move.w d1, state.EncodeSelectedCurrentShapeLen
	move.l (sp)+, d1
	move.l d1, state.EncodeSelectedCurrentShapePtr
	move.w (sp)+, d1
	move.w d1, state.EncodeSelectedMselModeLen
	move.l (sp)+, d1
	move.l d1, state.EncodeSelectedMselModePtr
	tst.l d0
	rts
	.bend  ; tkpkgMselEvalPairPartOperandV1

tkpkgMselEvalOperandV1	.block
	bsr.w tkpkgMselCurrentShapeCodeV1
	moveq #0, d7
	moveq #0, d6
	moveq #0, d5
	cmpi.b #TKPKG_MSEL_SURFACE_IMMEDIATE, d0
	bne.s checkDirectX
	moveq #1, d7
	bra.s haveShapeSurface

checkDirectX
	cmpi.b #TKPKG_MSEL_SURFACE_DIRECT_X, d0
	bne.s checkDirectY
	moveq #'x', d6
	bra.s haveShapeSurface

checkDirectY
	cmpi.b #TKPKG_MSEL_SURFACE_DIRECT_Y, d0
	bne.s checkIndirect
	moveq #'y', d6
	bra.s haveShapeSurface

checkIndirect
	cmpi.b #TKPKG_MSEL_SURFACE_INDIRECT, d0
	bne.s checkIndexedIndirectX
	moveq #1, d5
	bra.s haveShapeSurface

checkIndexedIndirectX
	cmpi.b #TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X, d0
	bne.s checkSurfaceIndexedY
	moveq #2, d5
	bra.s haveShapeSurface

checkSurfaceIndexedY
	cmpi.b #TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y, d0
	bne.s haveShapeSurface
	moveq #3, d5

haveShapeSurface
	tst.b d5
	bne.s haveOperandSurface
	bsr.w tkpkgMselCurrentModeParenCodeV1
	move.b d0, d5
	tst.b d6
	bne.s haveOperandSurface
	tst.b d5
	bne.s haveOperandSurface
	bsr.w tkpkgMselCurrentModeIndexSuffixV1
	move.b d0, d6

haveOperandSurface
	movea.l state.EncodeSelectedMselExprPtr, a0
	moveq #0, d0
	move.w state.EncodeSelectedMselExprLen, d0
	tst.b d7
	beq.s haveOperandText
	tst.l d0
	beq.s haveOperandText
	cmpi.b #'#', (a0)
	bne.s haveOperandText
	addq.l #1, a0
	subq.l #1, d0

haveOperandText
	tst.b d6
	beq.s evalParenOperandText
	bsr.w tkpkgMselStripIndexSuffixV1
	tst.b d1
	bne.s operandError

evalParenOperandText
	tst.b d5
	beq.s evalOperandText
	cmpi.b #2, d5
	beq.s stripIndexedIndirectX
	cmpi.b #3, d5
	beq.s stripIndirectIndexedY
	bsr.w tkpkgMselStripOuterParensV1
	bra.s evalOperandText

stripIndexedIndirectX
	bsr.w tkpkgMselStripOuterParensV1
	tst.b d1
	bne.s evalOperandText
	moveq #'x', d6
	bsr.w tkpkgMselStripIndexSuffixV1
	tst.b d1
	bne.s operandError
	moveq #1, d6
	bra.s evalOperandText

stripIndirectIndexedY
	moveq #'y', d6
	bsr.w tkpkgMselStripIndexSuffixV1
	tst.b d1
	bne.s evalOperandText
	bsr.w tkpkgMselStripOuterParensV1
	moveq #1, d6

evalOperandText
	bsr.w encodeSelectedOperandV1
	bne.s operandError
	move.l d3, state.EncodeSelectedMselValue
	clr.b state.EncodeSelectedMselUnstable
	tst.l d5
	beq.s ok
	move.b #1, state.EncodeSelectedMselUnstable

ok
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	rts

operandError
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	rts
	.bend  ; tkpkgMselEvalOperandV1

; Transitional native seam:
; - Package-owned shape and mode tags are collapsed into compact local surface
;   codes here before operand evaluation mutates source spans.
; - Keep this lookup table-driven; do not reintroduce per-shape compare ladders
;   elsewhere in tkpkg selector runtime code.
;
; Inputs:
; - A0: table of (`.long text`, `.word len`, `.byte code`, `.byte pad`) entries.
; - A1/D0: active text pointer and length.
; - D7: entry count minus one for DBF iteration.
;
; Outputs:
; - D0: matched surface code or `TKPKG_MSEL_SURFACE_NONE`.
;
; Clobbers:
; - D0-D5/D7/A0-A4/CCR
;
; CCR:
; - Reflects D0 on return.
tkpkgMselLookupTaggedTextCodeV1	.block
	movem.l d1-d5/a1-a4, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	move.w d0, d5
	move.w d7, d4

loop
	movea.l a4, a1
	move.w d5, d0
	movea.l (a3)+, a2
	moveq #0, d1
	move.w (a3)+, d1
	bsr.w tkpkgOperandStringEqAsciiCasefoldV1
	bne.s match
	addq.l #2, a3
	dbf d4, loop
	moveq #TKPKG_MSEL_SURFACE_NONE, d0
	bra.s return

match
	moveq #0, d0
	move.b (a3), d0

return
	movem.l (sp)+, d1-d5/a1-a4
	rts
	.bend  ; tkpkgMselLookupTaggedTextCodeV1

tkpkgMselCurrentShapeCodeV1	.block
	movea.l state.EncodeSelectedCurrentShapePtr, a1
	move.w state.EncodeSelectedCurrentShapeLen, d0
	lea CurrentShapeCodeTable(pc), a0
	moveq #6, d7
	bsr.w tkpkgMselLookupTaggedTextCodeV1
	rts

	.align 2
CurrentShapeCodeTable
	.long TkpkgMselShapeImmediateText
	.word 9
	.byte TKPKG_MSEL_SURFACE_IMMEDIATE, 0
	.long TkpkgMselShapeAccumulatorText
	.word 11
	.byte TKPKG_MSEL_SURFACE_ACCUMULATOR, 0
	.long TkpkgMselShapeDirectXText
	.word 8
	.byte TKPKG_MSEL_SURFACE_DIRECT_X, 0
	.long TkpkgMselShapeDirectYText
	.word 8
	.byte TKPKG_MSEL_SURFACE_DIRECT_Y, 0
	.long TkpkgMselShapeIndirectText
	.word 8
	.byte TKPKG_MSEL_SURFACE_INDIRECT, 0
	.long TkpkgMselShapeIndexedIndirectXText
	.word 18
	.byte TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X, 0
	.long TkpkgMselShapeIndirectIndexedYText
	.word 18
	.byte TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y, 0
	.bend  ; tkpkgMselCurrentShapeCodeV1

; Inputs:
; - Uses the current selected operand text stored in state.EncodeSelectedMselExprPtr/Len.
;
; Outputs:
; - D0: 1 when the trimmed operand text is exactly `a`, 0 otherwise.
;
; Clobbers:
; - D0-D2/A1/CCR
;
; CCR:
; - Reflects D0 on return.
tkpkgMselExprIsAccumulatorAV1	.block
	movem.l d1-d2/a1, -(sp)
	movea.l state.EncodeSelectedMselExprPtr, a1
	move.w state.EncodeSelectedMselExprLen, d0
trimLeading
	tst.w d0
	beq.s notAccumulator
	move.b (a1), d1
	cmpi.b #' ', d1
	beq.s skipLeading
	cmpi.b #9, d1
	beq.s skipLeading
	bra.s trimTrailing

skipLeading
	addq.l #1, a1
	subq.w #1, d0
	bra.s trimLeading

trimTrailing
	tst.w d0
	beq.s notAccumulator
	move.w d0, d1
	subq.w #1, d1
	move.b 0(a1, d1.w), d2
	cmpi.b #' ', d2
	beq.s skipTrailing
	cmpi.b #9, d2
	beq.s skipTrailing
	bra.s compareAccumulator

skipTrailing
	subq.w #1, d0
	bra.s trimTrailing

compareAccumulator
	cmpi.w #1, d0
	bne.s notAccumulator
	move.b (a1), d0
	ori.b #$20, d0
	cmpi.b #'a', d0
	bne.s notAccumulator
	moveq #1, d0
	bra.s return

notAccumulator
	moveq #0, d0

return
	movem.l (sp)+, d1-d2/a1
	rts
	.bend  ; tkpkgMselExprIsAccumulatorAV1

tkpkgMselCurrentModeParenCodeV1	.block
	movea.l state.EncodeSelectedMselModePtr, a1
	move.w state.EncodeSelectedMselModeLen, d0
	lea CurrentModeParenCodeTable(pc), a0
	moveq #2, d7
	bsr.w tkpkgMselLookupTaggedTextCodeV1
	rts

	.align 2
CurrentModeParenCodeTable
	.long TkpkgMselShapeIndirectText
	.word 8
	.byte TKPKG_MSEL_SURFACE_INDIRECT, 0
	.long TkpkgMselModeIndexedIndirectXText
	.word 16
	.byte TKPKG_MSEL_SURFACE_INDEXED_INDIRECT_X, 0
	.long TkpkgMselModeIndirectIndexedYText
	.word 16
	.byte TKPKG_MSEL_SURFACE_INDIRECT_INDEXED_Y, 0
	.bend  ; tkpkgMselCurrentModeParenCodeV1

tkpkgMselCurrentModeIndexSuffixV1	.block
	movea.l state.EncodeSelectedMselModePtr, a1
	move.w state.EncodeSelectedMselModeLen, d0
	cmpi.w #2, d0
	bcs.s none
	subq.w #1, d0
	move.b 0(a1, d0.w), d0
	ori.b #$20, d0
	cmpi.b #'x', d0
	beq.s return
	cmpi.b #'y', d0
	beq.s return

none
	moveq #0, d0

return
	rts
	.bend  ; tkpkgMselCurrentModeIndexSuffixV1

tkpkgMselStripOuterParensV1	.block
	movem.l d2-d3/a1, -(sp)
	moveq #1, d1
	move.l d0, d2

trimStartLoop
	tst.l d2
	beq.s return
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s trimStartOne
	cmpi.b #9, d3
	bne.s trimEndInit

trimStartOne
	addq.l #1, a0
	subq.l #1, d2
	bra.s trimStartLoop

trimEndInit
	cmpi.l #2, d2
	bcs.s return
	lea 0(a0, d2.l), a1
	subq.l #1, a1

trimEndLoop
	tst.l d2
	beq.s return
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s trimEndOne
	cmpi.b #9, d3
	bne.s haveEnd

trimEndOne
	subq.l #1, a1
	subq.l #1, d2
	bra.s trimEndLoop

haveEnd
	cmpi.l #2, d2
	bcs.s return
	cmpi.b #'(', (a0)
	bne.s return
	cmpi.b #')', (a1)
	bne.s return
	addq.l #1, a0
	subq.l #2, d2
	move.l d2, d0
	moveq #0, d1

return
	movem.l (sp)+, d2-d3/a1
	rts
	.bend  ; tkpkgMselStripOuterParensV1

tkpkgMselStripIndexSuffixV1	.block
	movem.l d2-d4/a1, -(sp)
	moveq #1, d1
	move.l d0, d2
	cmpi.l #2, d2
	bcs.s return
	lea 0(a0, d2.l), a1
	subq.l #1, a1

trimEndLoop
	tst.l d2
	beq.s return
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s trimEndOne
	cmpi.b #9, d3
	bne.s haveSuffixChar

trimEndOne
	subq.l #1, a1
	subq.l #1, d2
	bra.s trimEndLoop

haveSuffixChar
	move.b d3, d4
	cmpi.b #'A', d4
	bcs.s suffixFolded
	cmpi.b #'Z', d4
	bhi.s suffixFolded
	addi.b #32, d4

suffixFolded
	cmp.b d6, d4
	bne.s return
	cmpi.l #2, d2
	bcs.s return
	subq.l #1, a1
	subq.l #1, d2
	cmpi.b #',', (a1)
	bne.s return
	subq.l #1, d2
	beq.s return
	move.l d2, d0
	lea 0(a0, d0.l), a1
	subq.l #1, a1

trimBeforeCommaLoop
	tst.l d0
	beq.s return
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s trimBeforeCommaOne
	cmpi.b #9, d3
	bne.s ok

trimBeforeCommaOne
	subq.l #1, a1
	subq.l #1, d0
	bra.s trimBeforeCommaLoop

ok
	moveq #0, d1

return
	movem.l (sp)+, d2-d4/a1
	rts
	.bend  ; tkpkgMselStripIndexSuffixV1

tkpkgMselWriteCandidateEnvelopeV1	.block
	lea buffers.TokenScratchBuffer, a4
	move.w state.EncodeSelectedMselMnemonicLen, d0
	cmpi.w #255, d0
	bhi.w operandError
	move.b d0, (a4)+
	movea.l a5, a0
	bsr.w tkpkgMselCopyBytesV1
	move.b #1, (a4)+
	move.w state.EncodeSelectedMselModeLen, d0
	cmpi.w #255, d0
	bhi.w operandError
	move.b d0, (a4)+
	movea.l state.EncodeSelectedMselModePtr, a0
	bsr.w tkpkgMselCopyBytesV1
	tst.w d6
	beq.s writeNoOperands
	move.b #1, (a4)+
	move.b d6, (a4)+
	move.l state.EncodeSelectedMselValue, d3
	move.b d3, (a4)+
	cmpi.w #2, d6
	bne.s done
	lsr.l #8, d3
	move.b d3, (a4)+
	bra.s done

writeNoOperands
	move.b #0, (a4)+

done
	move.l a4, d1
	lea buffers.TokenScratchBuffer, a0
	sub.l a0, d1
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	rts

operandError
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	rts
	.bend  ; tkpkgMselWriteCandidateEnvelopeV1

tkpkgMselCopyBytesV1	.block
	tst.w d0
	beq.s done
	subq.w #1, d0

loop
	move.b (a0)+, (a4)+
	dbf d0, loop

done
	rts
	.bend  ; tkpkgMselCopyBytesV1

encodeSelectedOperandV1	.block
	movem.l d1-d2/d6-d7/a1-a2/a5-a6, -(sp)
	clr.w state.EncodeSelectedOperandStatus
	move.l d0, d5
	movea.l a0, a5
	movea.l state.EncodeSelectedSymbolResolverPtr, a3
	move.l a3, d0
	beq.s loadSymbolTables
	moveq #0, d0
	move.b (a5), d0
	cmpi.b #'A', d0
	blo.s selectedResolverPunctuation
	cmpi.b #'Z', d0
	bls.s selectedResolverCall
	cmpi.b #'a', d0
	blo.s selectedResolverPunctuation
	cmpi.b #'z', d0
	bls.s selectedResolverCall
selectedResolverPunctuation
	cmpi.b #'_', d0
	beq.s selectedResolverCall
	cmpi.b #'.', d0
	bne.s loadSymbolTables
selectedResolverCall
	movea.l a5, a0
	move.l d5, d0
	movem.l d1-d2/d4-d7/a1-a2/a5-a6, -(sp)
	jsr (a3)
	move.l d0, -(sp)
	move.l d3, -(sp)
	movem.l 8(sp), d1-d2/d4-d7/a1-a2/a5-a6
	move.l (sp)+, d3
	move.l (sp)+, d0
	adda.l #40, sp
	tst.l d0
	bne.s selectedResolverMiss
	moveq #0, d5
	moveq #0, d0
	bra.w return

selectedResolverMiss
	movea.l a5, a0

loadSymbolTables
	movea.l state.EncodeSelectedLabelNamePtr, a1
	movea.l state.EncodeSelectedLabelValuePtr, a2
	move.l state.EncodeSelectedLabelCount, d1
	bne.s haveSymbolTables
	jsr context.getSymbolTableSnapshotV1
	tst.b d0
	bne.w contextUnavailable
	movea.l a1, a2
	movea.l a0, a1

haveSymbolTables
	move.l d1, d0
	jsr context.getSymbolStabilityTableV1
	tst.b d0
	bne.w contextUnavailable
	movea.l a0, a6
	move.l d5, d0
	movea.l a5, a0
	move.l state.EncodeSelectedCurrentPc, d2
	moveq #0, d4
	move.w state.EncodeSelectedExvmOpcodeVersion, d4
	cmpi.w #1, d4
	beq.s haveExvm
	move.w #1, state.EncodeSelectedOperandStatus
	moveq #1, d0
	bra.w return

haveExvm
	tst.l d0
	bne.s haveText
	move.w #2, state.EncodeSelectedOperandStatus
	moveq #1, d0
	bra.w return

haveText
	moveq #0, d7
	move.b (a0), d7
	cmpi.b #'$', d7
	beq.w textOk

checkPercent
	cmpi.b #'%', d7
	beq.w textOk
	cmpi.b #'*', d7
	beq.w textOk
	cmpi.b #'(', d7
	beq.w textOk
	cmpi.b #'+', d7
	beq.w textOk
	cmpi.b #'-', d7
	beq.w textOk
	cmpi.b #'~', d7
	beq.w textOk
	cmpi.b #'!', d7
	beq.w textOk
	cmpi.b #39, d7
	beq.w textOk
	cmpi.b #'"', d7
	beq.w textOk
	cmpi.b #'<', d7
	beq.w textOk
	cmpi.b #'>', d7
	beq.w textOk
	cmpi.b #'0', d7
	bcs.w maybeLetter
	cmpi.b #'9', d7
	bls.w textOk

maybeLetter
	cmpi.b #'A', d7
	bcs.w unexpectedText
	cmpi.b #'Z', d7
	bls.w textOk
	cmpi.b #'a', d7
	bcs.w unexpectedText
	cmpi.b #'z', d7
	bls.w textOk

unexpectedText
	move.w #3, state.EncodeSelectedOperandStatus
	moveq #1, d0
	bra.w return

contextUnavailable
	move.w #4, state.EncodeSelectedOperandStatus
	moveq #1, d0
	bra.w return

textOk
	bsr.w encodeSelectedOperandTryLabelV1
	tst.l d7
	bne.w return
	bsr.w encodeSelectedOperandTryLastComponentV1
	tst.l d7
	bne.w return
	moveq #0, d5
	moveq #1, d5
	moveq #0, d6
	move.w state.EncodeSelectedSessionPass.l, d6
	jsr expr_bridge.opcoreExvmEvalOperandV1
	beq.w return
	cmpi.b #3, d0
	beq.s compileFail
	cmpi.b #4, d0
	beq.s finalizeFail
	cmpi.b #5, d0
	beq.s evalFail
	cmpi.b #31, d0
	beq.s hexParseFail
	cmpi.b #32, d0
	beq.s literalEmitFail
	cmpi.b #33, d0
	beq.s trailingFail
	cmpi.b #34, d0
	beq.s singleFail
	cmpi.b #51, d0
	bhs.s exprVmFail
	move.w #4, state.EncodeSelectedOperandStatus
	bra.w return

compileFail
	move.w #6, state.EncodeSelectedOperandStatus
	bra.w return

finalizeFail
	move.w #7, state.EncodeSelectedOperandStatus
	bra.w return

evalFail
	move.w #8, state.EncodeSelectedOperandStatus
	bra.w return

hexParseFail
	move.w #31, state.EncodeSelectedOperandStatus
	bra.w return

literalEmitFail
	move.w #32, state.EncodeSelectedOperandStatus
	bra.w return

trailingFail
	move.w #33, state.EncodeSelectedOperandStatus
	bra.w return

singleFail
	move.w #34, state.EncodeSelectedOperandStatus
	bra.w return

exprVmFail
	move.w d0, state.EncodeSelectedOperandStatus

return
	movem.l (sp)+, d1-d2/d6-d7/a1-a2/a5-a6
	tst.l d0
	rts
	.bend  ; encodeSelectedOperandV1

encodeSelectedOperandTryLabelV1	.block
	movem.l d0-d2/d4/d6/a0-a2/a6, -(sp)
	moveq #0, d7
	tst.l d1
	beq.s return
	moveq #0, d6

loop
	cmp.l d1, d6
	bhs.s return
	move.l d6, d2
	lsl.l #6, d2
	movea.l a1, a6
	adda.l d2, a6
	bsr.s encodeSelectedOperandLabelEqualsV1
	tst.l d7
	bne.s found
	addq.l #1, d6
	bra.s loop

found
	move.l d6, d2
	lsl.l #2, d2
	move.l 0(a2, d2.l), d3
	moveq #0, d5
	moveq #0, d0

return
	movem.l (sp)+, d0-d2/d4/d6/a0-a2/a6
	tst.l d7
	beq.s done
	moveq #0, d0

done
	rts
	.bend  ; encodeSelectedOperandTryLabelV1

; Try the final component of a dotted architecture-neutral identifier against
; the already-authorized selected snapshot. Import materialization owns
; visibility; this routine only makes the direct-label fast path consume the
; same retained fallback that expression preparation already accepted.
; Inputs: A0/D0 = operand token; A1/A2/D1 = snapshot names/values/count.
; Outputs: D7 = 1 and D3 = value on match, otherwise D7 = 0.
encodeSelectedOperandTryLastComponentV1	.block
	movem.l d0-d2/d4/a0/a6, -(sp)
	movea.l a0, a6
	move.l d0, d2
	moveq #0, d4
lastComponentScan
	tst.l d2
	beq.s lastComponentReady
	cmpi.b #'.', (a6)+
	bne.s lastComponentNext
	movea.l a6, a0
	move.l d2, d4
	subq.l #1, d4
lastComponentNext
	subq.l #1, d2
	bra.s lastComponentScan
lastComponentReady
	tst.l d4
	beq.s noLastComponent
	move.l d4, d0
	bsr.w encodeSelectedOperandTryLabelV1
	bra.s lastComponentReturn
noLastComponent
	moveq #0, d7
lastComponentReturn
	movem.l (sp)+, d0-d2/d4/a0/a6
	tst.l d7
	rts
	.bend  ; encodeSelectedOperandTryLastComponentV1

encodeSelectedOperandLabelEqualsV1	.block
	movem.l d0-d2/a0-a1/a6, -(sp)
	move.l d0, d2
	beq.s no

loop
	move.b (a0)+, d1
	move.b (a6)+, d0
	cmp.b d0, d1
	bne.s no
	subq.l #1, d2
	bne.s loop
	tst.b (a6)
	bne.s no
	moveq #1, d7
	bra.s return

no
	moveq #0, d7

return
	movem.l (sp)+, d0-d2/a0-a1/a6
	rts
	.bend  ; encodeSelectedOperandLabelEqualsV1

; Compare two ASCII strings case-insensitively.
; Inputs: A1 = first string bytes; A2 = second string bytes; D0/D1 = lengths.
; Outputs: D0 = 1 when strings match, 0 otherwise.
tkpkgOperandStringEqAsciiCasefoldV1	.block
	cmp.w d1, d0
	bne.s noMatch
	move.w d0, d4
	beq.s match
	subq.w #1, d4

loop
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d3
	move.b (a2)+, d3
	move.b d2, d0
	bsr.w tkpkgOperandFoldAsciiLowerV1
	move.b d0, d2
	move.b d3, d0
	bsr.w tkpkgOperandFoldAsciiLowerV1
	cmp.b d0, d2
	bne.s noMatch
	dbf d4, loop

match
	moveq #1, d0
	rts

noMatch
	moveq #0, d0
	rts
	.bend  ; tkpkgOperandStringEqAsciiCasefoldV1

tkpkgOperandFoldAsciiLowerV1	.block
	cmpi.b #'A', d0
	blo.s done
	cmpi.b #'Z', d0
	bhi.s done
	ori.b #$20, d0

done
	rts
	.bend  ; tkpkgOperandFoldAsciiLowerV1

; Locate one top-level operand in the selected source span.  Operand numbering
; and nesting are syntax-neutral; package input projections decide how the
; resulting span is interpreted.
; Inputs: D0.W = zero-based operand index.
; Outputs: A0/D0 = trimmed operand span, D1 = 0 success or 1 no such operand.
tkpkgMselLocateSemanticOperandV2	.block
	movem.l d2-d7/a1-a3, -(sp)
	move.w d0, d7
	movea.l state.EncodeSelectedMselExprPtr, a1
	moveq #0, d6
	move.w state.EncodeSelectedMselExprLen, d6
	movea.l a1, a2
	moveq #0, d5
	moveq #0, d4

scan
	tst.l d6
	beq.s atEnd
	move.b (a1), d3
	cmpi.b #'(', d3
	beq.s open
	cmpi.b #')', d3
	beq.s close
	cmpi.b #',', d3
	bne.s next
	tst.w d4
	bne.s next
	cmp.w d7, d5
	beq.s found
	addq.w #1, d5
	addq.l #1, a1
	subq.l #1, d6
	movea.l a1, a2
	bra.s scan

open
	addq.w #1, d4
	bra.s next

close
	tst.w d4
	beq.s fail
	subq.w #1, d4

next
	addq.l #1, a1
	subq.l #1, d6
	bra.s scan

atEnd
	tst.w d4
	bne.s fail
	cmp.w d7, d5
	bne.s fail

found
	movea.l a1, a3

trimStart
	cmpa.l a3, a2
	bhs.s fail
	move.b (a2), d3
	cmpi.b #' ', d3
	beq.s trimStartOne
	cmpi.b #9, d3
	bne.s trimEnd
trimStartOne
	addq.l #1, a2
	bra.s trimStart

trimEnd
	movea.l a3, a1
trimEndLoop
	cmpa.l a2, a1
	bls.s fail
	subq.l #1, a1
	move.b (a1), d3
	cmpi.b #' ', d3
	beq.s trimEndLoop
	cmpi.b #9, d3
	beq.s trimEndLoop
	addq.l #1, a1
	move.l a1, d0
	sub.l a2, d0
	movea.l a2, a0
	moveq #0, d1
	bra.s return

fail
	moveq #0, d0
	movea.l d0, a0
	moveq #1, d1

return
	movem.l (sp)+, d2-d7/a1-a3
	tst.l d1
	rts
	.bend  ; tkpkgMselLocateSemanticOperandV2

; Locate the base of one neutral member expression and require its final field.
; This mirrors Rust Expr::Member projection without assigning meaning to the
; field spelling.  A package plan supplies that spelling (for example `.L`).
; Inputs: D0.W = operand index; A1/D1.W = expected field (without the dot).
; Outputs: A0/D0 = trimmed member-base span, D1 = 0 success or 1 no match.
tkpkgMselLocateMemberBaseV2	.block
	movem.l d2-d7/a1-a3, -(sp)
	movea.l a1, a3
	moveq #0, d7
	move.w d1, d7
	bsr.w tkpkgMselLocateSemanticOperandV2
	bne.w memberFail
	movea.l a0, a2
	move.l d0, d6
	tst.w d7
	beq.w memberFail
	cmp.l d7, d6
	bls.w memberFail
	lea 0(a0, d6.l), a1

memberFindDot
	tst.l d6
	beq.w memberFail
	subq.l #1, a1
	subq.l #1, d6
	cmpi.b #'.', (a1)
	bne.s memberFindDot
	move.l d0, d5
	sub.l d6, d5
	subq.l #1, d5
	cmp.l d7, d5
	bne.s memberFindDot
	movea.l a1, a2
	addq.l #1, a2
	movea.l a2, a1
	move.l d7, d0
	movea.l a3, a2
	move.l d7, d1
	bsr.w tkpkgOperandStringEqAsciiCasefoldV1
	tst.b d0
	beq.w memberFail
	movea.l a0, a1
	adda.l d6, a1

memberTrimBaseEnd
	tst.l d6
	beq.w memberFail
	subq.l #1, a1
	move.b (a1), d2
	cmpi.b #' ', d2
	beq.s memberTrimOne
	cmpi.b #9, d2
	bne.s memberReady
memberTrimOne
	subq.l #1, d6
	bra.s memberTrimBaseEnd

memberReady
	move.l d6, d0
	moveq #0, d1
	bra.s memberReturn
memberFail
	moveq #0, d0
	suba.l a0, a0
	moveq #1, d1
memberReturn
	movem.l (sp)+, d2-d7/a1-a3
	tst.l d1
	rts
	.bend  ; tkpkgMselLocateMemberBaseV2

; Locate one neutral tuple item inside an indirect operand.  The source form
; `prefix(item,...)` projects the nonempty prefix as item zero, matching Rust's
; `Expr::Indirect(Expr::Tuple(...))` representation.  Nested parentheses are
; skipped while finding tuple separators.
; Inputs: D0.W = operand index; D1.W = tuple item index.
; Outputs: A0/D0 = trimmed item span; D1 = 0 success or 1 no match;
;          D2.W = tuple arity.
tkpkgMselLocateIndirectTupleItemV2	.block
	movem.l d3-d7/a1-a3, -(sp)
	move.w d1, d7
	bsr.w tkpkgMselLocateSemanticOperandV2
	bne.w tupleFail
	movea.l a0, a1
	movea.l a0, a2
	move.l d0, d5

tupleFindOpen
	tst.l d5
	beq.w tupleFail
	cmpi.b #'(', (a2)
	beq.s tupleHaveOpen
	addq.l #1, a2
	subq.l #1, d5
	bra.s tupleFindOpen

tupleHaveOpen
	lea 0(a0, d0.l), a0
	subq.l #1, a0
	cmpi.b #')', (a0)
	bne.w tupleFail
	moveq #0, d2
	moveq #0, d4
	moveq #0, d5
	movea.l d5, a3
	move.l a2, -(sp)
	bsr.w tupleRecordSpan
	movea.l (sp)+, a2
	addq.l #1, a2
	movea.l a2, a1
	moveq #0, d6

tupleScanInner
	cmpa.l a0, a2
	bhs.s tupleInnerEnd
	moveq #0, d5
	move.b (a2), d5
	cmpi.b #'(', d5
	beq.s tupleNestedOpen
	cmpi.b #')', d5
	beq.s tupleNestedClose
	cmpi.b #',', d5
	bne.s tupleInnerNext
	tst.w d6
	bne.s tupleInnerNext
	move.l a2, -(sp)
	bsr.w tupleRecordSpan
	movea.l (sp)+, a2
	tst.l d1
	bne.s tupleFail
	addq.l #1, a2
	movea.l a2, a1
	bra.s tupleScanInner

tupleNestedOpen
	addq.w #1, d6
	bra.s tupleInnerNext

tupleNestedClose
	tst.w d6
	beq.s tupleFail
	subq.w #1, d6

tupleInnerNext
	addq.l #1, a2
	bra.s tupleScanInner

tupleInnerEnd
	tst.w d6
	bne.s tupleFail
	movea.l a0, a2
	bsr.w tupleRecordSpan
	tst.l d1
	bne.s tupleFail
	move.l a3, d5
	tst.l d5
	beq.s tupleFail
	movea.l a3, a0
	move.l d4, d0
	moveq #0, d1
	bra.s tupleReturn

; A1/A2 are the candidate half-open span.  Empty spans are reported to the
; caller; a nonempty prefix before `(` is optional and is ignored by that
; caller, while every inner item must be nonempty.
tupleRecordSpan
	cmpa.l a2, a1
	bhs.s tupleSpanEmpty
tupleTrimStart
	cmpa.l a2, a1
	bhs.s tupleSpanEmpty
	moveq #0, d5
	move.b (a1), d5
	cmpi.b #' ', d5
	beq.s tupleTrimStartOne
	cmpi.b #9, d5
	bne.s tupleTrimEnd
tupleTrimStartOne
	addq.l #1, a1
	bra.s tupleTrimStart
tupleTrimEnd
	cmpa.l a1, a2
	bls.s tupleSpanEmpty
	moveq #0, d5
	move.b -1(a2), d5
	cmpi.b #' ', d5
	beq.s tupleTrimEndOne
	cmpi.b #9, d5
	bne.s tupleSpanReady
tupleTrimEndOne
	subq.l #1, a2
	bra.s tupleTrimEnd
tupleSpanReady
	cmp.w d7, d2
	bne.s tupleSpanCount
	movea.l a1, a3
	move.l a2, d4
	sub.l a1, d4
tupleSpanCount
	addq.w #1, d2
	moveq #0, d1
	rts
tupleSpanEmpty
	moveq #1, d1
	rts

tupleFail
	moveq #0, d0
	movea.l d0, a0
	moveq #0, d2
	moveq #1, d1
tupleReturn
	movem.l (sp)+, d3-d7/a1-a3
	tst.l d1
	rts
	.bend  ; tkpkgMselLocateIndirectTupleItemV2

; Evaluate a located semantic scalar through the same opcore bridge used by
; raw package plans.  The projection, not the runtime, decides whether an
; immediate marker is required and removed.
; Inputs: A0/D0 = trimmed operand span; D1.B = nonzero to require leading '#'.
; Outputs: D0 = selected status; D3.L = evaluated scalar on success.
tkpkgMselEvaluateSemanticSpanV2	.block
	tst.b d1
	beq.s evaluate
	tst.l d0
	beq.s fail
	cmpi.b #'#', (a0)
	bne.s fail
	addq.l #1, a0
	subq.l #1, d0
	beq.s fail
evaluate
	bsr.w encodeSelectedOperandV1
	tst.l d0
	beq.s ok
fail
	moveq #TKPKG_SELECTED_STATUS_OPERAND_ERROR, d0
	rts
ok
	moveq #TKPKG_SELECTED_STATUS_OK, d0
	rts
	.bend  ; tkpkgMselEvaluateSemanticSpanV2

	.endsection
	.endmodule
