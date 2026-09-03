; CPU-neutral interpreter for package-owned STVM runtime state.
; @opforge-owner: tkpkg.amigaos.state_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68k-runtime-directive-state-v1.toml

	.module tkpkg.amigaos.state_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.buffers
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	.use debug.amigaos.runtime_profile as runtime_profile
.endif

SCOPED_OWNER_FAMILY = 0
SCOPED_OWNER_CPU = 1
SCOPED_OWNER_DIALECT = 2
STATE_VM_OPCODE_VERSION_V1 = 1
STATE_VM_END = $ff

	.section data, kind=data

StateMalformedText
	.byte "OTR901: malformed package state program", 0
StateOperandCountText
	.byte "state directive requires exactly one argument", 0
StateArgumentText
	.byte "invalid package state directive argument", 0
StateCombinationText
	.byte "package state directive argument is not legal for active profile", 0
StateTargetText
	.byte " target "
StateTargetTextEnd
StateUnsupportedText
	.byte " is not supported on "
StateUnsupportedTextEnd
StateLegalPrefixText
	.byte "; legal ."
StateLegalPrefixTextEnd
StateTargetsForText
	.byte " targets for "
StateTargetsForTextEnd
StateListPrefixText
	.byte ": "
StateListPrefixTextEnd
StateListSeparatorText
	.byte ", "
STATE_LIST_SEPARATOR_TEXT_END

STATE_TARGET_TEXT_LEN = StateTargetTextEnd - StateTargetText
STATE_UNSUPPORTED_TEXT_LEN = StateUnsupportedTextEnd - StateUnsupportedText
STATE_LEGAL_PREFIX_TEXT_LEN = StateLegalPrefixTextEnd - StateLegalPrefixText
STATE_TARGETS_FOR_TEXT_LEN = StateTargetsForTextEnd - StateTargetsForText
STATE_LIST_PREFIX_TEXT_LEN = StateListPrefixTextEnd - StateListPrefixText
STATE_LIST_SEPARATOR_TEXT_LEN = STATE_LIST_SEPARATOR_TEXT_END - StateListSeparatorText

	.endsection

	.section code, kind=code
	.pub

; Select the unique highest-precedence STVM program for the active pipeline and
; materialize its profile defaults. The serialized program is authoritative.
; Outputs: D0 = 0 success/no STVM, 1 malformed; A1/D1 diagnostic on failure.
initializeActiveV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	clr.l buffers.ActiveStateProgramPtr
	clr.l buffers.ActiveStateProgramEndPtr
	clr.b buffers.ActiveStateKeyCount
	clr.l ActiveStateDirectiveTablePtr
	btst #5, buffers.PackageChunkFlagsExtra
	beq.w initializeOk
	lea buffers.StvmChunkOffsetLo, a3
	bsr.w chunkPtrFromLocatorV1
	bne.w initializeFail
	bsr.w readU32LeLow16V1
	bne.w initializeFail
	tst.w d0
	beq.w initializeFail
	move.w d0, d7
	subq.w #1, d7
	moveq #0, d6  ; selected owner rank

programLoop
	moveq #1, d0
	bsr.w requireBytesV1
	bne.w initializeFail
	moveq #0, d5
	move.b (a2)+, d5  ; owner tag
	bsr.w locateStringV1
	bne.w initializeFail
	movea.l a1, a4
	move.w d0, d4
	movea.l a2, a3
	move.w d5, d0
	bsr.w activeOwnerMatchesV1
	movea.l a3, a2
	move.w d0, d3  ; owner rank, zero means no match
	bsr.w locateStringV1  ; program id remains package-owned/opaque
	bne.w initializeFail
	bsr.w readU16LeV1
	bne.w initializeFail
	cmpi.w #STATE_VM_OPCODE_VERSION_V1, d0
	bne.w initializeFail
	bsr.w readU32LeLow16V1
	bne.w initializeFail
	move.w d0, d2
	moveq #0, d0
	move.w d2, d0
	bsr.w requireBytesV1
	bne.w initializeFail
	movea.l a2, a5
	adda.l d0, a5
	tst.w d3
	beq.s programNext
	cmp.w d6, d3
	bcs.s programNext
	bne.s programSelect
	; Rust's unique-state-program boundary rejects ambiguity in one owner scope.
	bra.w initializeFail

programSelect
	move.w d3, d6
	move.l a2, buffers.ActiveStateProgramPtr
	move.l a5, buffers.ActiveStateProgramEndPtr

programNext
	movea.l a5, a2
	dbf d7, programLoop
	cmpa.l a6, a2
	bne.w initializeFail
	tst.w d6
	beq.s initializeOk
	bsr.w resetActiveV1
	bne.s initializeFail

initializeOk
	moveq #0, d0
	bra.s initializeReturn

initializeFail
	lea StateMalformedText.l, a1
	moveq #39, d1
	moveq #1, d0

initializeReturn
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; initializeActiveV1

; Restore the active STVM profile defaults. Called at every assembly pass and
; after every successful pipeline switch.
; Outputs: D0 = 0 success/no STVM, 1 malformed.
resetActiveV1	.block
	movem.l d2-d7/a0-a6, -(sp)
	clr.l ActiveStateProfilePtr
	clr.w ActiveStateProfileLen
	movea.l buffers.ActiveStateProgramPtr, a2
	movea.l buffers.ActiveStateProgramEndPtr, a6
	move.l a2, d0
	beq.w resetOk
	moveq #1, d0
	bsr.w requireBytesV1
	bne.w resetFail
	moveq #0, d7
	move.b (a2)+, d7
	beq.w resetFail
	moveq #0, d6
	moveq #-1, d5

profileLoop
	bsr.w locateVmStringV1
	bne.w resetFail
	movea.l a1, a3
	move.w d0, d4
	movem.l a2, -(sp)
	lea buffers.ActiveCpuBuffer, a2
	movea.l a3, a0
	move.w d4, d0
	bsr.w stringEqNulCasefoldV1
	movem.l (sp)+, a2
	tst.b d0
	beq.s profileNext
	move.w d6, d5
	move.l a3, ActiveStateProfilePtr
	move.w d4, ActiveStateProfileLen
profileNext
	addq.w #1, d6
	cmp.w d7, d6
	blo.s profileLoop
	tst.w d5
	bmi.w resetFail
	move.b d5, buffers.ActiveStateProfileIndex

	moveq #1, d0
	bsr.w requireBytesV1
	bne.w resetFail
	moveq #0, d7
	move.b (a2)+, d7
	beq.w resetFail
	cmpi.w #buffers.STATE_VM_KEY_CAPACITY, d7
	bhi.w resetFail
	move.b d7, buffers.ActiveStateKeyCount
	moveq #0, d6

keyLoop
	bsr.w locateVmStringV1
	bne.w resetFail
	move.l d6, d4
	lsl.l #2, d4
	lea buffers.ActiveStateKeyPtrs, a0
	move.l a1, 0(a0, d4.l)
	lea buffers.ActiveStateKeyLens, a0
	move.b d0, 0(a0, d6.l)
	bsr.w readVarU32V1
	bne.w resetFail
	move.l d6, d4
	lsl.l #2, d4
	lea buffers.ActiveStateValues, a0
	move.l d0, 0(a0, d4.l)
	moveq #1, d0
	bsr.w requireBytesV1
	bne.w resetFail
	moveq #0, d5
	move.b (a2)+, d5
	beq.s keyReady
	subq.w #1, d5

overrideLoop
	moveq #1, d0
	bsr.w requireBytesV1
	bne.w resetFail
	moveq #0, d2
	move.b (a2)+, d2
	movem.l d2, -(sp)
	bsr.w readVarU32V1
	movem.l (sp)+, d2
	bne.w resetFail
	cmp.b buffers.ActiveStateProfileIndex, d2
	bne.s overrideNext
	move.l d6, d4
	lsl.l #2, d4
	lea buffers.ActiveStateValues, a0
	move.l d0, 0(a0, d4.l)
overrideNext
	dbf d5, overrideLoop

keyReady
	addq.w #1, d6
	cmp.w d7, d6
	blo.w keyLoop
	; Preserve the directive-table cursor for fast transition scans.
	move.l a2, ActiveStateDirectiveTablePtr

resetOk
	moveq #0, d0
	bra.s resetReturn
resetFail
	clr.b buffers.ActiveStateKeyCount
	clr.l ActiveStateDirectiveTablePtr
	clr.l ActiveStateProfilePtr
	clr.w ActiveStateProfileLen
	moveq #1, d0
resetReturn
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; resetActiveV1

; Apply one package-owned directive transactionally.
; Inputs: A0/D0 = directive text; A1/D1 = one raw argument token.
; Outputs: D0 = 0 not handled, 1 applied, 2 rejected/malformed; A1/D1 message on 2.
applyDirectiveV1	.block
	movem.l d2-d7/a0/a2-a6, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	movem.l (sp)+, d0-d1
.endif
	lea -32(sp), sp
	move.l a0, (sp)
	move.w d0, 4(sp)
	move.l a1, 6(sp)
	move.w d1, 10(sp)
	movea.l ActiveStateDirectiveTablePtr, a2
	movea.l buffers.ActiveStateProgramEndPtr, a6
	move.l a2, d0
	beq.w directiveNotHandled
	moveq #1, d0
	bsr.w requireBytesV1
	bne.w directiveMalformed
	moveq #0, d7
	move.b (a2)+, d7
	beq.w directiveNotHandled
	subq.w #1, d7

directiveLoop
	bsr.w locateVmStringV1
	bne.w directiveMalformed
	movea.l a1, a4
	move.w d0, d4
	move.l a4, 12(sp)
	move.w d4, 16(sp)
	movea.l a2, a3
	movea.l (sp), a0
	movea.l a4, a1
	move.w 4(sp), d0
	move.w d4, d2
	bsr.w stringEqSizedCasefoldV1
	movea.l a3, a2
	move.b d0, d6
	moveq #2, d0
	bsr.w requireBytesV1
	bne.w directiveMalformed
	moveq #0, d5
	move.b (a2)+, d5  ; key index
	moveq #0, d4
	move.b (a2)+, d4  ; argument count
	cmp.b buffers.ActiveStateKeyCount, d5
	bhs.w directiveMalformed
	tst.b d4
	beq.w directiveMalformed
	tst.b d6
	bne.s matchedDirective
	move.w d4, d6

skipArguments
	bsr.w skipStateArgumentV1
	bne.w directiveMalformed
	subq.b #1, d6
	bne.s skipArguments
	dbf d7, directiveLoop
	bra.w directiveNotHandled

matchedDirective
	move.l a2, 18(sp)
	move.w d4, 22(sp)
	bsr.w normalizeArgumentV1
	bne.w directiveOperandCount
	move.w d4, d7

argumentLoop
	bsr.w locateVmStringV1
	bne.w directiveMalformed
	movea.l a1, a4
	move.w d0, d6
	move.l a4, 24(sp)
	move.w d6, 28(sp)
	bsr.w readVarU32V1
	bne.w directiveMalformed
	move.l d0, -(sp)  ; proposed value
	moveq #1, d0
	bsr.w requireBytesV1
	bne.w directiveMalformedPop
	moveq #0, d0
	move.b (a2)+, d0
	beq.w directiveMalformedPop
	move.w d0, d3
	move.l a2, d0
	add.l d3, d0
	cmp.l a6, d0
	bhi.w directiveMalformedPop
	move.w d3, -(sp)  ; profile-mask length
	movea.l a2, a3
	movea.l 12(sp), a0
	movea.l a4, a1
	move.w 16(sp), d0
	move.w d6, d2
	bsr.w stringEqSizedCasefoldV1
	movea.l a3, a2
	move.w (sp)+, d6
	tst.b d0
	beq.s argumentNext
	moveq #0, d0
	move.b buffers.ActiveStateProfileIndex, d0
	move.w d0, d1
	lsr.w #3, d0
	cmp.w d6, d0
	bhs.w directiveIllegalPop
	moveq #0, d2
	move.b 0(a2, d0.w), d2
	andi.w #7, d1
	btst d1, d2
	beq.w directiveIllegalPop
	move.l (sp)+, d0
	move.l d5, d2
	lsl.l #2, d2
	lea buffers.ActiveStateValues, a0
	move.l d0, 0(a0, d2.l)
	moveq #1, d0
	bra.w directiveReturn

argumentNext
	adda.w d6, a2
	addq.l #4, sp
	subq.b #1, d7
	bne.w argumentLoop
	bra.w directiveInvalidArgument

directiveMalformedPop
	addq.l #4, sp
	bra.s directiveMalformed
directiveIllegalPop
	addq.l #4, sp
directiveIllegal
	movea.l 12(sp), a0
	move.w 16(sp), d0
	movea.l 24(sp), a1
	move.w 28(sp), d1
	movea.l 18(sp), a2
	move.w 22(sp), d2
	bsr.w renderIllegalCombinationV1
	tst.b d0
	beq.s directiveIllegalRendered
	lea StateCombinationText.l, a1
	moveq #64, d1
directiveIllegalRendered
	moveq #2, d0
	bra.s directiveReturn
directiveInvalidArgument
	lea StateArgumentText.l, a1
	moveq #40, d1
	moveq #2, d0
	bra.s directiveReturn
directiveOperandCount
	lea StateOperandCountText.l, a1
	moveq #45, d1
	moveq #2, d0
	bra.s directiveReturn
directiveMalformed
	lea StateMalformedText.l, a1
	moveq #39, d1
	moveq #2, d0
	bra.s directiveReturn
directiveNotHandled
	moveq #0, d0
directiveReturn
	lea 32(sp), sp
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	jsr runtime_profile.opforgeRuntimeProfileLeaveServiceV1
.endif
	movem.l (sp)+, d2-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; applyDirectiveV1

; Render the package-derived enum legality diagnostic used by Rust.  The
; serialized directive, argument, profile, and legal-value masks remain the
; sole source of target-specific names and combinations.
; Inputs: A0/D0 directive id; A1/D1 matched argument id; A2/D2 argument table;
;         A6 end of active STVM program.
; Outputs: D0 = 0 rendered with A1/D1 message, 1 use generic fallback.
renderIllegalCombinationV1	.block
	movem.l d2-d7/a0-a5, -(sp)
	lea -20(sp), sp
	move.l a0, (sp)
	move.w d0, 4(sp)
	move.l a1, 6(sp)
	move.w d1, 10(sp)
	move.l a2, 12(sp)
	move.w d2, 16(sp)
	clr.w 18(sp)
	move.l ActiveStateProfilePtr, d0
	beq.w illegalRenderFallback
	tst.w ActiveStateProfileLen
	beq.w illegalRenderFallback
	lea buffers.LastErrorBuffer, a5
	moveq #0, d5
	move.l #buffers.LAST_ERROR_BUFFER_CAPACITY, d4
	movea.l (sp), a0
	move.w 4(sp), d0
	bsr.w appendStateMessageUpperSliceV1
	bne.w illegalRenderFallback
	lea StateTargetText.l, a0
	moveq #STATE_TARGET_TEXT_LEN, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	movea.l 6(sp), a0
	move.w 10(sp), d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	lea StateUnsupportedText.l, a0
	moveq #STATE_UNSUPPORTED_TEXT_LEN, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	movea.l ActiveStateProfilePtr, a0
	move.w ActiveStateProfileLen, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	lea StateLegalPrefixText.l, a0
	moveq #STATE_LEGAL_PREFIX_TEXT_LEN, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	movea.l (sp), a0
	move.w 4(sp), d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	lea StateTargetsForText.l, a0
	moveq #STATE_TARGETS_FOR_TEXT_LEN, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	movea.l ActiveStateProfilePtr, a0
	move.w ActiveStateProfileLen, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	lea StateListPrefixText.l, a0
	moveq #STATE_LIST_PREFIX_TEXT_LEN, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback

	movea.l 12(sp), a2
	move.w 16(sp), d7
	beq.w illegalRenderFallback
	subq.w #1, d7
illegalRenderArgumentLoop
	bsr.w locateVmStringV1
	bne.w illegalRenderFallback
	movea.l a1, a3
	move.w d0, d3
	movem.l d3-d5/a3, -(sp)
	bsr.w readVarU32V1
	movem.l (sp)+, d3-d5/a3
	bne.w illegalRenderFallback
	moveq #1, d0
	bsr.w requireBytesV1
	bne.w illegalRenderFallback
	moveq #0, d6
	move.b (a2)+, d6
	beq.w illegalRenderFallback
	move.l a2, d0
	add.l d6, d0
	cmp.l a6, d0
	bhi.w illegalRenderFallback
	moveq #0, d0
	move.b buffers.ActiveStateProfileIndex, d0
	move.w d0, d1
	lsr.w #3, d0
	cmp.w d6, d0
	bhs.s illegalRenderArgumentNext
	moveq #0, d2
	move.b 0(a2, d0.w), d2
	andi.w #7, d1
	btst d1, d2
	beq.s illegalRenderArgumentNext
	adda.w d6, a2
	tst.w 18(sp)
	beq.s illegalRenderAppendArgument
	lea StateListSeparatorText.l, a0
	moveq #STATE_LIST_SEPARATOR_TEXT_LEN, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
illegalRenderAppendArgument
	movea.l a3, a0
	move.w d3, d0
	bsr.w appendStateMessageSliceV1
	bne.w illegalRenderFallback
	addq.w #1, 18(sp)
	bra.s illegalRenderArgumentDone
illegalRenderArgumentNext
	adda.w d6, a2
illegalRenderArgumentDone
	dbf d7, illegalRenderArgumentLoop
	tst.w 18(sp)
	beq.s illegalRenderFallback
	tst.l d4
	beq.s illegalRenderFallback
	clr.b (a5)
	move.w d5, d1
	lea 20(sp), sp
	movem.l (sp)+, d2-d7/a0-a5
	lea buffers.LastErrorBuffer, a1
	moveq #0, d0
	rts

illegalRenderFallback
	lea 20(sp), sp
	movem.l (sp)+, d2-d7/a0-a5
	moveq #1, d0
	rts
	.bend  ; renderIllegalCombinationV1

; Append one byte slice to the bounded diagnostic buffer.
; Inputs: A0/D0 source; A5 destination cursor; D4 remaining; D5 total length.
appendStateMessageSliceV1	.block
	andi.l #$ffff, d0
	move.w d0, d2
	beq.s appendStateMessageOk
	cmp.l d4, d0
	bhs.s appendStateMessageFail
	move.w d0, d1
	subq.w #1, d1
appendStateMessageLoop
	move.b (a0)+, (a5)+
	dbf d1, appendStateMessageLoop
	add.l d2, d5
	sub.l d2, d4
appendStateMessageOk
	moveq #0, d0
	rts
appendStateMessageFail
	moveq #1, d0
	rts
	.bend  ; appendStateMessageSliceV1

; Append one ASCII slice uppercased without changing package storage.
appendStateMessageUpperSliceV1	.block
	andi.l #$ffff, d0
	move.w d0, d2
	beq.s appendStateMessageUpperOk
	cmp.l d4, d0
	bhs.s appendStateMessageUpperFail
	move.w d0, d1
	subq.w #1, d1
appendStateMessageUpperLoop
	move.b (a0)+, d3
	cmpi.b #'a', d3
	blo.s appendStateMessageUpperByte
	cmpi.b #'z', d3
	bhi.s appendStateMessageUpperByte
	andi.b #$df, d3
appendStateMessageUpperByte
	move.b d3, (a5)+
	dbf d1, appendStateMessageUpperLoop
	add.l d2, d5
	sub.l d2, d4
appendStateMessageUpperOk
	moveq #0, d0
	rts
appendStateMessageUpperFail
	moveq #1, d0
	rts
	.bend  ; appendStateMessageUpperSliceV1

; Query one opaque state key.
; Inputs: A0/D0 key. Outputs: D0 = 0 found/D1 value, 1 absent.
getFlagV1	.block
	movem.l d2-d5/a1-a2, -(sp)
	movea.l a0, a2
	move.w d0, d5
	moveq #0, d4
	move.b buffers.ActiveStateKeyCount, d4
	beq.s flagAbsent
	moveq #0, d3
flagLoop
	lea buffers.ActiveStateKeyLens, a0
	moveq #0, d0
	move.b 0(a0, d3.l), d0
	cmp.w d5, d0
	bne.s flagNext
	move.w d0, d2
	move.l d3, d1
	lsl.l #2, d1
	lea buffers.ActiveStateKeyPtrs, a0
	movea.l 0(a0, d1.l), a1
	movea.l a2, a0
	move.l d3, -(sp)
	bsr.w stringEqSizedCasefoldV1
	move.l (sp)+, d3
	tst.b d0
	beq.s flagNext
	move.l d3, d2
	lsl.l #2, d2
	lea buffers.ActiveStateValues, a0
	move.l 0(a0, d2.l), d1
	moveq #0, d0
	bra.s flagReturn
flagNext
	addq.w #1, d3
	cmp.w d4, d3
	blo.s flagLoop
flagAbsent
	moveq #0, d1
	moveq #1, d0
flagReturn
	movem.l (sp)+, d2-d5/a1-a2
	tst.l d0
	rts
	.bend  ; getFlagV1

; Evaluate one compact-selector requirement (`key=v1+v2?diagnostic`) against
; the opaque active state table.
; Inputs: A0/D0 requirement text.
; Outputs: D0 = 0 allowed, 1 mismatch, 2 malformed; A1/D1 optional diagnostic
;          code slice on mismatch.
requirementAllowsV1	.block
	movem.l d2-d7/a2-a5, -(sp)
	movea.l a0, a2
	move.w d0, d7
	beq.w requirementMalformed
	moveq #0, d6

findEquals
	cmp.w d7, d6
	bhs.w requirementMalformed
	cmpi.b #'=', 0(a2, d6.w)
	beq.s haveEquals
	addq.w #1, d6
	bra.s findEquals

haveEquals
	tst.w d6
	beq.w requirementMalformed
	move.w d7, d3
	sub.w d6, d3
	subq.w #1, d3
	beq.w requirementMalformed
	movea.l a2, a0
	move.w d6, d0
	bsr.w getFlagV1
	move.l d1, d5
	move.w d0, d4  ; absent is a mismatch, not malformed
	lea 1(a2, d6.w), a3
	move.w d3, d6
	moveq #0, d2

findDiagnostic
	cmp.w d3, d2
	bhs.s noDiagnostic
	cmpi.b #'?', 0(a3, d2.w)
	beq.s haveDiagnostic
	addq.w #1, d2
	bra.s findDiagnostic

haveDiagnostic
	move.w d3, d1
	sub.w d2, d1
	subq.w #1, d1
	beq.w requirementMalformed
	lea 1(a3, d2.w), a1
	move.w d2, d6
	bra.s parseAllowed

noDiagnostic
	suba.l a1, a1
	moveq #0, d1

parseAllowed
	tst.w d6
	beq.w requirementMalformed
	moveq #0, d2  ; parsed value
	moveq #0, d7  ; digits in current value

allowedLoop
	moveq #0, d0
	move.b (a3)+, d0
	cmpi.b #'+', d0
	beq.s allowedBoundary
	cmpi.b #'0', d0
	blo.w requirementMalformed
	cmpi.b #'9', d0
	bhi.w requirementMalformed
	subi.b #'0', d0
	mulu.w #10, d2
	add.l d0, d2
	addq.w #1, d7
	subq.w #1, d6
	bne.s allowedLoop
	bra.s allowedLast

allowedBoundary
	tst.w d7
	beq.w requirementMalformed
	tst.w d4
	bne.s allowedNext
	cmp.l d5, d2
	beq.s requirementAllowed
allowedNext
	moveq #0, d2
	moveq #0, d7
	subq.w #1, d6
	beq.w requirementMalformed
	bra.s allowedLoop

allowedLast
	tst.w d7
	beq.w requirementMalformed
	tst.w d4
	bne.s requirementMismatch
	cmp.l d5, d2
	beq.s requirementAllowed

requirementMismatch
	moveq #1, d0
	bra.s requirementReturn
requirementAllowed
	suba.l a1, a1
	moveq #0, d1
	moveq #0, d0
	bra.s requirementReturn
requirementMalformed
	suba.l a1, a1
	moveq #0, d1
	moveq #2, d0
requirementReturn
	movem.l (sp)+, d2-d7/a2-a5
	tst.l d0
	rts
	.bend  ; requirementAllowsV1

	.priv

; Trim one raw argument and reject whitespace/comma-separated extras.
; Updates saved A1/D1 at 10/14(sp); BSR placed its return address below the
; applyDirectiveV1 local frame.
normalizeArgumentV1	.block
	movea.l 10(sp), a0
	moveq #0, d0
	move.w 14(sp), d0
trimLeading
	tst.w d0
	beq.s argumentBad
	cmpi.b #' ', (a0)
	beq.s trimLeadByte
	cmpi.b #9, (a0)
	bne.s trimTrailing
trimLeadByte
	addq.l #1, a0
	subq.w #1, d0
	bra.s trimLeading
trimTrailing
	movea.l a0, a1
	adda.w d0, a1
trimTailLoop
	tst.w d0
	beq.s argumentBad
	cmpi.b #' ', -1(a1)
	beq.s trimTailByte
	cmpi.b #9, -1(a1)
	bne.s stripQuotes
trimTailByte
	subq.l #1, a1
	subq.w #1, d0
	bra.s trimTailLoop
stripQuotes
	cmpi.b #'"', (a0)
	bne.s validateToken
	cmpi.w #2, d0
	blo.s argumentBad
	cmpi.b #'"', -1(a1)
	bne.s argumentBad
	addq.l #1, a0
	subq.w #2, d0
	beq.s argumentBad
validateToken
	movea.l a0, a1
	move.w d0, d1
	subq.w #1, d1
validateLoop
	move.b (a1)+, d2
	cmpi.b #',', d2
	beq.s argumentBad
	cmpi.b #' ', d2
	beq.s argumentBad
	cmpi.b #9, d2
	beq.s argumentBad
	dbf d1, validateLoop
	move.l a0, 10(sp)
	move.w d0, 14(sp)
	moveq #0, d0
	rts
argumentBad
	moveq #1, d0
	rts
	.bend  ; normalizeArgumentV1

skipStateArgumentV1	.block
	bsr.w locateVmStringV1
	bne.s skipArgumentFail
	bsr.w readVarU32V1
	bne.s skipArgumentFail
	moveq #1, d0
	bsr.w requireBytesV1
	bne.s skipArgumentFail
	moveq #0, d0
	move.b (a2)+, d0
	beq.s skipArgumentFail
	bsr.w requireBytesV1
	bne.s skipArgumentFail
	adda.w d0, a2
	moveq #0, d0
	rts
skipArgumentFail
	moveq #1, d0
	rts
	.bend  ; skipStateArgumentV1

activeOwnerMatchesV1	.block
	move.w d0, d2
	cmpi.b #SCOPED_OWNER_DIALECT, d2
	bne.s activeOwnerCpu
	lea buffers.ActiveDialectBuffer, a2
	tst.b (a2)
	beq.s activeOwnerNo
	moveq #3, d5
	bra.s activeOwnerCompare
activeOwnerCpu
	cmpi.b #SCOPED_OWNER_CPU, d2
	bne.s activeOwnerFamily
	lea buffers.ActiveCpuBuffer, a2
	moveq #2, d5
	bra.s activeOwnerCompare
activeOwnerFamily
	cmpi.b #SCOPED_OWNER_FAMILY, d2
	bne.s activeOwnerNo
	lea buffers.ActiveFamilyBuffer, a2
	moveq #1, d5
activeOwnerCompare
	movea.l a2, a1
	moveq #0, d2
activeOwnerLengthLoop
	tst.b 0(a1, d2.w)
	beq.s activeOwnerLengthReady
	addq.w #1, d2
	cmpi.w #buffers.PIPELINE_ID_BUFFER_CAPACITY, d2
	bhs.s activeOwnerNo
	bra.s activeOwnerLengthLoop
activeOwnerLengthReady
	cmp.w d4, d2
	bne.s activeOwnerNo
	movea.l a4, a0
	movea.l a2, a1
	move.w d4, d0
	bsr.w stringEqSizedCasefoldV1
	tst.b d0
	beq.s activeOwnerNo
	move.w d5, d0
	rts
activeOwnerNo
	moveq #0, d0
	rts
	.bend  ; activeOwnerMatchesV1

stringEqNulCasefoldV1	.block
	move.w d0, d1
	beq.s stringNulEnd
	subq.w #1, d1
stringNulLoop
	move.b (a0)+, d2
	move.b (a2)+, d3
	bsr.s foldPairV1
	cmp.b d3, d2
	bne.s stringNulNo
	dbf d1, stringNulLoop
stringNulEnd
	tst.b (a2)
	bne.s stringNulNo
	moveq #1, d0
	rts

stringNulNo
	moveq #0, d0
	rts
	.bend  ; stringEqNulCasefoldV1

stringEqSizedCasefoldV1	.block
	cmp.w d2, d0
	bne.s stringSizedNo
	move.w d0, d1
	beq.s stringYes
	subq.w #1, d1
stringSizedLoop
	move.b (a0)+, d3
	move.b (a1)+, d4
	move.b d3, d2
	move.b d4, d3
	bsr.s foldPairV1
	cmp.b d3, d2
	bne.s stringSizedNo
	dbf d1, stringSizedLoop
stringYes
	moveq #1, d0
	rts
stringSizedNo
	moveq #0, d0
	rts
	.bend  ; stringEqSizedCasefoldV1

foldPairV1	.block
	cmpi.b #'A', d2
	blo.s foldSecond
	cmpi.b #'Z', d2
	bhi.s foldSecond
	ori.b #$20, d2
foldSecond
	cmpi.b #'A', d3
	blo.s foldDone
	cmpi.b #'Z', d3
	bhi.s foldDone
	ori.b #$20, d3
foldDone
	rts
	.bend  ; foldPairV1

locateStringV1	.block
	bsr.w readU32LeLow16V1
	bne.s locateFail
	bsr.w requireBytesV1
	bne.s locateFail
	movea.l a2, a1
	adda.l d0, a2
	moveq #0, d1
	rts
locateFail
	moveq #1, d1
	rts
	.bend  ; locateStringV1

locateVmStringV1	.block
	moveq #1, d0
	bsr.w requireBytesV1
	bne.s locateVmFail
	moveq #0, d0
	move.b (a2)+, d0
	beq.s locateVmFail
	bsr.w requireBytesV1
	bne.s locateVmFail
	movea.l a2, a1
	adda.w d0, a2
	moveq #0, d1
	rts
locateVmFail
	moveq #1, d1
	rts
	.bend  ; locateVmStringV1

readVarU32V1	.block
	moveq #0, d0
	moveq #0, d2
	moveq #4, d3
varLoop
	move.l d0, -(sp)
	moveq #1, d0
	bsr.w requireBytesV1
	bne.s varFailPop
	move.l (sp)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	move.l d1, d4
	andi.l #$7f, d4
	lsl.l d2, d4
	or.l d4, d0
	btst #7, d1
	beq.s varOk
	addq.w #7, d2
	dbf d3, varLoop
	moveq #1, d1
	rts
varFailPop
	addq.l #4, sp
	moveq #1, d1
	rts
varOk
	moveq #0, d1
	rts
	.bend  ; readVarU32V1

readU16LeV1	.block
	moveq #2, d0
	bsr.w requireBytesV1
	bne.s readU16Fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts
readU16Fail
	moveq #1, d1
	rts
	.bend  ; readU16LeV1

readU32LeLow16V1	.block
	moveq #4, d0
	bsr.w requireBytesV1
	bne.s readU32Fail
	tst.b 2(a2)
	bne.s readU32Fail
	tst.b 3(a2)
	bne.s readU32Fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	addq.l #2, a2
	moveq #0, d1
	rts
readU32Fail
	moveq #1, d1
	rts
	.bend  ; readU32LeLow16V1

requireBytesV1	.block
	move.l a2, d1
	add.l d0, d1
	bcs.s requireFail
	cmp.l a6, d1
	bhi.s requireFail
	moveq #0, d1
	rts
requireFail
	moveq #1, d1
	rts
	.bend  ; requireBytesV1

chunkPtrFromLocatorV1	.block
	moveq #0, d0
	move.b (a3), d0
	moveq #0, d1
	move.b 1(a3), d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b 2(a3), d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b 3(a3), d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d2
	move.b 4(a3), d2
	moveq #0, d1
	move.b 5(a3), d1
	lsl.l #8, d1
	or.l d1, d2
	moveq #0, d1
	move.b 6(a3), d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d2
	moveq #0, d1
	move.b 7(a3), d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d2
	lea buffers.PackageStorage, a2
	adda.l d0, a2
	movea.l a2, a6
	adda.l d2, a6
	moveq #0, d0
	rts
	.bend  ; chunkPtrFromLocatorV1

	.endsection

	.section bss, kind=bss
	.priv

ActiveStateDirectiveTablePtr
	.res long, 1
ActiveStateProfilePtr
	.res long, 1
ActiveStateProfileLen
	.res word, 1

	.endsection
	.endmodule
