; Shared canonical encoding VM execution with caller-owned runtime state.
; @opforge-owner: tkpkg.amigaos.encoding_execution
	.module tkpkg.amigaos.encoding_execution
	.cpu 68020
	.include "telemetry_macros.i"
	.pub
; Ephemeral runtime context, outside offset-only source/package representations.
; Caller owns valid input records, output and fixup arrays for their stated counts.
; A6 points here for every entry; no legacy service or assembler globals are used.
; Output capacity and returned lengths fit u16 (at most 65535 bytes).
Context	.struct
Output	.long ?
Capacity	.long ?
Input	.long ?
InputCount	.word ?
FirstInputLen	.word ?
WriteOffset	.word ?
FixupCount	.word ?
FixupCapacity	.word ?
Pass	.word ?
Pc	.long ?
Unstable	.byte ?
Defer	.byte ?
HasSymbol	.byte ?
Reserved	.byte ?
Mnemonic	.long ?
MnemonicLength	.word ?
Reserved2	.word ?
FixupOffsets	.long ?
FixupAddends	.long ?
FixupWidths	.long ?
FixupTargets	.long ?
.endstruct

MALFORMED_TEXT_LEN = 30
RANGE_SUFFIX_LEN = 33
	.section data, kind=data
	.priv
MalformedText	.byte "OTR901: encode table malformed", 0
RangeSuffix	.byte " branch displacement out of range"
	.endsection
	.section code, kind=code
	.pub
; A6=Context, D4.W=version, A1/D1=program; outputs D0=status,D1=length,A1=bytes
; or diagnostic. Preserves D2-D7/A0/A2-A6; caller checks D0 explicitly.
; Execute one already owner-selected CSEM descriptor.
; Inputs: D4.W=opcode version, A1/D1=program. Outputs: D0/D1 as interpreter.
semantic	.block
	cmpi.w #2, d4
	beq.w encoding
	cmpi.w #6, d4
	beq.w encoding
	cmpi.w #5, d4
	beq.w branch
	cmpi.w #7, d4
	beq.w fixup
	cmpi.w #4, d4
	bne.w malformed
fixup
	bsr.w runFixup
	rts
branch
	bsr.w runBranch
	rts
encoding
	bsr.w runEncoding
	rts
malformed
	lea MalformedText, a1
	moveq #MALFORMED_TEXT_LEN, d1
	moveq #1, d0
	rts
	.bend  ; semantic
	.priv

; Direct Rust branch_vm::execute_branch_program port for SEMV/CSEM v5.  The
; selected envelope supplies opcode, target, requested candidate (`-1` means
; auto), and automatic class.  Candidate widths, suffixes, endian, position
; adjustment, unresolved placeholder, and reserved values remain package data.
; Inputs: A1/D1 = program; four scalar records in Context.Input.
; Outputs: D0 status; D1 total output length in Context.Output.
runBranch	.block
	movem.l d2-d7/a0/a2-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_BRANCH
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
	bsr.w loadInput
	bne.w branchFail
	cmpi.l #255, d3
	bhi.w branchFail
	move.l d3, 12(sp)
	moveq #1, d0
	bsr.w loadInput
	bne.w branchFail
	move.l d3, 16(sp)
	moveq #2, d0
	bsr.w loadInput
	bne.w branchFail
	cmpi.l #-1, d3
	beq.w branchRequestedReady
	cmpi.l #255, d3
	bhi.w branchFail
branchRequestedReady
	move.l d3, 20(sp)
	moveq #3, d0
	bsr.w loadInput
	bne.w branchFail
	cmpi.l #7, d3
	bhi.w branchFail
	move.w d3, d2
	move.w d2, 30(sp)
	moveq #1, d0
	lsl.b d2, d0
	move.w d0, 32(sp)
	move.l Context.Pc(a6), 24(sp)
	clr.w 34(sp)
	tst.b Context.Unstable(a6)
	beq.w branchExplicitDefer
	cmpi.w #1, Context.Pass(a6)
	beq.w branchMarkUnresolved

branchExplicitDefer
	cmpi.l #-1, 20(sp)
	beq.w branchUnresolvedReady
	tst.b Context.Defer(a6)
	beq.w branchUnresolvedReady
	tst.b Context.HasSymbol(a6)
	beq.w branchUnresolvedReady
branchMarkUnresolved
	move.w #1, 34(sp)
branchUnresolvedReady

	moveq #5, d0
	bsr.w requireBytes
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
	bsr.w requireBytes
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
	bsr.w requireBytes
	bne.w branchFail
	adda.w d4, a0
	moveq #0, d5
	move.b (a0)+, d5
	cmpi.w #1, d5
	beq.w branchWidthReady
	cmpi.w #2, d5
	beq.w branchWidthReady
	cmpi.w #4, d5
	bne.w branchFail
branchWidthReady
	moveq #0, d6
	move.b (a0)+, d6
	cmpi.w #1, d6
	bhi.w branchFail
	bsr.w readU32
	bne.w branchFail
	move.l d0, 44(sp)
	bsr.w readU32
	bne.w branchFail
	move.l d0, 48(sp)
	move.w d5, d2
	bsr.w fitsSignedWidth
	tst.l d0
	bne.w branchFail
	moveq #0, d4
	move.b (a0)+, d4
	cmpi.w #8, d4
	bhi.w branchFail
	move.w d4, d0
	lsl.w #2, d0
	bsr.w requireBytes
	bne.w branchFail

	clr.w 40(sp)
	clr.w 42(sp)
	move.l 20(sp), d0
	cmpi.l #-1, d0
	beq.w branchAutomaticCandidate
	cmp.w 36(sp), d0
	bne.w branchCandidateValueReady
	move.w #1, 40(sp)
	bra.w branchComputeCandidateValue

branchAutomaticCandidate
	tst.l 4(sp)
	bne.w branchCandidateValueReady
	move.w 38(sp), d0
	and.w 32(sp), d0
	beq.w branchCandidateValueReady
	tst.w 34(sp)
	beq.w branchAutomaticResolved
	move.w 36(sp), d0
	cmp.w 28(sp), d0
	bne.w branchCandidateValueReady
branchAutomaticResolved
	move.w #1, 40(sp)

branchComputeCandidateValue
	tst.w 34(sp)
	beq.w branchProjectCandidateValue
	move.l 48(sp), d3
	bra.w branchValidateCandidateValue
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
	bsr.w fitsSignedWidth
	tst.l d0
	beq.w branchCandidateValueReady
	cmpi.l #-1, 20(sp)
	bne.w branchRangeFail
	clr.w 40(sp)

branchCandidateValueReady
	clr.l 56(sp)
	moveq #0, d6
	movea.l d4, a4
	bra.w branchReservedCheck

branchReservedLoop
	bsr.w readU32
	bne.w branchFail
	move.l d0, d3
	move.w d5, d2
	bsr.w fitsSignedWidth
	tst.l d0
	bne.w branchFail
	tst.w d6
	beq.w branchReservedAscending
	move.l 56(sp), d0
	cmp.l d3, d0
	bge.w branchFail
branchReservedAscending
	move.l d3, 56(sp)
	moveq #1, d6
	cmp.l 48(sp), d3
	beq.w branchFail
	tst.w 40(sp)
	beq.w branchReservedNext
	cmp.l 52(sp), d3
	bne.w branchReservedNext
	move.w #1, 42(sp)
branchReservedNext
	subq.l #1, a4

branchReservedCheck
	move.l a4, d0
	bne.w branchReservedLoop
	tst.w 40(sp)
	beq.w branchCandidateNext
	tst.w 42(sp)
	beq.w branchSelectCandidate
	cmpi.l #-1, 20(sp)
	bne.w branchFail
	bra.w branchCandidateNext

branchSelectCandidate
	tst.l 4(sp)
	beq.w branchStoreCandidate
	cmpi.l #-1, 20(sp)
	bne.w branchFail
	bra.w branchCandidateNext
branchStoreCandidate
	move.l a3, 4(sp)
	move.l 52(sp), d0
	move.l d0, 8(sp)

branchCandidateNext
	subq.w #1, d7
	bne.w branchCandidateLoop
	moveq #1, d0
	bsr.w requireBytes
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
	movea.l Context.Output(a6), a2
	moveq #0, d0
	move.w Context.WriteOffset(a6), d0
	adda.w d0, a2
	move.l a2, d3
	movea.l Context.Output(a6), a1
	sub.l a1, d3
	addq.l #1, d3
	add.l d7, d3
	cmp.l Context.Capacity(a6), d3
	bhi.w branchFail
	move.l 12(sp), d0
	move.b d0, (a2)+
	tst.w d7
	beq.w branchSuffixReady
branchSuffixLoop
	move.b (a0)+, (a2)+
	subq.w #1, d7
	bne.w branchSuffixLoop
branchSuffixReady
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d4
	move.b (a0)+, d4
	move.l 8(sp), d0
	bsr.w emitUnit
	tst.l d3
	bne.w branchFail
	move.l a2, d1
	movea.l Context.Output(a6), a1
	sub.l a1, d1
	cmp.w Context.WriteOffset(a6), d1
	bls.w branchFail
	moveq #0, d0
	bra.w branchReturn

branchRangeFail
	moveq #0, d1
	move.w Context.MnemonicLength(a6), d1
	move.l d1, d2
	addi.l #RANGE_SUFFIX_LEN + 1, d2
	cmp.l Context.Capacity(a6), d2
	bhi.w branchFail
	movea.l Context.Output(a6), a2
	movea.l Context.Mnemonic(a6), a3
	move.l d1, d2
branchRangeMnemonicLoop
	tst.l d2
	beq.w branchRangeSuffixReady
	move.b (a3)+, (a2)+
	subq.l #1, d2
	bra.w branchRangeMnemonicLoop
branchRangeSuffixReady
	lea RangeSuffix, a3
	moveq #RANGE_SUFFIX_LEN, d2
branchRangeSuffixLoop
	move.b (a3)+, (a2)+
	subq.l #1, d2
	bne.w branchRangeSuffixLoop
	clr.b (a2)
	addi.w #RANGE_SUFFIX_LEN, d1
	movea.l Context.Output(a6), a1
	moveq #1, d0
	bra.w branchReturn

branchFail
	lea MalformedText, a1
	moveq #MALFORMED_TEXT_LEN, d1
	moveq #1, d0
branchReturn
	lea 60(sp), sp
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d2-d7/a0/a2-a6
	rts
	.bend  ; runBranch
; Inputs: D0 signed value, D2.W width (1/2/4). Output D0=0 fits, 1 fails.
fitsSignedWidth	.block
	cmpi.w #1, d2
	beq.w branchFitsByte
	cmpi.w #2, d2
	beq.w branchFitsWord
	cmpi.w #4, d2
	bne.w branchDoesNotFit
	moveq #0, d0
	rts
branchFitsByte
	cmpi.l #-128, d0
	blt.w branchDoesNotFit
	cmpi.l #127, d0
	bgt.w branchDoesNotFit
	moveq #0, d0
	rts
branchFitsWord
	cmpi.l #-32768, d0
	blt.w branchDoesNotFit
	cmpi.l #32767, d0
	bgt.w branchDoesNotFit
	moveq #0, d0
	rts
branchDoesNotFit
	moveq #1, d0
	rts
	.bend  ; fitsSignedWidth

; Direct Rust fixup_vm::execute_fixup_program v4/v7 port over the native
; signed-32 scalar transport. Fixup inputs use a seven-byte record: flags,
; little-endian u32, then an opaque big-endian target-symbol index. Bit zero
; carries Rust's target_reference property; bit one is unresolved. V7
; transforms remain package data and are interpreted generically; signed i64
; transform values outside native i32 transport fail closed.
; Inputs: A1/D1 = program; D4.W = opcode version; D5/D6/A3 = input records.
; Outputs: D0 status; D1 total output length in Context.Output.
runFixup	.block
	movem.l d2-d7/a0/a2-a5, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_FIXUP
	move.w d4, d7
	cmpi.w #4, d7
	beq.w fixupVersionReady
	cmpi.w #7, d7
	bne.w fixupFail
fixupVersionReady
	movea.l a1, a0
	moveq #0, d0
	move.w d1, d0
	lea 0(a0, d0.l), a5
	movea.l Context.Output(a6), a2
	moveq #0, d0
	move.w Context.WriteOffset(a6), d0
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
	bsr.w requireBytes
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
	bsr.w readU32
	bne.w fixupFrameFail
	move.l d0, 8(sp)
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, 12(sp)
	bsr.w readU32
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
	beq.w fixupWidthReady
	cmpi.w #2, d2
	beq.w fixupWidthReady
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
	bsr.w loadFixupInput
	bne.w fixupFrameFail
	btst #1, d6
	beq.w fixupResolved
	tst.w 12(sp)
	beq.w fixupFrameFail
	move.l 14(sp), d3
	bra.w fixupProjected

fixupResolved
	move.w 6(sp), d0
	beq.w fixupProjected
	cmpi.w #2, d0
	bne.w fixupApplyPosition
	btst #0, d6
	beq.w fixupProjected
fixupApplyPosition
	move.l Context.Pc(a6), d0
	add.l 8(sp), d0
	bvs.w fixupFrameFail
	sub.l d0, d3
	bvs.w fixupFrameFail

fixupProjected
	move.w 2(sp), d2
	cmpi.w #7, d7
	bne.w fixupTransformReady
	bsr.w applyTransform
	bne.w fixupFrameFail
fixupTransformReady
	move.w 18(sp), d0
	move.w 2(sp), d2
	bsr.w validateFixupRange
	bne.w fixupFrameFail
	cmpi.w #1, 20(sp)
	bne.w fixupRecordReady
	cmpi.w #$ffff, d5
	beq.w fixupRecordReady
	move.l a2, d0
	movea.l Context.Output(a6), a1
	sub.l a1, d0
	move.w d2, d1
	move.w d5, d2
	bsr.w recordFixup
	bne.w fixupFrameFail
	; Rust's PortableOutputFixup retains `width` and `target_index` as
	; independent fields. D2 carried the opaque target index only across the
	; record call; restore the package-declared output width before emission.
	move.w 2(sp), d2
fixupRecordReady
	move.l d3, d0
	move.w 4(sp), d4
	bsr.w emitUnit
	tst.l d3
	bne.w fixupFrameFail
	lea 22(sp), sp
	bra.w fixupLoop

fixupFrameFail
	lea 22(sp), sp
fixupFail
	lea MalformedText, a1
	moveq #MALFORMED_TEXT_LEN, d1
	moveq #1, d0
	bra.w fixupReturn
fixupEnd
	cmpa.l a5, a0
	bne.w fixupFail
	move.l a2, d1
	movea.l Context.Output(a6), a1
	sub.l a1, d1
	cmp.w Context.WriteOffset(a6), d1
	bls.w fixupFail
	moveq #0, d0
fixupReturn
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d2-d7/a0/a2-a5
	rts
	.bend  ; runFixup

; Apply one Rust FixupTransform carried by SEMV v7.  The native expression
; boundary transports signed i32 scalars, so range-map i64 fields must be exact
; sign extensions of i32 values.  This is the only representation difference
; from Rust; ordering, alignment, mapping, and overflow behavior are identical.
; Inputs: A0/A5 = transform cursor/program end; D2.W = output width;
;         D3.L = projected value; D6.W bit one = unresolved.
; Outputs: D0 = 0/1; D3.L transformed value; A0 advanced.
applyTransform	.block
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
	bsr.w requireBytes
	bne.w transformFail
	moveq #0, d0
	move.b (a0)+, d0
	bne.w transformCheckKind
	bra.w transformIdentity
transformCheckKind
	cmpi.b #1, d0
	bne.w transformCheckRangeMap
	bra.w transformAlignedBitOr
transformCheckRangeMap
	cmpi.b #2, d0
	bne.w transformFail

transformRangeMap
	moveq #5, d0
	bsr.w requireBytes
	bne.w transformFail
	bsr.w readU32
	bne.w transformFail
	move.l d0, 22(sp)
	bsr.w validateAlignment
	bne.w transformFail
	moveq #0, d7
	move.b (a0)+, d7
	beq.w transformFail
	move.w 30(sp), d0
	btst #1, d0
	bne.w transformRangeLoop
	move.l 22(sp), d4
	subq.l #1, d4
	move.l (sp), d0
	and.l d4, d0
	bne.w transformFail

transformRangeLoop
	moveq #24, d0
	bsr.w requireBytes
	bne.w transformFail
	bsr.w readI64
	bsr.w requireI32
	bne.w transformFail
	move.l d1, 14(sp)
	bsr.w readI64
	bsr.w requireI32
	bne.w transformFail
	move.l d1, 18(sp)
	bsr.w readI64
	bsr.w requireI32
	bne.w transformFail
	move.l d1, 26(sp)

	move.l 14(sp), d4
	cmp.l 18(sp), d4
	bgt.w transformFail
	tst.w 6(sp)
	beq.w transformRangeOrdered
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
	bne.w transformRangeNext
	move.l (sp), d5
	cmp.l 14(sp), d5
	blt.w transformRangeNext
	cmp.l 18(sp), d5
	bgt.w transformRangeNext
	tst.w 8(sp)
	bne.w transformFail
	add.l 26(sp), d5
	bvs.w transformFail
	move.l d5, (sp)
	move.w #1, 8(sp)
transformRangeNext
	subq.w #1, d7
	beq.w transformRangeDone
	bra.w transformRangeLoop
transformRangeDone
	move.w 30(sp), d0
	btst #1, d0
	beq.w transformRangeResolved
	bra.w transformIdentity
transformRangeResolved
	tst.w 8(sp)
	beq.w transformFail
	move.l (sp), d3
	bra.w transformOk

transformAlignedBitOr
	moveq #8, d0
	bsr.w requireBytes
	bne.w transformFail
	bsr.w readU32
	bne.w transformFail
	move.l d0, 22(sp)
	bsr.w validateAlignment
	bne.w transformFail
	bsr.w readU32
	bne.w transformFail
	move.l d0, 26(sp)
	move.w 4(sp), d2
	cmpi.w #1, d2
	bne.w transformMaskWord
	cmpi.l #$ff, d0
	bhi.w transformFail
	bra.w transformMaskReady
transformMaskWord
	cmpi.w #2, d2
	bne.w transformMaskLong
	cmpi.l #$ffff, d0
	bhi.w transformFail
	bra.w transformMaskReady
transformMaskLong
	cmpi.w #4, d2
	bne.w transformFail
transformMaskReady
	move.w 30(sp), d0
	btst #1, d0
	bne.w transformIdentity
	move.l 22(sp), d4
	subq.l #1, d4
	move.l (sp), d0
	and.l d4, d0
	bne.w transformFail
	move.l (sp), d3
	or.l 26(sp), d3
	bra.w transformOk

transformIdentity
	move.l (sp), d3
transformOk
	moveq #0, d0
	bra.w transformReturn
transformFail
	moveq #1, d0
transformReturn
	lea 32(sp), sp
	movem.l (sp)+, d1-d2/d4-d7/a1-a4
	tst.l d0
	rts
	.bend  ; applyTransform

; Validate a package u32 alignment as Rust's nonzero power-of-two contract.
; Input: D0.L alignment. Output: D0=0/1.
validateAlignment	.block
	move.l d1, -(sp)
	tst.l d0
	beq.w alignmentFail
	move.l d0, d1
	subq.l #1, d1
	and.l d0, d1
	bne.w alignmentFail
	moveq #0, d0
	bra.w alignmentReturn
alignmentFail
	moveq #1, d0
alignmentReturn
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; validateAlignment

; Read one signed package i64 stored little-endian.
; Inputs: A0 = cursor already proven to have eight bytes.
; Outputs: D0=high32, D1=low32, A0+=8.
readI64	.block
	move.l (a0)+, d1
	ror.w #8, d1
	swap d1
	ror.w #8, d1
	move.l (a0)+, d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	rts
	.bend  ; readI64

; Require one signed i64 pair to be exactly representable by native i32.
; Inputs: D0=high32, D1=low32. Output: D0=0/1.
requireI32	.block
	move.l d2, -(sp)
	moveq #0, d2
	tst.l d1
	bpl.w requireI32HighReady
	moveq #-1, d2
requireI32HighReady
	cmp.l d2, d0
	bne.w requireI32Fail
	moveq #0, d0
	bra.w requireI32Return
requireI32Fail
	moveq #1, d0
requireI32Return
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; requireI32

; Load one fixup record by index. Outputs D3=value, D5=opaque target symbol
; index, D6=flags, D0=0/1.  The seven-byte record is the native projection of
; Rust PortableFixupInput: flags, signed scalar, then target identity.
loadFixupInput	.block
	cmp.w Context.InputCount(a6), d0
	bhs.w fixupInputFail
	move.w d0, d4
	movea.l Context.Input(a6), a4
	move.w Context.FirstInputLen(a6), d2
	tst.w d4
	beq.w fixupInputReady
fixupInputScan
	adda.w d2, a4
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d4
	bne.w fixupInputScan
fixupInputReady
	cmpi.w #7, d2
	bne.w fixupInputFail
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
	moveq #0, d5
	move.b (a4)+, d5
	lsl.w #8, d5
	move.b (a4)+, d5
	moveq #0, d0
	rts
fixupInputFail
	moveq #1, d0
	rts
	.bend  ; loadFixupInput

; Retain one Rust PortableOutputFixup emitted by the active SEMV fixup step.
; Inputs: D0=step-relative output offset, D1.W=width, D2.W=opaque target
; symbol index, D3=encoded scalar before assembler section projection.
; Outputs: D0=0 success/1 capacity failure. Preserves all other registers.
recordFixup	.block
	movem.l d1-d7/a0-a2, -(sp)
	moveq #0, d7
	move.w Context.FixupCount(a6), d7
	cmp.w Context.FixupCapacity(a6), d7
	bhs.w outputFixupFail
	move.l d7, d6
	lsl.l #2, d6
	movea.l Context.FixupOffsets(a6), a0
	move.l d0, 0(a0, d6.l)
	movea.l Context.FixupAddends(a6), a0
	move.l d3, 0(a0, d6.l)
	move.l d7, d6
	add.w d6, d6
	movea.l Context.FixupWidths(a6), a0
	move.w d1, 0(a0, d6.w)
	movea.l Context.FixupTargets(a6), a0
	move.w d2, 0(a0, d6.w)
	addq.w #1, Context.FixupCount(a6)
	moveq #0, d0
	bra.w outputFixupReturn
outputFixupFail
	moveq #1, d0
outputFixupReturn
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; recordFixup

; Rust guarantees every PortableOutputFixup range is contained by
; PortableFixupResult.bytes.  Preserve that invariant at the native service
; boundary when a sequence leaves a step-local value in D1.
; Inputs: D1.W=candidate output length. Outputs: D0=0/1, D1=bounded length.
	.pub
normalizeFixupLength	.block
	movem.l d2-d7/a0, -(sp)
	moveq #0, d7
	move.w Context.FixupCount(a6), d7
	moveq #0, d6
outputFixupExtentLoop
	cmp.w d7, d6
	bhs.w outputFixupExtentDone
	move.l d6, d0
	lsl.l #2, d0
	movea.l Context.FixupOffsets(a6), a0
	move.l 0(a0, d0.l), d4
	move.l d6, d0
	add.w d0, d0
	movea.l Context.FixupWidths(a6), a0
	moveq #0, d5
	move.w 0(a0, d0.w), d5
	cmpi.w #1, d5
	beq.w outputFixupWidthReady
	cmpi.w #2, d5
	beq.w outputFixupWidthReady
	cmpi.w #4, d5
	bne.w outputFixupExtentFail
outputFixupWidthReady
	add.l d5, d4
	bcs.w outputFixupExtentFail
	cmp.l Context.Capacity(a6), d4
	bhi.w outputFixupExtentFail
	cmp.l d1, d4
	bls.w outputFixupExtentNext
	move.w d4, d1
outputFixupExtentNext
	addq.w #1, d6
	bra.w outputFixupExtentLoop
outputFixupExtentDone
	moveq #0, d0
	bra.w outputFixupExtentReturn
outputFixupExtentFail
	moveq #1, d0
outputFixupExtentReturn
	movem.l (sp)+, d2-d7/a0
	tst.l d0
	rts
	.bend  ; normalizeFixupLength
	.priv

; Apply Rust FixupRange to native i32/u32 scalar transport.
; Inputs: D3=value, D2=width, D0=range tag. Output D0=0/1.
validateFixupRange	.block
	cmpi.w #4, d2
	beq.w fixupRangeOk
	cmpi.w #1, d2
	beq.w fixupRangeByte
	cmpi.w #2, d2
	bne.w fixupRangeFail
	move.l d3, d1
	cmpi.w #1, d0
	beq.w fixupRangeUnsignedWord
	cmpi.w #2, d0
	beq.w fixupRangePatternWord
	cmpi.l #-32768, d1
	blt.w fixupRangeFail
	cmpi.l #$7fff, d1
	bgt.w fixupRangeFail
	bra.w fixupRangeOk
fixupRangeUnsignedWord
	tst.l d1
	bmi.w fixupRangeFail
fixupRangePatternWord
	cmpi.l #-32768, d1
	blt.w fixupRangeFail
	cmpi.l #$ffff, d1
	bgt.w fixupRangeFail
	bra.w fixupRangeOk
fixupRangeByte
	move.l d3, d1
	cmpi.w #1, d0
	beq.w fixupRangeUnsignedByte
	cmpi.w #2, d0
	beq.w fixupRangePatternByte
	cmpi.l #-128, d1
	blt.w fixupRangeFail
	cmpi.l #$7f, d1
	bgt.w fixupRangeFail
	bra.w fixupRangeOk
fixupRangeUnsignedByte
	tst.l d1
	bmi.w fixupRangeFail
fixupRangePatternByte
	cmpi.l #-128, d1
	blt.w fixupRangeFail
	cmpi.l #$ff, d1
	bgt.w fixupRangeFail
fixupRangeOk
	moveq #0, d0
	rts
fixupRangeFail
	moveq #1, d0
	rts
	.bend  ; validateFixupRange

; Direct Rust encoding_vm::execute_encoding_program port for SEMV/CSEM v2/v6.
; Inputs: A1/D1 = program; D4.W = opcode version;
;         D5/D6/A3 = scalar record count/first length/data.
; Outputs: D0 status; D1 output length in Context.Output.
runEncoding	.block
	movem.l d2-d7/a0/a2-a5, -(sp)
	move.w d4, -(sp)
	movea.l a1, a0
	moveq #0, d0
	move.w d1, d0
	lea 0(a0, d0.l), a5
	movea.l Context.Output(a6), a2
	moveq #0, d0
	move.w Context.WriteOffset(a6), d0
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
	bsr.w requireBytes
	bne.w encodingFail
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d4
	move.b (a0)+, d4
	bsr.w readU32
	bne.w encodingFail
	bsr.w validateUnit
	tst.l d3
	bne.w encodingFail
	bsr.w emitUnit
	tst.l d3
	bne.w encodingFail
	bra.w encodingLoop

encodingScalar
	moveq #19, d0
	bsr.w requireBytes
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
	bsr.w readU32
	bne.w encodingScalarFrameFail
	move.l d0, 12(sp)
	bsr.w readU32
	bne.w encodingScalarFrameFail
	move.l d0, 8(sp)
	bsr.w readU32
	bne.w encodingScalarFrameFail
	move.l d0, 4(sp)
	bsr.w readU32
	bne.w encodingScalarFrameFail
	move.l d0, (sp)
	moveq #0, d0
	move.w 16(sp), d0
	bsr.w loadInput
	bne.w encodingScalarFrameFail
	move.l d3, d7
	moveq #0, d6
	tst.l d7
	bpl.w encodingScalarInputHigh
	moveq #-1, d6
encodingScalarInputHigh
	cmp.l 8(sp), d6
	blt.w encodingScalarFrameFail
	bgt.w encodingScalarCheckMax
	cmp.l 12(sp), d7
	blo.w encodingScalarFrameFail
encodingScalarCheckMax
	cmp.l (sp), d6
	bgt.w encodingScalarFrameFail
	blt.w encodingScalarRangeOk
	cmp.l 4(sp), d7
	bhi.w encodingScalarFrameFail
encodingScalarRangeOk
	move.w 18(sp), d2
	move.w 20(sp), d4
	move.l d7, d0
	lea 22(sp), sp
	cmpi.w #1, d2
	beq.w encodingScalarMaskByte
	cmpi.w #2, d2
	beq.w encodingScalarMaskWord
	cmpi.w #4, d2
	bne.w encodingFail
	bra.w encodingScalarUnitReady
encodingScalarMaskByte
	andi.l #$000000FF, d0
	bra.w encodingScalarUnitReady
encodingScalarMaskWord
	andi.l #$0000FFFF, d0
encodingScalarUnitReady
	bsr.w validateUnit
	tst.l d3
	bne.w encodingFail
	bsr.w emitUnit
	tst.l d3
	bne.w encodingFail
	bra.w encodingLoop
encodingScalarFrameFail
	lea 22(sp), sp
	bra.w encodingFail

encodingFields
	moveq #7, d0
	bsr.w requireBytes
	bne.w encodingFail
	moveq #0, d2
	move.b (a0)+, d2
	moveq #0, d4
	move.b (a0)+, d4
	bsr.w readU32
	bne.w encodingFail
	move.l d0, d7
	bsr.w validateUnit
	tst.l d3
	bne.w encodingFail
	moveq #0, d6
	move.b (a0)+, d6
	tst.w d6
	beq.w encodingFail
	bra.w encodingFieldsReady

encodingInputFields
	cmpi.w #6, (sp)
	bne.w encodingFail
	moveq #4, d0
	bsr.w requireBytes
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
	bsr.w loadInput
	bne.w encodingInputFieldsLoadFail
	move.l d3, d7
	move.w (sp)+, d2
	move.w (sp)+, d4
	move.l d7, d0
	bsr.w validateUnit
	tst.l d3
	bne.w encodingFail

encodingFieldsReady
	move.w d6, d0
	lsl.w #2, d0
	bsr.w requireBytes
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
	bsr.w loadInput
	bne.w encodingFieldLocalsFail
	move.w 2(sp), d5
	move.w 4(sp), d4
	moveq #-1, d0
	cmpi.w #32, d4
	beq.w encodingFieldMaskReady
	moveq #32, d1
	sub.w d4, d1
	lsr.l d1, d0
encodingFieldMaskReady
	tst.w (sp)
	bne.w encodingFieldSigned
	tst.l d3
	bmi.w encodingFieldLocalsFail
	cmp.l d0, d3
	bhi.w encodingFieldLocalsFail
	bra.w encodingFieldRangeOk
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
	bsr.w emitUnit
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
	bne.w encodingFail
	move.l a2, d1
	movea.l Context.Output(a6), a1
	sub.l a1, d1
	tst.w d1
	beq.w encodingFail
	moveq #0, d0
	bra.w encodingReturn
encodingFail
	lea MalformedText, a1
	moveq #MALFORMED_TEXT_LEN, d1
	moveq #1, d0
encodingReturn
	addq.l #2, sp
	movem.l (sp)+, d2-d7/a0/a2-a5
	rts
	.bend  ; runEncoding

requireBytes	.block
	movea.l a0, a1
	adda.l d0, a1
	cmpa.l a5, a1
	bhi.w semanticRequireFail
	moveq #0, d1
	rts
semanticRequireFail
	moveq #1, d1
	rts
	.bend  ; requireBytes

readU32	.block
	moveq #4, d0
	bsr.w requireBytes
	bne.w semanticReadFail
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
	.bend  ; readU32

; Validate width/endian and that D0 fits the selected unit. D3 is status.
validateUnit	.block
	cmpi.w #1, d2
	beq.w semanticValidateByte
	cmpi.w #2, d2
	beq.w semanticValidateWord
	cmpi.w #4, d2
	bne.w semanticValidateFail
	bra.w semanticValidateEndian
semanticValidateByte
	cmpi.l #$000000FF, d0
	bhi.w semanticValidateFail
	bra.w semanticValidateEndian
semanticValidateWord
	cmpi.l #$0000FFFF, d0
	bhi.w semanticValidateFail
semanticValidateEndian
	cmpi.w #1, d4
	bhi.w semanticValidateFail
	moveq #0, d3
	rts
semanticValidateFail
	moveq #1, d3
	rts
	.bend  ; validateUnit

; Emit D0 in width D2 and endian D4.
emitUnit	.block
	move.l a2, d3
	movea.l Context.Output(a6), a4
	sub.l a4, d3
	add.l d2, d3
	cmp.l Context.Capacity(a6), d3
	bhi.w semanticEmitFail
	tst.b d4
	bne.w semanticEmitLittle
	cmpi.w #4, d2
	beq.w semanticEmitBig4
	cmpi.w #2, d2
	beq.w semanticEmitBig2
	move.b d0, (a2)+
	bra.w semanticEmitDone
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
	bra.w semanticEmitDone
semanticEmitLittle
	move.l d0, d3
	move.w d2, d5
	move.b d3, (a2)+
	subq.w #1, d5
	beq.w semanticEmitRestoreWidth
semanticEmitLittleLoop
	lsr.l #8, d3
	move.b d3, (a2)+
	subq.w #1, d5
	bne.w semanticEmitLittleLoop
semanticEmitRestoreWidth
semanticEmitDone
	moveq #0, d3
	rts
semanticEmitFail
	moveq #1, d3
	rts
	.bend  ; emitUnit

; Load one four-byte little-endian scalar record by index.
; Inputs: D0.W index; D5/D6/A3 selected operand records.
; Outputs: D0 status; D3 value.
loadInput	.block
	cmp.w Context.InputCount(a6), d0
	bhs.w semanticInputFail
	move.w d0, d4
	movea.l Context.Input(a6), a4
	move.w Context.FirstInputLen(a6), d2
	tst.w d4
	beq.w semanticInputReady
semanticInputScan
	adda.w d2, a4
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d4
	bne.w semanticInputScan
semanticInputReady
	cmpi.w #4, d2
	bne.w semanticInputFail
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
	.bend  ; loadInput

	.pub
; A6=Context, A1/D1=program, A3=operand records, D5.W=count, D6.W=first length.
; D0=status,D1=length,A1=output/diagnostic. Preserves D2-D7/A0/A2-A6.
; CCR reflects D0; records must be valid length-prefixed spans owned by caller.
table	.block
	movem.l d2-d7/a0/a2-a4, -(sp)
	movea.l a1, a0
	move.w d1, d7
	movea.l Context.Output(a6), a2
	clr.w d1

loop
	tst.w d7
	beq.w fail
	move.b (a0)+, d0
	subq.w #1, d7
	cmpi.b #$FF, d0
	beq.w endProgram
	cmpi.b #$01, d0
	beq.w emitU8
	cmpi.b #$02, d0
	beq.w emitOperand
	bra.w fail

emitU8
	tst.w d7
	beq.w fail
	move.l Context.Capacity(a6), d0
	cmp.w d0, d1
	bhs.w fail
	move.b (a0)+, (a2)+
	subq.w #1, d7
	addq.w #1, d1
	bra.w loop

emitOperand
	tst.w d7
	beq.w fail
	moveq #0, d3
	move.b (a0)+, d3
	subq.w #1, d7
	cmp.w d5, d3
	bhs.w fail
	movea.l a3, a4
	move.w d6, d2
	tst.w d3
	beq.w operandCopyStart

operandSelectLoop
	adda.w d2, a4
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d3
	bne.w operandSelectLoop

operandCopyStart
	moveq #0, d3
	move.w d1, d3
	add.w d2, d3
	bcs.w fail
	cmp.l Context.Capacity(a6), d3
	bhi.w fail
	move.w d2, d0
	beq.w loop

operandLoop
	move.b (a4)+, (a2)+
	addq.w #1, d1
	subq.w #1, d0
	bne.w operandLoop
	bra.w loop

endProgram
	tst.w d7
	bne.w fail
	movea.l Context.Output(a6), a1

ok
	moveq #0, d0
	bra.w return

fail
	lea MalformedText, a1
	moveq #MALFORMED_TEXT_LEN, d1
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0/a2-a4
	rts
	.bend  ; table
	.endsection
	.endmodule
