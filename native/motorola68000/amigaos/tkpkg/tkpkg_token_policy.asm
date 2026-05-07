; Package-backed token-policy owner resolution for the tkpkg native runtime.

	.module tkpkg.amigaos.token_policy
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi (STATUS_RUNTIME_ERROR_V1)
	.use tkpkg.amigaos.buffers (SCOPED_OWNER_DIALECT, SCOPED_OWNER_CPU)
	.use tkpkg.amigaos.buffers (SCOPED_OWNER_FAMILY, PackageStorage)
	.use tkpkg.amigaos.buffers (ToksChunkOffsetLo, PendingDialectOffsetLo)
	.use tkpkg.amigaos.buffers (PendingCpuOffsetLo, PendingFamilyOffsetLo)
	.use tkpkg.amigaos.buffers (PendingTokenPolicyOffsetLo)
	.use tkpkg.amigaos.buffers (PendingTokenPolicyOwnerTag)

MISSING_POLICY_TEXT_LEN             = 33
TOKS_ENTRY_FIXED_PREFIX_SIZE        = 9

	.section data, kind=data

MissingPolicyText
	.byte "OTR003: missing tokenizer policy", 0

	.endsection

	.section code, kind=code

; ---------------------------------------------------------------------------
; Resolve the active tokenizer token-policy locator.
;
; Token policy owner precedence matches the runtime model fallback order:
; dialect-specific policy first, then CPU, then family. The resolved record is
; staged in pendingTokenPolicy* fields for tkpkg_pipeline_commit_active_selection_v1.
;
; Inputs:
; - pendingDialectOffsetLo, pendingCpuOffsetLo, pendingFamilyOffsetLo contain
;   package string locators selected by the pipeline resolver.
;
; Outputs:
; - D0: 0 on success, STATUS_RUNTIME_ERROR_V1 when no policy can be found.
; - A1/D1: failure message pointer/length on missing-policy error.
; ---------------------------------------------------------------------------

tkpkgTokenPolicyResolveLocatorV1
	moveq #SCOPED_OWNER_DIALECT, d0  ; prefer dialect-specific tokenization rules when present
	lea PendingDialectOffsetLo, a3
	bsr.w tkpkgTokenPolicyFindOwnerV1
	tst.b d0
	beq.s tkpkgTokenPolicyResolveDone
	moveq #SCOPED_OWNER_CPU, d0  ; fall back to CPU-local policy
	lea PendingCpuOffsetLo, a3
	bsr.w tkpkgTokenPolicyFindOwnerV1
	tst.b d0
	beq.s tkpkgTokenPolicyResolveDone
	moveq #SCOPED_OWNER_FAMILY, d0  ; final fallback is family-wide policy
	lea PendingFamilyOffsetLo, a3
	bsr.w tkpkgTokenPolicyFindOwnerV1
	tst.b d0
	beq.s tkpkgTokenPolicyResolveDone
	lea MissingPolicyText, a1
	moveq #MISSING_POLICY_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgTokenPolicyResolveDone
	moveq #0, d0
	rts

; Find a TOKS record matching the scoped owner type in D0 and owner locator A3.
tkpkgTokenPolicyFindOwnerV1
	move.b d0, d6  ; D6 keeps the scoped-owner tag while D0 is reused by helpers
	move.l a3, -(sp)
	lea PendingTokenPolicyOffsetLo, a3
	clr.l (a3)+
	clr.b (a3)
	movea.l (sp)+, a3
	bsr.w tkpkgTokenPolicyReadLocatorPtrLenV1
	move.w d3, d5
	movea.l a1, a5
	lea ToksChunkOffsetLo, a3
	bsr.w tkpkgTokenPolicyChunkPtrFromLocatorV1
	bsr.w tkpkgTokenPolicyReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgTokenPolicyOwnerMissing
	tst.w d0
	beq.w tkpkgTokenPolicyOwnerMissing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

tkpkgTokenPolicyOwnerLoop
	movea.l a2, a4
	moveq #1, d0
	bsr.w tkpkgTokenPolicyRequireBytesV1
	tst.b d1
	bne.w tkpkgTokenPolicyOwnerMissing
	move.b (a2)+, d4
	bsr.w tkpkgTokenPolicyLocateStringV1
	tst.b d1
	bne.w tkpkgTokenPolicyOwnerMissing
	cmp.b d6, d4
	bne.w tkpkgTokenPolicySkipEntry
	move.w d0, d4
	movea.l a1, a0
	move.l a2, -(sp)
	move.w d4, d0
	move.w d5, d1
	movea.l a0, a1
	movea.l a5, a2
	bsr.w tkpkgTokenPolicyStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	bne.w tkpkgTokenPolicyFound

tkpkgTokenPolicySkipEntry
	bsr.w tkpkgTokenPolicySkipToksEntryV1
	tst.b d1
	bne.w tkpkgTokenPolicyOwnerMissing
	dbf d7, tkpkgTokenPolicyOwnerLoop

tkpkgTokenPolicyOwnerMissing
	moveq #1, d0
	rts

tkpkgTokenPolicyFound
	bsr.w tkpkgTokenPolicySkipToksEntryV1
	tst.b d1
	bne.w tkpkgTokenPolicyOwnerMissing
	lea PendingTokenPolicyOffsetLo, a3
	movea.l a4, a1
	move.l a2, d0
	sub.l a4, d0
	bsr.w tkpkgTokenPolicyStoreRecordLocatorV1
	move.b d6, PendingTokenPolicyOwnerTag
	moveq #0, d0
	rts

; Skip one TOKS entry, including optional tail extension fields.
tkpkgTokenPolicySkipToksEntryV1
	move.w d7, -(sp)
	moveq #TOKS_ENTRY_FIXED_PREFIX_SIZE, d0
	bsr.w tkpkgTokenPolicyRequireBytesV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	lea TOKS_ENTRY_FIXED_PREFIX_SIZE(a2), a2
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	moveq #1, d0
	bsr.w tkpkgTokenPolicyRequireBytesV1
	tst.b d1
	beq.s tkpkgTokenPolicyTailMarkerReady
	bra.w tkpkgTokenPolicySkipBoundsFail

tkpkgTokenPolicyTailMarkerReady
	cmpi.b #$FF, (a2)
	beq.s tkpkgTokenPolicySkipTailExt
	bra.w tkpkgTokenPolicySkipDone

tkpkgTokenPolicySkipTailExt
	moveq #1, d0
	bsr.w tkpkgTokenPolicyRequireBytesV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	addq.w #1, a2
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	moveq #1, d0
	bsr.w tkpkgTokenPolicyRequireBytesV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	tst.b (a2)+
	beq.s tkpkgTokenPolicySkipTailStrings
	moveq #1, d0
	bsr.w tkpkgTokenPolicyRequireBytesV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	addq.w #1, a2

tkpkgTokenPolicySkipTailStrings
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	bsr.w tkpkgTokenPolicyReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	move.w d0, d7
	lea 4(a2), a2
	tst.w d7
	beq.s tkpkgTokenPolicySkipDone
	subq.w #1, d7

tkpkgTokenPolicySkipOperatorsLoop
	bsr.w tkpkgTokenPolicySkipStringV1
	tst.b d1
	bne.w tkpkgTokenPolicySkipBoundsFail
	dbf d7, tkpkgTokenPolicySkipOperatorsLoop

tkpkgTokenPolicySkipDone
	move.w (sp)+, d7
	moveq #0, d1
	rts

tkpkgTokenPolicySkipBoundsFail
	move.w (sp)+, d7
	moveq #1, d1
	rts

tkpkgTokenPolicyStoreRecordLocatorV1
	move.l a6, -(sp)
	move.l a1, d2
	lea PackageStorage, a6
	sub.l a6, d2
	move.b d2, (a3)+
	lsr.w #8, d2
	move.b d2, (a3)+
	move.b d0, (a3)+
	lsr.w #8, d0
	move.b d0, (a3)+
	movea.l (sp)+, a6
	rts

tkpkgTokenPolicyReadLocatorPtrLenV1
	moveq #0, d2
	move.b (a3)+, d2
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d2
	moveq #0, d3
	move.b (a3)+, d3
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d3
	lea PackageStorage, a6
	lea 0(a6, d2.W), a1
	rts

tkpkgTokenPolicyChunkPtrFromLocatorV1
	moveq #0, d0
	move.b (a3)+, d0
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d7
	move.b (a3)+, d7
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d7
	lea PackageStorage, a6
	lea 0(a6, d0.W), a2
	lea 0(a2, d7.W), a6
	rts

; skip_string is an alias for callers that only need the A2 advance.
tkpkgTokenPolicySkipStringV1
tkpkgTokenPolicyLocateStringV1
	bsr.w tkpkgTokenPolicyReadU32LeLow16V1
	tst.b d1
	bne.s tkpkgTokenPolicyLocateStringBoundsFail
	move.l d0, d2
	move.l d0, d3
	addq.l #4, d3
	move.l d3, d0
	bsr.w tkpkgTokenPolicyRequireBytesV1
	tst.b d1
	bne.s tkpkgTokenPolicyLocateStringBoundsFail
	move.l d2, d0
	lea 4(a2), a1
	lea 4(a2), a2
	adda.l d0, a2
	moveq #0, d1
	rts

tkpkgTokenPolicyLocateStringBoundsFail
	moveq #0, d0
	movea.l d0, a1
	moveq #1, d1
	rts

tkpkgTokenPolicyReadU32LeLow16V1
	moveq #4, d0
	bsr.w tkpkgTokenPolicyRequireBytesV1
	tst.b d1
	bne.s tkpkgTokenPolicyReadU32BoundsFail
	moveq #0, d0
	move.b (a2), d0
	moveq #0, d1
	move.b 1(a2), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts

tkpkgTokenPolicyReadU32BoundsFail
	moveq #0, d0
	moveq #1, d1
	rts

tkpkgTokenPolicyRequireBytesV1
	movea.l a2, a1
	adda.l d0, a1
	cmpa.l a6, a1
	bhi.s tkpkgTokenPolicyRequireBytesFail
	moveq #0, d1
	rts

tkpkgTokenPolicyRequireBytesFail
	moveq #1, d1
	rts

tkpkgTokenPolicyStringEqAsciiCasefoldV1
	cmp.w d1, d0
	bne.s tkpkgTokenPolicyStringNoMatch
	move.w d0, d4
	tst.w d4
	beq.s tkpkgTokenPolicyStringMatch
	subq.w #1, d4

tkpkgTokenPolicyStringLoop
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d3
	move.b (a2)+, d3
	move.b d2, d0
	bsr.w tkpkgTokenPolicyFoldAsciiLowerV1
	move.b d0, d2
	move.b d3, d0
	bsr.w tkpkgTokenPolicyFoldAsciiLowerV1
	cmp.b d0, d2
	bne.s tkpkgTokenPolicyStringNoMatch
	dbf d4, tkpkgTokenPolicyStringLoop

tkpkgTokenPolicyStringMatch
	moveq #1, d0
	rts

tkpkgTokenPolicyStringNoMatch
	moveq #0, d0
	rts

tkpkgTokenPolicyFoldAsciiLowerV1
	cmpi.b #'A', d0
	blo.s tkpkgTokenPolicyFoldDone
	cmpi.b #'Z', d0
	bhi.s tkpkgTokenPolicyFoldDone
	ori.b #$20, d0

tkpkgTokenPolicyFoldDone
	rts

tkpkgTokenPolicyPlaceholder
	rts

	.endsection
	.endmodule
