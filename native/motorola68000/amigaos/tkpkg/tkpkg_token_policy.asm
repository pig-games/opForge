; Package-backed token-policy owner resolution for the tkpkg native runtime.

	.module tkpkg.amigaos.token_policy
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers

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

	.pub
resolveLocatorV1	.block
	moveq #buffers.SCOPED_OWNER_DIALECT, d0  ; prefer dialect-specific tokenization rules when present
	lea buffers.PendingDialectOffsetLo, a3
	bsr.w findOwner
	beq.s done
	moveq #buffers.SCOPED_OWNER_CPU, d0  ; fall back to CPU-local policy
	lea buffers.PendingCpuOffsetLo, a3
	bsr.w findOwner
	beq.s done
	moveq #buffers.SCOPED_OWNER_FAMILY, d0  ; final fallback is family-wide policy
	lea buffers.PendingFamilyOffsetLo, a3
	bsr.w findOwner
	beq.s done
	lea MissingPolicyText, a1
	moveq #MISSING_POLICY_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

done
	moveq #0, d0
	rts
	.bend  ; resolveLocatorV1
	.priv

; Find a TOKS record matching the scoped owner type in D0 and owner locator A3.
findOwner	.block
	move.b d0, d6  ; D6 keeps the scoped-owner tag while D0 is reused by helpers
	move.l a3, -(sp)
	lea buffers.PendingTokenPolicyOffsetLo, a3
	clr.l (a3)+
	clr.b (a3)
	movea.l (sp)+, a3
	bsr.w readLocatorPtrLen
	move.w d3, d5
	movea.l a1, a5
	lea buffers.ToksChunkOffsetLo, a3
	bsr.w chunkPtrFromLocator
	bsr.w readU32LeLow16
	tst.b d1
	bne.w missing
	tst.w d0
	beq.w missing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

loop
	movea.l a2, a4
	moveq #1, d0
	bsr.w requireBytes
	tst.b d1
	bne.w missing
	move.b (a2)+, d4
	bsr.w locateString
	tst.b d1
	bne.w missing
	cmp.b d6, d4
	bne.w skipEntry
	move.w d0, d4
	movea.l a1, a0
	move.l a2, -(sp)
	move.w d4, d0
	move.w d5, d1
	movea.l a0, a1
	movea.l a5, a2
	bsr.w stringEqAsciiCasefold
	movea.l (sp)+, a2
	tst.b d0
	bne.w found

skipEntry
	bsr.w skipToksEntry
	tst.b d1
	bne.w missing
	dbf d7, loop

missing
	moveq #1, d0
	rts

found
	bsr.w skipToksEntry
	tst.b d1
	bne.w missing
	lea buffers.PendingTokenPolicyOffsetLo, a3
	movea.l a4, a1
	move.l a2, d0
	sub.l a4, d0
	bsr.w storeRecordLocator
	move.b d6, buffers.PendingTokenPolicyOwnerTag
	moveq #0, d0
	rts
	.bend  ; findOwner

; Skip one TOKS entry, including optional tail extension fields.
skipToksEntry	.block
	move.w d7, -(sp)
	moveq #TOKS_ENTRY_FIXED_PREFIX_SIZE, d0
	bsr.w requireBytes
	tst.b d1
	bne.w boundsFail
	lea TOKS_ENTRY_FIXED_PREFIX_SIZE(a2), a2
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	moveq #1, d0
	bsr.w requireBytes
	tst.b d1
	beq.s tailMarkerReady
	bra.w boundsFail

tailMarkerReady
	cmpi.b #$FF, (a2)
	beq.s tailExt
	bra.w done

tailExt
	moveq #1, d0
	bsr.w requireBytes
	tst.b d1
	bne.w boundsFail
	addq.w #1, a2
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	moveq #1, d0
	bsr.w requireBytes
	tst.b d1
	bne.w boundsFail
	tst.b (a2)+
	beq.s tailStrings
	moveq #1, d0
	bsr.w requireBytes
	tst.b d1
	bne.w boundsFail
	addq.w #1, a2

tailStrings
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	bsr.w readU32LeLow16
	tst.b d1
	bne.w boundsFail
	move.w d0, d7
	lea 4(a2), a2
	tst.w d7
	beq.s done
	subq.w #1, d7

operatorsLoop
	bsr.w skipString
	tst.b d1
	bne.w boundsFail
	dbf d7, operatorsLoop

done
	move.w (sp)+, d7
	moveq #0, d1
	rts

boundsFail
	move.w (sp)+, d7
	moveq #1, d1
	rts
	.bend  ; skipToksEntry

storeRecordLocator	.block
	move.l a6, -(sp)
	move.l a1, d2
	lea buffers.PackageStorage, a6
	sub.l a6, d2
	move.b d2, (a3)+
	lsr.w #8, d2
	move.b d2, (a3)+
	move.b d0, (a3)+
	lsr.w #8, d0
	move.b d0, (a3)+
	movea.l (sp)+, a6
	rts
	.bend  ; storeRecordLocator

readLocatorPtrLen	.block
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
	lea buffers.PackageStorage, a6
	lea 0(a6, d2.W), a1
	rts
	.bend  ; readLocatorPtrLen

chunkPtrFromLocator	.block
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
	lea buffers.PackageStorage, a6
	lea 0(a6, d0.W), a2
	lea 0(a2, d7.W), a6
	rts
	.bend  ; chunkPtrFromLocator

; skip_string is an alias for callers that only need the A2 advance.
skipString	.block
	bsr.w locateString
	rts
	.bend  ; skipString

locateString	.block
	bsr.w readU32LeLow16
	tst.b d1
	bne.s boundsFail
	move.l d0, d2
	move.l d0, d3
	addq.l #4, d3
	move.l d3, d0
	bsr.w requireBytes
	tst.b d1
	bne.s boundsFail
	move.l d2, d0
	lea 4(a2), a1
	lea 4(a2), a2
	adda.l d0, a2
	moveq #0, d1
	rts

boundsFail
	moveq #0, d0
	movea.l d0, a1
	moveq #1, d1
	rts
	.bend  ; locateString

readU32LeLow16	.block
	moveq #4, d0
	bsr.w requireBytes
	tst.b d1
	bne.s boundsFail
	moveq #0, d0
	move.b (a2), d0
	moveq #0, d1
	move.b 1(a2), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts

boundsFail
	moveq #0, d0
	moveq #1, d1
	rts
	.bend  ; readU32LeLow16

requireBytes	.block
	movea.l a2, a1
	adda.l d0, a1
	cmpa.l a6, a1
	bhi.s fail
	moveq #0, d1
	rts

fail
	moveq #1, d1
	rts
	.bend  ; requireBytes

stringEqAsciiCasefold	.block
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
	bsr.w foldAsciiLower
	move.b d0, d2
	move.b d3, d0
	bsr.w foldAsciiLower
	cmp.b d0, d2
	bne.s noMatch
	dbf d4, loop

match
	moveq #1, d0
	rts

noMatch
	moveq #0, d0
	rts
	.bend  ; stringEqAsciiCasefold

foldAsciiLower	.block
	cmpi.b #'A', d0
	blo.s done
	cmpi.b #'Z', d0
	bhi.s done
	ori.b #$20, d0

done
	rts
	.bend  ; foldAsciiLower

placeholder	.block
	rts
	.bend  ; placeholder

	.endsection
	.endmodule
