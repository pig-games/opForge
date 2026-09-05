; Native configured-root module discovery.
; @opforge-owner: opforge.cli.module_discovery
; @opforge-slice: documentation/plans/slices/native-porting-slice-module-autoload.toml

	.module opforge.cli.module_discovery
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.dos
	.use opforge.cli.path
	.use opforge.cli.state
	.use opforge.cli.token_util
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	.use debug.amigaos.platform_profile as platform_profile
.endif

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Resolve NativeCliArgToken by scanning declared modules below configured roots.
;
; Inputs:
; - state.NativeCliArgToken: requested module id.
; - state.NativeCliModulePathTable/count: ordered input and `-M` roots.
;
; Outputs:
; - D0: 0 on one unique match, 1 on missing, ambiguous, or scan failure.
; - state.NativeCliIncludePath: selected source path on success.
;
; Clobbers:
; - D0-D7/A0-A5/CCR. Other caller-visible registers are protected.
;
; CCR:
; - Reflects D0 on return. The epilogue is CCR-neutral.
; ---------------------------------------------------------------------------
resolveDeclaredModuleV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	lea state.NativeCliArgToken, a0
	lea ModuleScanLookupName.l, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer
	clr.w ModuleScanMatchCount
	clr.b ModuleScanFoundPath
	tst.w ModuleScanIndexBuilt.l
	bne.s lookupIndex
	bsr.w buildModuleIndex
	tst.l d0
	bne.w fail

lookupIndex
	moveq #0, d7

indexLoop
	cmp.w ModuleScanIndexCount.l, d7
	bhs.s indexDone
	move.l d7, d0
	lsl.l #6, d0
	lea ModuleScanIndexNameTable.l, a0
	adda.l d0, a0
	lea ModuleScanLookupName.l, a1
	bsr.w compareFoldedNull
	tst.l d0
	beq.s indexNext
	tst.w ModuleScanMatchCount.l
	bne.s fail
	move.l d7, d0
	lsl.l #8, d0
	lea ModuleScanIndexPathTable.l, a0
	adda.l d0, a0
	lea ModuleScanFoundPath.l, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	move.l d7, d0
	lsl.l #2, d0
	lea ModuleScanIndexStartTable.l, a0
	move.l 0(a0, d0.l), state.NativeCliResolvedModuleStartOffset.l
	lea ModuleScanIndexEndTable.l, a0
	move.l 0(a0, d0.l), state.NativeCliResolvedModuleEndOffset.l
	move.w #1, ModuleScanMatchCount.l

indexNext
	addq.w #1, d7
	bra.s indexLoop

indexDone
	cmpi.w #1, ModuleScanMatchCount
	bne.s fail
	lea ModuleScanFoundPath.l, a0
	lea state.NativeCliIncludePath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; resolveDeclaredModuleV1

	.priv

; Build one declaration index for the configured roots. Rust resolves module
; declarations from the complete ordered root set; native keeps that semantic
; contract while avoiding a full directory/file rescan for every `.use`.
; Outputs: D0 = 0 success, 1 bounded/I/O failure.
buildModuleIndex	.block
	movem.l d1-d7/a0-a2, -(sp)
	clr.w ModuleScanIndexCount.l
	move.w #1, ModuleScanBuildIndex.l
	moveq #0, d7

rootLoop
	cmp.w state.NativeCliModulePathCount, d7
	bhs.s rootsDone
	bsr.w rootWasAlreadyScanned
	tst.l d0
	bne.s nextRoot
	move.l d7, d0
	lsl.l #8, d0
	lea state.NativeCliModulePathTable, a0
	adda.l d0, a0
	lea ModuleScanDirectoryPathTable.l, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s buildFail
	moveq #0, d0
	bsr.w scanDirectory
	tst.l d0
	bne.s buildFail

nextRoot
	addq.w #1, d7
	bra.s rootLoop

rootsDone
	clr.w ModuleScanBuildIndex.l
	move.w #1, ModuleScanIndexBuilt.l
	moveq #0, d0
	bra.s buildReturn

buildFail
	clr.w ModuleScanBuildIndex.l
	clr.w ModuleScanIndexCount.l
	moveq #1, d0

buildReturn
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; buildModuleIndex

; Recursively scan one directory using a depth-owned FileInfoBlock.
; Inputs: D0.W = directory depth; path is in ModuleScanDirectoryPathTable[depth].
; Outputs: D0 = 0 on complete scan, 1 on bounded/I/O/ambiguity failure.
; Clobbers: D0-D7/A0-A5/CCR. CCR: reflects D0 after the epilogue.
scanDirectory	.block
	movem.l d1-d7/a0-a5, -(sp)
	moveq #0, d7
	move.w d0, d7
	cmpi.w #constants.NATIVE_MODULE_SCAN_DEPTH_CAPACITY, d7
	bhs.w scanFail
	move.l d7, d0
	lsl.l #8, d0
	lea ModuleScanDirectoryPathTable.l, a0
	adda.l d0, a0
	jsr dos.lockRead
	tst.l d0
	beq.w scanFail
	movea.l d0, a4
	bsr.w scanFibPtr
	move.l a4, d1
	jsr dos.examine
	tst.l d0
	beq.w scanUnlockFail

entryLoop
	bsr.w scanFibPtr
	move.l a4, d1
	jsr dos.exNext
	tst.l d0
	beq.w entriesDone
	bsr.w scanFibPtr
	lea constants.FIB_FILE_NAME(a0), a2
	tst.b (a2)
	beq.s entryLoop
	cmpi.b #'.', (a2)
	bne.s buildCandidate
	tst.b 1(a2)
	beq.s entryLoop
	cmpi.b #'.', 1(a2)
	bne.s buildCandidate
	tst.b 2(a2)
	beq.s entryLoop

buildCandidate
	move.l d7, d0
	lsl.l #8, d0
	lea ModuleScanDirectoryPathTable.l, a0
	adda.l d0, a0
	lea ModuleScanCandidatePath.l, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w scanUnlockFail
	bsr.w scanFibPtr
	lea constants.FIB_FILE_NAME(a0), a0
	lea ModuleScanCandidatePath.l, a1
	jsr path.opforgeNativeCliAppendPathSegmentBuffer
	bne.w scanUnlockFail
	bsr.w scanFibPtr
	tst.l constants.FIB_DIR_ENTRY_TYPE(a0)
	bgt.w descend
	lea constants.FIB_FILE_NAME(a0), a0
	bsr.w hasModuleSourceExtension
	tst.l d0
	beq.w entryLoop
	movem.l d7, -(sp)
	bsr.w scanCandidateFile
	movem.l (sp)+, d7
	tst.l d0
	bmi.w scanUnlockFail
	tst.w ModuleScanBuildIndex.l
	beq.s targetScanResult
	tst.w ModuleScanSawExplicit.l
	bne.w entryLoop
	bsr.w scanFibPtr
	lea constants.FIB_FILE_NAME(a0), a0
	bsr.w copyFallbackModuleName
	tst.l d0
	bne.w scanUnlockFail
	clr.l ModuleScanCandidateStartOffset.l
	bsr.w recordCatalogCandidate
	tst.l d0
	bne.w scanUnlockFail
	bra.w entryLoop

targetScanResult
	bne.s candidateMatch
	tst.w ModuleScanSawExplicit
	bne.w entryLoop
	bsr.w scanFibPtr
	lea constants.FIB_FILE_NAME(a0), a0
	bsr.w fallbackFileNameMatches
	tst.l d0
	beq.w entryLoop
	clr.l ModuleScanCandidateStartOffset.l

candidateMatch
	bsr.w recordCandidateMatch
	tst.l d0
	bne.w scanUnlockFail
	bra.w entryLoop

descend
	move.l d7, d0
	addq.l #1, d0
	cmpi.l #constants.NATIVE_MODULE_SCAN_DEPTH_CAPACITY, d0
	bhs.w scanUnlockFail
	lsl.l #8, d0
	lea ModuleScanDirectoryPathTable.l, a1
	adda.l d0, a1
	lea ModuleScanCandidatePath.l, a0
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w scanUnlockFail
	move.l d7, d0
	addq.l #1, d0
	bsr.w scanDirectory
	tst.l d0
	bne.w scanUnlockFail
	bra.w entryLoop

entriesDone
	jsr dos.ioErr
	cmpi.l #constants.ERROR_NO_MORE_ENTRIES, d0
	bne.s scanUnlockFail
	move.l a4, d1
	jsr dos.unlock
	moveq #0, d0
	bra.s scanReturn

scanUnlockFail
	move.l a4, d1
	jsr dos.unlock

scanFail
	moveq #1, d0

scanReturn
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; scanDirectory

; Return nonzero when root D7.W duplicates an earlier configured root.
; Inputs: D7.W = current root index. Outputs: D0 = boolean.
; Clobbers: D0-D3/D6/A0-A1/CCR. CCR: reflects D0.
rootWasAlreadyScanned	.block
	moveq #0, d6

compareLoop
	cmp.w d7, d6
	bhs.s unique
	move.l d7, d0
	lsl.l #8, d0
	lea state.NativeCliModulePathTable, a0
	adda.l d0, a0
	move.l d6, d0
	lsl.l #8, d0
	lea state.NativeCliModulePathTable, a1
	adda.l d0, a1
	bsr.w compareFoldedNull
	tst.l d0
	bne.s duplicate
	addq.w #1, d6
	bra.s compareLoop

unique
	moveq #0, d0
	rts

duplicate
	moveq #1, d0
	rts
	.bend  ; rootWasAlreadyScanned

; Return the depth-owned, longword-aligned FileInfoBlock.
; Inputs: D7.W = scan depth. Outputs: A0 = FileInfoBlock.
; Clobbers: D0/A0/CCR. CCR: unspecified.
scanFibPtr	.block
	move.l d7, d0
	mulu #constants.FILE_INFO_BLOCK_SIZE, d0
	lea ModuleScanFibTable.l, a0
	adda.l d0, a0
	rts
	.bend  ; scanFibPtr

; Read one byte through a candidate-owned 8 KiB refill buffer.
; Inputs: D1 = open file handle; cursor/count reset before each candidate.
; Outputs: D0 = 1 and ModuleScanChar set, 0 at EOF, -1 on DOS read failure.
; Clobbers: D0-D3/A0-A1/A6/CCR. CCR: reflects D0.
; Short positive reads are consumed completely before another refill. Read-ahead
; never advances parser offsets; the caller counts only consumed bytes.
readCandidateByte	.block
	moveq #0, d2
	move.w ModuleScanReadCursor.l, d2
	cmp.w ModuleScanReadCount.l, d2
	bcs.s bufferedByte
	lea ModuleScanReadBuffer.l, a0
	move.l #8192, d0
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileClassModuleV1
.endif
	jsr dos.readInput
	tst.l d0
	ble.s readReturn
	move.w d0, ModuleScanReadCount.l
	clr.w ModuleScanReadCursor.l
	moveq #0, d2

bufferedByte
	lea ModuleScanReadBuffer.l, a0
	move.b 0(a0, d2.w), ModuleScanChar.l
	addq.w #1, ModuleScanReadCursor.l
	moveq #1, d0

readReturn
	rts
	.bend  ; readCandidateByte

; Scan one candidate without touching the active CLI source-line state.
; Outputs: D0 = 1 target declaration found, 0 not found, -1 read/line failure.
; Clobbers: D0-D5/A0-A1/CCR. CCR: reflects D0.
scanCandidateFile	.block
	clr.w ModuleScanReadCursor
	clr.w ModuleScanReadCount
	clr.w ModuleScanLineLen
	clr.w ModuleScanSawCr
	clr.w ModuleScanSawExplicit
	clr.w ModuleScanTargetFound
	clr.w ModuleScanTargetOpen
	clr.l ModuleScanByteOffset.l
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileClassModuleV1
	jsr platform_profile.opforgePlatformProfileRecordModuleCandidateV1
.endif
	clr.l ModuleScanLineStartOffset.l
	clr.l ModuleScanCandidateStartOffset.l
	clr.l ModuleScanCandidateEndOffset.l
	lea ModuleScanCandidatePath.l, a0
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileClassModuleV1
.endif
	jsr dos.openInput
	tst.l d0
	beq.w candidateFail
	move.l d0, d5

candidateReadLoop
	move.l d5, d1
.ifdef OPFORGE_MODULE_SCAN_BYTE_READ_REFERENCE
	lea ModuleScanChar.l, a0
	moveq #1, d0
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileClassModuleV1
.endif
	jsr dos.readInput
.else
	bsr.w readCandidateByte
.endif
	cmp.l #-1, d0
	beq.w candidateCloseFail
	tst.l d0
	beq.w candidateEof
	addq.l #1, ModuleScanByteOffset.l
	move.b ModuleScanChar, d0
	tst.w ModuleScanSawCr
	beq.s candidateCheckBreak
	clr.w ModuleScanSawCr
	cmpi.b #10, d0
	bne.s candidateCheckBreak
	move.l ModuleScanByteOffset.l, d0
	move.l d0, ModuleScanLineStartOffset.l
	bra.w candidateReadLoop

candidateCheckBreak
	cmpi.b #10, d0
	beq.s candidateLineDone
	cmpi.b #13, d0
	beq.s candidateCrDone
	move.w ModuleScanLineLen, d1
	cmpi.w #constants.SOURCE_LINE_BUFFER_CAPACITY, d1
	bhs.w candidateCloseFail
	lea ModuleScanLine.l, a1
	move.b d0, 0(a1, d1.w)
	addq.w #1, d1
	move.w d1, ModuleScanLineLen
	bra.w candidateReadLoop

candidateCrDone
	move.w #1, ModuleScanSawCr

candidateLineDone
	move.l d5, -(sp)
	bsr.w scanCurrentModuleLine
	move.l (sp)+, d5
	tst.l d0
	bmi.w candidateCloseFail
	clr.w ModuleScanLineLen
	move.l ModuleScanByteOffset.l, d0
	move.l d0, ModuleScanLineStartOffset.l
	bra.w candidateReadLoop

candidateEof
	tst.w ModuleScanLineLen
	beq.s candidateEofChecked
	move.l d5, -(sp)
	bsr.w scanCurrentModuleLine
	move.l (sp)+, d5
	tst.l d0
	bmi.w candidateCloseFail

candidateEofChecked
	tst.w ModuleScanTargetOpen.l
	beq.s candidateEofClosed
	clr.w ModuleScanTargetOpen.l
	move.l ModuleScanByteOffset.l, d0
	move.l d0, ModuleScanCandidateEndOffset.l
	tst.w ModuleScanBuildIndex.l
	beq.s candidateEofClosed
	bsr.w recordCatalogCandidate
	tst.l d0
	bne.w candidateCloseFail

candidateEofClosed
	tst.w ModuleScanTargetFound.l
	bne.s candidateCloseFound

candidateCloseNo
	move.l d5, d1
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileClassModuleV1
.endif
	jsr dos.close
	move.l ModuleScanByteOffset.l, d0
	move.l d0, ModuleScanCandidateEndOffset.l
	clr.l ModuleScanTargetOpen.l
	clr.l ModuleScanByteOffset.l
	clr.l ModuleScanLineStartOffset.l
	moveq #0, d0
	rts

candidateCloseFound
	move.l d5, d1
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileClassModuleV1
.endif
	jsr dos.close
	clr.l ModuleScanTargetOpen.l
	clr.l ModuleScanByteOffset.l
	clr.l ModuleScanLineStartOffset.l
	moveq #1, d0
	rts

candidateCloseFail
	move.l d5, d1
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileClassModuleV1
.endif
	jsr dos.close

candidateFail
	clr.l ModuleScanTargetOpen.l
	clr.l ModuleScanByteOffset.l
	clr.l ModuleScanLineStartOffset.l
	moveq #-1, d0
	rts
	.bend  ; scanCandidateFile

; Return nonzero for `.asm` or `.inc` filenames, case-insensitively.
; Inputs: A0 = NUL-terminated filename. Outputs: D0 = boolean.
; Clobbers: D0-D3/A0-A1/CCR. CCR: reflects D0.
hasModuleSourceExtension	.block
	movem.l a2, -(sp)
	movea.l a0, a2
	jsr token_util.opforgeNativeCliTokenLen
	cmpi.l #4, d0
	bcs.s extensionNo
	adda.l d0, a2
	suba.l #4, a2
	cmpi.b #'.', (a2)
	bne.s extensionNo
	move.b 1(a2), d1
	bsr.w foldAsciiD1
	cmpi.b #'a', d1
	beq.s extensionAsm
	cmpi.b #'i', d1
	bne.s extensionNo
	move.b 2(a2), d1
	bsr.w foldAsciiD1
	cmpi.b #'n', d1
	bne.s extensionNo
	move.b 3(a2), d1
	bsr.w foldAsciiD1
	cmpi.b #'c', d1
	bne.s extensionNo
	bra.s extensionYes

extensionAsm
	move.b 2(a2), d1
	bsr.w foldAsciiD1
	cmpi.b #'s', d1
	bne.s extensionNo
	move.b 3(a2), d1
	bsr.w foldAsciiD1
	cmpi.b #'m', d1
	bne.s extensionNo

extensionYes
	moveq #1, d0
	bra.s extensionReturn

extensionNo
	moveq #0, d0

extensionReturn
	movem.l (sp)+, a2
	rts
	.bend  ; hasModuleSourceExtension

; Track the requested `.module` block in the private scan line.
; Outputs: D0 = 0 while scanning, -1 for a duplicate/nested/malformed target.
; Clobbers: D0-D7/A0-A3/CCR. CCR: reflects D0.
scanCurrentModuleLine	.block
	lea ModuleScanLine.l, a0
	moveq #0, d0
	move.w ModuleScanLineLen, d0
	bsr.w skipScanWhitespace
	beq.w moduleLineNo
	movea.l a0, a2
	move.l d0, d7
	bsr.w endmoduleDirectiveStartsLine
	tst.l d0
	beq.s checkModuleLine
	tst.w ModuleScanTargetOpen.l
	beq.w moduleLineNo
	clr.w ModuleScanTargetOpen.l
	move.l ModuleScanByteOffset.l, d0
	move.l d0, ModuleScanCandidateEndOffset.l
	tst.w ModuleScanBuildIndex.l
	beq.s endmoduleDone
	bsr.w recordCatalogCandidate
	tst.l d0
	bne.w moduleLineFail

endmoduleDone
	moveq #0, d0
	rts

checkModuleLine
	movea.l a2, a0
	move.l d7, d0
	bsr.w moduleDirectiveStartsLine
	tst.l d0
	beq.w moduleLineNo
	tst.w ModuleScanTargetOpen.l
	beq.s moduleLineNotOpen
	clr.w ModuleScanTargetOpen.l
	move.l ModuleScanLineStartOffset.l, d0
	move.l d0, ModuleScanCandidateEndOffset.l
	tst.w ModuleScanBuildIndex.l
	beq.s moduleLineNotOpen
	bsr.w recordCatalogCandidate
	tst.l d0
	bne.w moduleLineFail

moduleLineNotOpen
	movea.l a2, a0
	move.l d7, d0
	adda.l #7, a0
	subi.l #7, d0
	bsr.w skipScanWhitespace
	beq.w moduleLineNo
	lea ModuleScanName.l, a1
	bsr.w copyScanWord
	tst.l d0
	bne.w moduleLineNo
	tst.b ModuleScanName
	beq.w moduleLineNo
	move.w #1, ModuleScanSawExplicit
	tst.w ModuleScanBuildIndex.l
	beq.s compareRequestedModule
	move.w #1, ModuleScanTargetOpen.l
	move.l ModuleScanLineStartOffset.l, d0
	move.l d0, ModuleScanCandidateStartOffset.l
	moveq #0, d0
	rts

compareRequestedModule
	lea ModuleScanName.l, a0
	lea ModuleScanLookupName.l, a1
	bsr.w compareFoldedNull
	tst.l d0
	beq.w moduleLineNo
	tst.w ModuleScanTargetFound.l
	bne.w moduleLineFail
	move.w #1, ModuleScanTargetFound.l
	move.w #1, ModuleScanTargetOpen.l
	move.l ModuleScanLineStartOffset.l, d0
	move.l d0, ModuleScanCandidateStartOffset.l

moduleLineNo
	moveq #0, d0
	rts

moduleLineFail
	moveq #-1, d0
	rts
	.bend  ; scanCurrentModuleLine

; Skip spaces and tabs in A0/D0. Outputs: advanced A0 and remaining D0.
; Clobbers: D0-D1/A0/CCR. CCR: reflects remaining D0.
skipScanWhitespace	.block
	tst.l d0
	beq.s scanWhitespaceDone

scanWhitespaceLoop
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s scanWhitespaceOne
	cmpi.b #9, d1
	bne.s scanWhitespaceDone

scanWhitespaceOne
	addq.l #1, a0
	subq.l #1, d0
	bne.s scanWhitespaceLoop

scanWhitespaceDone
	rts
	.bend  ; skipScanWhitespace

; Return nonzero when A0/D0 begins with the complete `.module` token.
; Inputs: A0/D0 = trimmed line. Outputs: D0 = boolean.
; Clobbers: D0-D3/A0-A1/A3/CCR. CCR: reflects D0.
moduleDirectiveStartsLine	.block
	cmpi.l #7, d0
	bcs.s moduleDirectiveNo
	movea.l a0, a3
	move.l d0, d3
	lea ModuleDirectiveText.l, a1
	moveq #7, d0
	bsr.w compareFoldedSlice
	tst.l d0
	beq.s moduleDirectiveNo
	cmpi.l #7, d3
	beq.s moduleDirectiveYes
	move.b 7(a3), d1
	cmpi.b #' ', d1
	beq.s moduleDirectiveYes
	cmpi.b #9, d1
	beq.s moduleDirectiveYes
	cmpi.b #';', d1
	beq.s moduleDirectiveYes

moduleDirectiveNo
	moveq #0, d0
	rts

moduleDirectiveYes
	moveq #1, d0
	rts
	.bend  ; moduleDirectiveStartsLine

; Return nonzero when A0/D0 begins with the complete `.endmodule` token.
; Inputs: A0/D0 = trimmed line. Outputs: D0 = boolean.
; Clobbers: D0-D3/A0-A1/A3/CCR. CCR: reflects D0.
endmoduleDirectiveStartsLine	.block
	cmpi.l #10, d0
	bcs.s endmoduleDirectiveNo
	movea.l a0, a3
	move.l d0, d3
	lea EndmoduleDirectiveText.l, a1
	moveq #10, d0
	bsr.w compareFoldedSlice
	tst.l d0
	beq.s endmoduleDirectiveNo
	cmpi.l #10, d3
	beq.s endmoduleDirectiveYes
	move.b 10(a3), d1
	cmpi.b #' ', d1
	beq.s endmoduleDirectiveYes
	cmpi.b #9, d1
	beq.s endmoduleDirectiveYes
	cmpi.b #';', d1
	beq.s endmoduleDirectiveYes

endmoduleDirectiveNo
	moveq #0, d0
	rts

endmoduleDirectiveYes
	moveq #1, d0
	rts
	.bend  ; endmoduleDirectiveStartsLine

; Copy one module-id word from A0/D0 into A1.
; Outputs: D0 = 0 success, 1 capacity failure; destination is terminated.
; Clobbers: D0-D2/D5-D6/A0-A1/CCR. CCR: reflects D0.
copyScanWord	.block
	moveq #constants.TOKEN_BUFFER_CAPACITY - 1, d6
	moveq #0, d5

scanWordLoop
	tst.l d0
	beq.s scanWordDone
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s scanWordDone
	cmpi.b #9, d2
	beq.s scanWordDone
	cmpi.b #';', d2
	beq.s scanWordDone
	cmpi.b #'(', d2
	beq.s scanWordDone
	cmpi.b #',', d2
	beq.s scanWordDone
	tst.l d6
	beq.s scanWordFail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s scanWordLoop

scanWordDone
	clr.b (a1)
	moveq #0, d0
	rts

scanWordFail
	clr.b (a1)
	moveq #1, d0
	rts
	.bend  ; copyScanWord

; Copy the filename stem from A0 into ModuleScanName for declaration-free
; compatibility files. Inputs are already extension-validated.
copyFallbackModuleName	.block
	movea.l a0, a2
	jsr token_util.opforgeNativeCliTokenLen
	subq.l #4, d0
	beq.s fallbackCopyFail
	cmpi.l #constants.TOKEN_BUFFER_CAPACITY, d0
	bhs.s fallbackCopyFail
	lea ModuleScanName.l, a1
	move.l d0, d1

fallbackCopyLoop
	move.b (a2)+, (a1)+
	subq.l #1, d1
	bne.s fallbackCopyLoop
	clr.b (a1)
	moveq #0, d0
	rts

fallbackCopyFail
	moveq #1, d0
	rts
	.bend  ; copyFallbackModuleName

; Append the current declaration/path/range to the bounded discovery index.
; Duplicate names remain separate rows so lookup fails ambiguous exactly as
; the Rust resolver does.
recordCatalogCandidate	.block
	movem.l d1-d4/a0-a2, -(sp)
	moveq #0, d4

indexDedupeLoop
	cmp.w ModuleScanIndexCount.l, d4
	bhs.s indexDedupeDone
	move.l d4, d0
	lsl.l #6, d0
	lea ModuleScanIndexNameTable.l, a0
	adda.l d0, a0
	lea ModuleScanName.l, a1
	bsr.w compareFoldedNull
	tst.l d0
	beq.s indexDedupeNext
	move.l d4, d0
	lsl.l #8, d0
	lea ModuleScanIndexPathTable.l, a0
	adda.l d0, a0
	lea ModuleScanCandidatePath.l, a1
	bsr.w compareFoldedNull
	tst.l d0
	bne.s indexRecordOk

indexDedupeNext
	addq.w #1, d4
	bra.s indexDedupeLoop

indexDedupeDone
	moveq #0, d4
	move.w ModuleScanIndexCount.l, d4
	cmpi.w #constants.NATIVE_MODULE_TABLE_CAPACITY, d4
	bhs.s indexRecordFail
	move.l d4, d0
	lsl.l #6, d0
	lea ModuleScanIndexNameTable.l, a1
	adda.l d0, a1
	lea ModuleScanName.l, a0
	jsr token_util.opforgeNativeCliCopyTokenBuffer
	move.l d4, d0
	lsl.l #8, d0
	lea ModuleScanIndexPathTable.l, a1
	adda.l d0, a1
	lea ModuleScanCandidatePath.l, a0
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s indexRecordFail
	move.l d4, d0
	lsl.l #2, d0
	lea ModuleScanIndexStartTable.l, a0
	move.l ModuleScanCandidateStartOffset.l, 0(a0, d0.l)
	lea ModuleScanIndexEndTable.l, a0
	move.l ModuleScanCandidateEndOffset.l, 0(a0, d0.l)
	addq.w #1, ModuleScanIndexCount.l

indexRecordOk
	moveq #0, d0
	bra.s indexRecordReturn

indexRecordFail
	moveq #1, d0

indexRecordReturn
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; recordCatalogCandidate

; Compare the implicit filename stem against the requested module id.
; Inputs: A0 = filename known to end in `.asm`/`.inc`. Outputs: D0 = boolean.
; Clobbers: D0-D4/A0-A2/CCR. CCR: reflects D0.
fallbackFileNameMatches	.block
	movea.l a0, a2
	jsr token_util.opforgeNativeCliTokenLen
	subq.l #4, d0
	move.l d0, d4
	lea ModuleScanLookupName.l, a1
	jsr token_util.opforgeNativeCliTokenLen
	cmp.l d4, d0
	bne.s fallbackNo
	movea.l a2, a0
	lea ModuleScanLookupName.l, a1
	move.l d4, d0
	bsr.w compareFoldedSlice
	rts

fallbackNo
	moveq #0, d0
	rts
	.bend  ; fallbackFileNameMatches

; Record the current candidate once; a distinct second path is ambiguous.
; Outputs: D0 = 0 on first/same-path match, 1 on ambiguity/copy failure.
; Clobbers: D0-D3/A0-A1/CCR. CCR: reflects D0.
recordCandidateMatch	.block
	tst.w ModuleScanMatchCount
	beq.s firstMatch
	lea ModuleScanCandidatePath.l, a0
	lea ModuleScanFoundPath.l, a1
	bsr.w compareFoldedNull
	tst.l d0
	beq.s ambiguous
	moveq #0, d0
	rts

firstMatch
	lea ModuleScanCandidatePath.l, a0
	lea ModuleScanFoundPath.l, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s matchReturn
	move.l ModuleScanCandidateStartOffset.l, d0
	move.l d0, ModuleScanFoundStartOffset.l
	move.l d0, state.NativeCliResolvedModuleStartOffset.l
	move.l ModuleScanCandidateEndOffset.l, d0
	move.l d0, ModuleScanFoundEndOffset.l
	move.l d0, state.NativeCliResolvedModuleEndOffset.l
	move.w #1, ModuleScanMatchCount
	moveq #0, d0

matchReturn
	rts

ambiguous
	moveq #1, d0
	rts
	.bend  ; recordCandidateMatch

; Case-folded NUL-terminated equality.
; Inputs: A0/A1 = strings. Outputs: D0 = boolean.
; Clobbers: D0-D3/A0-A1/CCR. CCR: reflects D0.
compareFoldedNull	.block
	move.b (a0)+, d1
	bsr.w foldAsciiD1
	move.b d1, d3
	move.b (a1)+, d1
	bsr.w foldAsciiD1
	cmp.b d1, d3
	bne.s foldedNullNo
	tst.b d3
	bne.s compareFoldedNull
	moveq #1, d0
	rts

foldedNullNo
	moveq #0, d0
	rts
	.bend  ; compareFoldedNull

; Case-folded equality for D0 bytes.
; Inputs: A0/A1 = slices, D0 = common length. Outputs: D0 = boolean.
; Clobbers: D0-D3/A0-A1/CCR. CCR: reflects D0.
compareFoldedSlice	.block
	move.l d0, d3
	beq.s foldedSliceYes

foldedSliceLoop
	move.b (a0)+, d1
	bsr.w foldAsciiD1
	move.b d1, d2
	move.b (a1)+, d1
	bsr.w foldAsciiD1
	cmp.b d1, d2
	bne.s foldedSliceNo
	subq.l #1, d3
	bne.s foldedSliceLoop

foldedSliceYes
	moveq #1, d0
	rts

foldedSliceNo
	moveq #0, d0
	rts
	.bend  ; compareFoldedSlice

; Fold one ASCII byte in D1 to lowercase.
; Clobbers: D1/CCR. CCR: reflects the final comparison/addition.
foldAsciiD1	.block
	cmpi.b #'A', d1
	bcs.s foldDone
	cmpi.b #'Z', d1
	bhi.s foldDone
	addi.b #32, d1
foldDone
	rts
	.bend  ; foldAsciiD1
	.endsection

	.section data, kind=data
ModuleDirectiveText
	.byte ".module", 0
EndmoduleDirectiveText
	.byte ".endmodule", 0
	.endsection

	.section bss, kind=bss
	.align 4
ModuleScanReadCursor
	.res word, 1
ModuleScanReadCount
	.res word, 1
ModuleScanReadBuffer
	.res byte, 8192
ModuleScanMatchCount
	.res word, 1
ModuleScanIndexBuilt
	.res word, 1
ModuleScanBuildIndex
	.res word, 1
ModuleScanIndexCount
	.res word, 1
ModuleScanSawExplicit
	.res word, 1
ModuleScanTargetOpen
	.res word, 1
ModuleScanTargetFound
	.res word, 1
ModuleScanSawCr
	.res word, 1
ModuleScanLineLen
	.res word, 1
ModuleScanChar
	.res byte, 1
	.align 4
ModuleScanByteOffset
	.res long, 1
ModuleScanLineStartOffset
	.res long, 1
ModuleScanCandidateStartOffset
	.res long, 1
ModuleScanCandidateEndOffset
	.res long, 1
ModuleScanFoundStartOffset
	.res long, 1
ModuleScanFoundEndOffset
	.res long, 1
ModuleScanLookupName
	.res byte, constants.TOKEN_BUFFER_CAPACITY
ModuleScanName
	.res byte, constants.TOKEN_BUFFER_CAPACITY
ModuleScanFoundPath
	.res byte, constants.PATH_BUFFER_CAPACITY
ModuleScanCandidatePath
	.res byte, constants.PATH_BUFFER_CAPACITY
ModuleScanLine
	.res byte, constants.SOURCE_LINE_BUFFER_CAPACITY
	.align 4
ModuleScanFibTable
	.res byte, constants.NATIVE_MODULE_SCAN_DEPTH_CAPACITY * constants.FILE_INFO_BLOCK_SIZE
ModuleScanDirectoryPathTable
	.res byte, constants.NATIVE_MODULE_SCAN_DEPTH_CAPACITY * constants.PATH_BUFFER_CAPACITY
ModuleScanIndexNameTable
	.res byte, constants.NATIVE_MODULE_TABLE_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
ModuleScanIndexPathTable
	.res byte, constants.NATIVE_MODULE_TABLE_CAPACITY * constants.PATH_BUFFER_CAPACITY
ModuleScanIndexStartTable
	.res long, constants.NATIVE_MODULE_TABLE_CAPACITY
ModuleScanIndexEndTable
	.res long, constants.NATIVE_MODULE_TABLE_CAPACITY

	.endsection
	.endmodule
