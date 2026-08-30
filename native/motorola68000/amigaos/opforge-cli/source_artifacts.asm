; Native post-assembly writer for source-declared output artifacts.

	.module opforge.cli.source_artifacts
	.cpu 68020

	.use opasm.amigaos.engine as engine
	.use opasm.amigaos.layout as layout
	.use opforge.cli.constants
	.use opforge.cli.directive_handlers
	.use opforge.cli.dos
	.use opforge.cli.line_text
	.use opforge.cli.output
	.use opforge.cli.path
	.use opforge.cli.state

NATIVE_SOURCE_MAP_BUFFER_CAPACITY = 8192

	.section code, kind=code
	.pub

; Render every captured root artifact request after the two-pass assembly has
; completed. The captured source line and current assembly state are the only
; authorities; an evidence filename or stored case name never participates.
; @opforge-owner: opforge.cli.output
; @opforge-slice: documentation/plans/slices/native-porting-slice-source-artifact-output.toml
; @opforge-role: implementation
; Outputs: D0.L = 0 on success, 1 when any request cannot be parsed or written.
; Clobbers: D0-D7/A0-A5/CCR.
opforgeNativeCliWriteSourceArtifactsV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	moveq #0, d0
	move.w state.NativeCliArtifactRequestCount, d0
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_CAPACITY, d0
	bhi.w fail
	moveq #0, d7

requestLoop
	cmp.w state.NativeCliArtifactRequestCount, d7
	bhs.w success
	bsr.w restoreRequestLineV1
	bne.w fail
	lea state.NativeCliArtifactRequestKinds, a0
	moveq #0, d6
	move.b 0(a0, d7.l), d6
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_OUTPUT, d6
	beq.w outputRequest
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_MAPFILE, d6
	beq.w mapRequest
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_EXPORTSECTIONS, d6
	beq.w exportRequest
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_METADATA_LIST, d6
	beq.w metadataListRequest
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_METADATA_HEX, d6
	beq.w metadataHexRequest
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_METADATA_BIN, d6
	beq.w nextRequest
	bra.w fail

outputRequest
	jsr directive_handlers.opforgeNativeCliParseOutputLine
	bne.w fail
	bsr.w selectedOutputPathV1
	bsr.w ensureParentDirectoryV1
	bne.w fail
	jsr output.opforgeNativeCliWriteFlatOutput
	bne.w fail
	bra.w nextRequest

mapRequest
	bsr.w parseMapRequestV1
	bne.w fail
	lea state.NativeCliMapPath, a0
	bsr.w ensureParentDirectoryV1
	bne.w fail
	bsr.w buildMapArtifactV1
	bne.w fail
	lea state.NativeCliMapPath, a0
	bsr.w writeMapArtifactV1
	bne.w fail
	bra.w nextRequest

exportRequest
	bsr.w parseExportRequestV1
	bne.w fail
	bsr.w ensureExportDirectoryV1
	bne.w fail
	bsr.w writeExportedSectionsV1
	bne.w fail
	bra.w nextRequest

metadataListRequest
	bsr.w selectMetadataListPathV1
	bne.w fail
	move.w #constants.NATIVE_OUTPUT_FORMAT_LST, state.NativeCliOutputFormat
	lea state.NativeCliLstPath, a0
	bsr.w ensureParentDirectoryV1
	bne.w fail
	jsr output.opforgeNativeCliWriteFlatOutput
	bne.w fail
	bra.w nextRequest

metadataHexRequest
	bsr.w selectMetadataHexPathV1
	bne.w fail
	move.w #constants.NATIVE_OUTPUT_FORMAT_HEX, state.NativeCliOutputFormat
	lea state.NativeCliHexPath, a0
	bsr.w ensureParentDirectoryV1
	bne.w fail
	jsr output.opforgeNativeCliWriteFlatOutput
	bne.w fail

nextRequest
	addq.w #1, d7
	bra.w requestLoop

success
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; opforgeNativeCliWriteSourceArtifactsV1

; Restore request D7.W into the authoritative current-source-line buffers.
restoreRequestLineV1	.block
	moveq #0, d0
	move.w d7, d0
	add.l d0, d0
	lea state.NativeCliArtifactRequestLengths, a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY, d1
	bhi.s fail
	move.w d1, state.NativeCliSourceLineLen
	move.l d7, d0
	lsl.l #8, d0
	add.l d0, d0
	lea state.NativeCliArtifactRequestTexts, a0
	adda.l d0, a0
	lea state.NativeCliSourceLine, a1
	move.l d1, d0
	beq.s terminate
	subq.l #1, d0
copy
	move.b (a0)+, (a1)+
	dbra d0, copy
terminate
	clr.b (a1)
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; restoreRequestLineV1

; Return A0 pointing at the selected path for the parsed source `.output`.
selectedOutputPathV1	.block
	move.w state.NativeCliOutputFormat, d0
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d0
	beq.s prg
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HEX, d0
	beq.s hex
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_LST, d0
	beq.s lst
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, d0
	beq.s hunk
	lea state.NativeCliBinPath, a0
	rts
prg
	lea state.NativeCliPrgPath, a0
	rts
hex
	lea state.NativeCliHexPath, a0
	rts
lst
	lea state.NativeCliLstPath, a0
	rts
hunk
	lea state.NativeCliHunkPath, a0
	rts
	.bend  ; selectedOutputPathV1

; Derive Rust's default metadata listing path by replacing the root source
; suffix with `.lst` while preserving the AmigaDOS volume/directory spelling.
selectMetadataListPathV1	.block
	movem.l d1-d3/a0-a2, -(sp)
	lea state.NativeCliInputPath, a0
	lea state.NativeCliLstPath, a1
	movea.l a1, a2
	moveq #0, d3
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d2
copy
	move.b (a0)+, d1
	beq.s copied
	tst.l d2
	beq.s fail
	move.b d1, (a1)+
	subq.l #1, d2
	cmpi.b #':', d1
	beq.s resetDot
	cmpi.b #'/', d1
	beq.s resetDot
	cmpi.b #'.', d1
	bne.s copy
	movea.l a1, a2
	subq.l #1, a2
	bra.s copy
resetDot
	movea.l a1, a2
	bra.s copy
copied
	movea.l a2, a1
	move.b #'.', (a1)+
	move.b #'l', (a1)+
	move.b #'s', (a1)+
	move.b #'t', (a1)+
	clr.b (a1)
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d3/a0-a2
	rts
	.bend  ; selectMetadataListPathV1

; Resolve `.hex "name"` as Rust's `name.hex` artifact next to the root source.
selectMetadataHexPathV1	.block
	movem.l d1-d4/a0-a2, -(sp)
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	cmpi.l #4, d0
	blo.s fail
	adda.l #4, a0
	subq.l #4, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliArtifactPathScratch, a1
	bsr.w copyQuotedValueV1
	bne.s fail
	movea.l a1, a2
	move.l #constants.PATH_BUFFER_CAPACITY - 5, d4
end
	tst.b (a2)
	beq.s append
	addq.l #1, a2
	subq.l #1, d4
	bne.s end
	bra.s fail
append
	move.b #'.', (a2)+
	move.b #'h', (a2)+
	move.b #'e', (a2)+
	move.b #'x', (a2)+
	clr.b (a2)
	lea state.NativeCliArtifactPathScratch, a0
	lea state.NativeCliHexPath, a1
	jsr directive_handlers.opforgeNativeCliResolveOutputPath
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; selectMetadataHexPathV1

; Ensure the one parent directory named by A0 exists. The mounted case root and
; any pre-existing ancestors remain authoritative; this creates only the final
; parent component required by an artifact path.
ensureParentDirectoryV1	.block
	movem.l d1-d4/a0-a2, -(sp)
	lea state.NativeCliArtifactPathScratch, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	lea state.NativeCliArtifactPathScratch, a1
	movea.l a1, a2
	suba.l a0, a0
scan
	move.b (a2), d0
	beq.s scanned
	cmpi.b #'/', d0
	bne.s advance
	movea.l a2, a0
advance
	addq.l #1, a2
	bra.s scan
scanned
	move.l a0, d0
	beq.s success
	clr.b (a0)
	lea state.NativeCliArtifactPathScratch, a0
	jsr dos.createDir
	tst.l d0
	bne.s unlockSuccess
	lea state.NativeCliArtifactPathScratch, a0
	jsr dos.lockRead
	tst.l d0
	beq.s fail
unlockSuccess
	move.l d0, d1
	jsr dos.unlock
success
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; ensureParentDirectoryV1

; Parse `.mapfile "path" [, symbols=all|public|none]`.
parseMapRequestV1	.block
	movem.l d1-d5/a0-a2, -(sp)
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	cmpi.l #8, d0
	blo.w fail
	adda.l #8, a0
	subq.l #8, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliArtifactPathScratch, a1
	bsr.w copyQuotedValueV1
	bne.w fail
	lea state.NativeCliArtifactPathScratch, a0
	lea state.NativeCliMapPath, a1
	jsr directive_handlers.opforgeNativeCliResolveOutputPath
	bne.w fail
	clr.w NativeSourceMapSymbolsMode
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea MapSymbolsAllToken, a1
	moveq #11, d1
	bsr.w containsFoldedTokenV1
	tst.l d0
	bne.s symbolsAll
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea MapSymbolsPublicToken, a1
	moveq #14, d1
	bsr.w containsFoldedTokenV1
	tst.l d0
	bne.s symbolsPublic
	bra.s success
symbolsAll
	move.w #constants.NATIVE_MAP_SYMBOLS_ALL, NativeSourceMapSymbolsMode
	bra.s success
symbolsPublic
	move.w #constants.NATIVE_MAP_SYMBOLS_PUBLIC, NativeSourceMapSymbolsMode
success
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d5/a0-a2
	rts
	.bend  ; parseMapRequestV1

; Parse `.exportsections dir="path", format=bin [, include=bss]`.
parseExportRequestV1	.block
	movem.l d1-d5/a0-a2, -(sp)
	clr.w NativeSourceExportIncludeBss
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea ExportDirToken, a1
	moveq #4, d1
	bsr.w findFoldedTokenV1
	tst.l d1
	beq.w fail
	lea state.NativeCliArtifactPathScratch, a1
	bsr.w copyQuotedValueV1
	bne.w fail
	lea state.NativeCliArtifactPathScratch, a0
	lea state.NativeCliExportDirPath, a1
	jsr directive_handlers.opforgeNativeCliResolveOutputPath
	bne.w fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea ExportFormatBinToken, a1
	moveq #10, d1
	bsr.w containsFoldedTokenV1
	tst.l d0
	beq.s fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea ExportIncludeBssToken, a1
	moveq #11, d1
	bsr.w containsFoldedTokenV1
	tst.l d0
	beq.s success
	move.w #1, NativeSourceExportIncludeBss
success
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d5/a0-a2
	rts
	.bend  ; parseExportRequestV1

; Copy a quoted value at A0/D0 into A1. Outputs D0 status; A0/D0 advance.
copyQuotedValueV1	.block
	cmpi.l #2, d0
	blo.s fail
	cmpi.b #'"', (a0)+
	bne.s fail
	subq.l #1, d0
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d2
copy
	tst.l d0
	beq.s fail
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'"', d1
	beq.s done
	tst.l d2
	beq.s fail
	move.b d1, (a1)+
	subq.l #1, d2
	bra.s copy
done
	clr.b (a1)
	moveq #0, d1
	moveq #0, d2
	rts
fail
	clr.b (a1)
	moveq #1, d0
	rts
	.bend  ; copyQuotedValueV1

; Find folded token A1/D1 within A0/D0. On success A0/D0 point just after it
; and D1=1; otherwise D1=0.
findFoldedTokenV1	.block
	movem.l d2-d6/a1-a3, -(sp)
	movea.l a1, a3
	move.l d1, d6
scan
	cmp.l d6, d0
	blo.s no
	movea.l a0, a1
	movea.l a3, a2
	move.l d6, d2
compare
	move.b (a1)+, d3
	move.b (a2)+, d4
	bsr.w foldD3D4V1
	cmp.b d4, d3
	bne.s next
	subq.l #1, d2
	bne.s compare
	adda.l d6, a0
	sub.l d6, d0
	moveq #1, d1
	bra.s return
next
	addq.l #1, a0
	subq.l #1, d0
	bra.s scan
no
	moveq #0, d1
return
	movem.l (sp)+, d2-d6/a1-a3
	rts
	.bend  ; findFoldedTokenV1

; Return D0=1 when folded token A1/D1 occurs within A0/D0.
containsFoldedTokenV1	.block
	bsr.w findFoldedTokenV1
	move.l d1, d0
	rts
	.bend  ; containsFoldedTokenV1

foldD3D4V1	.block
	cmpi.b #'A', d3
	blo.s foldSecond
	cmpi.b #'Z', d3
	bhi.s foldSecond
	ori.b #$20, d3
foldSecond
	cmpi.b #'A', d4
	blo.s done
	cmpi.b #'Z', d4
	bhi.s done
	ori.b #$20, d4
done
	rts
	.bend  ; foldD3D4V1

ensureExportDirectoryV1	.block
	movem.l d1/a0, -(sp)
	lea state.NativeCliExportDirPath, a0
	bsr.w ensureParentDirectoryV1
	bne.s fail
	lea state.NativeCliExportDirPath, a0
	jsr dos.createDir
	tst.l d0
	bne.s unlockSuccess
	lea state.NativeCliExportDirPath, a0
	jsr dos.lockRead
	tst.l d0
	beq.s fail
unlockSuccess
	move.l d0, d1
	jsr dos.unlock
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1/a0
	rts
	.bend  ; ensureExportDirectoryV1

; Write one `<section>.bin` for every retained section selected by the request.
writeExportedSectionsV1	.block
	movem.l d1-d7/a0-a4, -(sp)
	jsr layout.getSectionCountV1
	move.l d0, NativeSourceExportSectionCount
	clr.l NativeSourceExportSectionIndex
sectionLoop
	move.l NativeSourceExportSectionIndex, d6
	cmp.l NativeSourceExportSectionCount, d6
	bhs.w success
	move.w d6, d5
	jsr layout.getSectionInfoV1
	move.l d0, d4
	move.l d1, d3
	move.l d2, d0
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d0
	bne.s selected
	tst.w NativeSourceExportIncludeBss
	beq.w next
	clr.l d3
selected
	move.l d4, NativeSourceExportSectionBase
	move.l d3, NativeSourceExportSectionSize
	move.w d6, d5
	jsr layout.getSectionNameV1
	move.l d0, d2
	lea state.NativeCliSourceOutputSectionNames, a1
	move.l d2, d1
	beq.w fail
	subq.l #1, d1
copySelectedName
	move.b (a0)+, (a1)+
	dbra d1, copySelectedName
	clr.b (a1)
	move.w #1, state.NativeCliSourceOutputSectionCount
	clr.w state.NativeCliSourceOutputImageSet
	clr.w state.NativeCliSourceOutputFillSet
	move.w #constants.NATIVE_OUTPUT_FORMAT_BIN, state.NativeCliOutputFormat
	lea state.NativeCliExportDirPath, a0
	lea state.NativeCliArtifactPathScratch, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w fail
	lea state.NativeCliArtifactPathScratch, a0
	lea state.NativeCliSourceOutputSectionNames, a1
	move.l #constants.PATH_BUFFER_CAPACITY, d0
	jsr dos.addPart
	beq.w fail
	lea ExportBinSuffix, a0
	lea state.NativeCliArtifactPathScratch, a1
	jsr path.opforgeNativeCliAppendPathBuffer
	bne.w fail
	lea state.NativeCliArtifactPathScratch, a0
	jsr dos.openOutput
	tst.l d0
	beq.w fail
	move.l d0, NativeSourceExportHandle
	move.l NativeSourceExportSectionSize, d3
	beq.s closeExport
	jsr engine.opasmEngineGetSessionOriginV1
	move.l NativeSourceExportSectionBase, d1
	sub.l d0, d1
	bcs.w closeExportFail
	move.l d1, d0
	add.l d3, d0
	bcs.w closeExportFail
	move.l d0, d1
	jsr engine.opasmEngineGetImageByteCountV1
	cmp.l d0, d1
	bhi.w closeExportFail
	move.l NativeSourceExportSectionBase, d1
	jsr engine.opasmEngineGetSessionOriginV1
	sub.l d0, d1
	jsr engine.opasmEngineGetImageBufferPtrV1
	adda.l d1, a0
	move.l d3, d0
	move.l NativeSourceExportHandle, d1
	jsr dos.writeOutput
	move.l NativeSourceExportSectionSize, d3
	cmp.l d3, d0
	bne.w closeExportFail
closeExport
	move.l NativeSourceExportHandle, d1
	jsr dos.close
next
	move.l NativeSourceExportSectionIndex, d6
	addq.l #1, d6
	move.l d6, NativeSourceExportSectionIndex
	bra.w sectionLoop
success
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d7/a0-a4
	rts
closeExportFail
	move.l NativeSourceExportHandle, d1
	jsr dos.close
	moveq #1, d0
	bra.s return
	.bend  ; writeExportedSectionsV1
; Build the Rust mapfile text for the retained 6502/65C02 layout state.
buildMapArtifactV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	lea NativeSourceMapBuffer, a5
	movea.l a5, a4
	lea MapRegionsHeader, a0
	bsr.w mapAppendCStringV1
	jsr layout.getRegionCountV1
	move.l d0, d7
	moveq #0, d6
regionLoop
	cmp.l d7, d6
	bhs.s regionsDone
	move.w d6, d5
	jsr layout.getRegionNameV1
	bsr.w mapAppendBytesV1
	bsr.w mapAppendSpaceV1
	move.w d6, d5
	jsr layout.getRegionInfoV1
	movem.l d0-d3, -(sp)
	bsr.w mapAppendAddressV1
	bsr.w mapAppendSpaceV1
	move.l 4(sp), d0
	bsr.w mapAppendAddressV1
	bsr.w mapAppendSpaceV1
	move.l 8(sp), d0
	sub.l (sp), d0
	bsr.w mapAppendDecimalV1
	bsr.w mapAppendSpaceV1
	move.l 4(sp), d0
	sub.l (sp), d0
	addq.l #1, d0
	move.l 8(sp), d1
	sub.l (sp), d1
	sub.l d1, d0
	bsr.w mapAppendDecimalV1
	bsr.w mapAppendSpaceV1
	move.l 12(sp), d0
	bsr.w mapAppendDecimalV1
	bsr.w mapAppendNewlineV1
	movem.l (sp)+, d0-d3
	addq.l #1, d6
	bra.s regionLoop
regionsDone
	bsr.w mapAppendNewlineV1
	lea MapSectionsHeader, a0
	bsr.w mapAppendCStringV1
	jsr layout.getSectionCountV1
	move.l d0, d7
	moveq #0, d6
sectionLoop
	cmp.l d7, d6
	bhs.s sectionsDone
	move.w d6, d5
	jsr layout.getSectionNameV1
	bsr.w mapAppendBytesV1
	bsr.w mapAppendSpaceV1
	move.w d6, d5
	jsr layout.getSectionInfoV1
	movem.l d0-d3, -(sp)
	bsr.w mapAppendAddressV1
	bsr.w mapAppendSpaceV1
	move.l 4(sp), d0
	bsr.w mapAppendDecimalV1
	bsr.w mapAppendSpaceV1
	move.l 8(sp), d0
	bsr.w mapAppendSectionKindV1
	bsr.w mapAppendSpaceV1
	move.l 12(sp), d5
	cmpi.w #layout.OPASM_LAYOUT_INDEX_NONE, d5
	beq.s noRegion
	jsr layout.getRegionNameV1
	bsr.w mapAppendBytesV1
	bra.s regionDone
noRegion
	move.b #'-', (a4)+
regionDone
	bsr.w mapAppendNewlineV1
	movem.l (sp)+, d0-d3
	addq.l #1, d6
	bra.s sectionLoop
sectionsDone
	tst.w NativeSourceMapSymbolsMode
	beq.s finish
	bsr.w mapAppendNewlineV1
	lea MapSymbolsHeader, a0
	bsr.w mapAppendCStringV1
	bsr.w mapAppendSymbolsV1
finish
	move.l a4, d1
	move.l a5, d0
	sub.l d0, d1
	cmpi.l #NATIVE_SOURCE_MAP_BUFFER_CAPACITY, d1
	bhi.s fail
	move.l d1, NativeSourceMapLength
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; buildMapArtifactV1

writeMapArtifactV1	.block
	movem.l d1-d4/a0, -(sp)
	jsr dos.openOutput
	tst.l d0
	beq.s fail
	move.l d0, d4
	lea NativeSourceMapBuffer, a0
	move.l NativeSourceMapLength, d0
	move.l d0, d3
	move.l d4, d1
	jsr dos.writeOutput
	cmp.l d3, d0
	bne.s closeFail
	move.l d4, d1
	jsr dos.close
	moveq #0, d0
	bra.s return
closeFail
	move.l d4, d1
	jsr dos.close
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d4/a0
	rts
	.bend  ; writeMapArtifactV1

; Emit labels in Rust's ASCII-case-folded lexical order.
mapAppendSymbolsV1	.block
	movem.l d1-d7/a0-a3, -(sp)
	jsr engine.opasmEngineGetLabelCountV1
	move.l d0, d6
	clr.l d5
nextSymbol
	moveq #-1, d7
	moveq #0, d4
scan
	cmp.l d6, d4
	bhs.s selected
	btst d4, d5
	bne.s scanNext
	cmpi.w #constants.NATIVE_MAP_SYMBOLS_PUBLIC, NativeSourceMapSymbolsMode
	bne.s eligible
	move.l d4, d0
	bsr.w labelIsPublicV1
	tst.l d0
	beq.s scanNext
eligible
	tst.l d7
	bmi.s choose
	move.l d4, d0
	jsr engine.opasmEngineGetLabelNameV1
	movea.l a0, a3
	move.l d7, d0
	jsr engine.opasmEngineGetLabelNameV1
	movea.l a0, a1
	movea.l a3, a0
	bsr.w compareFoldedNamesV1
	tst.l d0
	bpl.s scanNext
choose
	move.l d4, d7
scanNext
	addq.l #1, d4
	bra.s scan
selected
	tst.l d7
	bmi.s done
	bset d7, d5
	bsr.w appendRootModuleNameV1
	move.b #'.', (a4)+
	move.l d7, d0
	jsr engine.opasmEngineGetLabelNameV1
	bsr.w mapAppendCStringV1
	bsr.w mapAppendSpaceV1
	move.l d7, d0
	jsr engine.opasmEngineGetLabelPlacedValueV1
	bsr.w mapAppendAddressV1
	bsr.w mapAppendSpaceV1
	move.l d7, d0
	bsr.w labelIsPublicV1
	tst.l d0
	beq.s private
	lea MapPublicText, a0
	bra.s visibility
private
	lea MapPrivateText, a0
visibility
	bsr.w mapAppendCStringV1
	bsr.w mapAppendNewlineV1
	bra.w nextSymbol
done
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; mapAppendSymbolsV1

appendRootModuleNameV1	.block
	moveq #0, d0
	move.w state.NativeCliRootModuleId, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a0
	adda.l d0, a0
	bsr.w mapAppendCStringV1
	rts
	.bend  ; appendRootModuleNameV1

; Return D0=1 when label index D0 is a public export of the root module.
labelIsPublicV1	.block
	movem.l d1-d7/a0-a3, -(sp)
	move.l d0, d7
	jsr engine.opasmEngineGetLabelNameV1
	movea.l a0, a3
	moveq #0, d6
loop
	cmp.w state.NativeCliOrdinaryExportCount, d6
	bhs.s no
	move.l d6, d0
	add.l d0, d0
	lea state.NativeCliOrdinaryExportOwnerTable, a0
	move.w 0(a0, d0.l), d1
	cmp.w state.NativeCliRootModuleId, d1
	bne.s next
	move.l d6, d0
	lsl.l #2, d0
	lea state.NativeCliOrdinaryExportNameOffsetTable, a1
	move.l 0(a1, d0.l), d0
	lea state.NativeCliOrdinaryExportNamePool, a1
	adda.l d0, a1
	movea.l a3, a0
	bsr.w namesEqualFoldedV1
	tst.l d0
	bne.s yes
next
	addq.l #1, d6
	bra.s loop
yes
	moveq #1, d0
	bra.s return
no
	moveq #0, d0
return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; labelIsPublicV1

; Compare NUL strings A0 and A1 case-insensitively. D0=-1/0/1.
compareFoldedNamesV1	.block
loop
	moveq #0, d2
	moveq #0, d3
	move.b (a0)+, d2
	move.b (a1)+, d3
	cmpi.b #'A', d2
	blo.s foldRight
	cmpi.b #'Z', d2
	bhi.s foldRight
	ori.b #$20, d2
foldRight
	cmpi.b #'A', d3
	blo.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	ori.b #$20, d3
compare
	cmp.b d3, d2
	blo.s less
	bhi.s greater
	tst.b d2
	bne.s loop
	moveq #0, d0
	rts
less
	moveq #-1, d0
	rts
greater
	moveq #1, d0
	rts
	.bend  ; compareFoldedNamesV1

namesEqualFoldedV1	.block
	bsr.w compareFoldedNamesV1
	tst.l d0
	bne.s no
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; namesEqualFoldedV1

mapAppendSectionKindV1	.block
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d0
	beq.s bss
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_DATA, d0
	beq.s data
	lea MapCodeText, a0
	bra.s append
data
	lea MapDataText, a0
	bra.s append
bss
	lea MapBssText, a0
append
	bsr.w mapAppendCStringV1
	rts
	.bend  ; mapAppendSectionKindV1

mapAppendAddressV1	.block
	movem.l d0-d3/a0, -(sp)
	lea HexDigits, a0
	move.l d0, d2
	moveq #3, d3
loop
	rol.w #4, d2
	move.l d2, d0
	andi.l #$F, d0
	move.b 0(a0, d0.l), (a4)+
	dbra d3, loop
	movem.l (sp)+, d0-d3/a0
	rts
	.bend  ; mapAppendAddressV1

; The active proof corpus is 6502/65C02, so layout counts fit an unsigned word.
mapAppendDecimalV1	.block
	movem.l d0-d5, -(sp)
	move.l d0, d1
	moveq #0, d5
	move.w #10000, d2
	bsr.s decimalDigit
	move.w #1000, d2
	bsr.s decimalDigit
	move.w #100, d2
	bsr.s decimalDigit
	move.w #10, d2
	bsr.s decimalDigit
	move.l d1, d0
	addi.b #'0', d0
	move.b d0, (a4)+
	movem.l (sp)+, d0-d5
	rts
decimalDigit
	move.l d1, d0
	divu.w d2, d0
	move.l d0, d3
	andi.l #$FFFF, d3
	swap d0
	andi.l #$FFFF, d0
	move.l d0, d1
	tst.l d3
	bne.s emit
	tst.l d5
	beq.s skip
emit
	moveq #1, d5
	move.l d3, d4
	addi.b #'0', d4
	move.b d4, (a4)+
skip
	rts
	.bend  ; mapAppendDecimalV1

mapAppendBytesV1	.block
	tst.l d0
	beq.s done
	subq.l #1, d0
loop
	move.b (a0)+, (a4)+
	dbra d0, loop
done
	rts
	.bend  ; mapAppendBytesV1

mapAppendCStringV1	.block
	tst.b (a0)
	beq.s done
loop
	move.b (a0)+, (a4)+
	tst.b (a0)
	bne.s loop
done
	rts
	.bend  ; mapAppendCStringV1

mapAppendSpaceV1	.block
	move.b #' ', (a4)+
	rts
	.bend  ; mapAppendSpaceV1

mapAppendNewlineV1	.block
	move.b #10, (a4)+
	rts
	.bend  ; mapAppendNewlineV1

	.endsection

	.section data, kind=data

MapSymbolsAllToken
	.byte "symbols=all"
MapSymbolsPublicToken
	.byte "symbols=public"
ExportDirToken
	.byte "dir="
ExportFormatBinToken
	.byte "format=bin"
ExportIncludeBssToken
	.byte "include=bss"
ExportBinSuffix
	.byte ".bin", 0
MapRegionsHeader
	.byte "Regions", 10, "name start end used free align", 10, 0
MapSectionsHeader
	.byte "Sections", 10, "name base size kind region", 10, 0
MapSymbolsHeader
	.byte "Symbols", 10, "name value visibility", 10, 0
MapCodeText
	.byte "code", 0
MapDataText
	.byte "data", 0
MapBssText
	.byte "bss", 0
MapPublicText
	.byte "public", 0
MapPrivateText
	.byte "private", 0
HexDigits
	.byte "0123456789ABCDEF"

	.endsection

	.section bss, kind=bss
	.align 4

NativeSourceMapSymbolsMode
	.res word, 1
NativeSourceExportIncludeBss
	.res word, 1
	.align 4
NativeSourceExportSectionCount
	.res long, 1
NativeSourceExportSectionIndex
	.res long, 1
NativeSourceExportSectionBase
	.res long, 1
NativeSourceExportSectionSize
	.res long, 1
NativeSourceExportHandle
	.res long, 1
NativeSourceMapLength
	.res long, 1
NativeSourceMapBuffer
	.res byte, NATIVE_SOURCE_MAP_BUFFER_CAPACITY

	.endsection
	.endmodule
