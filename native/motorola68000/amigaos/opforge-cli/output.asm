; Native AmigaOS opForge CLI flat output writer.

	.module opforge.cli.output
	.cpu 68020

	.use opasm.amigaos.output_artifacts as artifacts
	.use opasm.amigaos.layout as layout
	.use opforge.cli.state
	.use opforge.cli.constants
	.use opforge.cli.dos
	.use opforge.cli.strings
	.use opforge.cli.hunk_output as hunk_output

	.section code, kind=code
	.pub

NATIVE_SOURCE_OUTPUT_BUFFER_CAPACITY = constants.NATIVE_IMAGE_BUFFER_CAPACITY + 2

; Inputs:
;   state.NativeCliOutputFormat selects the artifact/path pair.
;   opasm output artifact layer can render the selected flat output
; Outputs:
;   D0.L = 0 on success, 1 when the output file cannot be opened or written fully
; Clobbers:
;   D0-D4/A0-A2/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliWriteFlatOutput	.block
	movem.l d1-d4/a0-a2, -(sp)
	tst.w state.NativeCliSourceOutputSectionCount
	beq.s buildDefaultArtifact
	move.w state.NativeCliOutputFormat, d0
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_BIN, d0
	beq.s buildSourceArtifact
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d0
	beq.s buildSourceArtifact
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, d0
	bne.s buildDefaultArtifact
	bsr.w rejectUnsupportedHunkFixupV1
	bne.w unsupportedArtifactFail
	jsr hunk_output.buildSelectedSectionsV1
	bra.s artifactBuilt

buildSourceArtifact
	bsr.w buildSelectedSourceArtifactV1
	bra.s artifactBuilt

buildDefaultArtifact
	move.w state.NativeCliOutputFormat, d0
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_BIN, d0
	beq.s buildBin
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, d0
	beq.s buildHunk
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HEX, d0
	beq.s buildHex
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_LST, d0
	beq.s buildLst
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d0
	bne.w artifactFail
	moveq #-1, d2
	tst.w state.NativeCliPrgLoadAddrSet
	beq.s buildPrg
	move.l state.NativeCliPrgLoadAddr, d2

buildPrg
	jsr artifacts.opasmOutputBuildPrgArtifactV1
	bra.s artifactBuilt

buildHex
	jsr artifacts.opasmOutputBuildHexArtifactV1
	bra.s artifactBuilt

buildLst
	jsr artifacts.opasmOutputBuildListingArtifactV1
	bra.s artifactBuilt

buildBin
	jsr artifacts.opasmOutputBuildBinArtifactV1
	bra.s artifactBuilt

buildHunk
	bsr.w rejectUnsupportedHunkFixupV1
	bne.w unsupportedArtifactFail
	jsr hunk_output.buildFlatCodeV1

artifactBuilt
	bne.w artifactFail
	movea.l a0, a2
	move.l d1, d3
payloadReady
	move.w state.NativeCliOutputFormat, d0
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_BIN, d0
	beq.s payloadBinPath
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, d0
	beq.s payloadHunkPath
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HEX, d0
	beq.s payloadHexPath
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_LST, d0
	beq.s payloadLstPath
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d0
	bne.s artifactFail
	lea state.NativeCliPrgPath, a0
	bra.s payloadOpen

payloadBinPath
	lea state.NativeCliBinPath, a0
	bra.s payloadOpen

payloadHunkPath
	lea state.NativeCliHunkPath, a0
	bra.s payloadOpen

payloadHexPath
	lea state.NativeCliHexPath, a0
	bra.s payloadOpen

payloadLstPath
	lea state.NativeCliLstPath, a0

payloadOpen
	jsr dos.openOutput
	tst.l d0
	beq.s openFail
	move.l d0, d4
	movea.l a2, a0
	move.l d3, d0
	move.l d4, d1
	jsr dos.writeOutput
	cmp.l d3, d0
	bne.s writeFail
	move.l d4, d1
	jsr dos.close
	moveq #0, d0
	bra.s return

artifactFail
	move.l #strings.NativeOutputArtifactFailureText, d1
	jsr dos.putErrStr
	bra.s fail

unsupportedArtifactFail
	moveq #1, d0
	bra.s return

writeFail
	move.l d4, d1
	jsr dos.close
	move.l #strings.NativeOutputShortWriteText, d1
	jsr dos.putErrStr
	bra.s fail

openFail
	move.l #strings.NativeOutputOpenFailureText, d1
	jsr dos.putErrStr

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; opforgeNativeCliWriteFlatOutput

; Render the format-specific diagnostic retained by the neutral layout layer.
; Rust records this condition during assembly but rejects only when building a
; Hunk artifact; native follows the same boundary so BIN/listing output remains
; unaffected by unsupported executable-Hunk relocation shapes.
; @opforge-owner: opforge.cli.output
; @opforge-slice: documentation/plans/slices/native-porting-slice-hunk-fixup-relocation-v1.toml
; @opforge-role: facade
; Outputs: D0=0 when clear, 1 after emitting the retained Hunk diagnostic.
rejectUnsupportedHunkFixupV1	.block
	movem.l d4-d5/a0, -(sp)
	jsr layout.getUnsupportedHunkFixupV1
	tst.w d0
	beq.w unsupportedHunkClear
	cmpi.w #2, d0
	beq.s unsupportedHunkEmit
	move.l #strings.NativeHunkUnsupportedLongFixupText, d1
	jsr dos.putErrStr
	bra.s unsupportedHunkSection
unsupportedHunkEmit
	move.l #strings.NativeHunkUnsupportedEmitLongFixupText, d1
	jsr dos.putErrStr

unsupportedHunkSection
	jsr layout.getUnsupportedHunkFixupV1
	moveq #0, d5
	move.w d1, d5
	beq.s unsupportedHunkNewline
	lea NativeHunkSectionNameScratch.l, a1
unsupportedHunkSectionCopy
	move.b (a0)+, (a1)+
	subq.l #1, d5
	bne.s unsupportedHunkSectionCopy
	clr.b (a1)
	move.l #strings.NativeHunkSectionSeparatorText, d1
	jsr dos.putErrStr
	move.l #NativeHunkSectionNameScratch, d1
	jsr dos.putErrStr
unsupportedHunkNewline
	move.l #strings.NewlineText, d1
	jsr dos.putErrStr
	moveq #1, d0
	movem.l (sp)+, d4-d5/a0
	rts
unsupportedHunkClear
	moveq #0, d0
	movem.l (sp)+, d4-d5/a0
	rts
	.bend  ; rejectUnsupportedHunkFixupV1

; Build a source-declared BIN/PRG payload from its selected placed sections.
; The source directive's exact section list is authoritative; bytes are taken
; from the current native image by retained section base/size, never by a
; reference filename or stored case name.
; Outputs: D0.L = status; A0 = payload; D1.L = byte count.
; Clobbers: D0-D7/A0-A4/CCR.
buildSelectedSourceArtifactV1	.block
	movem.l d2-d7/a2-a4, -(sp)
	lea NativeSourceOutputBuffer.l, a2
	movea.l a2, a3
	moveq #-1, d0
	movea.l d0, a4
	moveq #0, d7
	move.w state.NativeCliOutputFormat, d0
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d0
	bne.s payloadStartReady
	addq.l #2, a3

payloadStartReady
	tst.w state.NativeCliSourceOutputImageSet
	beq.s sectionLoopBegin
	tst.w state.NativeCliSourceOutputFillSet
	beq.w fail
	move.l state.NativeCliSourceOutputImageEnd, d7
	sub.l state.NativeCliSourceOutputImageStart, d7
	bcs.w fail
	addq.l #1, d7
	move.l d7, d0
	move.w state.NativeCliOutputFormat, d2
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d2
	bne.s imageCapacityReady
	addq.l #2, d0
imageCapacityReady
	cmpi.l #NATIVE_SOURCE_OUTPUT_BUFFER_CAPACITY, d0
	bhi.w fail
	move.l d7, d3
	beq.s sectionLoopBegin
	move.b state.NativeCliSourceOutputFill, d0
fillLoop
	move.b d0, (a3)+
	subq.l #1, d3
	bne.s fillLoop

sectionLoopBegin
	moveq #0, d6

sectionLoop
	cmp.w state.NativeCliSourceOutputSectionCount, d6
	bhs.w sectionsDone
	move.l d6, d0
	lsl.l #5, d0
	lea state.NativeCliSourceOutputSectionNames, a0
	adda.l d0, a0
	movea.l a0, a1
	moveq #0, d0
nameLenLoop
	tst.b (a1)+
	beq.s nameLenReady
	addq.l #1, d0
	bra.s nameLenLoop
nameLenReady
	jsr layout.findSectionByNameV1
	bne.w fail
	jsr layout.getSectionInfoV1
	move.l a4, d4
	cmpi.l #$FFFFFFFF, d4
	bne.s firstBaseReady
	movea.l d0, a4
firstBaseReady
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d2
	beq.w nextSection
	move.l d0, -(sp)
	move.l d1, -(sp)
	jsr artifacts.opasmOutputGetSessionOriginV1
	move.l 4(sp), d2
	sub.l d0, d2
	bcs.w sectionFail
	move.l (sp), d3
	move.l d2, d4
	add.l d3, d4
	bcs.w sectionFail
	jsr artifacts.opasmOutputGetImageRangeV1
	bne.w sectionFail
	tst.w state.NativeCliSourceOutputImageSet
	beq.s appendSectionBytes
	move.l 4(sp), d4
	sub.l state.NativeCliSourceOutputImageStart, d4
	bcs.w sectionFail
	move.l d4, d2
	add.l d3, d2
	bcs.w sectionFail
	cmp.l d7, d2
	bhi.w sectionFail
	lea NativeSourceOutputBuffer.l, a3
	move.w state.NativeCliOutputFormat, d2
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d2
	bne.s imageDestinationReady
	addq.l #2, a3
imageDestinationReady
	adda.l d4, a3
	bra.s copySectionBytes

appendSectionBytes
	move.l d7, d2
	add.l d3, d2
	bcs.w sectionFail
	cmpi.l #constants.NATIVE_IMAGE_BUFFER_CAPACITY, d2
	bhi.w sectionFail
	move.l d2, d7

copySectionBytes
	tst.l d3
	beq.s sectionCopied
copySectionLoop
	move.b (a0)+, (a3)+
	subq.l #1, d3
	bne.s copySectionLoop
sectionCopied
	addq.l #8, sp

nextSection
	addq.w #1, d6
	bra.w sectionLoop

sectionFail
	addq.l #8, sp
	bra.s fail

sectionsDone
	lea NativeSourceOutputBuffer.l, a0
	move.l d7, d1
	move.w state.NativeCliOutputFormat, d2
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d2
	bne.s success
	move.l a4, d2
	cmpi.l #$FFFFFFFF, d2
	beq.s defaultLoadAddress
	tst.w state.NativeCliPrgLoadAddrSet
	beq.s haveLoadAddress
	move.l state.NativeCliPrgLoadAddr, d2
	bra.s haveLoadAddress
defaultLoadAddress
	moveq #0, d2
haveLoadAddress
	cmpi.l #$FFFF, d2
	bhi.s fail
	move.b d2, (a0)
	lsr.w #8, d2
	move.b d2, 1(a0)
	addq.l #2, d1

success
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a2-a4
	rts
	.bend  ; buildSelectedSourceArtifactV1

	.endsection

	.section bss, kind=bss

NativeSourceOutputBuffer
	.res byte, NATIVE_SOURCE_OUTPUT_BUFFER_CAPACITY

NativeHunkSectionNameScratch
	.res byte, layout.OPASM_LAYOUT_NAME_CAPACITY + 1

	.endsection
	.endmodule
