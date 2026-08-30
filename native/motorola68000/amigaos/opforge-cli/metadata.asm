; Native root-module metadata router and output-selection capture.

	.module opforge.cli.metadata
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.assembly_session
	.use opforge.cli.directive_handlers
	.use opforge.cli.line_text
	.use opforge.cli.state
	.use opforge.cli.strings

	.section code, kind=code
	.pub

; Consume root `.meta` block/inline forms before the package parser sees them.
; Metadata output selections are preserved as same-source artifact requests;
; names and versions are metadata only and never select a foreign target.
; @opforge-owner: opforge.cli.metadata
; @opforge-slice: documentation/plans/slices/native-porting-slice-root-metadata-output.toml
; @opforge-role: implementation
; Outputs: D0.L = 0 not metadata, 1 handled, -1 malformed nesting.
; Clobbers: D0-D4/A0-A2/CCR.
opforgeNativeCliRouteRootMetadataLineV1	.block
	movem.l d1-d4/a0-a2, -(sp)
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w notMetadata
	movea.l a0, a2
	move.l d0, d4

	bsr.w matchesInlineTargetNameV1
	bne.w handled
	lea strings.MetaOutputNameDirectiveText, a1
	moveq #17, d1
	bsr.w matches
	bne.w handled
	lea strings.MetaVersionDirectiveText, a1
	moveq #13, d1
	bsr.w matches
	bne.w handled
	lea strings.MetaNameDirectiveText, a1
	moveq #10, d1
	bsr.w matches
	bne.w handled

	lea strings.MetaDirectiveText, a1
	moveq #5, d1
	bsr.w matches
	beq.s checkEndMeta
	tst.w state.NativeCliMetadataDepth
	bne.w malformed
	move.w #1, state.NativeCliMetadataDepth
	bra.w handled

checkEndMeta
	lea strings.EndmetaDirectiveText, a1
	moveq #8, d1
	bsr.w matches
	beq.s requireMeta
	cmpi.w #1, state.NativeCliMetadataDepth
	bne.w malformed
	tst.w state.NativeCliMetadataOutputDepth
	bne.w malformed
	tst.w state.NativeCliMetadataTargetDepth
	bne.w malformed
	clr.w state.NativeCliMetadataDepth
	bra.w handled

requireMeta
	tst.w state.NativeCliMetadataDepth
	beq.w notMetadata

	lea strings.OutputDirectiveText, a1
	moveq #7, d1
	bsr.w matches
	beq.s checkEndOutput
	tst.w state.NativeCliMetadataOutputDepth
	bne.w malformed
	move.w #1, state.NativeCliMetadataOutputDepth
	bra.w handled

checkEndOutput
	lea strings.EndoutputDirectiveText, a1
	moveq #10, d1
	bsr.w matches
	beq.s metadataValues
	cmpi.w #1, state.NativeCliMetadataOutputDepth
	bne.w malformed
	tst.w state.NativeCliMetadataTargetDepth
	bne.w malformed
	clr.w state.NativeCliMetadataOutputDepth
	bra.w handled

metadataValues
	lea strings.NameDirectiveText, a1
	moveq #5, d1
	bsr.w matches
	bne.w handled
	lea strings.VersionDirectiveText, a1
	moveq #8, d1
	bsr.w matches
	bne.w handled
	tst.w state.NativeCliMetadataOutputDepth
	beq.w malformed

	lea strings.ListDirectiveText, a1
	moveq #5, d1
	bsr.w matches
	beq.s checkHex
	moveq #constants.NATIVE_ARTIFACT_REQUEST_METADATA_LIST, d1
	bra.s capture
checkHex
	lea strings.HexDirectiveText, a1
	moveq #4, d1
	bsr.w matches
	beq.s checkBin
	moveq #constants.NATIVE_ARTIFACT_REQUEST_METADATA_HEX, d1
	bra.s capture
checkBin
	lea strings.BinDirectiveText, a1
	moveq #4, d1
	bsr.w matches
	beq.s checkTargetBoundary
	moveq #constants.NATIVE_ARTIFACT_REQUEST_METADATA_BIN, d1
capture
	jsr directive_handlers.opforgeNativeCliCaptureArtifactRequestLineV1
	tst.l d0
	bne.s malformed
	bra.s handled

checkTargetBoundary
	bsr.w routeStructuralTargetBoundaryV1
	tst.l d0
	bne.s handled
	bra.s malformed

handled
	jsr assembly_session.opforgeNativeCliRecordSourceLine
	tst.l d0
	bne.s malformed
	moveq #1, d0
	bra.s return
notMetadata
	moveq #0, d0
	bra.s return
malformed
	moveq #-1, d0
return
	movem.l (sp)+, d1-d4/a0-a2
	rts

matches
	movem.l d2-d4/a2-a3, -(sp)
	movea.l a2, a0
	move.l d4, d0
	jsr line_text.opforgeNativeCliLineStartsWith
	movem.l (sp)+, d2-d4/a2-a3
	rts

; Recognize `.meta.output.<target>.name` without naming any CPU or family in
; the generic CLI. The target is a structural identifier and metadata names do
; not affect artifact selection.
; Inputs: A2 = trimmed line, D4.L = line length.
; Outputs: D0.L = 1 match, 0 no match.
; Clobbers: D0/CCR.
matchesInlineTargetNameV1	.block
	movem.l d1-d7/a0-a3, -(sp)
	cmpi.l #19, d4
	bcs.w no
	movea.l a2, a0
	lea strings.MetaOutputTargetPrefixText, a1
	moveq #13, d1
	bsr.w bytesEqualFoldedV1
	tst.l d0
	beq.w no
	moveq #13, d2
	moveq #0, d3
targetLoop
	cmp.l d4, d2
	bcc.w no
	moveq #0, d0
	move.b 0(a2, d2.l), d0
	cmpi.b #'.', d0
	beq.s targetDone
	bsr.w isTargetIdentifierByteV1
	tst.l d0
	beq.w no
	addq.l #1, d2
	addq.l #1, d3
	cmpi.l #constants.TOKEN_BUFFER_CAPACITY - 1, d3
	bhi.w no
	bra.s targetLoop
targetDone
	tst.l d3
	beq.w no
	move.l d4, d0
	sub.l d2, d0
	cmpi.l #5, d0
	bcs.w no
	lea 0(a2, d2.l), a0
	lea strings.NameDirectiveText, a1
	moveq #5, d1
	bsr.w bytesEqualFoldedV1
	tst.l d0
	beq.w no
	addq.l #5, d2
	bsr.w hasLineBoundaryV1
	bra.s returnInline
no
	moveq #0, d0
returnInline
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; matchesInlineTargetNameV1

; Open or close one structurally named target block inside `.meta/.output`.
; The exact opening identifier is retained so a different `.end<target>` can
; never close it. CPU/family interpretation remains package-owned.
; Inputs: A2 = trimmed line, D4.L = line length.
; Outputs: D0.L = 1 handled, 0 not a valid boundary.
; Clobbers: D0/CCR.
routeStructuralTargetBoundaryV1	.block
	movem.l d1-d7/a0-a3, -(sp)
	tst.w state.NativeCliMetadataTargetDepth
	bne.w closeTarget
	tst.l d4
	beq.w noBoundary
	cmpi.b #'.', (a2)
	bne.w noBoundary
	moveq #1, d2
	moveq #0, d3
measureOpen
	cmp.l d4, d2
	bcc.s openMeasured
	moveq #0, d0
	move.b 0(a2, d2.l), d0
	cmpi.b #' ', d0
	beq.s openMeasured
	cmpi.b #9, d0
	beq.s openMeasured
	cmpi.b #';', d0
	beq.s openMeasured
	bsr.w isTargetIdentifierByteV1
	tst.l d0
	beq.w noBoundary
	addq.l #1, d2
	addq.l #1, d3
	cmpi.l #constants.TOKEN_BUFFER_CAPACITY - 1, d3
	bhi.w noBoundary
	bra.s measureOpen
openMeasured
	tst.l d3
	beq.w noBoundary
	movea.l a2, a0
	lea strings.EndTargetPrefixText, a1
	moveq #4, d1
	cmpi.l #3, d3
	bcs.s storeOpen
	bsr.w bytesEqualFoldedV1
	tst.l d0
	bne.w noBoundary
storeOpen
	addq.l #1, a0
	move.w d3, state.NativeCliMetadataTargetNameLen
	lea state.NativeCliMetadataTargetName, a1
	move.l d3, d1
	subq.l #1, d1
copyOpen
	move.b (a0)+, (a1)+
	dbra d1, copyOpen
	clr.b (a1)
	move.w #1, state.NativeCliMetadataTargetDepth
	moveq #1, d0
	bra.w returnBoundary

closeTarget
	cmpi.l #5, d4
	bcs.s noBoundary
	movea.l a2, a0
	lea strings.EndTargetPrefixText, a1
	moveq #4, d1
	bsr.w bytesEqualFoldedV1
	tst.l d0
	beq.s noBoundary
	moveq #0, d3
	move.w state.NativeCliMetadataTargetNameLen, d3
	move.l d3, d2
	addq.l #4, d2
	cmp.l d2, d4
	bcs.s noBoundary
	lea 4(a2), a0
	lea state.NativeCliMetadataTargetName, a1
	move.l d3, d1
	bsr.w bytesEqualFoldedV1
	tst.l d0
	beq.s noBoundary
	bsr.w hasLineBoundaryV1
	tst.l d0
	beq.s noBoundary
	clr.w state.NativeCliMetadataTargetDepth
	clr.w state.NativeCliMetadataTargetNameLen
	clr.b state.NativeCliMetadataTargetName
	moveq #1, d0
	bra.s returnBoundary
noBoundary
	moveq #0, d0
returnBoundary
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; routeStructuralTargetBoundaryV1

; Inputs: A0/A1 byte spans, D1.L = length. Outputs D0.L boolean.
bytesEqualFoldedV1	.block
	movem.l d1-d3/a0-a1, -(sp)
	tst.l d1
	beq.s equal
	subq.l #1, d1
compareLoop
	moveq #0, d2
	moveq #0, d3
	move.b (a0)+, d2
	move.b (a1)+, d3
	cmpi.b #'A', d2
	bcs.s foldRight
	cmpi.b #'Z', d2
	bhi.s foldRight
	addi.b #32, d2
foldRight
	cmpi.b #'A', d3
	bcs.s compareByte
	cmpi.b #'Z', d3
	bhi.s compareByte
	addi.b #32, d3
compareByte
	cmp.b d3, d2
	bne.s different
	dbra d1, compareLoop
equal
	moveq #1, d0
	bra.s returnEqual
different
	moveq #0, d0
returnEqual
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; bytesEqualFoldedV1

; Inputs: D0.B candidate. Outputs D0.L boolean.
isTargetIdentifierByteV1	.block
	cmpi.b #'A', d0
	bcs.s lower
	cmpi.b #'Z', d0
	bls.s yesIdentifier
lower
	cmpi.b #'a', d0
	bcs.s digit
	cmpi.b #'z', d0
	bls.s yesIdentifier
digit
	cmpi.b #'0', d0
	bcs.s punctuation
	cmpi.b #'9', d0
	bls.s yesIdentifier
punctuation
	cmpi.b #'_', d0
	beq.s yesIdentifier
	cmpi.b #'-', d0
	beq.s yesIdentifier
	moveq #0, d0
	rts
yesIdentifier
	moveq #1, d0
	rts
	.bend  ; isTargetIdentifierByteV1

; Inputs: D2.L offset, A2 line, D4.L length. Outputs D0.L boolean.
hasLineBoundaryV1	.block
	cmp.l d2, d4
	beq.s yesBoundary
	moveq #0, d0
	move.b 0(a2, d2.l), d0
	cmpi.b #' ', d0
	beq.s yesBoundary
	cmpi.b #9, d0
	beq.s yesBoundary
	cmpi.b #';', d0
	beq.s yesBoundary
	moveq #0, d0
	rts
yesBoundary
	moveq #1, d0
	rts
	.bend  ; hasLineBoundaryV1

	.bend  ; opforgeNativeCliRouteRootMetadataLineV1

	.endsection
	.endmodule
