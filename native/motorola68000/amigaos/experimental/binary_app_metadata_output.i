; @opforge-owner: experimental.amigaos.binary_app
; Source-directed BIN artifacts over one completed numeric flat image.
; D0/CCR=status. Path resolution owns names; the writer owns range clipping/fill.
; Explicit CLI BIN suppresses this vector through the metadata output owner.
writeMetadataBins	.block
	lea MetadataOutput, a0
	clr.l metadata_output.Frame.BinIndex(a0)
	clr.w metadata_output.Frame.AutoRangeSet(a0)
	clr.l metadata_output.Frame.AutoRangeStart(a0)
	lea EmissionSpans, a1
	tst.l memory.Block.Used(a1)
	beq.w preflight
	movea.l memory.Block.Pointer(a1), a1
	move.l record_output.Span.Address(a1), metadata_output.Frame.AutoRangeStart(a0)
	move.w #1, metadata_output.Frame.AutoRangeSet(a0)
preflight
	; Validate the complete vector before opening its first file. A later bad
	; name must not leave an earlier BIN artifact behind on configuration failure.
	lea MetadataOutput, a0
	jsr metadata_output.resolveBin
	cmpi.l #metadata_output.BIN_END, d0
	beq.w ready
	tst.l d0
	bne.w bad
	bra.w preflight
ready
	lea MetadataOutput, a0
	clr.l metadata_output.Frame.BinIndex(a0)
next
	lea MetadataOutput, a0
	jsr metadata_output.resolveBin
	cmpi.l #metadata_output.BIN_END, d0
	beq.w good
	tst.l d0
	bne.w bad
	move.l a1, SelectedOutputPath
	move.w d1, SelectedRangeSet
	move.l d2, SelectedRangeStart
	move.l d3, SelectedRangeEnd
	; Metadata range rendering is flat-source policy. Never discard a section
	; selection's geometry by sending its concatenated payload to this writer.
	lea Work, a0
	movea.l assembly.Frame.Sections(a0), a1
	tst.w sections.State.Mode(a1)
	bne.w bad
	bsr.w writeFlat
	bne.w bad
	bra.w next
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; writeMetadataBins
