; @opforge-owner: experimental.amigaos.binary_app
; Ordinary source-origin Bin transport. Section selections retain their own
; payload geometry; only the flat numeric initialized image reaches this path.
; D0/CCR=status; caller owns spans, payload and reusable transport buffer.
writeFlat	.block
	lea	TextBuffer, a0
	move.l	#4096, d0
	jsr	memory.reserveExact
	bne.w	bad
	lea	FlatFrame, a1
	move.w SelectedRangeSet, flat_output.Frame.RangeSet(a1)
	move.l SelectedRangeStart, flat_output.Frame.RangeStart(a1)
	move.l SelectedRangeEnd, flat_output.Frame.RangeEnd(a1)
	move.w MetadataConfig+metadata.Config.Fill+2, flat_output.Frame.Fill(a1)
	move.l	memory.Block.Pointer(a0), flat_output.Frame.Buffer(a1)
	move.l	#4096, flat_output.Frame.Capacity(a1)
	lea	Output, a0
	move.l	memory.Block.Pointer(a0), flat_output.Frame.Data(a1)
	lea	Work, a0
	move.l	assembly.Frame.Used(a0), flat_output.Frame.DataBytes(a1)
	lea	EmissionSpans, a0
	move.l	memory.Block.Pointer(a0), flat_output.Frame.Spans(a1)
	move.l	memory.Block.Used(a0), flat_output.Frame.SpanBytes(a1)
	movea.l	a1, a0
	jsr	flat_output.init
	bne.w	bad
	lea	OutputWrite, a0
	move.l	SelectedOutputPath, output_io.Frame.Path(a0)
	clr.l	output_io.Frame.PrefixBytes(a0)
	move.l	#flat_output.next, output_io.Frame.Generator(a0)
	move.l	#FlatFrame, output_io.Frame.Context(a0)
	movea.l	DosBase, a6
	jsr	output_io.write
	rts
bad
	moveq	#1, d0
	rts
	.bend  ; writeFlat
