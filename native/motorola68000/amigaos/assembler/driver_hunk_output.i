; @opforge-owner: opforge.assembler.driver
; Hunk section selection, relocation adaptation and bounded file output.
; Stream validated Hunk parts through the shared transport. Metadata uses one
; reusable 4 KiB buffer; payloads borrow the completed assembly output directly.
; D0/CCR=status. No section, fixup or transport ownership is transferred.
writeHunk	.block
	lea TextBuffer, a0
	move.l #4096, d0
	jsr memory.reserveExact
	bne.w bad
	lea HunkStream, a1
	move.l #HunkFrame, hunk_stream.Frame.Source(a1)
	move.l memory.Block.Pointer(a0), hunk_stream.Frame.Buffer(a1)
	move.l #4096, hunk_stream.Frame.Capacity(a1)
	movea.l a1, a0
	jsr hunk_stream.begin
	bne.w bad
	lea OutputWrite, a0
	move.l SelectedOutputPath, output_io.Frame.Path(a0)
	clr.l output_io.Frame.PrefixBytes(a0)
	move.l #hunk_stream.next, output_io.Frame.Generator(a0)
	move.l #HunkStream, output_io.Frame.Context(a0)
	movea.l DosBase, a6
	jsr output_io.write
	rts
bad
	moveq #1, d0
	rts
	.bend  ; writeHunk

; Build selected native Hunk sections from numeric assembly metadata.
; The flat path keeps its existing write buffer. A Hunk selection borrows its
; parts until writeSelected consumes it. D0/CCR=status.
selectOutput	.block
	movem.l d1-d7/a0-a6, -(sp)
	clr.w HunkSelected
	lea Work, a5
	move.l assembly.Frame.Used(a5), WriteBytes
	lea Output, a0
	move.l memory.Block.Pointer(a0), WritePointer
	movea.l assembly.Frame.Sections(a5), a6
	cmpi.w #1, SelectedOutputKind
	beq.w requireFlat
	cmpi.w #output_plan.HUNK, SelectedOutputKind
	bne.w sourceFormat
	; CLI Hunk output still needs the source-owned section selection.
	; Mapped flat geometry must not silently provide an implicit selection.
	tst.w sections.State.OutputSeen(a6)
	beq.w badSelect
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a6)
	bne.w badSelect
	bra.w sourceFormat
requireFlat
	; Fragment geometry alone does not choose a Hunk artifact. Without a
	; source Hunk descriptor, Bin owns only the initialized assembly buffer.
	tst.w sections.State.OutputSeen(a6)
	bne.w badSelect
	bra.w selected
sourceFormat
	cmpi.w #5, sections.State.Mode(a6)
	bne.w selected
	lea HunkParts, a4
	lea HunkFrame, a1
	clr.l hunk.Frame.Count(a1)
	lea SlotToPart, a1
	moveq #7, d0
clearPartMap
	move.b #$ff, (a1)+
	dbra d0, clearPartMap
	moveq #0, d7
	move.w sections.State.OrderCount(a6), d6
	lea sections.ORDER(a6), a3
part
	tst.w d6
	beq.w serialize
	moveq #0, d0
	move.b (a3)+, d0
	move.l d0, d4
	mulu.w #sections.HUNK_SLOT_BYTES, d0
	lea sections.HUNK_SLOTS(a6), a2
	adda.l d0, a2
	moveq #0, d1
	move.w sections.HunkSlot.Kind(a2), d1
	move.l sections.HunkSlot.Used(a2), d2
	cmpi.w #3, d1
	beq.w keepPart
	tst.l d2
	beq.w nextPart
keepPart
	lea SlotToPart, a1
	move.b d7, 0(a1, d4.w)
	lea PartSlots, a1
	move.b d4, 0(a1, d7.w)
	move.w d1, hunk.Part.Kind(a4)
	clr.w hunk.Part.Reserved(a4)
	clr.l hunk.Part.Data(a4)
	move.l d2, hunk.Part.Used(a4)
	move.l sections.HunkSlot.Allocation(a2), hunk.Part.Size(a4)
	clr.l hunk.Part.Fixups(a4)
	clr.l hunk.Part.FixupCount(a4)
	cmpi.w #3, d1
	beq.w partReady
	move.l sections.HunkSlot.Start(a2), d0
	add.l d2, d0
	bcs.w badSelect
	cmp.l assembly.Frame.Used(a5), d0
	bhi.w badSelect
	move.l memory.Block.Pointer(a0), d0
	add.l sections.HunkSlot.Start(a2), d0
	bcs.w badSelect
	move.l d0, hunk.Part.Data(a4)
partReady
	adda.w #hunk.SEGMENT_BYTES, a4
	addq.l #1, d7
nextPart
	subq.w #1, d6
	bra.w part
serialize
	bsr.w collectRelocs
	bne.w badSelect
	lea HunkFrame, a0
	move.l #HunkParts, hunk.Frame.Segments(a0)
	move.l d7, hunk.Frame.Count(a0)
	clr.l hunk.Frame.Output(a0)
	clr.l hunk.Frame.Capacity(a0)
	jsr hunk.build
	bne.w badSelect
	move.w #1, HunkSelected
selected
	moveq #0, d0
	bra.w selectDone
badSelect
	moveq #1, d0
selectDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; selectOutput

; Convert slot-number relocation records to emitted Hunk indices. Output parts
; stay in selected order; each part's fixups are contiguous and source-ordered.
collectRelocs	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea RelocBlock, a0
	move.l memory.Block.Used(a0), d0
	cmpi.l #786420, d0
	bhi.w bad
	divu.w #assembly.OUTPUT_RELOC_BYTES, d0
	move.l d0, d1
	swap d1
	tst.w d1
	bne.w bad
	moveq #0, d1
	move.w d0, d1
	mulu.w #hunk.FIXUP_BYTES, d1
	move.l d1, d0
	lea HunkRelocs, a0
	jsr memory.reserve
	bne.w bad
	clr.l memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a5
	move.l d7, d6
	moveq #0, d4
part
	cmp.l d6, d4
	bhs.w doneParts
	move.l d4, d0
	mulu.w #hunk.SEGMENT_BYTES, d0
	lea HunkParts, a4
	adda.l d0, a4
	clr.l hunk.Part.FixupCount(a4)
	move.l a5, hunk.Part.Fixups(a4)
	lea PartSlots, a0
	moveq #0, d3
	move.b 0(a0, d4.w), d3
	lea RelocBlock, a0
	movea.l memory.Block.Pointer(a0), a6
	move.l memory.Block.Used(a0), d5
record
	tst.l d5
	beq.w nextPart
	cmpi.l #assembly.OUTPUT_RELOC_BYTES, d5
	blo.w bad
	cmp.l assembly.OutputReloc.Source(a6), d3
	bne.w skipRecord
	move.l assembly.OutputReloc.Target(a6), d0
	cmpi.l #8, d0
	bhs.w bad
	lea SlotToPart, a0
	moveq #0, d1
	move.b 0(a0, d0.w), d1
	cmpi.w #$ff, d1
	beq.w bad
	move.l d1, hunk.Reloc.Target(a5)
	move.l assembly.OutputReloc.Offset(a6), hunk.Reloc.Offset(a5)
	addq.l #1, hunk.Part.FixupCount(a4)
	adda.w #hunk.FIXUP_BYTES, a5
skipRecord
	adda.w #assembly.OUTPUT_RELOC_BYTES, a6
	subi.l #assembly.OUTPUT_RELOC_BYTES, d5
	bra.w record
nextPart
	addq.l #1, d4
	bra.w part
doneParts
	lea HunkRelocs, a0
	move.l a5, d0
	sub.l memory.Block.Pointer(a0), d0
	move.l d0, memory.Block.Used(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; collectRelocs
