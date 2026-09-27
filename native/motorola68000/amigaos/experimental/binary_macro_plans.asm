; Session-owned macro descriptors. The VM owns all selected boundaries.
; Stored rows contain arena and packed-record offsets, never source pointers.
	.module experimental.amigaos.binary_macro_plans
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.include "telemetry_macros.i"
	.pub
ROW_BYTES = 32
HEADER_BYTES = 8
MAX_ROWS = 64
STATE_BYTES = memory.Block.Used+4
Frame	.struct
Arena	.long ?
Events	.long ?
Count	.long ?
Source	.long ?
SourceBytes	.long ?
PackedMap	.long ?
TokenCount	.long ?
	.endstruct
Plan	.struct
Count	.long ?
Bytes	.long ?
	.endstruct
Row	.struct
Kind	.word ?
Flags	.word ?
PackedStart	.long ?
PackedEnd	.long ?
SpellingStart	.long ?
SpellingEnd	.long ?
Aux0	.long ?
Aux1	.long ?
Aux2	.long ?
	.endstruct
GeneratedFrame	.struct
Arena	.long ?
PackedEvents	.long ?
PackedCount	.long ?
SpellingEvents	.long ?
SpellingCount	.long ?
Source	.long ?
SourceBytes	.long ?
PackedBytes	.long ?
	.endstruct
GENERATED_FRAME_BYTES = GeneratedFrame.PackedBytes+4
FRAME_BYTES = Frame.TokenCount+4
	.section code, kind=code
	; A0=zero-initialized arena or prior session. Release storage and reset usage.
; D0/CCR=zero; preserves other registers.
begin	.block
	jsr memory.release
	clr.l memory.Block.Used(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=arena. Release all owned descriptors; D0/CCR=zero, others preserved.
finish	.block
	bra.w begin
	.bend  ; finish

; A0=Frame. D0/CCR=status; D1=arena offset plus one, or zero on failure.
; Preserves other registers. Publication is atomic; a failed prefix is unused.
create	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l Frame.Arena(a6), a4
	movea.l Frame.Events(a6), a2
	move.l Frame.Count(a6), d7
	beq.w bad
	cmpi.l #MAX_ROWS, d7
	bhi.w bad
	move.l Row.SpellingStart(a2), d4
	move.l Row.SpellingEnd(a2), d5
	cmp.l d4, d5
	blo.w bad
	cmp.l Frame.SourceBytes(a6), d5
	bhi.w bad
	sub.l d4, d5
	move.l d7, d6
	lsl.l #5, d6
	add.l #HEADER_BYTES, d6
	add.l d5, d6
	addq.l #1, d6
	andi.l #$fffffffe, d6
	move.l memory.Block.Used(a4), d3
	add.l d3, d6
	bcs.w bad
	move.l d6, d0
	movea.l a4, a0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a4), a3
	adda.l d3, a3
	move.l d7, (a3)+
	move.l d6, (a3)+
	move.l d7, d2
	lsl.l #5, d2
	add.l d3, d2
	add.l #HEADER_BYTES, d2
	movea.l memory.Block.Pointer(a4), a5
	adda.l d2, a5
	movea.l Frame.Source(a6), a0
	adda.l d4, a0
	move.l d5, d0
copySpelling
	tst.l d0
	beq.w rows
	move.b (a0)+, (a5)+
	subq.l #1, d0
	bra.w copySpelling
rows
	moveq #7, d0
copyRow
	move.l (a2)+, (a3)+
	dbra d0, copyRow
	lea -ROW_BYTES(a3), a1
	move.l Row.PackedStart(a1), d0
	cmp.l Frame.TokenCount(a6), d0
	bhi.w bad
	move.l Row.PackedEnd(a1), d1
	cmp.l d0, d1
	blo.w bad
	cmp.l Frame.TokenCount(a6), d1
	bhi.w bad
	add.l d0, d0
	add.l d1, d1
	movea.l Frame.PackedMap(a6), a0
	moveq #0, d5
	move.w 0(a0, d0.l), d5
	cmpi.l #256, d5
	bhi.w bad
	move.l d5, Row.PackedStart(a1)
	moveq #0, d5
	move.w 0(a0, d1.l), d5
	cmpi.l #256, d5
	bhi.w bad
	cmp.l Row.PackedStart(a1), d5
	blo.w bad
	move.l d5, Row.PackedEnd(a1)
	move.l Row.SpellingStart(a1), d0
	move.l Row.SpellingEnd(a1), d1
	cmp.l d0, d1
	blo.w bad
	cmp.l d4, d0
	blo.w bad
	cmp.l Frame.SourceBytes(a6), d1
	bhi.w bad
	movea.l Frame.Events(a6), a0
	cmp.l Row.SpellingEnd(a0), d1
	bhi.w bad
	sub.l d4, d0
	sub.l d4, d1
	add.l d2, d0
	add.l d2, d1
	move.l d0, Row.SpellingStart(a1)
	move.l d1, Row.SpellingEnd(a1)
	cmpi.w #10, Row.Kind(a1)
	beq.w formalType
	cmpi.w #8, Row.Kind(a1)
	bne.w rowReady
	move.l Row.Aux2(a1), d0
	cmpi.l #-1, d0
	beq.w rowReady
	cmp.l Frame.TokenCount(a6), d0
	bhs.w bad
	add.l d0, d0
	movea.l Frame.PackedMap(a6), a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	move.l d1, Row.Aux2(a1)
	bra.w rowReady
formalType
	move.l Row.Aux0(a1), d0
	cmpi.l #-1, d0
	beq.w rowReady
	cmp.l Frame.TokenCount(a6), d0
	bhs.w bad
	add.l d0, d0
	movea.l Frame.PackedMap(a6), a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	cmpi.l #256, d1
	bhi.w bad
	move.l d1, Row.Aux0(a1)
rowReady
	subq.l #1, d7
	bne.w rows
	move.l d6, memory.Block.Used(a4)
	.TELEMETRY_COMPACT runtime_profile.compactProgramRows, Frame.Count(a6)
	move.l d6, d0
	sub.l d3, d0
	.TELEMETRY_COMPACT runtime_profile.compactMetadataBytes, d0
	move.l d3, d1
	addq.l #1, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; create

	; Merge independent VM representations by argument ordinal. Packed offsets
; already address the execution record; spelling offsets address the literal list.
; A0=GeneratedFrame. D0/CCR=status,D1=handle; preserves others.
createGenerated	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l GeneratedFrame.PackedCount(a6), d7
	cmp.l GeneratedFrame.SpellingCount(a6), d7
	bne.w bad
	tst.l d7
	beq.w bad
	cmpi.l #MAX_ROWS, d7
	bhi.w bad
	cmpi.l #256, GeneratedFrame.PackedBytes(a6)
	bhi.w bad
	suba.l #FRAME_BYTES+MAX_ROWS*ROW_BYTES+514, sp
	movea.l sp, a5
	move.l GeneratedFrame.Arena(a6), Frame.Arena(a5)
	lea FRAME_BYTES(a5), a3
	move.l a3, Frame.Events(a5)
	move.l d7, Frame.Count(a5)
	move.l GeneratedFrame.Source(a6), Frame.Source(a5)
	move.l GeneratedFrame.SourceBytes(a6), Frame.SourceBytes(a5)
	move.l GeneratedFrame.PackedBytes(a6), Frame.TokenCount(a5)
	lea FRAME_BYTES+MAX_ROWS*ROW_BYTES(a5), a4
	move.l a4, Frame.PackedMap(a5)
	moveq #0, d0
map
	move.w d0, (a4)+
	addq.w #1, d0
	cmpi.w #257, d0
	blo.w map
	movea.l GeneratedFrame.PackedEvents(a6), a1
	movea.l GeneratedFrame.SpellingEvents(a6), a2
rows
	move.w Row.Kind(a1), d0
	cmp.w Row.Kind(a2), d0
	bne.w stagedBad
	movea.l a3, a4
	moveq #7, d0
copy
	move.l (a1)+, (a3)+
	dbra d0, copy
	move.l Row.SpellingStart(a2), Row.SpellingStart(a4)
	move.l Row.SpellingEnd(a2), Row.SpellingEnd(a4)
	adda.l #ROW_BYTES, a2
	subq.l #1, d7
	bne.w rows
	movea.l a5, a0
	bsr.w create
	bra.w stagedDone
stagedBad
	moveq #1, d0
	moveq #0, d1
stagedDone
	adda.l #FRAME_BYTES+MAX_ROWS*ROW_BYTES+514, sp
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; createGenerated

; A0=arena,D1=nonzero handle. D0/CCR=status,A1=plan on success.
; Preserves other registers. The bounded plan must remain in the live arena.
resolve	.block
	movem.l d1-d3, -(sp)
	tst.l d1
	beq.w bad
	subq.l #1, d1
	move.l memory.Block.Used(a0), d2
	sub.l d1, d2
	bcs.w bad
	cmpi.l #HEADER_BYTES, d2
	blo.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d1, a1
	move.l Plan.Count(a1), d3
	beq.w bad
	cmpi.l #MAX_ROWS, d3
	bhi.w bad
	lsl.l #5, d3
	add.l #HEADER_BYTES, d3
	cmp.l d2, d3
	bhi.w bad
	move.l Plan.Bytes(a1), d2
	cmp.l memory.Block.Used(a0), d2
	bhi.w bad
	sub.l d1, d2
	cmp.l d3, d2
	blo.w bad
	.TELEMETRY_COMPACT runtime_profile.compactLookup, #1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3
	tst.l d0
	rts
	.bend  ; resolve
	.endsection
	.endmodule
