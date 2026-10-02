; Session-owned macro descriptors. The VM owns all selected boundaries.
; Stored rows contain arena and packed-record offsets, never source pointers.
; @opforge-owner: experimental.amigaos.binary_macro_plans
	.module experimental.amigaos.binary_macro_plans
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.use experimental.amigaos.binary_memory as memory
	.include "telemetry_macros.i"
	.pub
ROW_BYTES = 32
HEADER_BYTES = 12
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
RecipeEvents	.long ?
RecipeCount	.long ?
	.endstruct
Plan	.struct
Count	.long ?
Bytes	.long ?
Recipes	.long ?  ; optional arena offset to count + VM fragment rows
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
RecipeEvents	.long ?
RecipeCount	.long ?
	.endstruct
GENERATED_FRAME_BYTES = GeneratedFrame.RecipeCount+4
FRAME_BYTES = Frame.RecipeCount+4
BindFrame	.struct
Arena	.long ?
Source	.long ?  ; read-only memory.Block view of captured plans
Handle	.long ?
PackedMap	.long ?
TokenCount	.long ?
IncludeRecipes	.word ?
Reserved	.word ?
.endstruct
BIND_FRAME_BYTES = BindFrame.Reserved+2
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
	moveq #0, d1
	tst.l Frame.RecipeEvents(a6)
	beq.w reserve
	move.l Frame.RecipeCount(a6), d1
	cmpi.l #MAX_ROWS, d1
	bhi.w bad
	lsl.l #5, d1
	addq.l #4, d1
reserve
	bsr.w reservePlan
	bne.w bad
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
	bsr.w storeRecipes
	bne.w bad
	move.l d6, memory.Block.Used(a4)
	.TELEMETRY_COMPACT runtime_profile.compactProgramRows, Frame.Count(a6)
	.TELEMETRY_COMPACT runtime_profile.compactProgramRows, Frame.RecipeCount(a6)
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

; A0=BindFrame. Captured rows retain token indices via an identity map.
; Copy selected spelling/recipes and apply the replay writer's fresh map through
; create; the captured arena is immutable. D0/CCR=status,D1=session plan handle.
; Other registers preserved. Temporary recipe rows use at most MAX_ROWS*32 bytes.
bind	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	suba.w #FRAME_BYTES, sp
	movea.l sp, a5
	moveq #0, d7
	clr.l Frame.RecipeEvents(a5)
	clr.l Frame.RecipeCount(a5)
	movea.l BindFrame.Source(a6), a0
	move.l BindFrame.Handle(a6), d1
	bsr.w resolve
	bne.w bad
	movea.l a1, a4
	movea.l BindFrame.Arena(a6), a0
	movea.l BindFrame.Source(a6), a2
	move.l memory.Block.Pointer(a0), d0
	cmp.l memory.Block.Pointer(a2), d0
	beq.w bad  ; destination reservation cannot relocate captured input
	move.l a0, Frame.Arena(a5)
	move.l memory.Block.Pointer(a2), Frame.Source(a5)
	move.l Plan.Bytes(a4), Frame.SourceBytes(a5)
	lea HEADER_BYTES(a4), a3
	move.l a3, Frame.Events(a5)
	move.l Plan.Count(a4), Frame.Count(a5)
	move.l BindFrame.PackedMap(a6), Frame.PackedMap(a5)
	move.l BindFrame.TokenCount(a6), Frame.TokenCount(a5)
	; The root spelling must lie inside this candidate, beyond its event rows.
	move.l a3, d0
	sub.l memory.Block.Pointer(a2), d0
	move.l Plan.Count(a4), d1
	lsl.l #5, d1
	add.l d1, d0
	move.l Row.SpellingStart(a3), d4
	cmp.l d0, d4
	blo.w bad
	move.l Row.SpellingEnd(a3), d5
	cmp.l d4, d5
	blo.w bad
	cmp.l Plan.Bytes(a4), d5
	bhi.w bad
	tst.w BindFrame.IncludeRecipes(a6)
	beq.w createBound
	move.l Plan.Recipes(a4), d1
	beq.w createBound
	cmp.l d0, d1
	blo.w bad
	move.l d4, d2
	sub.l d1, d2
	bcs.w bad
	cmpi.l #4, d2
	blo.w bad
	movea.l memory.Block.Pointer(a2), a1
	adda.l d1, a1
	move.l (a1)+, d3
	cmpi.l #MAX_ROWS, d3
	bhi.w bad
	move.l d3, d6
	lsl.l #5, d6
	addq.l #4, d6
	cmp.l d2, d6
	bhi.w bad
	move.l d3, Frame.RecipeCount(a5)
	move.l d3, d7
	lsl.l #5, d7
	suba.l d7, sp
	move.l sp, Frame.RecipeEvents(a5)
	movea.l sp, a2
recipe
	tst.l d3
	beq.w createBound
	movea.l a2, a0
	moveq #7, d0
copyRecipe
	move.l (a1)+, (a2)+
	dbra d0, copyRecipe
	move.l Row.SpellingStart(a0), d0
	sub.l d4, d0
	bcs.w bad
	move.l d0, Row.SpellingStart(a0)
	move.l Row.SpellingEnd(a0), d0
	cmp.l d5, d0
	bhi.w bad
	sub.l d4, d0
	bcs.w bad
	move.l d0, Row.SpellingEnd(a0)
	cmpi.w #abi.PRVM_RESULT_MACRO_NAMED, Row.Kind(a0)
	bne.w nextRecipe
	move.l Row.Aux0(a0), d0
	sub.l d4, d0
	bcs.w bad
	move.l d0, Row.Aux0(a0)
	move.l Row.Aux1(a0), d0
	sub.l d4, d0
	bcs.w bad
	move.l d0, Row.Aux1(a0)
nextRecipe
	subq.l #1, d3
	bra.w recipe
createBound
	movea.l a5, a0
	bsr.w create
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	adda.l d7, sp
	adda.w #FRAME_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; bind

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
	movea.l GeneratedFrame.Arena(a6), a4
	movea.l GeneratedFrame.SpellingEvents(a6), a2
	move.l Row.SpellingStart(a2), d4
	move.l Row.SpellingEnd(a2), d5
	cmp.l d4, d5
	blo.w bad
	cmp.l GeneratedFrame.SourceBytes(a6), d5
	bhi.w bad
	sub.l d4, d5
	moveq #0, d1
	tst.l GeneratedFrame.RecipeEvents(a6)
	beq.w reserve
	move.l GeneratedFrame.RecipeCount(a6), d1
	cmpi.l #MAX_ROWS, d1
	bhi.w bad
	lsl.l #5, d1
	addq.l #4, d1
reserve
	bsr.w reservePlan
	bne.w bad
	movea.l GeneratedFrame.Source(a6), a0
	adda.l d4, a0
	move.l d5, d0
copySpelling
	tst.l d0
	beq.w rowsStart
	move.b (a0)+, (a5)+
	subq.l #1, d0
	bra.w copySpelling
rowsStart
	movea.l GeneratedFrame.PackedEvents(a6), a5
rows
	move.w Row.Kind(a5), d0
	cmp.w Row.Kind(a2), d0
	bne.w bad
	moveq #7, d0
copyRow
	move.l (a5)+, (a3)+
	dbra d0, copyRow
	lea -ROW_BYTES(a3), a1
	move.l Row.PackedStart(a1), d0
	cmp.l GeneratedFrame.PackedBytes(a6), d0
	bhi.w bad
	move.l Row.PackedEnd(a1), d1
	cmp.l d0, d1
	blo.w bad
	cmp.l GeneratedFrame.PackedBytes(a6), d1
	bhi.w bad
	move.l Row.SpellingStart(a2), d0
	move.l Row.SpellingEnd(a2), d1
	cmp.l d0, d1
	blo.w bad
	cmp.l d4, d0
	blo.w bad
	cmp.l GeneratedFrame.SourceBytes(a6), d1
	bhi.w bad
	movea.l GeneratedFrame.SpellingEvents(a6), a0
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
	bra.w auxOffset
formalType
	move.l Row.Aux0(a1), d0
auxOffset
	cmpi.l #-1, d0
	beq.w rowReady
	cmp.l GeneratedFrame.PackedBytes(a6), d0
	bhs.w bad
rowReady
	adda.l #ROW_BYTES, a2
	subq.l #1, d7
	bne.w rows
	; The shared normalizer needs only initial-frame event/recipe fields.
	suba.l #FRAME_BYTES, sp
	movea.l sp, a0
	move.l GeneratedFrame.SpellingEvents(a6), Frame.Events(a0)
	move.l GeneratedFrame.RecipeEvents(a6), Frame.RecipeEvents(a0)
	move.l GeneratedFrame.RecipeCount(a6), Frame.RecipeCount(a0)
	move.l a6, -(sp)
	movea.l a0, a6
	bsr.w storeRecipes
	movea.l (sp)+, a6
	adda.l #FRAME_BYTES, sp
	tst.l d0
	bne.w bad
	move.l d6, memory.Block.Used(a4)
	.TELEMETRY_COMPACT runtime_profile.compactProgramRows, GeneratedFrame.PackedCount(a6)
	.TELEMETRY_COMPACT runtime_profile.compactProgramRows, GeneratedFrame.RecipeCount(a6)
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
	.priv
; Normalize VM-selected fragment spans into the same owned spelling region.
; A4=arena,A6=initial frame,D2=spelling offset,D3=plan offset. D0=status;
; preserves others. The caller publishes Used only after this succeeds.
storeRecipes	.block
	movem.l d1-d7/a0-a3, -(sp)
	tst.l Frame.RecipeEvents(a6)
	beq.w good
	movea.l memory.Block.Pointer(a4), a0
	adda.l d3, a0
	move.l Plan.Recipes(a0), d0
	beq.w bad
	movea.l memory.Block.Pointer(a4), a1
	adda.l d0, a1
	move.l Frame.RecipeCount(a6), d7
	move.l d7, (a1)+
	movea.l Frame.Events(a6), a0
	move.l Row.SpellingEnd(a0), d4
	sub.l Row.SpellingStart(a0), d4
	movea.l Frame.RecipeEvents(a6), a2
	moveq #0, d5
next
	tst.l d7
	beq.w complete
	cmp.l Row.SpellingStart(a2), d5
	bne.w bad
	move.l Row.SpellingEnd(a2), d5
	cmp.l Row.SpellingStart(a2), d5
	bls.w bad
	cmp.l d4, d5
	bhi.w bad
	cmpi.w #abi.PRVM_RESULT_MACRO_LITERAL, Row.Kind(a2)
	blo.w bad
	cmpi.w #abi.PRVM_RESULT_MACRO_SUPPLIED_LIST, Row.Kind(a2)
	bhi.w bad
	movea.l a1, a3
	moveq #7, d0
copy
	move.l (a2)+, (a1)+
	dbra d0, copy
	add.l d2, Row.SpellingStart(a3)
	add.l d2, Row.SpellingEnd(a3)
	cmpi.w #abi.PRVM_RESULT_MACRO_NAMED, Row.Kind(a3)
	bne.w advance
	move.l Row.Aux0(a3), d0
	cmp.l -ROW_BYTES+Row.SpellingStart(a2), d0
	blo.w bad
	move.l Row.Aux1(a3), d1
	cmp.l d0, d1
	bls.w bad
	cmp.l d5, d1
	bhi.w bad
	add.l d2, Row.Aux0(a3)
	add.l d2, Row.Aux1(a3)
advance
	subq.l #1, d7
	bra.w next
complete
	cmp.l d4, d5
	bne.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; storeRecipes

; A4=arena,D7=count,D4=source start,D5=spelling bytes,D1=recipe region bytes.
; D0/CCR=status. D2=spelling offset,D3=prior Used,D6=reserved end;
; A3=rows,A5=spelling destination. Clobbers A0; Used remains unpublished.
reservePlan	.block
	move.l d7, d6
	lsl.l #5, d6
	add.l #HEADER_BYTES, d6
	add.l d5, d6
	add.l d1, d6
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
	clr.l (a3)
	tst.l d1
	beq.w noRecipes
	move.l d2, (a3)
	add.l d1, d2
noRecipes
	addq.l #4, a3
	movea.l memory.Block.Pointer(a4), a5
	adda.l d2, a5
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; reservePlan

	.endsection
	.endmodule
