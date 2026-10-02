; Bind VM-selected captured call fragments without rescanning marker grammar.
	.module experimental.amigaos.binary_macro_fragments
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_macro_plans as plans
	.include "telemetry_macros.i"
	.pub
Frame	.struct
Arena	.long ?
Plan	.long ?
Header	.long ?
FormalCount	.long ?
Text	.long ?
TextEnds	.long ?
TextBytes	.long ?
Full	.long ?
FullBytes	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
	.endstruct
FRAME_BYTES = Frame.Used+4
Fragment	.struct
Pointer	.long ?
Bytes	.long ?
	.endstruct
FRAGMENT_BYTES = Fragment.Bytes+4
	.section code, kind=code
; A0=Frame. D0/CCR=status, Frame.Used=payload bytes on success.
; Preserves other registers. All arena spans must belong to their selected plan.
run	.block
	moveq #0, d0
	bra.w bind
	.bend  ; run
; A0=Frame. Output/Capacity select an array of pointer,long-byte-length pairs.
; D0/CCR=status; D1=logical bytes, D2=nonempty fragment count on success.
; Frame.Used=descriptor bytes; preserves other registers. On failure D1/D2=0.
; Pointers borrow arena/call buffers and are valid only while those stay alive.
runFragments	.block
	moveq #1, d0
	bra.w bind
	.bend  ; runFragments
	.priv
Locals	.struct
Mode	.long ?
Bytes	.long ?
Count	.long ?
	.endstruct
LOCAL_BYTES = Locals.Count+4
bind	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	move.l d0, Locals.Mode(sp)
	clr.l Locals.Bytes(sp)
	clr.l Locals.Count(sp)
	movea.l a0, a6
	clr.l Frame.Used(a6)
	movea.l Frame.Arena(a6), a0
	move.l Frame.Plan(a6), d1
	jsr plans.resolve
	bne.w bad
	move.l plans.Plan.Bytes(a1), d7
	lea plans.HEADER_BYTES(a1), a2
	cmpi.w #8, plans.Row.Kind(a2)
	bne.w bad
	move.l plans.Row.SpellingStart(a2), d5
	move.l plans.Row.SpellingEnd(a2), d6
	cmp.l d5, d6
	blo.w bad
	cmp.l d7, d6
	bhi.w bad
	move.l Frame.Plan(a6), d0
	subq.l #1, d0
	move.l plans.Plan.Count(a1), d1
	lsl.l #5, d1
	addi.l #plans.HEADER_BYTES, d1
	add.l d0, d1
	cmp.l d1, d5
	blo.w bad
	move.l plans.Plan.Recipes(a1), d0
	beq.w bad
	cmp.l d1, d0
	blo.w bad
	move.l d5, d1
	sub.l d0, d1
	bcs.w bad
	cmpi.l #4, d1
	blo.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l d0, a2
	move.l (a2)+, d4
	move.l d1, d0
	subq.l #4, d0
	lsr.l #5, d0
	cmp.l d0, d4
	bhi.w bad
	movea.l Frame.Output(a6), a5
fragment
	tst.l d4
	beq.w success
	tst.l plans.Row.PackedStart(a2)
	bne.w bad
	tst.l plans.Row.PackedEnd(a2)
	bne.w bad
	move.l plans.Row.SpellingStart(a2), d0
	move.l plans.Row.SpellingEnd(a2), d1
	cmp.l d0, d1
	blo.w bad
	cmp.l d5, d0
	blo.w bad
	cmp.l d6, d1
	bhi.w bad
	move.w plans.Row.Kind(a2), d2
	cmpi.w #abi.PRVM_RESULT_MACRO_LITERAL, d2
	beq.w literal
	cmpi.w #abi.PRVM_RESULT_MACRO_POSITIONAL, d2
	beq.w positional
	cmpi.w #abi.PRVM_RESULT_MACRO_NAMED, d2
	beq.w named
	cmpi.w #abi.PRVM_RESULT_MACRO_SUPPLIED_LIST, d2
	bne.w bad
	movea.l Frame.Full(a6), a3
	move.l Frame.FullBytes(a6), d3
	bra.w copy
literal
	move.l d1, d3
	sub.l d0, d3
	movea.l Frame.Arena(a6), a0
	movea.l memory.Block.Pointer(a0), a3
	adda.l d0, a3
	bra.w copy
positional
	move.l plans.Row.Aux0(a2), d2
argument
	cmpi.l #9, d2
	bhs.w bad
	add.l d2, d2
	movea.l Frame.TextEnds(a6), a0
	moveq #0, d3
	move.w 0(a0, d2.w), d3
	moveq #0, d0
	tst.w d2
	beq.w first
	move.w -2(a0, d2.w), d0
first
	cmp.l d0, d3
	blo.w bad
	cmp.l Frame.TextBytes(a6), d3
	bhi.w bad
	sub.l d0, d3
	movea.l Frame.Text(a6), a3
	adda.l d0, a3
	bra.w copy
named
	move.l plans.Row.Aux0(a2), d0
	move.l plans.Row.Aux1(a2), d1
	cmp.l plans.Row.SpellingStart(a2), d0
	blo.w bad
	cmp.l d0, d1
	bls.w bad
	cmp.l plans.Row.SpellingEnd(a2), d1
	bhi.w bad
	move.l d1, d3
	sub.l d0, d3
	movea.l Frame.Arena(a6), a0
	movea.l memory.Block.Pointer(a0), a3
	adda.l d0, a3
	move.l Frame.Header(a6), d1
	jsr plans.resolve
	bne.w bad
	move.l Frame.FormalCount(a6), d0
	cmpi.l #9, d0
	bhi.w bad
	addq.l #1, d0
	cmp.l plans.Plan.Count(a1), d0
	bhi.w bad
	lea plans.HEADER_BYTES+plans.ROW_BYTES(a1), a4
	moveq #0, d2
formal
	cmp.l Frame.FormalCount(a6), d2
	bhs.w unresolved
	cmpi.w #10, plans.Row.Kind(a4)
	bne.w bad
	move.l plans.Row.SpellingStart(a4), d0
	move.l plans.Row.SpellingEnd(a4), d1
	cmp.l d0, d1
	blo.w bad
	cmp.l plans.Plan.Bytes(a1), d1
	bhi.w bad
	move.l plans.Plan.Count(a1), d7
	lsl.l #5, d7
	addi.l #plans.HEADER_BYTES, d7
	add.l Frame.Header(a6), d7
	subq.l #1, d7
	cmp.l d7, d0
	blo.w bad
	sub.l d0, d1
	cmp.l d3, d1
	bne.w nextFormal
	movea.l Frame.Arena(a6), a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	moveq #0, d7
compare
	cmp.l d3, d7
	bhs.w argument
	moveq #0, d0
	move.b 0(a0, d7.l), d0
	moveq #0, d1
	move.b 0(a3, d7.l), d1
	cmpi.b #'A', d0
	blo.w leftReady
	cmpi.b #'Z', d0
	bhi.w leftReady
	addi.b #32, d0
leftReady
	cmpi.b #'A', d1
	blo.w rightReady
	cmpi.b #'Z', d1
	bhi.w rightReady
	addi.b #32, d1
rightReady
	cmp.b d0, d1
	bne.w nextFormal
	addq.l #1, d7
	bra.w compare
nextFormal
	adda.w #plans.ROW_BYTES, a4
	addq.l #1, d2
	bra.w formal
unresolved
	move.l plans.Row.SpellingStart(a2), d0
	move.l plans.Row.SpellingEnd(a2), d1
	bra.w literal
copy
	tst.l Locals.Mode(sp)
	beq.w copyText
	tst.l d3
	beq.w advance
	move.l Locals.Bytes(sp), d0
	add.l d3, d0
	bcs.w bad
	cmpi.l #1024, d0
	bhi.w bad
	move.l d0, Locals.Bytes(sp)
	move.l Frame.Used(a6), d0
	addi.l #FRAGMENT_BYTES, d0
	bcs.w bad
	cmp.l Frame.Capacity(a6), d0
	bhi.w bad
	move.l d0, Frame.Used(a6)
	move.l a3, (a5)+
	move.l d3, (a5)+
	addq.l #1, Locals.Count(sp)
	bra.w advance
copyText
	move.l Frame.Used(a6), d0
	add.l d3, d0
	bcs.w bad
	cmpi.l #252, d0
	bhi.w bad
	cmp.l Frame.Capacity(a6), d0
	bhi.w bad
	move.l d0, Frame.Used(a6)
	tst.l d3
	beq.w advance
copyByte
	move.b (a3)+, (a5)+
	subq.l #1, d3
	bne.w copyByte
advance
	adda.w #plans.ROW_BYTES, a2
	subq.l #1, d4
	bra.w fragment
success
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l Locals.Mode(sp)
	beq.w restore
	moveq #0, d1
	moveq #0, d2
	tst.l d0
	bne.w outputs
	move.l Locals.Bytes(sp), d1
	move.l Locals.Count(sp), d2
outputs
	move.l d1, LOCAL_BYTES(sp)
	move.l d2, LOCAL_BYTES+4(sp)
restore
	adda.w #LOCAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; bind
	.endsection
	.endmodule
