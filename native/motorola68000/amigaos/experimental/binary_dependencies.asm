; Resolve absolute constant graphs before layout, using only packed records.
; @opforge-owner: experimental.amigaos.binary_dependencies

	.module experimental.amigaos.binary_dependencies
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_memory as memory
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as runtime
	.pub
ABSOLUTE = 2
	.priv
PENDING = 3
VISITING = 4
LAYOUT = 5
LABEL = 6
Entry	.struct
Id	.word ?
Cursor	.word ?
Flags	.word ?
Reserved	.word ?
.endstruct
ENTRY_BYTES = 8
	.section bss, kind=bss
Base	.res long, 1
Limit	.res long, 1
Depth	.res long, 1
Constants	.res long, 1
Scratch	.res byte, memory.Block.Used+4
	.endsection
	.section code, kind=code
	.pub
; A0=packed records, D0=record bytes, A2=Context with zeroed symbol arrays.
; D0/CCR=status; other registers preserved. Pending values contain offsets only
; during this call. Absolute values retain Defined=ABSOLUTE; all other entries
; are cleared before return. Scratch is bounded by source IDs and always freed.
resolve	.block
	movem.l d1-d7/a0-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	move.l a0, Base
	add.l a0, d0
	bcs.w bad
	move.l d0, Limit
	clr.l Depth
	clr.l Constants
	movea.l a2, a6
	move.l pkg.Context.Count(a6), d0
	beq.w bad
	cmpi.l #65536, d0
	bhi.w bad
	movea.l pkg.Context.Package(a6), a0
	moveq #0, d1
	move.w pkg.Header.NameCount(a0), d1
	sub.l d1, d0
	bcs.w bad
	cmpi.l #512, d0
	bhi.w bad
	bsr.w index
	tst.l d0
	bne.w clearFailure
	tst.l Constants
	beq.w success
	move.l Constants, d0
	lsl.l #3, d0
	lea Scratch, a0
	jsr memory.reserve
	bne.w clearFailure
	movea.l pkg.Context.Values(a6), a4
	movea.l pkg.Context.Defined(a6), a5
	movea.l pkg.Context.Package(a6), a0
	moveq #0, d7
	move.w pkg.Header.NameCount(a0), d7
root
	cmp.l pkg.Context.Count(a6), d7
	bhs.w success
	cmpi.b #PENDING, 0(a5, d7.l)
	bne.w nextRoot
	move.l d7, d0
	bsr.w push
	bne.w clearFailure
visit
	move.l Depth, d0
	beq.w nextRoot
	subq.l #1, d0
	lsl.l #3, d0
	lea Scratch, a0
	movea.l memory.Block.Pointer(a0), a3
	adda.l d0, a3
	moveq #0, d4
	move.w Entry.Id(a3), d4
	move.l d4, d5
	lsl.l #2, d5
	movea.l Base, a0
	adda.l 0(a4, d5.l), a0
	moveq #0, d6
	move.b 1(a0), d6
	addq.w #2, d6
	moveq #0, d3
	move.w Entry.Cursor(a3), d3
	cmp.w d6, d3
	bhs.w clearFailure
	moveq #0, d0
	move.b 0(a0, d3.w), d0
	beq.w completed
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d0
	beq.w dependency
	moveq #1, d1
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d0
	beq.w current
	cmpi.b #runtime.COMPACT_NEGATE, d0
	beq.w advance
	cmpi.b #runtime.COMPACT_ADD, d0
	beq.w advance
	cmpi.b #runtime.COMPACT_SUBTRACT, d0
	beq.w advance
	cmpi.b #runtime.COMPACT_MULTIPLY, d0
	beq.w advance
	moveq #2, d1
	cmpi.b #runtime.COMPACT_I8, d0
	beq.w advance
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d0
	beq.w advance
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d0
	beq.w advance
	moveq #3, d1
	cmpi.b #runtime.COMPACT_I16, d0
	beq.w advance
	moveq #5, d1
	cmpi.b #runtime.COMPACT_I32, d0
	bne.w clearFailure
advance
	add.w d1, d3
	cmp.w d6, d3
	bhs.w clearFailure
	move.w d3, Entry.Cursor(a3)
	bra.w visit
current
	move.w #1, Entry.Flags(a3)
	bra.w advance
dependency
	move.w d3, d1
	addq.w #3, d1
	cmp.w d6, d1
	bhs.w clearFailure
	moveq #0, d0
	move.b 2(a0, d3.w), d0
	lsl.w #8, d0
	move.b 1(a0, d3.w), d0
	cmp.l pkg.Context.Count(a6), d0
	bhs.w clearFailure
	moveq #0, d2
	move.b 0(a5, d0.l), d2
	cmpi.b #PENDING, d2
	beq.w descend
	cmpi.b #ABSOLUTE, d2
	beq.w consumed
	cmpi.b #LAYOUT, d2
	beq.w dependentLayout
	cmpi.b #LABEL, d2
	bne.w clearFailure  ; missing names and visiting nodes are errors
dependentLayout
	move.w #1, Entry.Flags(a3)
consumed
	move.w d1, Entry.Cursor(a3)
	bra.w visit
descend
	; Parent stays on this symbol until its child has completed.
	bsr.w push
	bne.w clearFailure
	bra.w visit
completed
	addq.w #1, d3
	cmp.w d6, d3
	bne.w clearFailure
	tst.w Entry.Flags(a3)
	bne.w deferred
	movea.l a0, a1
	adda.l d6, a1
	movea.l a6, a2
	jsr expr.evaluate
	tst.l d0
	bne.w clearFailure
	tst.l d2
	bne.w clearFailure
	cmpa.l a1, a0
	bne.w clearFailure
	move.l d1, 0(a4, d5.l)
	move.b #ABSOLUTE, 0(a5, d4.l)
	bra.w pop
deferred
	move.b #LAYOUT, 0(a5, d4.l)
pop
	subq.l #1, Depth
	bra.w visit
nextRoot
	addq.l #1, d7
	bra.w root
success
	moveq #0, d7
	bra.w normalize
clearFailure
	moveq #1, d7
normalize
	movea.l pkg.Context.Package(a6), a0
	moveq #0, d2
	move.w pkg.Header.NameCount(a0), d2
	move.l pkg.Context.Count(a6), d1
	sub.l d2, d1
	beq.w normalized
	movea.l pkg.Context.Values(a6), a0
	movea.l pkg.Context.Defined(a6), a1
	adda.l d2, a1
	lsl.l #2, d2
	adda.l d2, a0
clearLoop
	tst.l d7
	bne.w erase
	cmpi.b #ABSOLUTE, (a1)
	beq.w keep
erase
	clr.l (a0)
	clr.b (a1)
keep
	addq.l #4, a0
	addq.l #1, a1
	subq.l #1, d1
	bne.w clearLoop
normalized
	move.l d7, d0
	bra.w release
bad
	moveq #1, d0
release
	lea Scratch, a0
	jsr memory.release
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; resolve

	.priv
; D0=source ID. Mark visiting and append a bounded frame. Other registers saved.
push	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.l Depth, d1
	cmp.l Constants, d1
	bhs.w bad
	lsl.l #3, d1
	lea Scratch, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d1, a0
	move.w d0, Entry.Id(a0)
	move.w #2, Entry.Cursor(a0)
	clr.w Entry.Flags(a0)
	clr.w Entry.Reserved(a0)
	movea.l pkg.Context.Defined(a6), a1
	move.b #VISITING, 0(a1, d0.l)
	addq.l #1, Depth
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; push

; Index declaration ownership and constant offsets; leave expression evaluation
; to the graph walk. Packed records remain immutable. Other registers preserved.
index	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l Base, a0
	movea.l Limit, a1
	movea.l pkg.Context.Package(a6), a2
	movea.l pkg.Context.Values(a6), a4
	movea.l pkg.Context.Defined(a6), a5
line
	cmpa.l a1, a0
	beq.w good
	bhi.w bad
	moveq #0, d6
	move.b (a0), d6
	addq.w #1, d6
	cmpi.w #4, d6
	blo.w bad
	move.l a1, d0
	sub.l a0, d0
	cmp.l d0, d6
	bhi.w bad
	cmpi.b #1, 1(a0)
	bhi.w bad
	cmpi.w #9, d6
	blo.w next
	lea 4(a0), a3
	moveq #9, d2
	cmpi.b #7, (a3)
	beq.w directive
	cmpi.b #1, (a3)
	bhi.w next
	moveq #LABEL, d7
	cmpi.b #5, 4(a3)
	beq.w declaration
	moveq #PENDING, d7
	cmpi.b #34, 4(a3)
	bne.w next
declaration
	tst.b 3(a3)
	bne.w bad
	moveq #0, d4
	move.w 1(a3), d4
	cmp.w pkg.Header.NameCount(a2), d4
	blo.w bad
	cmp.l pkg.Context.Count(a6), d4
	bhs.w bad
	tst.b 0(a5, d4.l)
	bne.w bad
	move.b d7, 0(a5, d4.l)
	cmpi.b #PENDING, d7
	bne.w labelTail
	cmpi.w #12, d6
	blo.w bad
	addq.l #5, a3
	cmpi.b #expr.COMPILED_TAG, (a3)
	bne.w bad
	moveq #0, d0
	move.b 1(a3), d0
	beq.w bad
	addi.w #11, d0
	cmp.w d6, d0
	bne.w bad
	cmpi.b #runtime.EXPRVM_V2_OPCODE_END, -1(a0, d6.w)
	bne.w bad
	move.l a3, d0
	sub.l Base, d0
	lsl.l #2, d4
	move.l d0, 0(a4, d4.l)
	addq.l #1, Constants
	bra.w next
labelTail
	; An explicit label may precede .end on the same record.
	cmpi.w #14, d6
	blo.w next
	addq.l #5, a3
	moveq #14, d2
	cmpi.b #7, (a3)
	bne.w next
directive
	cmpi.b #1, 1(a3)
	bhi.w next
	move.w 2(a3), d0
	cmp.w pkg.Header.EndDirective(a2), d0
	bne.w next
	tst.b 4(a3)
	bne.w bad
	cmp.w d2, d6
	bne.w bad
	bra.w good
next
	adda.l d6, a0
	bra.w line
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; index
	.endsection
	.endmodule
