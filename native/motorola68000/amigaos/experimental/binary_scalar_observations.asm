; Prior numeric layout values, separate from executed declaration ownership.
; Allocate only after sizing requests replay; compound arena offsets never survive.
; @opforge-owner: experimental.amigaos.binary_scalar_observations
	.module experimental.amigaos.binary_scalar_observations
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_dependencies as dependencies
	.use experimental.amigaos.binary_dependency_layout as graph
	.use experimental.amigaos.binary_repeat_inventory as inventory
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_module_records as compiled
	.use exprvm.amigaos.runtime as runtime
	.include "memory_telemetry.i"
	.pub
Row	.struct
Low	.long ?
High	.long ?
Defined	.byte ?
Section	.byte ?
Flags	.byte ?
Reserved	.byte ?
.endstruct
ROW_BYTES = Row.Reserved+1
ELIGIBLE_BIT = 0
SEEN_BIT = 1
State	.struct
Rows	.res memory.Block.Used+4
Context	.long ?
Count	.long ?
Changed	.word ?
Reserved	.word ?
.endstruct
STATE_BYTES = State.Reserved+2
	.section code, kind=code
; A0=zero-initialized state,A1=context. D0/CCR=status; others preserved.
; One 12-byte row per source ID, at most 768 KiB. Eligible IDs are global
; labels or readonly layout roots with an expression; selected-only, scoped,
; mutable and snapshot roots are excluded. Only scalar values are captured.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	tst.l State.Count(a5)
	bne.w good
	movea.l a1, a6
	move.l a1, State.Context(a5)
	move.l pkg.Context.Count(a6), d6
	beq.w bad
	cmpi.l #65536, d6
	bhi.w bad
	jsr dependencies.observationMetadata
	bne.w done
	move.l d6, d0
	mulu.l #ROW_BYTES, d0
	lea State.Rows(a5), a0
	jsr memory.reserveExact
	bne.w done
	move.l d6, d1
	mulu.l #ROW_BYTES, d1
	move.l d1, memory.Block.Used(a0)
	move.l d6, State.Count(a5)
	movea.l memory.Block.Pointer(a0), a5
	moveq #0, d7
next
	clr.l Row.Low(a5)
	clr.l Row.High(a5)
	clr.l Row.Defined(a5)
	moveq #0, d0
	move.b 0(a3, d7.l), d0
	cmpi.b #graph.LABEL, d0
	beq.w inventoryCheck
	cmpi.b #graph.LAYOUT, d0
	bne.w advance
	move.l d7, d0
	lsl.l #graph.OFFSET_SHIFT, d0
	tst.l graph.Offset.Expression(a4, d0.l)
	beq.w advance
inventoryCheck
	move.l a2, d0
	beq.w eligible
	move.l d7, d0
	lsl.l #inventory.DECL_SHIFT, d0
	tst.w inventory.Decl.Owner(a2, d0.l)
	bne.w advance
	move.w inventory.Decl.Kind(a2, d0.l), d0
	andi.w #inventory.CONDITIONAL+inventory.UNCONDITIONAL, d0
	cmpi.w #inventory.CONDITIONAL, d0
	beq.w advance
eligible
	bset #ELIGIBLE_BIT, Row.Flags(a5)
	bsr.w numeric
	bne.w done
	move.l d1, Row.Low(a5)
	move.l d2, Row.High(a5)
	move.w d4, Row.Defined(a5)
advance
	adda.w #ROW_BYTES, a5
	addq.l #1, d7
	cmp.l d6, d7
	blo.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepare

; A0=state. Aggregate execution across all mapped sweeps in one sizing pass.
; D0/CCR=0; others preserved. No allocation during replay.
beginPass	.block
	movem.l d1/a1, -(sp)
	clr.w State.Changed(a0)
	move.l State.Count(a0), d1
	beq.w done
	.MEMORY_COUNTER_INC ActivePasses
	movea.l State.Rows+memory.Block.Pointer(a0), a1
next
	bclr #SEEN_BIT, Row.Flags(a1)
	adda.w #ROW_BYTES, a1
	subq.l #1, d1
	bne.w next
done
	moveq #0, d0
	movem.l (sp)+, d1/a1
	rts
	.bend  ; beginPass

; A0=state,A1=record,A2=stream end. D0/CCR=status; others preserved.
; Called after control selection, before independent output filters. Bounds
; come from this record, so a following record cannot supply its marker.
mark	.block
	movem.l d1-d2/a0-a2, -(sp)
	tst.l State.Count(a0)
	beq.w good
	move.l a2, d0
	sub.l a1, d0
	bcs.w bad
	beq.w bad
	moveq #0, d2
	move.b (a1), d2
	addq.w #1, d2
	cmp.l d0, d2
	bhi.w bad
	cmpi.l #source.LayoutRecord.Opcode+1, d2
	blo.w good
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a1)
	beq.w sourceDeclaration
	cmpi.b #compiled.OP_OWNERS, source.LayoutRecord.Opcode(a1)
	bne.w good
	; Selection already removed dead whole owners. Capture the placed public
	; cells at endPass even though their source declarations were retired.
	move.l a0, -(sp)
	movea.l a1, a0
	move.l d2, d0
	jsr compiled.ownersView
	movea.l (sp)+, a0
	bne.w bad
compiledLabel
	tst.l d1
	beq.w good
	moveq #0, d2
	move.w (a1)+, d2
	cmp.l State.Count(a0), d2
	bhs.w bad
	mulu.l #ROW_BYTES, d2
	movea.l State.Rows+memory.Block.Pointer(a0), a2
	adda.l d2, a2
	bset #SEEN_BIT, Row.Flags(a2)
	subq.l #1, d1
	bra.w compiledLabel
sourceDeclaration
	cmpi.l #inventory.PackedRecord.Rhs, d2
	blo.w good
	cmpi.b #1, inventory.PackedRecord.NameKind(a1)
	bhi.w good
	cmpi.b #source.TOKEN_LABEL_DECLARATION, inventory.PackedRecord.Marker(a1)
	beq.w named
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, inventory.PackedRecord.Marker(a1)
	bne.w good
named
	moveq #0, d1
	move.w inventory.PackedRecord.NameId(a1), d1
	cmp.l State.Count(a0), d1
	bhs.w bad
	mulu.l #ROW_BYTES, d1
	movea.l State.Rows+memory.Block.Pointer(a0), a1
	adda.l d1, a1
	bset #SEEN_BIT, Row.Flags(a1)
good
	moveq #0, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; mark

; A0=state. Publish only completed numeric observations. A declaration that
; was not executed loses its previous availability. D0=status,D1=changed.
endPass	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a4
	move.l State.Count(a4), d6
	beq.w good
	movea.l State.Context(a4), a6
	movea.l State.Rows+memory.Block.Pointer(a4), a5
	moveq #0, d7
next
	btst #ELIGIBLE_BIT, Row.Flags(a5)
	beq.w advance
	moveq #0, d1
	moveq #0, d2
	moveq #0, d4
	btst #SEEN_BIT, Row.Flags(a5)
	beq.w compare
	bsr.w numeric
	bne.w done
compare
	cmp.l Row.Low(a5), d1
	bne.w changed
	cmp.l Row.High(a5), d2
	bne.w changed
	cmp.w Row.Defined(a5), d4
	beq.w advance
changed
	.MEMORY_COUNTER_INC ChangedRows
	move.w #1, State.Changed(a4)
	move.l d1, Row.Low(a5)
	move.l d2, Row.High(a5)
	move.w d4, Row.Defined(a5)
advance
	adda.w #ROW_BYTES, a5
	addq.l #1, d7
	cmp.l d6, d7
	blo.w next
good
	moveq #0, d1
	move.w State.Changed(a4), d1
	moveq #0, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; endPass

; A0=state. After destructive layout reset, restore only captured scalar cells.
; Typed kinds remain zero; no descriptor or ownership decision is restored.
; D0/CCR=0; others preserved.
restore	.block
	movem.l d1-d3/a0-a4, -(sp)
	move.l State.Count(a0), d3
	beq.w done
	movea.l State.Context(a0), a1
	movea.l pkg.Context.Values(a1), a2
	movea.l pkg.Context.Defined(a1), a3
	movea.l pkg.Context.SectionIds(a1), a4
	movea.l State.Rows+memory.Block.Pointer(a0), a0
next
	btst #ELIGIBLE_BIT, Row.Flags(a0)
	beq.w advance
	tst.b Row.Defined(a0)
	beq.w advance
	.MEMORY_COUNTER_INC RestoredRows
	move.l Row.Low(a0), runtime.Value.Low(a2)
	move.l Row.High(a0), runtime.Value.High(a2)
	move.b Row.Defined(a0), (a3)
	move.b Row.Section(a0), (a4)
advance
	adda.w #ROW_BYTES, a0
	addq.l #runtime.VALUE_BYTES, a2
	addq.l #1, a3
	addq.l #1, a4
	subq.l #1, d3
	bne.w next
done
	moveq #0, d0
	movem.l (sp)+, d1-d3/a0-a4
	rts
	.bend  ; restore

; A0=state. Safe on partial preparation and all assembly exits; registers kept.
release	.block
	move.l a0, -(sp)
	clr.l State.Count(a0)
	clr.l State.Context(a0)
	clr.w State.Changed(a0)
	lea State.Rows(a0), a0
	jsr memory.release
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; release
	.priv
; A6=context,D7=ID. D1=low,D2=high,D4=Defined/Section word.
; Typed or unavailable cells become an unavailable scalar observation.
numeric	.block
	movem.l d3/a0-a1, -(sp)
	movea.l pkg.Context.Owner(a6), a0
	move.l a0, d0
	beq.w scalar
	move.l d7, d1
	jsr values.getKind
	tst.l d0
	bne.w done
	tst.l d2
	bne.w unavailable
scalar
	moveq #0, d4
	movea.l pkg.Context.Defined(a6), a0
	move.b 0(a0, d7.l), d4
	beq.w unavailable
	bmi.w unavailable
	lsl.w #8, d4
	movea.l pkg.Context.SectionIds(a6), a0
	move.b 0(a0, d7.l), d4
	move.l d7, d3
	lsl.l #3, d3
	movea.l pkg.Context.Values(a6), a0
	move.l runtime.Value.Low(a0, d3.l), d1
	move.l runtime.Value.High(a0, d3.l), d2
	moveq #0, d0
	bra.w done
unavailable
	moveq #0, d1
	moveq #0, d2
	moveq #0, d4
	moveq #0, d0
done
	movem.l (sp)+, d3/a0-a1
	tst.l d0
	rts
	.bend  ; numeric
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
	.section bss, kind=bss
; Aggregate work only. ActivePasses includes final emission with live rows.
ActivePasses	.res long, 1
ChangedRows	.res long, 1
RestoredRows	.res long, 1
	.endsection
.endif
.endif
	.endmodule
