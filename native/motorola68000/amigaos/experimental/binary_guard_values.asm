; @opforge-owner: experimental.amigaos.binary_guard_values
; Statement-time immutable snapshots used only by runtime while guards.
; Ordinary emission retains the dependency resolver's eager bindings.
	.module experimental.amigaos.binary_guard_values
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_repeat_inventory as inventory
	.use opasm.amigaos.binary_expression as expression
	.use prvm.amigaos.packed_loop as parser
	.use prvm.amigaos.abi as abi
	.use exprvm.amigaos.runtime as runtime
	.pub
PackedRecord	.struct
Length	.byte ?
Flags	.byte ?
SourceLine	.word ?
NameKind	.byte ?
NameId	.word ?
NameQualifier	.byte ?
Marker	.byte ?
Rhs	.byte ?
	.endstruct
Decl	.struct
NameKind	.byte ?
NameId	.word ?
NameQualifier	.byte ?
Marker	.byte ?
Rhs	.byte ?
	.endstruct
DECLARATION_PREFIX_BYTES = Decl.Rhs
RECORD_DECLARATION_PREFIX_BYTES = PackedRecord.Rhs
Row	.struct
Id	.long ?
Low	.long ?
High	.long ?
Defined	.long ?
Kind	.long ?
SavedLow	.long ?
SavedHigh	.long ?
SavedDefined	.long ?
SavedKind	.long ?
	.endstruct
ROW_BYTES = Row.SavedKind+4
State	.struct
Pass	.long ?
Active	.long ?
Records	.long ?
Declarations	.long ?
Block	.res memory.Block.Used+4
	.endstruct
STATE_BYTES = State.Block+memory.Block.Used+4
	.section code, kind=code
; A0=state,A1=records,D0=bytes,A2=context,A3=optional scoped declaration map.
; Scoped snapshots stay in their live iteration cells, outside global overlays.
; Allocate only when while exists.
; Records have already passed frontend and dependency bounds validation.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l State.Block+memory.Block.Used(a5)
	clr.l State.Active(a5)
	move.l a1, State.Records(a5)
	move.l a3, State.Declarations(a5)
	movea.l a1, a4
	movea.l a1, a6
	adda.l d0, a6
	moveq #0, d7
	moveq #0, d6
scan
	cmpa.l a6, a4
	bhs.w scanned
	moveq #0, d5
	move.b PackedRecord.Length(a4), d5
	addq.w #1, d5
	cmpi.w #RECORD_DECLARATION_PREFIX_BYTES, d5
	blo.w next
	btst #3, PackedRecord.Flags(a4)
	bne.w next
	cmpi.b #1, PackedRecord.NameKind(a4)
	bhi.w control
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, PackedRecord.Marker(a4)
	bne.w control
	bsr.w scoped
	bne.w control
	addq.l #1, d7
control
	movea.l a4, a0
	move.l d5, d0
	movea.l pkg.Context.Package(a2), a1
	move.l pkg.Header.LoopPlanBytes(a1), d1
	adda.l pkg.Header.LoopPlan(a1), a1
	jsr parser.classify
	cmpi.l #abi.PRVM_LOOP_WHILE, d0
	bne.w next
	moveq #1, d6
next
	adda.l d5, a4
	bra.w scan
scanned
	clr.l State.Pass(a5)
	tst.l d6
	beq.w good
	move.l #1, State.Pass(a5)
	tst.l d7
	beq.w good
	cmpi.l #memory.LIMIT/ROW_BYTES, d7
	bhi.w bad
	mulu.w #ROW_BYTES, d7
	move.l d7, d0
	lea State.Block(a5), a0
	jsr memory.reserve
	bne.w bad
	move.l d7, State.Block+memory.Block.Used(a5)
	movea.l State.Block+memory.Block.Pointer(a5), a3
	movea.l State.Records(a5), a4
fill
	cmpa.l a6, a4
	bhs.w good
	moveq #0, d5
	move.b PackedRecord.Length(a4), d5
	addq.w #1, d5
	cmpi.w #RECORD_DECLARATION_PREFIX_BYTES, d5
	blo.w fillNext
	btst #3, PackedRecord.Flags(a4)
	bne.w fillNext
	cmpi.b #1, PackedRecord.NameKind(a4)
	bhi.w fillNext
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, PackedRecord.Marker(a4)
	bne.w fillNext
	bsr.w scoped
	bne.w fillNext
	moveq #0, d0
	move.w PackedRecord.NameId(a4), d0
	move.l d0, Row.Id(a3)
	clr.l Row.Low(a3)
	clr.l Row.High(a3)
	clr.l Row.Defined(a3)
	clr.l Row.Kind(a3)
	adda.w #ROW_BYTES, a3
fillNext
	adda.l d5, a4
	bra.w fill
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepare
; A0=state,D0=pass. Reset provisional capture; retain pass-one snapshots for pass two.
beginPass	.block
	tst.l State.Pass(a0)
	beq.w good
	move.l d0, State.Pass(a0)
	cmpi.l #1, d0
	bne.w good
	movem.l d1/a1, -(sp)
	movea.l State.Block+memory.Block.Pointer(a0), a1
	move.l State.Block+memory.Block.Used(a0), d1
clear
	tst.l d1
	beq.w cleared
	clr.l Row.Low(a1)
	clr.l Row.High(a1)
	clr.l Row.Defined(a1)
	clr.l Row.Kind(a1)
	adda.w #ROW_BYTES, a1
	subi.l #ROW_BYTES, d1
	bra.w clear
cleared
	movem.l (sp)+, d1/a1
good
	moveq #0, d0
	rts
	.bend  ; beginPass
; A0=state,A2=context. Swap only immutable cells; live runtime bindings stay live.
enter	.block
	moveq #0, d0
	tst.l State.Pass(a0)
	beq.w done
	tst.l State.Active(a0)
	bne.w bad
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l State.Block+memory.Block.Pointer(a5), a4
	move.l State.Block+memory.Block.Used(a5), d7
	movea.l pkg.Context.Values(a2), a6
	movea.l pkg.Context.Defined(a2), a3
row
	tst.l d7
	beq.w good
	move.l Row.Id(a4), d1
	move.l d1, d6
	lsl.l #3, d6
	move.l runtime.Value.Low(a6, d6.l), Row.SavedLow(a4)
	move.l runtime.Value.High(a6, d6.l), Row.SavedHigh(a4)
	moveq #0, d0
	move.b 0(a3, d1.l), d0
	move.l d0, Row.SavedDefined(a4)
	movea.l pkg.Context.Owner(a2), a0
	bsr.w getKind
	bne.w failed
	move.l d2, Row.SavedKind(a4)
	addi.l #ROW_BYTES, State.Active(a5)
	move.l Row.Kind(a4), d2
	bsr.w setKind
	bne.w failed
	move.l Row.Low(a4), runtime.Value.Low(a6, d6.l)
	move.l Row.High(a4), runtime.Value.High(a6, d6.l)
	move.l Row.Defined(a4), d0
	move.b d0, 0(a3, d1.l)
	adda.w #ROW_BYTES, a4
	subi.l #ROW_BYTES, d7
	bra.w row
good
	moveq #0, d0
	bra.w restored
failed
	movea.l a5, a0
	bsr.w leave
	moveq #1, d0
restored
	movem.l (sp)+, d1-d7/a0-a6
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; enter
; A0=state,A2=context. Restore bindings after guarded evaluation; others preserved.
leave	.block
	tst.l State.Active(a0)
	beq.w done
	movem.l d0-d7/a0-a6, -(sp)
	move.l State.Active(a0), d7
	clr.l State.Active(a0)
	movea.l State.Block+memory.Block.Pointer(a0), a4
	movea.l pkg.Context.Values(a2), a6
	movea.l pkg.Context.Defined(a2), a3
row
	tst.l d7
	beq.w restored
	move.l Row.Id(a4), d1
	move.l d1, d6
	lsl.l #3, d6
	move.l Row.SavedLow(a4), runtime.Value.Low(a6, d6.l)
	move.l Row.SavedHigh(a4), runtime.Value.High(a6, d6.l)
	move.l Row.SavedDefined(a4), d0
	move.b d0, 0(a3, d1.l)
	movea.l pkg.Context.Owner(a2), a0
	move.l Row.SavedKind(a4), d2
	bsr.w setKind
	adda.w #ROW_BYTES, a4
	subi.l #ROW_BYTES, d7
	bra.w row
restored
	movem.l (sp)+, d0-d7/a0-a6
done
	rts
	.bend  ; leave
; A0=state,A1=name+marker+RHS,A2=context,A3=RHS end.
; Snapshot at executed declaration time, preserving compound kind/descriptor.
declaration	.block
	tst.l State.Pass(a0)
	beq.w good
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	movea.l a3, a6
	moveq #0, d4
	move.w Decl.NameId(a4), d4
	; Iteration-owned constants retain snapshots in their full local cells.
	move.l State.Declarations(a5), d0
	beq.w globalDeclaration
	movea.l d0, a0
	move.l d4, d0
	lsl.l #inventory.DECL_SHIFT, d0
	tst.w inventory.Decl.Owner(a0, d0.l)
	bne.w localDeclaration
globalDeclaration
	movea.l a5, a0
	jsr enter
	bne.w bad
	lea Decl.Rhs(a4), a0
	movea.l a6, a1
	jsr expression.evaluateValue
	tst.l d0
	bne.w leaveBad
	cmpa.l a6, a0
	bne.w leaveBad
	tst.l d2
	beq.w valueReady
	cmpi.l #1, State.Pass(a5)
	bne.w leaveBad
valueReady
	move.l d2, d6
	movea.l State.Block+memory.Block.Pointer(a5), a4
	move.l State.Block+memory.Block.Used(a5), d7
find
	tst.l d7
	beq.w leaveBad
	cmp.l Row.Id(a4), d4
	beq.w found
	adda.w #ROW_BYTES, a4
	subi.l #ROW_BYTES, d7
	bra.w find
found
	move.l d1, Row.Low(a4)
	move.l pkg.Context.High(a2), Row.High(a4)
	move.l pkg.Context.Kind(a2), Row.Kind(a4)
	moveq #1, d0
	tst.l d6
	beq.w defined
	moveq #0, d0
defined
	tst.l d0
	beq.w publish
	tst.l Row.Kind(a4)
	beq.w publish
	bset #expression.COMPOUND_BIT, d0
publish
	move.l d0, Row.Defined(a4)
	movea.l a5, a0
	jsr leave
	moveq #0, d0
	bra.w done
localDeclaration
	moveq #0, d0
	bra.w done
leaveBad
	movea.l a5, a0
	jsr leave
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
good
	moveq #0, d0
	rts
	.bend  ; declaration
; A0=state. Called only outside shadow evaluation. All registers preserved.
release	.block
	movem.l d0/a0, -(sp)
	clr.l State.Pass(a0)
	clr.l State.Active(a0)
	clr.l State.Block+memory.Block.Used(a0)
	clr.l State.Records(a0)
	lea State.Block(a0), a0
	jsr memory.release
	movem.l (sp)+, d0/a0
	rts
	.bend  ; release
	.priv
; A0=optional value owner,D1=ID. Scalar-only contexts need no kind storage.
getKind	.block
	move.l a0, d0
	bne.w owned
	moveq #0, d2
	rts
owned
	jsr values.getKind
	rts
	.bend  ; getKind
setKind	.block
	move.l a0, d0
	bne.w owned
	tst.l d2
	beq.w good
	moveq #1, d0
	rts
good
	moveq #0, d0
	rts
owned
	jsr values.setKind
	rts
	.bend  ; setKind
; A4=declaration,A5=guard state. D0/CCR=nonzero for iteration-owned IDs.
; Other registers preserved; global snapshots cannot overwrite scoped cells.
scoped	.block
	movem.l d1/a0, -(sp)
	move.l State.Declarations(a5), d0
	beq.w done
	movea.l d0, a0
	moveq #0, d1
	move.w PackedRecord.NameId(a4), d1
	lsl.l #inventory.DECL_SHIFT, d1
	moveq #0, d0
	move.w inventory.Decl.Owner(a0, d1.l), d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; scoped
	.endsection
	.endmodule
