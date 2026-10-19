; Shared assembly callbacks. They preserve all registers except D0/CCR.
allocate	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea Image, a0
	jsr memory.reserveExact
	bne.w bad
	move.l memory.Block.Pointer(a0), assembly.Frame.Output(a1)
	move.l memory.Block.Capacity(a0), assembly.Frame.Capacity(a1)
	bra.w done
bad
	move.l #ERROR, Status
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; allocate

rejectReloc	.block
	moveq #1, d0
	rts
	.bend  ; rejectReloc

; The compact owner has no global-sized parameter table. Every pass is seeded
; from the bridge: scalars are absolute, foreign address labels have base zero.
seed	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l BridgeOwner+memory.Block.Pointer, a4
	movea.l Cells+pkg.Context.Values, a2
	movea.l Cells+pkg.Context.Defined, a3
	move.l ExecBase, d6
	move.l d6, d0
	lsl.l #3, d0
	adda.l d0, a2
	adda.l d6, a3
	move.l BridgeCount, d7
next
	tst.l d7
	beq.w good
	tst.l Bridge.Known(a4)
	beq.w label
	move.l Bridge.Low(a4), exprvm.Value.Low(a2)
	move.l Bridge.High(a4), exprvm.Value.High(a2)
	move.b #dependencies.ABSOLUTE, (a3)
	bra.w advance
label
	tst.l Bridge.Block(a4)
	bne.w advance
	clr.l exprvm.Value.Low(a2)
	clr.l exprvm.Value.High(a2)
	move.b #1, (a3)
advance
	adda.w #BRIDGE_BYTES, a4
	adda.w #exprvm.VALUE_BYTES, a2
	addq.l #1, a3
	subq.l #1, d7
	bra.w next
good
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seed

; Locate the exact original row from its temporary record offset. Final encoded
; bytes may escape only into our image allocation; no package pointer survives.
encoded	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d1, d7
	move.l d2, d6
	move.l d3, d5
	move.l Count, d4
	movea.l Rows+memory.Block.Pointer, a4
find
	tst.l d4
	beq.w bad
	move.l assembly.Frame.RecordOffset(a6), d0
	cmp.l Row.Temp(a4), d0
	beq.w found
	adda.w #ROW_BYTES, a4
	subq.l #1, d4
	bra.w find
found
	tst.l Row.Instruction(a4)
	beq.w bad
	tst.l Row.Length(a4)
	bne.w bad
	; Encoded points into the shared encoder's scratch buffer before emitBytes.
	; This flat subset has no gaps: its PC is the eventual image buffer offset.
	move.l Cells+pkg.Context.Pc, d0
	move.l d0, Row.Data(a4)
	add.l d7, d0
	bcs.w bad
	cmp.l Image+memory.Block.Capacity, d0
	bhi.w bad
	move.l d7, Row.Length(a4)
	jsr encoding.outputFixupCount
	move.l d0, d4
	jsr encoding.outputPositionUsed
	tst.l d0
	bne.w positional
	tst.l d4
	bne.w absolute
	; No address provenance and no instruction-PC read: bytes are invariant.
	tst.l d5
	bne.w bad
	tst.l d6
	bne.w bad
	bra.w good
positional
	tst.l d4
	bne.w bad
	cmpi.l #1, d5
	bne.w bad
	jsr encoding.outputPositionProof
	cmpi.l #1, d0
	bne.w bad
	; In flat mode the proof target is the compact symbol identity.
	move.l d1, d0
	bsr.w compactBridge
	bne.w bad
	move.l Bridge.Block(a0), d0
	beq.w bad
	cmp.l Row.Block(a4), d0
	bne.w bad
	bra.w good
absolute
	cmpi.l #1, d4
	bne.w bad
	cmpi.l #1, d5
	bne.w bad
	moveq #0, d0
	jsr encoding.outputFixupOrder
	bne.w bad
	movea.l Caller, a0
	movea.l Frame.Package(a0), a0
	cmp.w pkg.Header.LittleEndian(a0), d1
	bne.w bad
	moveq #0, d0
	jsr encoding.outputFixup
	bne.w bad
	cmpi.l #4, d2
	bne.w bad
	move.l d1, Row.Fixup(a4)
	addq.l #4, d1
	cmp.l d7, d1
	bhi.w bad
	move.l d3, d0
	bsr.w compactBridge
	bne.w bad
	move.l Bridge.Global(a0), Row.Target(a4)
	; Fixup metadata carries the encoded scalar; subtract the temporary value.
	move.l d3, d0
	lsl.l #3, d0
	movea.l Cells+pkg.Context.Values, a0
	tst.l exprvm.Value.High(a0, d0.l)
	bne.w bad
	sub.l exprvm.Value.Low(a0, d0.l), d4
	bcs.w bad
	tst.l d4
	bne.w bad  ; retain only zero addends until final-placement range proof exists
	move.l d4, Row.Addend(a4)
good
	moveq #0, d0
	bra.w done
bad
	move.l #INELIGIBLE, Status
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; encoded

; D0=compact full symbol identity -> A0=bridge row,D0/CCR=status.
; Preserve other registers. Package names have no address provenance.
compactBridge	.block
	movem.l d1, -(sp)
	sub.l ExecBase, d0
	bcs.w bad
	cmp.l BridgeCount, d0
	bhs.w bad
	mulu.w #BRIDGE_BYTES, d0
	movea.l BridgeOwner+memory.Block.Pointer, a0
	adda.l d0, a0
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; compactBridge
