; Source-ordered evaluation of finalized declaration capsules in private storage.
; @opforge-owner: experimental.amigaos.binary_leaf_evaluate
	.module experimental.amigaos.binary_leaf_evaluate
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_values as values
	.use opasm.amigaos.binary_expression as expression
	.use exprvm.amigaos.runtime as runtime
	.pub
State	.struct
Values	.res memory.Block.Used+4
Defined	.res memory.Block.Used+4
Owner	.res values.OWNER_BYTES
Expression	.res expression.FRAME_BYTES
Count	.long ?
	.endstruct
STATE_BYTES = State.Count+4
	.section code, kind=code
; A0=private zeroed/released State,D0=complete source-ID universe size.
; D0/CCR=status; others preserved. No live scope, plans or known cells are used.
begin	.block
	movem.l d1-d3/a0-a2, -(sp)
	movea.l a0, a2
	cmpi.l #65535, d0
	bhi.w bad
	move.l d0, State.Count(a2)
	move.l d0, d3
	lea State.Owner(a2), a0
	jsr values.init
	bne.w done
	lea State.Values(a2), a0
	move.l d3, d0
	lsl.l #3, d0
	jsr memory.reserveExact
	bne.w done
	move.l d3, d1
	lsl.l #3, d1
	move.l d1, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	bsr.w clear
	lea State.Defined(a2), a0
	move.l d3, d0
	jsr memory.reserveExact
	bne.w done
	move.l d3, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	move.l d3, d1
	bsr.w clear
	lea State.Expression(a2), a1
	move.l #expression.FRAME_BYTES, d1
	bsr.w clear
	move.l State.Values+memory.Block.Pointer(a2), State.Expression+expression.Frame.Values(a2)
	move.l State.Defined+memory.Block.Pointer(a2), State.Expression+expression.Frame.Defined(a2)
	move.l d3, State.Expression+expression.Frame.Count(a2)
	lea State.Owner(a2), a0
	move.l a0, State.Expression+expression.Frame.Owner(a2)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; begin

; A0=State. Release all private evaluation storage; other registers preserved.
release	.block
	movem.l d0/a0-a1, -(sp)
	movea.l a0, a1
	lea State.Values(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.Defined(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.Owner(a1), a0
	jsr values.release
	movem.l (sp)+, d0/a0-a1
	rts
	.bend  ; release

; A0=State,D0=declaration ID,A1=final capsule,A2=bounded end.
; D0/CCR=status,D1/D2=scalar low/high; other registers preserved. Evaluate once
; through shared ExprVM, require exact consumption, defined absolute scalar
; result, and publish only into private source-ordered cells. No compilation.
scalar	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d7
	movea.l a2, a4
	movea.l a1, a0
	movea.l a2, a1
	lea State.Expression(a6), a2
	clr.l expression.Frame.Kind(a2)
	clr.l expression.Frame.Provenance(a2)
	clr.l expression.Frame.High(a2)
	jsr expression.evaluateValue
	bne.w bad
	tst.l d2
	bne.w bad
	cmpa.l a4, a0
	bne.w bad
	tst.l State.Expression+expression.Frame.Kind(a6)
	bne.w bad
	tst.l State.Expression+expression.Frame.Provenance(a6)
	bne.w bad
	move.l State.Expression+expression.Frame.High(a6), d2
	move.l d7, d0
	moveq #values.SCALAR, d3
	bsr.w store
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; scalar

; A0=State,D0=declaration ID,A1=external finalized seed arena,D1=seed offset,
; D2=arena bytes. D0/CCR=status; other registers preserved. Copy the plain schema
; into the private value owner, then expose its current ID for later expressions.
type	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d7
	move.l d2, d0
	lea State.Owner(a6), a0
	moveq #values.STRUCT_TYPE, d3
	jsr values.copyRecord
	bne.w done
	move.l d2, d1
	moveq #0, d2
	move.l d7, d0
	bsr.w store
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; type
	.priv
; A6=State,D0=ID,D1/D2=low/high,D3=kind. D0/CCR=status; others preserved.
store	.block
	movem.l d1-d5/a0-a2, -(sp)
	cmp.l State.Count(a6), d0
	bhs.w bad
	move.l d0, d3
	movea.l State.Defined+memory.Block.Pointer(a6), a2
	tst.b 0(a2, d3.l)
	bne.w bad
	move.l d1, d4
	move.l d2, d5
	move.l d0, d1
	move.l 8(sp), d2  ; saved D3 is the explicit value kind
	lea State.Owner(a6), a0
	jsr values.setKind
	bne.w done
	move.b #1, 0(a2, d3.l)
	lsl.l #3, d3
	movea.l State.Values+memory.Block.Pointer(a6), a1
	move.l d4, runtime.Value.Low(a1, d3.l)
	move.l d5, runtime.Value.High(a1, d3.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a0-a2
	tst.l d0
	rts
	.bend  ; store

; A1=destination,D1=bytes. Clobbers A1/D1; CCR unspecified.
clear	.block
	tst.l d1
	beq.w done
byte
	clr.b (a1)+
	subq.l #1, d1
	bne.w byte
done
	rts
	.bend  ; clear
	.endsection
	.endmodule
