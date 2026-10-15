; Owned whole-line macro proofs; shared macro plans own fragment semantics.
; @opforge-owner: experimental.amigaos.binary_line_proofs
	.module experimental.amigaos.binary_line_proofs
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_macro_plans as plans
	.pub
STATUS_OK = 0
STATUS_INVALID = 1
STATUS_UNSUPPORTED = 2
MAX_ROWS = 64
MAX_SOURCE_BYTES = 1024
HEADER_BYTES = 12
FRAGMENT_BYTES = 10
ROW_BYTES = 32
SCRATCH_BYTES = MAX_ROWS*ROW_BYTES
ARENA_LIMIT = $fffffffe
Container	.struct
End	.long ?  ; absolute arena extent, including alignment
SpellingBytes	.word ?
TokenCount	.word ?
RecipeCount	.word ?
Reserved	.word ?
.endstruct
Fragment	.struct
Kind	.word ?
Start	.word ?
End	.word ?
Aux0	.word ?
Aux1	.word ?
.endstruct
ViewFrame	.struct
Arena	.long ?
Handle	.long ?
Rows	.long ?
Capacity	.long ?  ; row capacity, not bytes
Source	.long ?
SourceBytes	.long ?
TokenCount	.long ?
Count	.long ?
Staging	.long ?  ; caller-owned SCRATCH_BYTES; disjoint from Rows/input/frame
.endstruct
FRAME_BYTES = ViewFrame.Staging+4
	.section code, kind=code
; A0=plans.Frame. D0/CCR=status,D1=offset+1 handle (zero on failure).
; Other registers preserved. INVALID means malformed input/allocation failure;
; UNSUPPORTED means valid but noncanonical input, with no arena mutation.
; Source bytes are copied once; Used is published only after all writes.
create	.block
	jsr plans.validateCreate
	bne.w rejected
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l plans.Frame.Events(a6), a2
	cmpi.l #1, plans.Frame.Count(a6)
	bne.w unsupported
	cmpi.w #abi.PRVM_RESULT_MACRO_LINE, plans.Row.Kind(a2)
	bne.w unsupported
	tst.w plans.Row.Flags(a2)
	bne.w unsupported
	tst.l plans.Row.PackedStart(a2)
	bne.w unsupported
	move.l plans.Frame.TokenCount(a6), d4
	cmpi.l #MAX_ROWS, d4
	bhi.w unsupported
	cmp.l plans.Row.PackedEnd(a2), d4
	bne.w unsupported
	tst.l plans.Row.SpellingStart(a2)
	bne.w unsupported
	tst.l plans.Row.Aux0(a2)
	bne.w unsupported
	tst.l plans.Row.Aux1(a2)
	bne.w unsupported
	tst.l plans.Row.Aux2(a2)
	bne.w unsupported
	cmpi.l #MAX_SOURCE_BYTES, plans.Frame.SourceBytes(a6)
	bhi.w unsupported
	move.l plans.Row.SpellingEnd(a2), d5
	cmpi.l #MAX_SOURCE_BYTES, d5
	bhi.w unsupported
	tst.l plans.Frame.RecipeEvents(a6)
	beq.w unsupported
	move.l plans.Frame.RecipeCount(a6), d7
	beq.w unsupported
	cmpi.l #MAX_ROWS, d7
	bhi.w unsupported
	movea.l plans.Frame.RecipeEvents(a6), a2
	move.l d7, d3
check
	tst.w plans.Row.Flags(a2)
	bne.w unsupported
	tst.l plans.Row.PackedStart(a2)
	bne.w unsupported
	tst.l plans.Row.PackedEnd(a2)
	bne.w unsupported
	tst.l plans.Row.Aux2(a2)
	bne.w unsupported
	cmpi.l #MAX_SOURCE_BYTES, plans.Row.Aux0(a2)
	bhi.w unsupported
	cmpi.l #MAX_SOURCE_BYTES, plans.Row.Aux1(a2)
	bhi.w unsupported
	adda.w #ROW_BYTES, a2
	subq.l #1, d3
	bne.w check
	movea.l plans.Frame.Arena(a6), a4
	move.l memory.Block.Used(a4), d3
	btst #0, d3
	bne.w invalid
	move.l d7, d6
	mulu.w #FRAGMENT_BYTES, d6
	addi.l #HEADER_BYTES, d6
	add.l d5, d6
	addq.l #1, d6
	andi.l #$fffffffe, d6
	add.l d3, d6
	bcs.w invalid
	movea.l a4, a0
	move.l d6, d0
	move.l #ARENA_LIMIT, d1
	jsr memory.reserveBounded
	bne.w invalid
	movea.l memory.Block.Pointer(a4), a3
	adda.l d3, a3
	move.l d6, (a3)+
	move.w d5, (a3)+
	move.w d4, (a3)+
	move.w d7, (a3)+
	clr.w (a3)+
	movea.l plans.Frame.RecipeEvents(a6), a2
copyRows
	move.w plans.Row.Kind(a2), (a3)+
	move.l plans.Row.SpellingStart(a2), d0
	move.w d0, (a3)+
	move.l plans.Row.SpellingEnd(a2), d0
	move.w d0, (a3)+
	move.l plans.Row.Aux0(a2), d0
	move.w d0, (a3)+
	move.l plans.Row.Aux1(a2), d0
	move.w d0, (a3)+
	adda.w #ROW_BYTES, a2
	subq.l #1, d7
	bne.w copyRows
	movea.l plans.Frame.Source(a6), a2
	move.l d5, d0
copySource
	tst.l d0
	beq.w aligned
	move.b (a2)+, (a3)+
	subq.l #1, d0
	bra.w copySource
aligned
	btst #0, d5
	beq.w publish
	clr.b (a3)+
publish
	move.l d6, memory.Block.Used(a4)
	move.l d3, d1
	addq.l #1, d1
	moveq #STATUS_OK, d0
	bra.w done
unsupported
	moveq #STATUS_UNSUPPORTED, d0
	bra.w failure
invalid
	moveq #STATUS_INVALID, d0
failure
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
rejected
	moveq #0, d1
	tst.l d0
	rts
	.bend  ; create

; A0=ViewFrame; caller provides Rows, Capacity and SCRATCH_BYTES of Staging.
; D0/CCR=status,D1=count,A1=Rows,D2=0,A2=owned source on success.
; Other registers preserved. Outputs, including caller rows, are atomic on error.
; Staging may change on error. Its full extent must be disjoint from the full
; SCRATCH_BYTES Rows extent, frame and input arena. Pointers expire on mutation.
view	.block
	movem.l d3-d7/a0/a3-a6, -(sp)
	movea.l a0, a6
	move.l ViewFrame.Staging(a6), d0
	beq.w invalid
	move.l d0, d1
	addi.l #SCRATCH_BYTES, d1
	bcs.w invalid
	move.l ViewFrame.Rows(a6), d2
	beq.w invalid
	move.l d2, d3
	addi.l #SCRATCH_BYTES, d3
	bcs.w invalid
	cmp.l d2, d1
	bls.w separate
	cmp.l d3, d0
	blo.w invalid
separate
	movea.l d0, a3
	movea.l ViewFrame.Arena(a6), a0
	move.l ViewFrame.Handle(a6), d1
	bsr.w decode
	bne.w done
	cmp.l ViewFrame.Capacity(a6), d7
	bhi.w invalid
	tst.l ViewFrame.Rows(a6)
	beq.w invalid
	movea.l ViewFrame.Rows(a6), a1
	movea.l ViewFrame.Staging(a6), a3
	move.l d7, d0
	lsl.l #3, d0
	subq.l #1, d0
copy
	move.l (a3)+, (a1)+
	dbra d0, copy
	move.l a2, ViewFrame.Source(a6)
	move.l d5, ViewFrame.SourceBytes(a6)
	move.l d4, ViewFrame.TokenCount(a6)
	move.l d7, ViewFrame.Count(a6)
	move.l d7, d1
	movea.l ViewFrame.Rows(a6), a1
	moveq #0, d2
	moveq #STATUS_OK, d0
	bra.w done
invalid
	moveq #STATUS_INVALID, d0
done
	movem.l (sp)+, d3-d7/a0/a3-a6
	tst.l d0
	rts
	.bend  ; view

; A0=memory.Block,D1=handle,A3=caller-owned SCRATCH_BYTES staging, disjoint
; from the input arena. Fully validate compact container and fragments.
; D0/CCR=status,A1=source,D1=bytes,D2=token count on success; others preserved.
; Staging contents may change even on error; A3 is preserved. Source expires
; on arena mutation. No expanded rows are allocated on the guest stack.
sourceView	.block
	movem.l d3-d7/a0/a2-a6, -(sp)
	bsr.w decode
	bne.w done
	movea.l a2, a1
	move.l d5, d1
	move.l d4, d2
	moveq #STATUS_OK, d0
done
	movem.l (sp)+, d3-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; sourceView
	.priv
; A0=arena,D1=handle,A3=2048-byte staging rows. D0/CCR=status;
; on success A2=source,D4=tokens,D5=bytes,D7=count. Clobbers D1-D3/D6/A1/A3-A5.
; All extents are checked before dereference. Grammar remains shared authority.
decode	.block
	move.l a3, d0
	beq.w invalid
	addi.l #SCRATCH_BYTES, d0
	bcs.w invalid
	tst.l d1
	beq.w invalid
	subq.l #1, d1
	btst #0, d1
	bne.w invalid
	move.l memory.Block.Used(a0), d2
	cmp.l memory.Block.Capacity(a0), d2
	bhi.w invalid
	sub.l d1, d2
	bcs.w invalid
	cmpi.l #HEADER_BYTES, d2
	blo.w invalid
	tst.l memory.Block.Pointer(a0)
	beq.w invalid
	movea.l memory.Block.Pointer(a0), a1
	adda.l d1, a1
	move.l Container.End(a1), d6
	cmp.l memory.Block.Used(a0), d6
	bhi.w invalid
	sub.l d1, d6
	bcs.w invalid
	cmpi.l #HEADER_BYTES, d6
	blo.w invalid
	tst.w Container.Reserved(a1)
	bne.w invalid
	moveq #0, d5
	move.w Container.SpellingBytes(a1), d5
	cmpi.l #MAX_SOURCE_BYTES, d5
	bhi.w invalid
	moveq #0, d4
	move.w Container.TokenCount(a1), d4
	cmpi.l #MAX_ROWS, d4
	bhi.w invalid
	moveq #0, d7
	move.w Container.RecipeCount(a1), d7
	beq.w invalid
	cmpi.l #MAX_ROWS, d7
	bhi.w invalid
	move.l d7, d3
	mulu.w #FRAGMENT_BYTES, d3
	addi.l #HEADER_BYTES, d3
	move.l d3, d0
	add.l d5, d0
	addq.l #1, d0
	andi.l #$fffffffe, d0
	cmp.l d6, d0
	bne.w invalid
	movea.l a1, a5
	adda.l d3, a5
	lea HEADER_BYTES(a1), a1
	movea.l a3, a4
	move.l d7, d6
expand
	movea.l a3, a2
	moveq #7, d0
clear
	clr.l (a3)+
	dbra d0, clear
	move.w Fragment.Kind(a1), plans.Row.Kind(a2)
	moveq #0, d0
	move.w Fragment.Start(a1), d0
	move.l d0, plans.Row.SpellingStart(a2)
	moveq #0, d0
	move.w Fragment.End(a1), d0
	move.l d0, plans.Row.SpellingEnd(a2)
	moveq #0, d0
	move.w Fragment.Aux0(a1), d0
	cmpi.l #MAX_SOURCE_BYTES, d0
	bhi.w invalid
	move.l d0, plans.Row.Aux0(a2)
	moveq #0, d0
	move.w Fragment.Aux1(a1), d0
	cmpi.l #MAX_SOURCE_BYTES, d0
	bhi.w invalid
	move.l d0, plans.Row.Aux1(a2)
	adda.w #FRAGMENT_BYTES, a1
	subq.l #1, d6
	bne.w expand
	movea.l a4, a2
	jsr plans.validateRecipes
	bne.w invalid
	movea.l a5, a2
	moveq #STATUS_OK, d0
	rts
invalid
	moveq #STATUS_INVALID, d0
	rts
	.bend  ; decode
	.endsection
	.endmodule
