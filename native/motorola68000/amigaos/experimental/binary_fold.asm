; Fold maximal constant subtrees using the checked shared ExprVM arithmetic.
; @opforge-owner: opasm.amigaos.binary_fold
	.module opasm.amigaos.binary_fold
	.cpu 68020
	.use exprvm.amigaos.runtime as runtime
	.priv
Entry	.struct
Start	.word ?
Constant	.word ?
	.endstruct
ENTRY_BYTES = 4
MAP_BYTES = 256
SCRATCH_BYTES = MAP_BYTES+runtime.EXPRVM_STACK_CAPACITY*ENTRY_BYTES
	.pub
	.section code, kind=code
; A0/D0=compiler-validated v2 payload and length, including END (1..255).
; Returns D0/CCR=status (0=success, 1=arithmetic failure), D1=new length.
; Preserves D2-D7/A0-A6. Uses bounded temporary stack storage; no source access.
; Original compiler limits apply before this call. Rewrites only on success
; paths; a failed expression remains uncommitted preparation scratch.
fold	.block
	; A literal plus END occupies ten bytes; no shorter program can contain
	; a nontrivial constant subtree. Avoid allocating/scanning scratch for it.
	move.l d0, d1
	cmpi.l #10, d0
	bhi.w prepare
	moveq #0, d0
	rts
prepare
	movem.l d2-d7/a0-a6, -(sp)
	lea -SCRATCH_BYTES(sp), sp
	movea.l sp, a1
	lea MAP_BYTES(a1), a2
	movea.l a2, a5
	movea.l a0, a4
	movea.l a0, a3
	move.l d0, d7
	moveq #MAP_BYTES/4-1, d0
clear
	clr.l (a1)+
	dbra d0, clear
	movea.l sp, a1
scan
	move.l a3, d6
	sub.l a4, d6
	moveq #0, d0
	move.b (a3)+, d0
	cmpi.b #runtime.EXPRVM_V2_OPCODE_END, d0
	beq.w scanned
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_LITERAL, d0
	beq.w literal
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d0
	beq.w unary
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d0
	beq.w binary
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d0
	bne.w dynamic
	addq.l #2, a3
dynamic
	move.w d6, (a5)+
	clr.w (a5)+
	bra.w scan
literal
	addq.l #8, a3
	move.w d6, (a5)+
	move.w #1, (a5)+
	bra.w scan
unary
	addq.l #1, a3
	bra.w scan
binary
	addq.l #1, a3
	suba.w #ENTRY_BYTES, a5
	lea -ENTRY_BYTES(a5), a0
	tst.w Entry.Constant(a0)
	beq.w mixed
	tst.w Entry.Constant(a5)
	bne.w scan
	moveq #0, d0
	move.w Entry.Start(a0), d0
	moveq #0, d1
	move.w Entry.Start(a5), d1
	bsr.w mark
mixed
	tst.w Entry.Constant(a5)
	beq.w merged
	moveq #0, d0
	move.w Entry.Start(a5), d0
	move.l d6, d1
	bsr.w mark
merged
	clr.w Entry.Constant(a0)
	bra.w scan
scanned
	tst.w Entry.Constant(a2)
	beq.w rewrite
	moveq #0, d0
	move.w Entry.Start(a2), d0
	move.l d6, d1
	bsr.w mark
rewrite
	movea.l a4, a3
	movea.l a4, a6
	moveq #0, d6
next
	moveq #0, d0
	move.b 0(a1, d6.w), d0
	bne.w evaluate
	move.b (a3)+, (a6)+
	addq.w #1, d6
	cmp.w d7, d6
	blo.w next
	move.l a6, d1
	sub.l a4, d1
	moveq #0, d0
	bra.w done
evaluate
	; Temporarily terminate this subtree in the original scratch program.
	; The next opcode is restored before continuing the compacting copy.
	movea.l a3, a0
	movea.l a3, a5
	adda.l d0, a5
	moveq #0, d2
	move.b (a5), d2
	move.l d2, -(sp)
	move.l d0, -(sp)
	clr.b (a5)
	addq.l #1, d0
	moveq #0, d1
	moveq #0, d2
	jsr runtime.evalNumeric32
	move.l (sp)+, d2
	move.l (sp)+, d1
	move.b d1, (a5)
	tst.l d0
	bne.w done
	move.b #runtime.EXPRVM_V2_OPCODE_PUSH_LITERAL, (a6)+
	tst.l d3
	smi d1
	.for 4
	move.b d3, (a6)+
	lsr.l #8, d3
	.endfor
	.for 4
	move.b d1, (a6)+
	.endfor
	adda.l d2, a3
	add.w d2, d6
	bra.w next
done
	lea SCRATCH_BYTES(sp), sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; fold
	.priv

; D0=start offset, D1=end offset. Record only nontrivial constant subtrees.
; A1=zeroed span map. Clobbers D1/CCR; scan state and other registers survive.
mark	.block
	sub.l d0, d1
	cmpi.w #9, d1
	bls.w done
	move.b d1, 0(a1, d0.w)
done
	rts
	.bend  ; mark
	.endsection
	.endmodule
