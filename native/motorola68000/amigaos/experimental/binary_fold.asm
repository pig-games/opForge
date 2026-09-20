; Fold constant subtrees and lower once to compact runtime expressions.
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
; Returns D0/CCR=status (0=success, 1=failure), D1=compact payload length.
; Preserves D2-D7/A0-A6. Uses bounded temporary stack storage; no source access.
; Original compiler limits apply before this call. Rewrites only on success
; paths; a failed expression remains uncommitted preparation scratch.
prepare	.block
	movem.l d2-d7/a0-a6, -(sp)
	; A lone literal needs narrowing but no subtree analysis or span map.
	cmpi.l #10, d0
	bne.w scratch
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_LITERAL, (a0)
	bne.w scratch
	movea.l a0, a6
	lea 1(a0), a3
	bsr.w readLiteral
	bsr.w writeLiteral
	clr.b (a6)+
	move.l a6, d1
	sub.l a0, d1
	moveq #0, d0
	bra.w restore
scratch
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
	beq.w constant
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
constant
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
	cmp.w d7, d6
	bhs.w complete
	moveq #0, d0
	move.b 0(a1, d6.w), d0
	bne.w evaluate
	move.b (a3)+, d0
	addq.w #1, d6
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_LITERAL, d0
	beq.w literal
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d0
	beq.w negate
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d0
	beq.w binaryOperator
	move.b d0, (a6)+
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d0
	bne.w next
	move.b (a3)+, (a6)+
	move.b (a3)+, (a6)+
	addq.w #2, d6
	bra.w next
literal
	bsr.w readLiteral
	addq.w #8, d6
	bsr.w writeLiteral
	bra.w next
negate
	move.b (a3)+, d0
	addq.w #1, d6
	cmpi.b #runtime.EXPRVM_UNARY_MINUS, d0
	bne.w unaryPair
	move.b #runtime.COMPACT_NEGATE, (a6)+
	bra.w next
unaryPair
	move.b #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, (a6)+
	move.b d0, (a6)+
	bra.w next
binaryOperator
	move.b (a3)+, d0
	addq.w #1, d6
	cmpi.b #runtime.EXPRVM_BINARY_ADD, d0
	beq.w add
	cmpi.b #runtime.EXPRVM_BINARY_SUBTRACT, d0
	beq.w subtract
	cmpi.b #runtime.EXPRVM_BINARY_MULTIPLY, d0
	bne.w binaryPair
	move.b #runtime.COMPACT_MULTIPLY, (a6)+
	bra.w next
binaryPair
	move.b #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, (a6)+
	move.b d0, (a6)+
	bra.w next
add
	move.b #runtime.COMPACT_ADD, (a6)+
	bra.w next
subtract
	move.b #runtime.COMPACT_SUBTRACT, (a6)+
	bra.w next
complete
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
	adda.l d2, a3
	add.w d2, d6
	bsr.w writeLiteral
	bra.w next
done
	lea SCRATCH_BYTES(sp), sp
restore
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepare
	.priv

; Read the low signed32 value of a validated canonical literal at A3.
; Advances A3 by eight payload bytes. Clobbers D3/CCR.
readLiteral	.block
	moveq #0, d3
	move.b 3(a3), d3
	lsl.l #8, d3
	move.b 2(a3), d3
	lsl.l #8, d3
	move.b 1(a3), d3
	lsl.l #8, d3
	move.b (a3), d3
	addq.l #8, a3
	rts
	.bend  ; readLiteral

; Write signed D3 at A6 with the narrowest explicit width. Output is always
; smaller than its canonical source span. Clobbers D0-D3/A6/CCR.
writeLiteral	.block
	move.l d3, d1
	ext.w d1
	ext.l d1
	moveq #runtime.COMPACT_I8, d0
	moveq #0, d2
	cmp.l d3, d1
	beq.w bytes
	move.l d3, d1
	ext.l d1
	moveq #runtime.COMPACT_I16, d0
	moveq #1, d2
	cmp.l d3, d1
	beq.w bytes
	moveq #runtime.COMPACT_I32, d0
	moveq #3, d2
bytes
	move.b d0, (a6)+
loop
	move.b d3, (a6)+
	lsr.l #8, d3
	dbra d2, loop
	rts
	.bend  ; writeLiteral

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
