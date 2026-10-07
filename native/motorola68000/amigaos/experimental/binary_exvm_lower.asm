; Lower canonical scalar arena nodes to ExprVM v2 postfix bytes.
; @opforge-owner: experimental.amigaos.binary_exvm_lower
	.module experimental.amigaos.binary_exvm_lower
	.cpu 68020
	.use experimental.amigaos.binary_exvm as compiler
	.use exprvm.amigaos.runtime as runtime
	.pub
STATUS_OK = 0
STATUS_MALFORMED = 1
STATUS_OUTPUT = 2
STATUS_DEPTH = 3
NODE_LIMIT = 128
PATH_BYTES = NODE_LIMIT*4
STACK_DEPTH = PATH_BYTES
MODE = STACK_DEPTH+4
LEN_NAME = MODE+4
SCRATCH_BYTES = LEN_NAME+4
	.section code, kind=code
; A0=even arena, D0=used bytes, D1=root offset, A3=output, A4=exclusive end.
; A1=even caller-owned scratch, D2=capacity >= SCRATCH_BYTES; disjoint buffers.
; D0/CCR=status, A3=after payload including END on success. Others preserved.
; Failure may leave partial scratch bytes. No END is emitted on failure.
; Uses caller scratch plus bounded recursive stack frames; supports flat
; expression chains independently of the compiler's syntactic nesting limit.
lower	.block
	move.l d3, -(sp)
	moveq #0, d3
	bsr.w lowerMode
	move.l (sp)+, d3
	tst.l d0
	rts
	.bend  ; lower

; Same buffers as lower; D4=package-bound .len name ID. Emits canonical
; typed ExprVM operations, with a 128-value bound. Range remains unsupported.
lowerValue	.block
	move.l d3, -(sp)
	moveq #1, d3
	bsr.w lowerMode
	move.l (sp)+, d3
	tst.l d0
	rts
	.bend  ; lowerValue
	.priv
lowerMode	.block
	movem.l d1-d7/a0-a2/a4-a6, -(sp)
	movea.l a1, a5
	move.l a5, d7
	beq.w malformed
	btst #0, d7
	bne.w malformed
	cmpi.l #SCRATCH_BYTES, d2
	blo.w output
	add.l d2, d7
	bcs.w malformed
	move.l d3, MODE(a5)
	move.l d4, LEN_NAME(a5)
	clr.l STACK_DEPTH(a5)
	move.l d0, d5
	beq.w malformed
	cmpi.l #NODE_LIMIT*compiler.NODE_BYTES, d5
	bhi.w depth
	move.l a0, d0
	btst #0, d0
	bne.w malformed
	add.l d5, d0
	bcs.w malformed
	move.l a4, d0
	cmp.l a3, d0
	bcs.w output
	move.l d5, d0
	divu.w #compiler.NODE_BYTES, d0
	swap d0
	tst.w d0
	bne.w malformed
	moveq #0, d6
	moveq #0, d7
	bsr.w node
	bne.w finish
	cmpi.l #1, STACK_DEPTH(a5)
	bne.w malformed
	moveq #1, d0
	bsr.w room
	bne.w finish
	move.b #runtime.EXPRVM_V2_OPCODE_END, (a3)+
	moveq #STATUS_OK, d0
	bra.w finish
malformed
	moveq #STATUS_MALFORMED, d0
	bra.w finish
output
	moveq #STATUS_OUTPUT, d0
	bra.w finish
depth
	moveq #STATUS_DEPTH, d0
finish
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a2/a4-a6
	rts
	.bend  ; lowerMode
	.priv
; D1=node offset, D5=used, D6=active depth, D7=visited count.
; A5=active offset path. Preserves D2/A2 across child recursion.
node	.block
	movem.l d2/a2, -(sp)
	cmp.l d5, d1
	bcc.w malformed
	move.l d1, d0
	divu.w #compiler.NODE_BYTES, d0
	swap d0
	tst.w d0
	bne.w malformed
	moveq #0, d0
checkPath
	cmp.l d6, d0
	bcc.w checked
	move.l d0, d3
	lsl.l #2, d3
	cmp.l 0(a5, d3.l), d1
	beq.w malformed
	addq.l #1, d0
	bra.w checkPath
checked
	cmpi.l #NODE_LIMIT, d7
	bcc.w depth
	cmpi.l #NODE_LIMIT, d6
	bcc.w depth
	move.l d6, d3
	lsl.l #2, d3
	move.l d1, 0(a5, d3.l)
	addq.l #1, d6
	addq.l #1, d7
	lea 0(a0, d1.l), a2
	moveq #0, d2
	move.b compiler.Node.Kind(a2), d2
	cmpi.b #compiler.KIND_LITERAL, d2
	beq.w literal
	cmpi.b #compiler.KIND_SYMBOL, d2
	beq.w symbol
	cmpi.b #compiler.KIND_CURRENT, d2
	beq.w current
	cmpi.b #compiler.KIND_UNARY, d2
	beq.w unary
	cmpi.b #compiler.KIND_BINARY, d2
	beq.w binary
	tst.l MODE(a5)
	beq.w malformedActive
	cmpi.b #compiler.KIND_LIST, d2
	beq.w list
	cmpi.b #compiler.KIND_CALL, d2
	beq.w functionCall
	cmpi.b #compiler.KIND_INDEX, d2
	bne.w malformedActive
	cmpi.w #2, compiler.Node.Count(a2)
	bne.w malformedActive
	move.l compiler.Node.First(a2), d1
	bsr.w node
	bne.w leave
	move.l compiler.Node.Second(a2), d1
	bsr.w node
	bne.w leave
	subq.l #1, STACK_DEPTH(a5)
	moveq #1, d0
	bsr.w room
	bne.w leave
	move.b #$61, (a3)+  ; canonical IndexValue
	bra.w success
functionCall
	cmpi.w #1, compiler.Node.Count(a2)
	bne.w malformedActive
	move.l compiler.Node.First(a2), d0
	cmp.l LEN_NAME(a5), d0
	bne.w malformedActive
	move.l compiler.Node.Second(a2), d1
	moveq #1, d2
	bra.w sequence
list
	moveq #0, d2
	move.w compiler.Node.Count(a2), d2
	cmpi.l #NODE_LIMIT, d2
	bhi.w depth
	move.l compiler.Node.First(a2), d1
sequence
	tst.l d2
	beq.w sequenceEnd
item
	move.l d1, -(sp)
	bsr.w node
	move.l (sp)+, d1
	tst.l d0
	bne.w leave
	lea 0(a0, d1.l), a1
	move.l compiler.Node.Next(a1), d1
	subq.l #1, d2
	bne.w item
sequenceEnd
	cmpi.l #compiler.NONE, d1
	bne.w malformedActive
	moveq #0, d2
	move.w compiler.Node.Count(a2), d2
	sub.l d2, STACK_DEPTH(a5)
	addq.l #1, STACK_DEPTH(a5)
	cmpi.l #NODE_LIMIT, STACK_DEPTH(a5)
	bhi.w depth
	cmpi.b #compiler.KIND_CALL, compiler.Node.Kind(a2)
	beq.w emitCall
	moveq #3, d0
	bsr.w room
	bne.w leave
	move.b #$51, (a3)+  ; canonical BuildList, count u16 LE
	bra.w count
emitCall
	moveq #4, d0
	bsr.w room
	bne.w leave
	move.b #$62, (a3)+  ; canonical CallBuiltin
	move.b #1, (a3)+  ; shared ExprBuiltin::Len
count
	move.b d2, (a3)+
	lsr.w #8, d2
	move.b d2, (a3)+
	bra.w success
binary
	cmpi.w #2, compiler.Node.Count(a2)
	bne.w malformedActive
	move.l compiler.Node.First(a2), d1
	bsr.w node
	bne.w leave
	move.l compiler.Node.Second(a2), d1
	bsr.w node
	bne.w leave
	subq.l #1, STACK_DEPTH(a5)
	lea BinaryMap, a1
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d2
	bra.w operator
unary
	cmpi.w #1, compiler.Node.Count(a2)
	bne.w malformedActive
	move.l compiler.Node.First(a2), d1
	bsr.w node
	bne.w leave
	lea UnaryMap, a1
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d2
operator
	moveq #0, d1
	move.b compiler.Node.Operator(a2), d1
	cmpi.w #24, d1
	bhi.w malformedActive
	moveq #0, d4
	move.b 0(a1, d1.w), d4
	cmpi.b #$ff, d4
	beq.w malformedActive
	moveq #2, d0
	bsr.w room
	bne.w leave
	move.b d2, (a3)+
	move.b d4, (a3)+
	bra.w success
literal
	moveq #9, d0
	bsr.w leafRoom
	bne.w leave
	move.b #runtime.EXPRVM_V2_OPCODE_PUSH_LITERAL, (a3)+
	move.l compiler.Node.First(a2), d1
	ror.w #8, d1
	swap d1
	ror.w #8, d1
	move.l d1, (a3)+
	clr.l (a3)+
	bra.w success
symbol
	move.l compiler.Node.First(a2), d1
	cmpi.l #65535, d1
	bhi.w malformedActive
	moveq #3, d0
	bsr.w leafRoom
	bne.w leave
	move.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, (a3)+
	move.b d1, (a3)+
	lsr.w #8, d1
	move.b d1, (a3)+
	bra.w success
current
	moveq #1, d0
	bsr.w leafRoom
	bne.w leave
	move.b #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, (a3)+
success
	moveq #STATUS_OK, d0
	bra.w leave
malformedActive
	moveq #STATUS_MALFORMED, d0
leave
	subq.l #1, d6
	bra.w restore
malformed
	moveq #STATUS_MALFORMED, d0
	bra.w restore
depth
	moveq #STATUS_DEPTH, d0
restore
	tst.l d0
	movem.l (sp)+, d2/a2
	rts
	.bend  ; node
leafRoom	.block
	tst.w compiler.Node.Count(a2)
	bne.w malformed
	tst.l MODE(a5)
	bne.w typedDepth
	cmpi.l #runtime.EXPRVM_STACK_CAPACITY, STACK_DEPTH(a5)
	bcc.w depth
	bra.w bounded
typedDepth
	cmpi.l #NODE_LIMIT, STACK_DEPTH(a5)
	bcc.w depth
bounded
	bsr.w room
	bne.w finish
	addq.l #1, STACK_DEPTH(a5)
	moveq #STATUS_OK, d0
finish
	rts
malformed
	moveq #STATUS_MALFORMED, d0
	rts
depth
	moveq #STATUS_DEPTH, d0
	rts
	.bend  ; leafRoom
; D0=required bytes. D3 scratch; no pointer wrapping or partial instruction.
room	.block
	move.l a4, d3
	sub.l a3, d3
	cmp.l d0, d3
	bcs.w output
	moveq #STATUS_OK, d0
	rts
output
	moveq #STATUS_OUTPUT, d0
	rts
	.bend  ; room
	.endsection
; Tables are indexed by package::ExvmOperatorKind wire values, including
; invalid slot zero. Range operators have no scalar ExprVM representation.
	.section data, kind=data
UnaryMap
	.byte $ff, runtime.EXPRVM_UNARY_PLUS, runtime.EXPRVM_UNARY_MINUS
	.byte $ff, $ff, $ff, $ff
	.byte runtime.EXPRVM_UNARY_BIT_NOT, runtime.EXPRVM_UNARY_LOGIC_NOT
	.byte runtime.EXPRVM_UNARY_LOW, runtime.EXPRVM_UNARY_HIGH
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
BinaryMap
	.byte $ff, runtime.EXPRVM_BINARY_ADD, runtime.EXPRVM_BINARY_SUBTRACT
	.byte runtime.EXPRVM_BINARY_MULTIPLY, runtime.EXPRVM_BINARY_DIVIDE
	.byte runtime.EXPRVM_BINARY_MOD, runtime.EXPRVM_BINARY_POWER, $ff, $ff
	.byte runtime.EXPRVM_BINARY_LT, runtime.EXPRVM_BINARY_GT
	.byte runtime.EXPRVM_BINARY_SHIFT_LEFT, runtime.EXPRVM_BINARY_SHIFT_RIGHT
	.byte runtime.EXPRVM_BINARY_EQ, runtime.EXPRVM_BINARY_NE
	.byte runtime.EXPRVM_BINARY_GE, runtime.EXPRVM_BINARY_LE
	.byte runtime.EXPRVM_BINARY_BIT_AND, runtime.EXPRVM_BINARY_BIT_OR
	.byte runtime.EXPRVM_BINARY_BIT_XOR, runtime.EXPRVM_BINARY_LOGIC_AND
	.byte runtime.EXPRVM_BINARY_LOGIC_OR, runtime.EXPRVM_BINARY_LOGIC_XOR
	.byte $ff, $ff
	.align 2  ; keep following modules word-aligned
	.endsection
	.endmodule
