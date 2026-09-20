; Rewrite provisional numeric identities after declaration-aware scope binding.
; @opforge-owner: experimental.amigaos.binary_binding_records
	.module experimental.amigaos.binary_binding_records
	.cpu 68020
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as runtime
	.pub
Entry	.struct
Name	.word ?
Length	.word ?
Owner	.word ?
Leaf	.word ?
Flags	.word ?
Target	.word ?
Next	.word ?
ScopeKind	.word ?
.endstruct
ENTRY_BYTES = Entry.ScopeKind+2
	.section code, kind=code

; A0=packed records,D0=bytes,A1=Entry array,
; D1=source ID base,D2=entry count. D0/CCR=status; other registers preserved.
; Literal payloads are skipped using the compact-expression contract. Only
; symbol operands are rewritten, with their original BE/LE representation.
remap	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	move.l d1, d6
	move.l d2, d7
	movea.l a0, a5
	adda.l d0, a5
line
	cmpa.l a5, a0
	beq.w ok
	bhi.w bad
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	cmpi.w #4, d0
	blo.w bad
	movea.l a0, a4
	adda.w d0, a4
	cmpa.l a5, a4
	bhi.w bad
	addq.l #4, a0
token
	cmpa.l a4, a0
	beq.w line
	bhi.w bad
	moveq #0, d0
	move.b (a0)+, d0
	cmpi.b #1, d0
	bls.w name
	cmpi.b #expr.COMPILED_TAG, d0
	beq.w expression
	cmpi.b #2, d0
	bne.w token
	addq.l #4, a0
	bra.w token
name
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	moveq #0, d0
	move.w (a0), d0
	bsr.w identity
	bne.w bad
	move.w d1, (a0)
	addq.l #3, a0
	bra.w token
expression
	cmpa.l a4, a0
	bhs.w bad
	moveq #0, d0
	move.b (a0)+, d0
	beq.w bad
	movea.l a0, a3
	adda.w d0, a3
	cmpa.l a4, a3
	bhi.w bad
opcode
	cmpa.l a3, a0
	bhs.w bad
	moveq #0, d0
	move.b (a0)+, d0
	beq.w endExpression
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d0
	beq.w symbol
	cmpi.b #runtime.COMPACT_I8, d0
	beq.w one
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d0
	beq.w one
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d0
	beq.w one
	cmpi.b #runtime.COMPACT_I16, d0
	beq.w two
	cmpi.b #runtime.COMPACT_I32, d0
	beq.w four
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_NEGATE, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_ADD, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_SUBTRACT, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_MULTIPLY, d0
	beq.w opcode
	bra.w bad
one
	addq.l #1, a0
	bra.w opcode
two
	addq.l #2, a0
	bra.w opcode
four
	addq.l #4, a0
	bra.w opcode
symbol
	move.l a3, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	moveq #0, d0
	move.b 1(a0), d0
	lsl.w #8, d0
	move.b (a0), d0
	bsr.w identity
	bne.w bad
	move.b d1, (a0)+
	lsr.w #8, d1
	move.b d1, (a0)+
	bra.w opcode
endExpression
	cmpa.l a3, a0
	bne.w bad
	bra.w token
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; remap
	.priv

; D0=ID; D1=final ID,D0/CCR=status. A1 scratch; package IDs remain unchanged.
identity	.block
	move.l d0, d1
	sub.l d6, d0
	bcs.w ok
	cmp.l d7, d0
	bhs.w bad
	lsl.l #4, d0
	movea.l a6, a1
	adda.l d0, a1
	moveq #0, d1
	move.w Entry.Target(a1), d1
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; identity
	.endsection
	.endmodule
