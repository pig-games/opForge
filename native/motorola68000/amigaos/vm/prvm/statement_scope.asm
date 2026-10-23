; Bounded keyword-scope validation and longest-prefix routing primitives.
; @opforge-owner: opforge.vm.prvm.statement_scope
	.module opforge.vm.prvm.statement_scope
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_inputs as inputs
	.use opforge.vm.prvm.statement_text as utf8
	.use opforge.vm.prvm.statement_text_match as text
	.priv
Local	.struct
Head	.long ?
HeadBytes	.long ?
Scope	.long ?
ScopeCount	.long ?
Keywords	.long ?
Policy	.long ?
Budget	.long ?
Prefix	.long ?
	.endstruct
LOCAL_BYTES = Local.Prefix+4
	.pub
	.section code, kind=code
; A0/D0=structurally validated scope/count,A1/D1=keywords/bytes,A2=budget.
; D0=status/CCR; others preserved. Every word is nonempty, bounded valid UTF8.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	move.l d0, d7
	move.l d1, d6
next
	tst.l d7
	beq.w valid
	movea.l a2, a0
	jsr inputs.tick
	bne.w done
	move.l (a3), d4  ; ScopeRow.KeywordOffset (zero offset)
	move.l abi.ScopeRow.KeywordBytes(a3), d5
	beq.w invalidWord
	cmp.l d6, d4
	bhi.w invalid
	move.l d6, d0
	sub.l d4, d0
	cmp.l d0, d5
	bhi.w invalid
	movea.l a4, a0
	adda.l d4, a0
	move.l d5, d0
	movea.l a2, a1
	jsr utf8.validate
	bne.w done
	adda.w #abi.SCOPE_ROW_BYTES, a3
	subq.l #1, d7
	bra.w next
invalidWord
	moveq #abi.INVALID_TOKEN, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w done
valid
	moveq #abi.OK, d0
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend
; A0/D0=head bytes,A1/D1=validated scope/count,A2=keyword arena,
; A3=validated A9,A4=budget cell. D0=status/CCR,D1=longest prefix bytes.
; Other registers preserved. All spans/scope words must be validated first.
longest	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea -LOCAL_BYTES(sp), sp
	movea.l sp, a6
	move.l a0, (a6)  ; Local.Head (zero offset)
	move.l d0, Local.HeadBytes(a6)
	move.l a1, Local.Scope(a6)
	move.l d1, Local.ScopeCount(a6)
	move.l a2, Local.Keywords(a6)
	move.l a3, Local.Policy(a6)
	move.l a4, Local.Budget(a6)
	clr.l Local.Prefix(a6)
next
	tst.l Local.ScopeCount(a6)
	beq.w valid
	movea.l Local.Budget(a6), a0
	jsr inputs.tick
	bne.w failed
	movea.l Local.Scope(a6), a5
	move.l abi.ScopeRow.KeywordBytes(a5), d4
	cmp.l Local.Prefix(a6), d4
	bls.w advance
	cmp.l Local.HeadBytes(a6), d4
	bhi.w advance
	movea.l (a6), a0  ; Local.Head (zero offset)
	movea.l Local.Keywords(a6), a1
	adda.l (a5), a1  ; ScopeRow.KeywordOffset (zero offset)
	move.l d4, d0
	movea.l Local.Policy(a6), a2
	movea.l Local.Budget(a6), a3
	jsr text.equal
	bne.w failed
	tst.l d1
	beq.w advance
	move.l d4, Local.Prefix(a6)
advance
	addi.l #abi.SCOPE_ROW_BYTES, Local.Scope(a6)
	subq.l #1, Local.ScopeCount(a6)
	bra.w next
valid
	move.l Local.Prefix(a6), d1
	moveq #abi.OK, d0
	bra.w done
failed
	moveq #0, d1
done
	lea LOCAL_BYTES(sp), sp
	tst.l d0
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend
	.endsection
	.endmodule
