; Decode the current package-owned signature grammar and canonical render rows.
; @opforge-owner: opforge.vm.prvm.statement_policy
	.module opforge.vm.prvm.statement_policy
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_inputs as inputs
	.use opforge.vm.prvm.statement_text as text
	.priv
Local	.struct
TypeRows	.long ?
RenderRows	.long ?
RenderLow	.long ?
RenderHigh	.long ?
	.endstruct
LOCAL_BYTES = Local.RenderHigh+4
	.pub
	.section code, kind=code
; A0/D0=compile program/bytes,A2=budget cell. D0=status/CCR,D1=type count,
; D2=render count,A0=type rows,A1=render rows. Others preserved.
; All names/render text are validated UTF8; IDs and render kinds are unique.
compile	.block
	movem.l d3-d7/a2-a6, -(sp)
	lea -LOCAL_BYTES(sp), sp
	movea.l sp, a6
	movea.l a0, a4
	movea.l a2, a3
	cmpi.l #abi.COMPILE_HEADER_BYTES+3, d0
	blo.w invalid
	movea.l a0, a5
	adda.l d0, a5
	jsr inputs.span
	bne.w failed
	movea.l a3, a0
	jsr inputs.budget
	bne.w failed
	cmpi.b #abi.OP_COMPILE, (a4)  ; CompileProgram.Opcode (zero offset)
	bne.w invalid
	cmpi.b #abi.POLICY_TYPE_COUNT, abi.CompileProgram.TypeCount(a4)
	bne.w invalid
	adda.w #abi.COMPILE_HEADER_BYTES, a4
	move.l a4, (a6)  ; Local.TypeRows (zero offset)
	moveq #abi.POLICY_TYPE_COUNT, d7
	moveq #0, d6
typeRow
	bsr.w rowHeader
	bne.w failed
	moveq #0, d0
	move.b (a4), d0  ; TypeName.Id (zero offset)
	cmpi.l #abi.TYPE_FIRST, d0
	blo.w invalid
	cmpi.l #abi.TYPE_LAST, d0
	bhi.w invalid
	subq.l #abi.TYPE_FIRST, d0
	bset d0, d6
	bne.w invalid
	bsr.w rowText
	bne.w failed
	subq.l #1, d7
	bne.w typeRow
	cmpa.l a5, a4
	bhs.w invalid
	cmpi.b #abi.RENDER_COUNT, (a4)+
	bne.w invalid
	move.l a4, Local.RenderRows(a6)
	clr.l Local.RenderLow(a6)
	clr.l Local.RenderHigh(a6)
	moveq #abi.RENDER_COUNT, d7
renderRow
	bsr.w rowHeader
	bne.w failed
	moveq #0, d0
	move.b (a4), d0  ; RenderText.Kind (zero offset)
	cmpi.l #abi.RENDER_FIRST, d0
	blo.w invalid
	cmpi.l #abi.RENDER_LAST, d0
	bhi.w invalid
	subi.l #abi.RENDER_FIRST, d0
	cmpi.l #32, d0
	blo.w low
	subi.l #32, d0
	move.l Local.RenderHigh(a6), d6
	bset d0, d6
	bne.w invalid
	move.l d6, Local.RenderHigh(a6)
	bra.w renderText
low
	move.l Local.RenderLow(a6), d6
	bset d0, d6
	bne.w invalid
	move.l d6, Local.RenderLow(a6)
renderText
	bsr.w rowText
	bne.w failed
	subq.l #1, d7
	bne.w renderRow
	move.l a5, d0
	sub.l a4, d0
	cmpi.l #2, d0
	bne.w invalid
	cmpi.b #abi.OP_PUBLISH, (a4)+
	bne.w invalid
	cmpi.b #abi.OP_END, (a4)
	bne.w invalid
	movea.l (a6), a0  ; Local.TypeRows (zero offset)
	movea.l Local.RenderRows(a6), a1
	moveq #abi.POLICY_TYPE_COUNT, d1
	moveq #abi.RENDER_COUNT, d2
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_PROGRAM, d0
failed
	moveq #0, d1
	moveq #0, d2
	suba.l a0, a0
	suba.l a1, a1
done
	lea LOCAL_BYTES(sp), sp
	tst.l d0
	movem.l (sp)+, d3-d7/a2-a6
	rts
	.bend  ; compile
; A0/D0=current A7 program/bytes,A2=budget cell. D0=status/CCR,
; A0=type rows,A1=A9 policy on success, null on error. Others preserved.
; A9 is part of the one current matcher contract; no former-tail acceptance.
match	.block
	movem.l d1-d3/a2-a4, -(sp)
	movea.l a0, a4
	cmpi.l #abi.MATCH_PROGRAM_BYTES, d0
	bne.w invalid
	jsr inputs.span
	bne.w failed
	movea.l a2, a0
	jsr inputs.budget
	bne.w failed
	cmpi.b #abi.OP_MATCH, (a4)  ; MatchProgram.Opcode (zero offset)
	bne.w invalid
	cmpi.b #1, abi.MatchProgram.Adjacency(a4)
	bne.w invalid
	cmpi.b #abi.POLICY_TYPE_COUNT, abi.MatchProgram.TypeCount(a4)
	bne.w invalid
	lea abi.MATCH_HEADER_BYTES(a4), a3
	moveq #abi.TYPE_FIRST, d1
row
	movea.l a2, a0
	jsr inputs.tick
	bne.w failed
	cmp.b (a3), d1  ; TypePolicy.Id (zero offset)
	bne.w invalid
	cmpi.b #2, abi.TypePolicy.Identifier(a3)
	bhi.w invalid
	tst.l abi.TypePolicy.Reserved(a3)
	bne.w invalid
	adda.w #abi.TYPE_POLICY_BYTES, a3
	addq.l #1, d1
	cmpi.l #abi.TYPE_LAST, d1
	bls.w row
	cmpi.b #abi.OP_INVOCATION, (a3)  ; InvocationPolicy.Opcode (zero offset)
	bne.w invalid
	moveq #0, d0
	move.b abi.InvocationPolicy.Flags(a3), d0
	andi.l #$ff-abi.INVOCATION_FLAGS, d0
	bne.w invalid
	move.b abi.InvocationPolicy.UpperFirst(a3), d0
	cmp.b abi.InvocationPolicy.UpperLast(a3), d0
	bhi.w invalid
	move.b abi.InvocationPolicy.LowerFirst(a3), d0
	cmp.b abi.InvocationPolicy.LowerLast(a3), d0
	bhi.w invalid
	move.b abi.InvocationPolicy.DigitFirst(a3), d0
	cmp.b abi.InvocationPolicy.DigitLast(a3), d0
	bhi.w invalid
	cmpi.b #abi.OP_PUBLISH, abi.INVOCATION_POLICY_BYTES(a3)
	bne.w invalid
	cmpi.b #abi.OP_END, abi.INVOCATION_POLICY_BYTES+1(a3)
	bne.w invalid
	lea abi.MATCH_HEADER_BYTES(a4), a0
	movea.l a3, a1
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_PROGRAM, d0
failed
	suba.l a0, a0
	suba.l a1, a1
done
	tst.l d0
	movem.l (sp)+, d1-d3/a2-a4
	rts
	.bend  ; match

; A0=validated render rows,D0=token kind. A0=canonical bytes or null,
; D0=byte count/CCR (zero means no render row). Others preserved.
render	.block
	movem.l d1-d2, -(sp)
	move.l d0, d1
	moveq #abi.RENDER_COUNT, d2
next
	cmp.b (a0), d1  ; RenderText.Kind (zero offset)
	beq.w found
	moveq #0, d0
	move.b abi.RenderText.TextBytes(a0), d0
	adda.w #abi.RENDER_HEADER_BYTES, a0
	adda.l d0, a0
	subq.l #1, d2
	bne.w next
	suba.l a0, a0
	moveq #0, d0
	bra.w done
found
	moveq #0, d0
	move.b abi.RenderText.TextBytes(a0), d0
	adda.w #abi.RENDER_HEADER_BYTES, a0
done
	tst.l d0
	movem.l (sp)+, d1-d2
	rts
	.bend  ; render
	.priv
; Check the variable row prefix at A4, bounded by A5, charging A3's budget.
; D0=status/CCR,D4=nonzero name/text bytes. Others preserved.
rowHeader	.block
	movea.l a3, a0
	jsr inputs.tick
	bne.w done
	move.l a5, d0
	sub.l a4, d0
	cmpi.l #abi.TYPE_NAME_HEADER_BYTES, d0
	blo.w invalid
	moveq #0, d4
	move.b abi.TypeName.NameBytes(a4), d4
	beq.w invalid
	subi.l #abi.TYPE_NAME_HEADER_BYTES, d0
	cmp.l d0, d4
	bhi.w invalid
	moveq #abi.OK, d0
	rts
invalid
	moveq #abi.INVALID_PROGRAM, d0
done
	rts
	.bend  ; rowHeader
; Consume the validated row's UTF8 text. D0=status/CCR; A4 advances on success.
rowText	.block
	adda.w #abi.TYPE_NAME_HEADER_BYTES, a4
	movea.l a4, a0
	move.l d4, d0
	movea.l a3, a1
	jsr text.validate
	beq.w valid
	cmpi.l #abi.BUDGET_EXCEEDED, d0
	beq.w done
	moveq #abi.INVALID_PROGRAM, d0
	rts
valid
	adda.l d4, a4
	moveq #abi.OK, d0
done
	rts
	.bend  ; rowText
	.endsection
	.endmodule
