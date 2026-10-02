; Bind lexical members using contextual forms supplied by the CPU package.
; @opforge-owner: experimental.amigaos.binary_members

	.module experimental.amigaos.binary_members
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_source as writer
	.use tkvm.amigaos.runtime as tokenizer
	.priv
TOKEN_COLON = tokenizer.TK_KIND_COLON
TOKEN_COMMA = tokenizer.TK_KIND_COMMA
TOKEN_OPEN_PAREN = tokenizer.TK_KIND_OPEN_PAREN
TOKEN_CLOSE_PAREN = tokenizer.TK_KIND_CLOSE_PAREN
TOKEN_OPEN_BRACKET = tokenizer.TK_KIND_OPEN_BRACKET
TOKEN_CLOSE_BRACKET = tokenizer.TK_KIND_CLOSE_BRACKET
TOKEN_OPEN_BRACE = tokenizer.TK_KIND_OPEN_BRACE
TOKEN_CLOSE_BRACE = tokenizer.TK_KIND_CLOSE_BRACE
	.section code, kind=code
	.pub
; A0=writer.Frame,A1=current TKVM token,A2=validated package,A3=lookup callback.
; Lookup: A0/D0=bounded spelling,A1=writer context; D0=0 found/1 absent,
; D1=name,D2=qualifier,D3=dictionary roles; preserves D4-D7/A2-A6.
; Return D0=0 member/1 ordinary/2 invalid; D1=base ID,D2=field ID on member,
; D3=base qualifier on member,D2=0 otherwise. Preserves D4-D7/A2-A6;
; clobbers A0/A1; CCR=D0.
; Only complete instruction operands participate. Exact package register names
; retain priority. Capture rows and owned lexemes are never changed.
bind	.block
	movem.l d4-d7/a2-a6, -(sp)
	suba.w #12, sp
	movea.l a0, a5
	movea.l a1, a4
	movea.l a2, a6
	moveq #0, d5
	move.l writer.Token.Length(a4), d6
	cmpi.l #3, d6
	blo.w absent
	movea.l writer.Frame.Lexemes(a5), a2
	adda.l writer.Token.Offset(a4), a2
	move.l d6, d0
lastDot
	subq.l #1, d0
	beq.w absent
	cmpi.b #'.', 0(a2, d0.l)
	bne.w lastDot
	move.l d0, d5
	addq.l #1, d0
	cmp.l d6, d0
	bhs.w absent
	move.l d5, (sp)
	movea.l a2, a0
	move.l d6, d0
	bsr.w lookup
	tst.l d0
	bne.w field
	btst #package.DICTIONARY_REGISTER_OR_NAMED_BIT, d3
	bne.w absent
field
	lea 1(a2, d5.l), a0
	move.l d6, d0
	sub.l d5, d0
	subq.l #1, d0
	bsr.w lookup
	tst.l d0
	bne.w absent
	btst #package.DICTIONARY_MEMBER_BIT, d3
	beq.w absent
	move.w d1, 4(sp)
	movea.l writer.Frame.Tokens(a5), a0
	move.l writer.Frame.Count(a5), d7
	beq.w absent
	; Skip a source label, with or without its optional colon. A dot head is
	; shared directive/template processing and never an instruction member.
	cmpi.w #1, writer.Token.Kind(a0)
	bhi.w absent
	cmpi.l #2, d7
	blo.w absent
	cmpi.w #TOKEN_COLON, writer.Token.Kind+20(a0)
	beq.w colonLabel
	cmpi.l #1, writer.Token.Start(a0)
	bhi.w head
	cmpi.w #1, writer.Token.Kind+20(a0)
	bhi.w head
	adda.w #20, a0
	subq.l #1, d7
	bra.w head
colonLabel
	adda.w #40, a0
	subq.l #2, d7
head
	cmpi.l #2, d7
	blo.w absent
	cmpi.w #1, writer.Token.Kind(a0)
	bhi.w absent
	tst.w writer.Token.Reserved(a0)
	bne.w absent
	movea.l a0, a1
	move.l writer.Token.Offset(a1), d0
	move.l writer.Frame.LexemeBytes(a5), d1
	sub.l d0, d1
	bcs.w invalid
	move.l writer.Token.Length(a1), d0
	cmp.l d1, d0
	bhi.w invalid
	movea.l writer.Frame.Lexemes(a5), a0
	adda.l writer.Token.Offset(a1), a0
	move.l a1, 8(sp)
	bsr.w lookup
	tst.l d0
	bne.w absent
	move.w d1, 6(sp)
	; Retain qualifier in D6; field remains the complete word at stack+4.
	moveq #0, d6
	move.b d2, d6
	movea.l 8(sp), a0
	adda.w #20, a0
	subq.l #1, d7
	moveq #0, d5  ; operand index
	moveq #0, d4  ; delimiter depth
operand
	cmpa.l a4, a0
	beq.w atToken
	tst.l d7
	beq.w absent
	move.w writer.Token.Kind(a0), d0
	cmpi.w #TOKEN_OPEN_PAREN, d0
	beq.w open
	cmpi.w #TOKEN_OPEN_BRACKET, d0
	beq.w open
	cmpi.w #TOKEN_OPEN_BRACE, d0
	beq.w open
	cmpi.w #TOKEN_CLOSE_PAREN, d0
	beq.w close
	cmpi.w #TOKEN_CLOSE_BRACKET, d0
	beq.w close
	cmpi.w #TOKEN_CLOSE_BRACE, d0
	beq.w close
	cmpi.w #TOKEN_COMMA, d0
	bne.w advance
	tst.l d4
	bne.w advance
	addq.l #1, d5
	bra.w advance
open
	addq.l #1, d4
	bra.w advance
close
	subq.l #1, d4
	bmi.w absent
advance
	adda.w #20, a0
	subq.l #1, d7
	bra.w operand
atToken
	tst.l d4
	bne.w absent
	; A lexical member must occupy its whole operand, not a term nested in
	; another expression. Previous delimiter is the head or a top-level comma.
	movea.l 8(sp), a1
	adda.w #20, a1
	cmpa.l a1, a0
	beq.w endOperand
	cmpi.w #TOKEN_COMMA, writer.Token.Kind-20(a0)
	bne.w absent
endOperand
	cmpi.l #1, d7
	beq.w contextual
	cmpi.w #TOKEN_COMMA, writer.Token.Kind+20(a0)
	bne.w absent
contextual
	move.l package.Header.MemberBindingCount(a6), d7
	movea.l a6, a0
	adda.l package.Header.MemberBindings(a6), a0
nextForm
	tst.l d7
	beq.w absent
	move.w 6(sp), d0
	cmp.w package.MemberBinding.Name(a0), d0
	bne.w next
	cmp.b package.MemberBinding.Qualifier(a0), d6
	bne.w next
	cmp.b package.MemberBinding.Operand(a0), d5
	bne.w next
	move.w 4(sp), d0
	cmp.w package.MemberBinding.Field(a0), d0
	beq.w matched
next
	adda.w #package.MEMBER_BINDING_BYTES, a0
	subq.l #1, d7
	bra.w nextForm
matched
	movea.l a2, a0
	move.l (sp), d0
	moveq #0, d2
	movea.l writer.Frame.Context(a5), a1
	movea.l writer.Frame.Binder(a5), a3
	jsr (a3)
	tst.l d0
	bne.w invalid
	move.l d2, d3
	moveq #0, d2
	move.w 4(sp), d2
	moveq #0, d0
	bra.w done
absent
	moveq #1, d0
	moveq #0, d2
	bra.w done
invalid
	moveq #2, d0
	moveq #0, d2
done
	adda.w #12, sp
	movem.l (sp)+, d4-d7/a2-a6
	tst.l d0
	rts
	.bend  ; bind
	.priv
; A0/D0=spelling; callback preserves the package/token/lexeme owners.
lookup	.block
	movea.l writer.Frame.Context(a5), a1
	jsr (a3)
	rts
	.bend  ; lookup
	.endsection
	.endmodule
