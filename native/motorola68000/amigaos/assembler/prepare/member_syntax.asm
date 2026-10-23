; Acquisition-only package member facets, selected by numeric binding rows.
; @opforge-owner: opforge.assembler.prepare.member_syntax
.module opforge.assembler.prepare.member_syntax
.cpu 68020
.use opforge.assembler.source.records as source
.use opforge.packages.format as package
.use opforge.vm.tkvm.runtime as tokens
.pub
.section code, kind=code
; A0=writer.Frame,A1=field token,A2=validated package,A3=dictionary lookup.
; Lookup ABI matches binary_members.bind. Return D0=0 syntax member/1 ordinary,
; D1=package field ID,D2=syntax role. Preserve D3-D7/A0-A6; no publication.
field	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	movea.l a2, a6
	movea.l source.Frame.Tokens(a5), a2
	move.l source.Frame.Count(a5), d0
	cmpi.l #source.MAX_LINE, d0
	bhi.w absent
	mulu.w #source.TOKEN_BYTES, d0
	cmp.l source.Frame.TokenBytes(a5), d0
	bhi.w absent
	movea.l a2, a1
	adda.l d0, a1
	cmpa.l a2, a1
	blo.w absent
	suba.w #source.TOKEN_BYTES, a1
	cmpa.l a1, a4
	bhi.w absent
	move.l a4, d0
	sub.l a2, d0
	bcs.w absent
	divu.w #source.TOKEN_BYTES, d0
	swap d0
	tst.w d0
	bne.w absent
	moveq #0, d0
	move.w source.Frame.HeadToken(a5), d0
	cmp.l source.Frame.Count(a5), d0
	bhs.w absent
	mulu.w #source.TOKEN_BYTES, d0
	adda.l d0, a2
	cmpa.l a2, a4
	bls.w absent
	cmpi.w #source.TOKEN_NAME_MAX, source.Token.Kind(a2)
	bhi.w absent
	tst.w source.Token.Reserved(a2)
	bne.w absent
	cmpa.l source.Frame.Tokens(a5), a2
	beq.w root
	cmpi.w #tokens.TK_KIND_DOT, source.Token.Kind-source.TOKEN_BYTES(a2)
	beq.w absent  ; shared dot heads cannot select package operand syntax
root
	lea source.TOKEN_BYTES(a2), a0
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #2*source.TOKEN_BYTES, d0
	blo.w absent
	cmpi.w #tokens.TK_KIND_DOT, source.Token.Kind-source.TOKEN_BYTES(a4)
	bne.w absent
	lea -2*source.TOKEN_BYTES(a4), a1
	cmpi.w #tokens.TK_KIND_NUMBER, source.Token.Kind(a1)
	beq.w boundary
	cmpi.w #tokens.TK_KIND_CLOSE_PAREN, source.Token.Kind(a1)
	bne.w absent
	moveq #1, d0
paren
	cmpa.l a0, a1
	bls.w absent
	suba.w #source.TOKEN_BYTES, a1
	cmpi.w #tokens.TK_KIND_CLOSE_PAREN, source.Token.Kind(a1)
	bne.w opening
	addq.l #1, d0
	bra.w paren
opening
	cmpi.w #tokens.TK_KIND_OPEN_PAREN, source.Token.Kind(a1)
	bne.w paren
	subq.l #1, d0
	bne.w paren
boundary
	cmpa.l a0, a1
	beq.w fieldName
	move.w source.Token.Kind-source.TOKEN_BYTES(a1), d0
	cmpi.w #tokens.TK_KIND_COMMA, d0
	beq.w fieldName
	cmpi.w #tokens.TK_KIND_OPEN_PAREN, d0
	beq.w fieldName
	cmpi.w #tokens.TK_KIND_OPEN_BRACKET, d0
	bne.w absent
fieldName
	movea.l a4, a1
	bsr.w lookupToken
	bne.w absent
	btst #package.DICTIONARY_MEMBER_BIT, d3
	beq.w absent
	move.l d1, d7
	movea.l a2, a1
	bsr.w lookupToken
	bne.w absent
	move.l d1, d6
	move.l d2, d5
	lea source.TOKEN_BYTES(a2), a0
	moveq #0, d4  ; top-level operand index
	moveq #0, d3  ; delimiter depth
scan
	cmpa.l a4, a0
	beq.w forms
	move.w source.Token.Kind(a0), d0
	cmpi.w #tokens.TK_KIND_OPEN_PAREN, d0
	beq.w open
	cmpi.w #tokens.TK_KIND_OPEN_BRACKET, d0
	beq.w open
	cmpi.w #tokens.TK_KIND_OPEN_BRACE, d0
	beq.w open
	cmpi.w #tokens.TK_KIND_CLOSE_PAREN, d0
	beq.w close
	cmpi.w #tokens.TK_KIND_CLOSE_BRACKET, d0
	beq.w close
	cmpi.w #tokens.TK_KIND_CLOSE_BRACE, d0
	beq.w close
	cmpi.w #tokens.TK_KIND_COMMA, d0
	bne.w next
	tst.l d3
	bne.w next
	addq.l #1, d4
	bra.w next
open
	addq.l #1, d3
	bra.w next
close
	subq.l #1, d3
	bmi.w absent
next
	adda.w #source.TOKEN_BYTES, a0
	bra.w scan
forms
	move.l package.Header.MemberBindingCount(a6), d3
	movea.l a6, a0
	adda.l package.Header.MemberBindings(a6), a0
row
	tst.l d3
	beq.w absent
	cmp.w package.MemberBinding.Name(a0), d6
	bne.w advance
	cmp.b package.MemberBinding.Qualifier(a0), d5
	bne.w advance
	cmp.b package.MemberBinding.Operand(a0), d4
	bne.w advance
	cmp.w package.MemberBinding.Field(a0), d7
	beq.w matched
advance
	adda.w #package.MEMBER_BINDING_BYTES, a0
	subq.l #1, d3
	bra.w row
matched
	move.l d7, d1
	move.l #source.SYNTAX_MEMBER_QUALIFIER, d2
	moveq #0, d0
	bra.w done
absent
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
.bend  ; field
.priv
; A1=bounded captured token; lookup callback preserves A2-A6/D4-D7.
lookupToken	.block
	move.l source.Token.Offset(a1), d0
	move.l source.Frame.LexemeBytes(a5), d1
	sub.l d0, d1
	bcs.w absent
	move.l source.Token.Length(a1), d0
	cmp.l d1, d0
	bhi.w absent
	movea.l source.Frame.Lexemes(a5), a0
	adda.l source.Token.Offset(a1), a0
	movea.l source.Frame.Context(a5), a1
	jsr (a3)
	rts
absent
	moveq #1, d0
	rts
.bend  ; lookupToken
.endsection
.endmodule
