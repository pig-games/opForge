; Experimental lowering of one TKVM token line into owned binary source.
; @opforge-owner: experimental.amigaos.binary_source

	.module experimental.amigaos.binary_source
	.cpu 68020
	.use tkvm.amigaos.runtime as runtime
	.pub

STATUS_OK = 0
STATUS_INVALID = 1
STATUS_OVERFLOW = 2
STATUS_UNSUPPORTED = 3
STATUS_BIND_FAILED = 4
MAX_LINE = 256
FLAG_INDENT = 1
FLAG_BLOCK_OPEN = 2
FLAG_BLOCK_CLOSE = 4
FLAG_OMIT = 8
FLAG_LAYOUT = 16
FLAG_PLAN = 32
FLAG_ALLOWED = FLAG_INDENT+FLAG_BLOCK_OPEN+FLAG_BLOCK_CLOSE+FLAG_OMIT+FLAG_LAYOUT+FLAG_PLAN
COMPOSITE = 41
FRAGMENT_LITERAL = 0
FRAGMENT_FULL_LIST = 10
FRAGMENT_NAMED = 11
MACRO_PLAN = 42

Frame	.struct
Tokens	.long ?
TokenBytes	.long ?
Count	.long ?
Lexemes	.long ?
LexemeBytes	.long ?
Output	.long ?
Capacity	.long ?
Binder	.long ?
Context	.long ?
SourceLine	.word ?
Used	.word ?
Source	.long ?
SourceBytes	.long ?
NameDirective	.word ?  ; package ID whose first operand uses the binder; 0 disables
Reserved	.word ?
PackedMap	.long ?  ; optional Count+1 u16 packed offsets
	.endstruct
FRAME_BYTES = Frame.PackedMap+4

Token	.struct
Kind	.word ?
Reserved	.word ?
Start	.long ?
End	.long ?
Offset	.long ?
Length	.long ?
	.endstruct

	.section code, kind=code

; A0=Frame, whose input/output storage must not overlap. Tokens are native
; 20-byte TKVM records; Count records must fit TokenBytes. SourceLine is u16.
; Binder: A0=lexeme, D0=length, A1=Context; returns D0=0, D1=u16 canonical
; identifier ID, D2=u8 qualifier. It preserves D3-D7/A2-A6; CCR unspecified.
; Binder owns namespace/alias resolution; this writer contains no CPU semantics.
; NameDirective is a package-owned directive ID, or zero to disable numeric-name
; operands. Its first operand uses the same binder as identifier spellings.
; Result: [u8(total length-1), u8(flags), u16 source line], followed by
; Binder input D2 is 1 for the leading name token, 2 for a dotted
; statement head, otherwise 0.
; The callback returns the existing D2 qualifier.
; TKVM kind bytes: kinds 0/1 have u16 ID,u8 qualifier; kind2 has u32 value;
; kind 3 has [u8 decoded byte count, decoded bytes]. Kinds 4..40 have no
; payload. Kind 41 is a bounded composite recipe:
; [41, payload byte count, logical kind (0/1), fragment count, fragments].
; A literal fragment is [0, byte count, identifier bytes]; 1..9 denote
; positional arguments. Tags 10 (full argument list) and 11 (named argument,
; followed by one-based formal index) are reserved for the template expander.
; The record has no pointers: all fragment data is within its own byte count.
; All multibyte output fields are big-endian, with no alignment padding.
; Unknown kinds are unsupported.
; Bit zero is indentation; scope lowering adds numeric block-open/close bits.
; Returns D0=status, D1=length; Frame.Used equals D1. Preserves other registers.
; CCR reflects D0. On failure Used/D1 are zero, and any written prefix is zero
; (an invalid record length). Payload bytes and binder side effects may remain.
writeLine	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.w Frame.Used(a5)
	movea.l Frame.Output(a5), a3
	move.l Frame.Capacity(a5), d0
	beq.w overflow
	clr.b (a3)
	cmpi.l #4, d0
	blo.w overflow
	cmpi.l #MAX_LINE, d0
	bls.w capacityReady
	move.l #MAX_LINE, d0
capacityReady
	add.l a3, d0
	bcs.w invalid
	movea.l d0, a4
	move.l Frame.Count(a5), d7
	cmpi.l #MAX_LINE-4, d7
	bhi.w overflow
	move.l d7, d0
	mulu.w #20, d0
	cmp.l Frame.TokenBytes(a5), d0
	bhi.w invalid
	movea.l Frame.Tokens(a5), a2
	add.l a2, d0
	bcs.w invalid
	move.l Frame.Lexemes(a5), d0
	add.l Frame.LexemeBytes(a5), d0
	bcs.w invalid
	addq.l #1, a3
	moveq #0, d0
	tst.l d7
	beq.w flagsReady
	cmpi.l #1, Token.Start(a2)
	bls.w flagsReady
	moveq #1, d0
flagsReady
	move.b d0, (a3)+
	move.w Frame.SourceLine(a5), d0
	lsr.w #8, d0
	move.b d0, (a3)+
	move.w Frame.SourceLine(a5), d0
	move.b d0, (a3)+
loop
	bsr.w mapCursor
	tst.l d7
	beq.w complete
	move.l Token.Offset(a2), d0
	cmp.l Frame.LexemeBytes(a5), d0
	bhi.w invalid
	move.l Frame.LexemeBytes(a5), d1
	sub.l d0, d1
	move.l Token.Length(a2), d6
	cmp.l d1, d6
	bhi.w invalid
	movea.l Frame.Lexemes(a5), a0
	adda.l d0, a0
	moveq #0, d3
	move.w Token.Kind(a2), d3
	move.w Token.Reserved(a2), d0
	andi.w #runtime.TOKEN_RECIPE_INVALID, d0
	bne.w unsupported
	move.w Token.Reserved(a2), d0
	andi.w #runtime.TOKEN_RECIPE_VALID, d0
	bne.w composedName
	cmpi.w #40, d3
	bhi.w unsupported
	cmpi.w #3, d3
	beq.w compositeString
	bra.w regularToken
composedName
	move.l Frame.LexemeBytes(a5), d0
	sub.l Token.Offset(a2), d0
	sub.l d6, d0
	cmpi.l #2, d0
	blo.w invalid
	adda.l d6, a0
	moveq #0, d4
	move.b (a0)+, d4
	beq.w invalid
	cmp.l d7, d4
	bhi.w invalid
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.l #2, d1
	blo.w invalid
	subq.l #2, d0
	cmp.l d0, d1
	bhi.w invalid
	move.l a4, d0
	sub.l a3, d0
	move.l d1, d2
	addq.l #2, d2
	cmp.l d0, d2
	bhi.w overflow
	move.b #COMPOSITE, (a3)+
	move.b d1, (a3)+
copyComposedName
	move.b (a0)+, (a3)+
	subq.l #1, d1
	bne.w copyComposedName
	subq.l #1, d4
	move.l d4, d0
	move.l Frame.PackedMap(a5), d1
	beq.w recipeMapped
	movem.l d4/a0, -(sp)
	movea.l d1, a0
	move.l a2, d1
	sub.l Frame.Tokens(a5), d1
	divu.w #20, d1
	andi.l #$ffff, d1
	add.l d1, d1
	adda.l d1, a0
	move.w (a0), d1
recipeMap
	tst.l d4
	beq.w recipeMapDone
	addq.l #2, a0
	move.w d1, (a0)
	subq.l #1, d4
	bra.w recipeMap
recipeMapDone
	movem.l (sp)+, d4/a0
recipeMapped
	sub.l d4, d7
	mulu.w #20, d4
	adda.l d4, a2
	bra.w next
compositeString
	bsr.w literalString
	tst.l d0
	beq.w next
	bra.w overflow
regularToken
	cmpi.w #2, d3
	bne.w tokenKindReady
	bsr.w nameOperand
	tst.l d0
	beq.w tokenKindReady
	moveq #0, d3  ; numeric-looking spellings can be package-owned names
tokenKindReady
	moveq #1, d4
	cmpi.w #2, d3
	bhi.w sizeReady
	moveq #4, d4
	cmpi.w #2, d3
	blo.w sizeReady
	moveq #5, d4
sizeReady
	move.l a4, d0
	sub.l a3, d0
	cmp.l d4, d0
	blo.w overflow
	cmpi.w #2, d3
	bhi.w punctuation
	tst.l d6
	beq.w invalid
	move.l d6, d0
	cmpi.w #2, d3
	beq.w numeric
	move.l Frame.Binder(a5), d1
	beq.w bindFailed
	movea.l d1, a6
	movea.l Frame.Context(a5), a1
	; Input D2 identifies a leading name, independently of package spelling.
	move.l a3, d2
	sub.l Frame.Output(a5), d2
	cmpi.l #4, d2
	seq d2
	andi.l #1, d2
	tst.l d2
	bne.w bindName
	cmpa.l Frame.Tokens(a5), a2
	beq.w bindName
	cmpi.w #7, Token.Kind-20(a2)
	bne.w bindName
	move.l a3, d2
	sub.l Frame.Output(a5), d2
	cmpi.l #5, d2
	beq.w directiveName
	cmpi.l #9, d2
	beq.w directiveName
	cmpi.l #10, d2
	bne.w operandName
directiveName
	moveq #2, d2
	bra.w bindName
operandName
	moveq #0, d2
bindName
	jsr (a6)
	tst.l d0
	bne.w bindFailed
	cmpi.l #$ffff, d1
	bhi.w bindFailed
	cmpi.l #$ff, d2
	bhi.w bindFailed
	move.b d3, (a3)+
	move.w d1, d0
	lsr.w #8, d0
	move.b d0, (a3)+
	move.b d1, (a3)+
	move.b d2, (a3)+
	bra.w next
numeric
	cmpi.w #runtime.NUMBER_VALID, Token.Reserved(a2)
	bne.w invalid
	move.l Frame.LexemeBytes(a5), d0
	sub.l Token.Offset(a2), d0
	sub.l d6, d0
	cmpi.l #8, d0
	blo.w invalid
	adda.l d6, a0
	move.l (a0)+, d1
	bne.w invalid
	move.l (a0), d1
	move.b d3, (a3)+
	.for 4
	rol.l #8, d1
	move.b d1, (a3)+
	.endfor
	bra.w next
punctuation
	move.b d3, (a3)+
next
	adda.w #20, a2
	subq.l #1, d7
	bra.w loop
complete
	bsr.w mapCursor
	move.l a3, d1
	movea.l Frame.Output(a5), a0
	sub.l a0, d1
	move.w d1, Frame.Used(a5)
	move.l d1, d0
	subq.w #1, d0
	move.b d0, (a0)
	moveq #STATUS_OK, d0
	bra.w done
invalid
	moveq #STATUS_INVALID, d0
	bra.w failed
overflow
	moveq #STATUS_OVERFLOW, d0
	bra.w failed
unsupported
	moveq #STATUS_UNSUPPORTED, d0
	bra.w failed
bindFailed
	moveq #STATUS_BIND_FAILED, d0
failed
	moveq #0, d1
done
	tst.l d0
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; writeLine

; A3=current output,A5=Frame. D0=1 for the first operand of NameDirective,
; otherwise zero. Preserve other registers and the numeric token's lexeme A0.
; Recognize the emitted statement prefix, including an optional leading label.
nameOperand	.block
	movem.l d1/a1, -(sp)
	tst.w Frame.NameDirective(a5)
	beq.w no
	movea.l Frame.Output(a5), a1
	move.l a3, d0
	sub.l a1, d0
	cmpi.l #9, d0
	beq.w directive
	cmpi.l #13, d0
	beq.w implicitLabel
	cmpi.l #14, d0
	bne.w no
	cmpi.b #1, 4(a1)
	bhi.w no
	cmpi.b #5, 8(a1)
	bne.w no
	addq.l #5, a1
	bra.w directive
implicitLabel
	; Column-one labels acquire their colon in shared scope normalization,
	; after the writer has already bound this directive's numeric operand.
	tst.b 1(a1)
	bne.w no
	cmpi.b #1, 4(a1)
	bhi.w no
	addq.l #4, a1
directive
	cmpi.b #7, 4(a1)
	bne.w no
	cmpi.b #1, 5(a1)
	bhi.w no
	tst.b 8(a1)
	bne.w no
	moveq #0, d1
	move.b 6(a1), d1
	lsl.w #8, d1
	move.b 7(a1), d1
	cmp.w Frame.NameDirective(a5), d1
	bne.w no
	moveq #1, d0
	bra.w done
no
	moveq #0, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; nameOperand

	.priv

	; Record the packed cursor for the current lexical index. Recipe members
; share their recipe start; the following lexical boundary records its end.
mapCursor	.block
	movem.l d0-d2/a0, -(sp)
	move.l Frame.PackedMap(a5), d0
	beq.w done
	movea.l d0, a0
	move.l a2, d1
	sub.l Frame.Tokens(a5), d1
	divu.w #20, d1
	andi.l #$ffff, d1
	add.l d1, d1
	move.l a3, d2
	sub.l Frame.Output(a5), d2
	move.w d2, 0(a0, d1.l)
done
	movem.l (sp)+, d0-d2/a0
	rts
	.bend  ; mapCursor

	.pub
; A0=completed writer Frame,D1=nonzero arena handle. Append the six-byte
; typed trailer. D0/CCR=status,D1=record bytes on success. Preserves others.
appendPlan	.block
	movem.l d2/a1, -(sp)
	tst.l d1
	beq.w bad
	moveq #0, d2
	move.w Frame.Used(a0), d2
	addq.l #6, d2
	cmpi.l #MAX_LINE, d2
	bhi.w bad
	cmp.l Frame.Capacity(a0), d2
	bhi.w bad
	movea.l Frame.Output(a0), a1
	adda.w Frame.Used(a0), a1
	move.b #MACRO_PLAN, (a1)+
	.for 4
	rol.l #8, d1
	move.b d1, (a1)+
	.endfor
	move.b #6, (a1)
	move.w d2, Frame.Used(a0)
	movea.l Frame.Output(a0), a1
	ori.b #FLAG_PLAN, 1(a1)
	move.l d2, d1
	subq.w #1, d2
	move.b d2, (a1)
	moveq #0, d0
	bra.w done
bad
	moveq #STATUS_OVERFLOW, d0
	moveq #0, d1
done
	movem.l (sp)+, d2/a1
	tst.l d0
	rts
	.bend  ; appendPlan

	.pub
; TKVM already decodes string escapes into its lexeme buffer. Encode those
; bytes as [3, u8 length, bytes] without revisiting source text. A0/D6 are
; the decoded lexeme; A3/A4 are the bounded output cursor/end. D0=0 success,
; 3 overflow. A3 advances; other registers are preserved.
literalString	.block
	movem.l d1/a0, -(sp)
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #2, d0
	blo.w overflow
	move.l d6, d1
	addq.l #2, d1
	cmp.l d1, d0
	blo.w overflow
	cmpi.l #250, d6
	bhi.w overflow
	move.b #3, (a3)+
	move.b d6, (a3)+
	move.l d6, d1
copy
	tst.l d1
	beq.w complete
	move.b (a0)+, (a3)+
	subq.l #1, d1
	bra.w copy
complete
	moveq #0, d0
	bra.w done
overflow
	moveq #3, d0
done
	movem.l (sp)+, d1/a0
	rts
	.bend  ; literalString

	.endsection
	.endmodule
