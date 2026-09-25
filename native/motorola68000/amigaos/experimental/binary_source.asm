; Experimental lowering of one TKVM token line into owned binary source.
; @opforge-owner: experimental.amigaos.binary_source

	.module experimental.amigaos.binary_source
	.cpu 68020
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
FLAG_CALL_TEXT = 32
FLAG_ALLOWED = FLAG_INDENT+FLAG_BLOCK_OPEN+FLAG_BLOCK_CLOSE+FLAG_OMIT+FLAG_LAYOUT+FLAG_CALL_TEXT
COMPOSITE = 41
FRAGMENT_LITERAL = 0
FRAGMENT_FULL_LIST = 10
FRAGMENT_NAMED = 11
CALL_TEXT = 42

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
	.endstruct

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
; Result: [u8(total length-1), u8(flags), u16 source line], followed by
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
	cmpi.w #40, d3
	bhi.w unsupported
	cmpi.w #3, d3
	beq.w compositeString
	cmpi.w #40, d3
	bne.w checkIdentifierComposite
	bsr.w leadingComposite
	tst.l d0
	beq.w regularToken
	cmpi.l #1, d0
	beq.w next
	cmpi.l #2, d0
	beq.w unsupported
	bra.w overflow
checkIdentifierComposite
	cmpi.w #1, d3
	bhi.w regularToken
	bsr.w identifierComposite
	tst.l d0
	beq.w regularToken
	cmpi.l #1, d0
	beq.w next
	cmpi.l #2, d0
	beq.w unsupported
	bra.w overflow
compositeString
	bsr.w literalString
	tst.l d0
	beq.w next
	bra.w overflow
regularToken
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
	bsr.w parseNumber
	bne.w invalid
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
	bsr.w appendCallText
	bne.w overflow
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

	.priv

; @digit followed immediately by identifier bytes is one composite name.
; TKVM may scan digit+suffix as one permissive number token; bare @digit
; remains an ordinary positional placeholder.
leadingComposite	.block
	movem.l d1-d6/a0-a1/a4-a6, -(sp)
	moveq #0, d0
	cmpi.l #2, d7
	blo.w leadingDone
	lea 20(a2), a1
	cmpi.w #2, Token.Kind(a1)
	bne.w leadingDone
	move.l Token.End(a2), d1
	cmp.l Token.Start(a1), d1
	bne.w leadingDone
	move.l Token.Offset(a1), d0
	move.l d0, d1
	add.l Token.Length(a1), d1
	cmp.l Frame.LexemeBytes(a5), d1
	bhi.w leadingUnsupported
	tst.l Token.Length(a1)
	beq.w leadingUnsupported
	movea.l Frame.Lexemes(a5), a0
	adda.l d0, a0
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #'1', d2
	blo.w leadingUnsupported
	cmpi.b #'9', d2
	bhi.w leadingUnsupported
	move.l Token.Length(a1), d6
	moveq #2, d4  ; consumed TKVM token count
	cmpi.l #1, d6
	beq.w leadingSeparateSuffix
	subq.l #1, d6
	movea.l a0, a6
	addq.l #1, a6
	bra.w leadingValidate
leadingSeparateSuffix
	cmpi.l #3, d7
	blo.w leadingBare
	move.l Token.End(a1), d1
	lea 40(a2), a1
	cmpi.w #1, Token.Kind(a1)
	bhi.w leadingBare
	cmp.l Token.Start(a1), d1
	bne.w leadingBare
	move.l Token.Length(a1), d6
	tst.l d6
	beq.w leadingUnsupported
	move.l Token.Offset(a1), d0
	move.l d0, d1
	add.l d6, d1
	cmp.l Frame.LexemeBytes(a5), d1
	bhi.w leadingUnsupported
	movea.l Frame.Lexemes(a5), a6
	adda.l d0, a6
	moveq #3, d4
	bra.w leadingValidate
leadingBare
	moveq #0, d0
	bra.w leadingDone
leadingValidate
	cmpi.l #247, d6
	bhi.w leadingOverflow
	movea.l a6, a1
	move.l d6, d1
leadingValidateByte
	moveq #0, d0
	move.b (a1)+, d0
	cmpi.b #'A', d0
	blo.w leadingLower
	cmpi.b #'Z', d0
	bls.w leadingByteGood
leadingLower
	cmpi.b #'a', d0
	blo.w leadingDigit
	cmpi.b #'z', d0
	bls.w leadingByteGood
leadingDigit
	cmpi.b #'0', d0
	blo.w leadingUnderscore
	cmpi.b #'9', d0
	bls.w leadingByteGood
leadingUnderscore
	cmpi.b #'_', d0
	bne.w leadingUnsupported
leadingByteGood
	subq.l #1, d1
	bne.w leadingValidateByte
	move.l a4, d0
	sub.l a3, d0
	move.l d6, d1
	addq.l #7, d1  ; kind, size, logical kind, count, index, literal tag/size
	cmp.l d1, d0
	blo.w leadingOverflow
	move.b #COMPOSITE, (a3)+
	move.l d6, d0
	addq.l #5, d0
	move.b d0, (a3)+  ; payload is logical kind through literal bytes
	clr.b (a3)+  ; logical identifier kind
	move.b #2, (a3)+
	subi.b #'0', d2
	move.b d2, (a3)+
	clr.b (a3)+  ; literal fragment
	move.b d6, (a3)+
leadingCopy
	move.b (a6)+, (a3)+
	subq.l #1, d6
	bne.w leadingCopy
	move.w d4, d0
	subq.w #1, d0
	mulu.w #20, d0
	adda.l d0, a2
	subq.w #1, d4
	sub.w d4, d7
	moveq #1, d0
	bra.w leadingDone
leadingUnsupported
	moveq #2, d0
	bra.w leadingDone
leadingOverflow
	moveq #3, d0
leadingDone
	movem.l (sp)+, d1-d6/a0-a1/a4-a6
	rts
	.bend  ; leadingComposite

; Retain only the exact argument region of a dotted call-shaped line. The
; trailing size locates this offset-only sidecar without changing token IDs.
; A5=writer frame,A2=first TKVM token,A3/A4=output cursor/end,D7=zero.
; D0/CCR=status; other registers preserved except D1/D2/D4/D6/A0/A1.
appendCallText	.block
	movea.l Frame.Tokens(a5), a2
	move.l Frame.Count(a5), d6
	cmpi.l #2, d6
	blo.w noText
	cmpi.w #7, Token.Kind(a2)
	beq.w dotFirst
	cmpi.w #1, Token.Kind(a2)
	bhi.w noText
	lea 20(a2), a1
	cmpi.w #7, Token.Kind(a1)
	beq.w nameFirst
	cmpi.w #5, Token.Kind(a1)
	bne.w noText
	cmpi.l #4, d6
	blo.w noText
	lea 40(a2), a1
	cmpi.w #7, Token.Kind(a1)
	bne.w noText
	lea 60(a2), a1
	bra.w callName
dotFirst
	lea 20(a2), a1
	bra.w callName
nameFirst
	lea 40(a2), a1
callName
	cmpi.w #1, Token.Kind(a1)
	bhi.w noText
	move.l Token.End(a1), d2
	subq.l #1, d2
	move.l d6, d0
	subq.l #1, d0
	mulu.w #20, d0
	adda.l d0, a2
	move.l Token.End(a2), d4
	subq.l #1, d4
	cmp.l d2, d4
	blo.w invalidText
	cmp.l Frame.SourceBytes(a5), d4
	bhi.w invalidText
	sub.l d2, d4
	beq.w noText
	cmpi.l #251, d4
	bhi.w invalidText
	move.l a4, d0
	sub.l a3, d0
	move.l d4, d1
	addq.l #3, d1
	cmp.l d1, d0
	blo.w invalidText
	movea.l Frame.Source(a5), a0
	adda.l d2, a0
	move.b #CALL_TEXT, (a3)+
	move.b d4, (a3)+
copyCallText
	move.b (a0)+, (a3)+
	subq.l #1, d4
	bne.w copyCallText
	move.b d1, (a3)+
	movea.l Frame.Output(a5), a0
	ori.b #FLAG_CALL_TEXT, 1(a0)
noText
	moveq #0, d0
	rts
invalidText
	moveq #1, d0
	rts
	.bend  ; appendCallText

; A0 is the first identifier lexeme, D6 its length, A2 the TKVM token, D7 the
; remaining count, A3 the packed cursor and A4 its limit. D0 returns 0 when
; this is an ordinary identifier, 1 after writing a composite, 2 unsupported,
; or 3 overflow. On success A2/D7 additionally consume the adjacent @/digit.
identifierComposite	.block
	movem.l d1-d6/a0-a1/a4-a6, -(sp)
	moveq #0, d0
	cmpi.l #3, d7
	blo.w done
	movea.l a2, a1
	adda.w #20, a1
	cmpi.w #40, Token.Kind(a1)
	bne.w done
	move.l Token.End(a2), d1
	cmp.l Token.Start(a1), d1
	bne.w done
	move.l Token.End(a1), d1
	lea 20(a1), a6
	cmpi.w #2, Token.Kind(a6)
	bne.w done
	cmp.l Token.Start(a6), d1
	bne.w done
	movea.l a3, a6  ; recipe start
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #4, d0
	blo.w overflow
	move.b #COMPOSITE, (a3)+
	addq.l #3, a3
	moveq #0, d5  ; fragment count
	moveq #1, d4  ; consumed token count
	bsr.w appendCompositeLiteral
	bne.w done
	movea.l a2, a1
	adda.w #20, a1
	move.l Token.End(a2), d1
compositeNext
	cmp.l d7, d4
	bhs.w compositeComplete
	cmp.l Token.Start(a1), d1
	bne.w compositeComplete
	cmpi.w #40, Token.Kind(a1)
	beq.w compositeAt
	cmpi.w #1, Token.Kind(a1)
	bhi.w compositeComplete
	move.l Token.Offset(a1), d0
	move.l Token.Length(a1), d6
	tst.l d6
	beq.w unsupported
	cmpi.l #250, d6
	bhi.w overflow
	move.l d0, d2
	add.l d6, d2
	cmp.l Frame.LexemeBytes(a5), d2
	bhi.w unsupported
	movea.l Frame.Lexemes(a5), a0
	adda.l d0, a0
	bsr.w appendCompositeLiteral
	bne.w done
	move.l Token.End(a1), d1
	adda.w #20, a1
	addq.w #1, d4
	bra.w compositeNext
compositeAt
	move.w d4, d0
	addq.w #1, d0
	cmp.l d7, d0
	bhs.w unsupported
	lea 20(a1), a0
	move.l Token.End(a1), d1
	cmp.l Token.Start(a0), d1
	bne.w unsupported
	cmpi.w #2, Token.Kind(a0)
	bne.w unsupported
	move.l Token.Length(a0), d6
	tst.l d6
	beq.w unsupported
	move.l Token.Offset(a0), d0
	move.l d0, d2
	add.l d6, d2
	cmp.l Frame.LexemeBytes(a5), d2
	bhi.w unsupported
	movea.l Frame.Lexemes(a5), a0
	adda.l d0, a0
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #'1', d2
	blo.w unsupported
	cmpi.b #'9', d2
	bhi.w unsupported
	cmpa.l a4, a3
	bhs.w overflow
	subi.b #'0', d2
	move.b d2, (a3)+
	addq.w #1, d5
	cmpi.l #1, d6
	beq.w compositeNumberDone
	addq.l #1, a0
	subq.l #1, d6
	bsr.w validateCompositeSuffix
	bne.w unsupported
	bsr.w appendCompositeLiteral
	bne.w done
compositeNumberDone
	lea 20(a1), a0
	move.l Token.End(a0), d1
	adda.w #40, a1
	addq.w #2, d4
	bra.w compositeNext
compositeComplete
	move.l a3, d0
	sub.l a6, d0
	subq.l #2, d0
	cmpi.l #255, d0
	bhi.w overflow
	move.b d0, 1(a6)
	move.b d3, 2(a6)
	move.b d5, 3(a6)
	move.w d4, d0
	subq.w #1, d0
	mulu.w #20, d0
	adda.l d0, a2
	subq.w #1, d4
	sub.w d4, d7
	moveq #1, d0
	bra.w done
appendCompositeLiteral
	move.l a4, d0
	sub.l a3, d0
	move.l d6, d2
	addq.l #2, d2
	cmp.l d2, d0
	blo.w literalOverflow
	move.b #FRAGMENT_LITERAL, (a3)+
	move.b d6, (a3)+
	move.l d6, d2
literalCopy
	move.b (a0)+, (a3)+
	subq.l #1, d2
	bne.w literalCopy
	addq.w #1, d5
	moveq #0, d0
	rts
literalOverflow
	moveq #3, d0
	rts
validateCompositeSuffix
	movem.l d1-d2/a0, -(sp)
	move.l d6, d1
validateSuffixByte
	moveq #0, d2
	move.b (a0)+, d2
	cmpi.b #'A', d2
	blo.w suffixLower
	cmpi.b #'Z', d2
	bls.w suffixGood
suffixLower
	cmpi.b #'a', d2
	blo.w suffixDigit
	cmpi.b #'z', d2
	bls.w suffixGood
suffixDigit
	cmpi.b #'0', d2
	blo.w suffixUnderscore
	cmpi.b #'9', d2
	bls.w suffixGood
suffixUnderscore
	cmpi.b #'_', d2
	bne.w suffixBad
suffixGood
	subq.l #1, d1
	bne.w validateSuffixByte
	moveq #0, d0
	bra.w suffixDone
suffixBad
	moveq #1, d0
suffixDone
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	rts
unsupported
	moveq #2, d0
	bra.w done
overflow
	moveq #3, d0
done
	movem.l (sp)+, d1-d6/a0-a1/a4-a6
	rts
	.bend  ; identifierComposite

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

	.priv
; A0=nonempty number lexeme, D0=byte count. D0=status, D1=u32 value;
; clobbers D2/A0; preserves D3-D6. CCR reflects D0. Underscores are separators.
; Accept decimal, $hex, %binary and 0x/0X hex. Overflow is never truncated.
parseNumber	.block
	movem.l d3-d6, -(sp)
	moveq #10, d3
	move.l #429496729, d4
	moveq #5, d5
	cmpi.b #'$', (a0)
	beq.w hexPrefix
	cmpi.b #'%', (a0)
	beq.w binaryPrefix
	cmpi.l #2, d0
	blo.w body
	cmpi.b #'0', (a0)
	bne.w body
	move.b 1(a0), d2
	ori.b #$20, d2
	cmpi.b #'x', d2
	bne.w body
	addq.l #1, a0
	subq.l #1, d0
hexPrefix
	moveq #16, d3
	move.l #$0fffffff, d4
	moveq #15, d5
	bra.w skipPrefix
binaryPrefix
	moveq #2, d3
	move.l #$7fffffff, d4
	moveq #1, d5
skipPrefix
	addq.l #1, a0
	subq.l #1, d0
body
	moveq #0, d1
	moveq #0, d6
digitLoop
	tst.l d0
	beq.w endDigits
	subq.l #1, d0
	moveq #0, d2
	move.b (a0)+, d2
	cmpi.b #'_', d2
	beq.w digitLoop
	cmpi.b #'0', d2
	blo.w bad
	cmpi.b #'9', d2
	bls.w decimalDigit
	ori.b #$20, d2
	subi.b #'a', d2
	cmpi.b #5, d2
	bhi.w bad
	addi.l #10, d2
	bra.w checkDigit
decimalDigit
	subi.b #'0', d2
checkDigit
	cmp.l d3, d2
	bhs.w bad
	cmp.l d4, d1
	bhi.w bad
	blo.w accumulate
	cmp.l d5, d2
	bhi.w bad
accumulate
	cmpi.w #10, d3
	beq.w decimalValue
	cmpi.w #16, d3
	beq.w hexValue
	add.l d1, d1
	bra.w addDigit
hexValue
	lsl.l #4, d1
	bra.w addDigit
decimalValue
	add.l d1, d1
	move.l d1, d6
	lsl.l #2, d1
	add.l d6, d1
addDigit
	add.l d2, d1
	moveq #1, d6
	bra.w digitLoop
endDigits
	tst.l d6
	beq.w bad
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_INVALID, d0
done
	movem.l (sp)+, d3-d6
	rts
	.bend  ; parseNumber

	.endsection
	.endmodule
