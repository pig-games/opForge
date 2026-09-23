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
FLAG_ALLOWED = FLAG_INDENT+FLAG_BLOCK_OPEN+FLAG_BLOCK_CLOSE+FLAG_OMIT+FLAG_LAYOUT

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
; kinds 4..39 have no payload. All multibyte output fields are big-endian,
; with no alignment padding. Strings and unknown kinds are unsupported.
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
	cmpi.w #39, d3
	bhi.w unsupported
	cmpi.w #3, d3
	beq.w unsupported
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
