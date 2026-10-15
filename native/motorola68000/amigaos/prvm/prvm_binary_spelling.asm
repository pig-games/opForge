; Experimental pointer-free token/spelling view. Integers are big endian.
; @opforge-owner: prvm.amigaos.binary_spelling
	.module prvm.amigaos.binary_spelling
	.cpu 68020
	.pub
MAX_TOKENS = 64
MAX_ATOMS = 129
MAX_SPELLING_BYTES = 1024
K_IDENTIFIER = 0
K_COMMA = 4
K_COLON = 5
K_DOT = 7
K_OPEN_BRACKET = 10
K_CLOSE_BRACKET = 11
K_OPEN_BRACE = 12
K_CLOSE_BRACE = 13
K_OPEN_PAREN = 14
K_CLOSE_PAREN = 15
K_LAST = 40
GAP = $ffff
OK = 0
INVALID_ARGUMENT = 4
INVALID_TOKEN = 5
OUTPUT_OVERFLOW = 7
Header	.struct
Total	.long ?
AtomCount	.word ?
TokenCount	.word ?
SpellingBytes	.word ?
Reserved	.word ?
PayloadOffset	.long ?
	.endstruct
HEADER_BYTES = Header.PayloadOffset+4
Atom	.struct
Kind	.word ?
TokenIndex	.word ?
Start	.word ?
Bytes	.word ?
Offset	.long ?
	.endstruct
ATOM_BYTES = Atom.Offset+4
Frame	.struct
Source	.long ?
SourceBytes	.long ?
Tokens	.long ?
TokenCount	.long ?
Result	.long ?
Capacity	.long ?
	.endstruct
FRAME_BYTES = Frame.Capacity+4
MAX_BYTES = HEADER_BYTES+MAX_ATOMS*ATOM_BYTES+MAX_SPELLING_BYTES
	.section code, kind=code
; Build once from immutable TKVM 20-byte records and raw source. The caller's
; source extent may include a trailing comment: retain whitespace after the last
; token and stop at the first other byte. Never interpret comment delimiters.
; A0 frame. D0 status, D1 token count, D2 spelling bytes, D3 block bytes.
; Preserves D4-D7/A0-A6; CCR reflects D0. Output untouched on any failure.
run	.block
	movem.l d4-d7/a0-a6, -(sp)
	move.l a0, d0
	beq invalidFrame
	btst #0, d0
	bne invalidFrame
	movea.l a0, a4
	suba.l #MAX_BYTES, sp
	movea.l sp, a3
	tst.l Frame.Capacity(a4)
	bmi invalid
	move.l Frame.SourceBytes(a4), d7
	cmpi.l #MAX_SPELLING_BYTES, d7
	bhi invalid
	move.l Frame.TokenCount(a4), d6
	cmpi.l #MAX_TOKENS, d6
	bhi invalid
	move.l Frame.Source(a4), d0
	beq sourceNull
	add.l d7, d0
	bcs invalid
	bra sourceReady
sourceNull
	tst.l d7
	bne invalid
sourceReady
	movea.l Frame.Source(a4), a6
	move.l Frame.Tokens(a4), d0
	tst.l d6
	beq tokensReady
	tst.l d0
	beq invalid
	btst #0, d0
	bne invalid
	move.l d6, d1
	mulu.w #20, d1
	add.l d1, d0
	bcs invalid
tokensReady
	movea.l Frame.Tokens(a4), a5
	lea HEADER_BYTES(a3), a2
	clr.l d4
	clr.l d5
	clr.l d2
spanLoop
	cmp.l d6, d5
	bcc tail
	moveq #0, d0
	move.w (a5), d0
	cmpi.w #K_LAST, d0
	bhi badToken
	move.l 4(a5), d1
	subq.l #1, d1
	cmp.l d4, d1
	bcs badToken
	move.l 8(a5), d3
	subq.l #1, d3
	cmp.l d1, d3
	bls badToken
	cmp.l d7, d3
	bhi badToken
	cmp.l d4, d1
	beq tokenRow
	move.w #GAP, Atom.Kind(a2)
	move.w #GAP, Atom.TokenIndex(a2)
	move.w d4, Atom.Start(a2)
	sub.l d4, d1
	move.w d1, Atom.Bytes(a2)
	clr.l Atom.Offset(a2)
	add.l d1, d4
	adda.w #ATOM_BYTES, a2
	addq.l #1, d2
tokenRow
	move.w d0, Atom.Kind(a2)
	move.w d5, Atom.TokenIndex(a2)
	move.w d4, Atom.Start(a2)
	move.l d3, d1
	sub.l d4, d1
	move.w d1, Atom.Bytes(a2)
	clr.l Atom.Offset(a2)
	move.l d3, d4
	adda.w #ATOM_BYTES, a2
	addq.l #1, d2
	addq.l #1, d5
	adda.w #20, a5
	bra spanLoop
tail
	movea.l a2, a5
	move.l d4, d1
trailingWhitespace
	cmp.l d7, d1
	bcc extentReady
	movea.l a6, a2
	adda.l d1, a2
	move.l d7, d0
	sub.l d1, d0
	bsr.w whitespaceWidth
	tst.l d0
	beq extentReady
	add.l d0, d1
	bra trailingWhitespace
extentReady
	movea.l a5, a2
	move.l d1, d7
	cmp.l d4, d7
	beq rowsReady
	move.w #GAP, Atom.Kind(a2)
	move.w #GAP, Atom.TokenIndex(a2)
	move.w d4, Atom.Start(a2)
	sub.l d4, d1
	move.w d1, Atom.Bytes(a2)
	clr.l Atom.Offset(a2)
	adda.w #ATOM_BYTES, a2
	addq.l #1, d2
rowsReady
	move.l a2, d3
	sub.l a3, d3
	move.l d3, Header.PayloadOffset(a3)
	move.w d2, Header.AtomCount(a3)
	move.w d6, Header.TokenCount(a3)
	move.w d7, Header.SpellingBytes(a3)
	clr.w Header.Reserved(a3)
	lea HEADER_BYTES(a3), a0
	move.l d2, d0
	beq offsetsReady
setOffset
	moveq #0, d1
	move.w Atom.Start(a0), d1
	add.l d3, d1
	move.l d1, Atom.Offset(a0)
	adda.w #ATOM_BYTES, a0
	subq.l #1, d0
	bne setOffset
offsetsReady
	add.l d7, d3
	move.l d3, Header.Total(a3)
	cmp.l Frame.Capacity(a4), d3
	bhi overflow
	move.l Frame.Result(a4), d0
	beq invalid
	btst #0, d0
	bne invalid
	add.l d3, d0
	bcs invalid
	move.l d7, d0
	beq payloadReady
copyPayload
	move.b (a6)+, (a2)+
	subq.l #1, d0
	bne copyPayload
payloadReady
	movea.l a3, a0
	move.l d3, d0
	bsr.w validate
	bne failed
	movea.l Frame.Result(a4), a0
	movea.l a3, a1
	move.l d3, d0
publish
	move.b (a1)+, (a0)+
	subq.l #1, d0
	bne publish
	move.l d6, d1
	move.l d7, d2
	moveq #OK, d0
	bra done
badToken
	moveq #INVALID_TOKEN, d0
	bra failed
overflow
	moveq #OUTPUT_OVERFLOW, d0
	bra failed
invalid
	moveq #INVALID_ARGUMENT, d0
failed
	clr.l d1
	clr.l d2
	clr.l d3
done
	adda.l #MAX_BYTES, sp
	movem.l (sp)+, d4-d7/a0-a6
	tst.l d0
	rts
invalidFrame
	moveq #INVALID_ARGUMENT, d0
	clr.l d1
	clr.l d2
	clr.l d3
	movem.l (sp)+, d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run

; Validate canonical bounded layout, contiguous coverage and token numbering.
; A0 block, D0 exact available bytes. D0 status, D1 tokens, D2 spelling bytes,
; D3 block bytes. Preserves D4-D7/A0-A6; CCR reflects D0.
validate	.block
	movem.l d4-d7/a0-a6, -(sp)
	move.l a0, d1
	beq invalid
	btst #0, d1
	bne invalid
	cmpi.l #HEADER_BYTES, d0
	bcs invalid
	cmp.l Header.Total(a0), d0
	bne invalid
	move.l d0, d3
	add.l d1, d0
	bcs invalid
	tst.w Header.Reserved(a0)
	bne invalid
	moveq #0, d7
	move.w Header.AtomCount(a0), d7
	cmpi.w #MAX_ATOMS, d7
	bhi invalid
	moveq #0, d1
	move.w Header.TokenCount(a0), d1
	cmpi.w #MAX_TOKENS, d1
	bhi invalid
	moveq #0, d2
	move.w Header.SpellingBytes(a0), d2
	cmpi.w #MAX_SPELLING_BYTES, d2
	bhi invalid
	move.l d7, d6
	mulu.w #ATOM_BYTES, d6
	addi.l #HEADER_BYTES, d6
	cmp.l Header.PayloadOffset(a0), d6
	bne invalid
	move.l d6, d0
	add.l d2, d0
	cmp.l d3, d0
	bne invalid
	lea HEADER_BYTES(a0), a1
	clr.l d4
	clr.l d5
	moveq #0, d0
rowLoop
	tst.l d7
	beq complete
	moveq #0, d0
	move.w Atom.Start(a1), d0
	cmp.l d4, d0
	bne invalid
	add.l d6, d0
	cmp.l Atom.Offset(a1), d0
	bne invalid
	moveq #0, d0
	move.w Atom.Bytes(a1), d0
	beq invalid
	add.l d0, d4
	cmp.l d2, d4
	bhi invalid
	move.w Atom.Kind(a1), d0
	cmpi.w #GAP, d0
	beq gapAtom
	cmpi.w #K_LAST, d0
	bhi invalid
	cmp.w Atom.TokenIndex(a1), d5
	bne invalid
	addq.l #1, d5
	bra next
gapAtom
	cmpi.w #GAP, Atom.TokenIndex(a1)
	bne invalid
	; Canonical gaps are never adjacent.
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #HEADER_BYTES, d0
	beq next
	cmpi.w #GAP, -ATOM_BYTES+Atom.Kind(a1)
	beq invalid
next
	; Every atom is independently valid UTF-8, including gap rows.
	movea.l a0, a2
	adda.l Atom.Offset(a1), a2
	moveq #0, d0
	move.w Atom.Bytes(a1), d0
	move.l d4, -(sp)
	move.l d0, d4
atomUtf8
	tst.l d4
	beq atomDone
	move.l d4, d0
	cmpi.w #GAP, Atom.Kind(a1)
	bne tokenUtf8
	bsr.w whitespaceWidth
	tst.l d0
	beq atomInvalid
	bra characterReady
tokenUtf8
	bsr.w utf8Width
	tst.l d0
	beq atomInvalid
characterReady
	adda.l d0, a2
	sub.l d0, d4
	bra atomUtf8
atomInvalid
	move.l (sp)+, d4
	bra invalid
atomDone
	move.l (sp)+, d4
	adda.w #ATOM_BYTES, a1
	subq.l #1, d7
	bra rowLoop
complete
	cmp.l d1, d5
	bne invalid
	cmp.l d2, d4
	bne invalid
	moveq #OK, d0
	bra done
invalid
	moveq #INVALID_TOKEN, d0
	clr.l d1
	clr.l d2
	clr.l d3
done
	movem.l (sp)+, d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validate

; Find a token in an already validated view. A0 block, D0 token index.
; D0 status, A1 atom pointer (zero on failure). Preserves all other registers.
; CCR reflects D0. Bounded linear lookup avoids a second index representation.
token	.block
	move.l d1, -(sp)
	moveq #0, d1
	move.w Header.TokenCount(a0), d1
	cmp.l d1, d0
	bcc invalid
	lea HEADER_BYTES(a0), a1
	move.w Header.AtomCount(a0), d1
loop
	cmp.w Atom.TokenIndex(a1), d0
	beq found
	adda.w #ATOM_BYTES, a1
	subq.w #1, d1
	bne loop
invalid
	suba.l a1, a1
	moveq #INVALID_TOKEN, d0
	bra done
found
	moveq #OK, d0
done
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; token
	.priv
; A2 character, D0 remaining -> D0 TKVM gap whitespace width or zero.
whitespaceWidth	.block
	tst.l d0
	beq done
	cmpi.b #32, (a2)
	beq one
	cmpi.b #9, (a2)
	beq one
	clr.l d0
	bra done
one
	moveq #1, d0
done
	rts
	.bend  ; whitespaceWidth

; A2 character, D0 remaining bytes -> D0 UTF-8 width or zero. Other registers kept.
utf8Width	.block
	movem.l d1-d3, -(sp)
	move.l d0, d3
	beq invalid
	moveq #0, d1
	move.b (a2), d1
	cmpi.b #$80, d1
	bcs one
	cmpi.b #$c2, d1
	bcs invalid
	cmpi.b #$e0, d1
	bcs two
	cmpi.b #$f0, d1
	bcs three
	cmpi.b #$f5, d1
	bcc invalid
	cmpi.l #4, d3
	bcs invalid
	move.b 1(a2), d2
	cmpi.b #$f0, d1
	bne notF0
	cmpi.b #$90, d2
	bcs invalid
notF0
	cmpi.b #$f4, d1
	bne fourReady
	cmpi.b #$8f, d2
	bhi invalid
fourReady
	moveq #4, d0
	bra continuations
three
	cmpi.l #3, d3
	bcs invalid
	move.b 1(a2), d2
	cmpi.b #$e0, d1
	bne notE0
	cmpi.b #$a0, d2
	bcs invalid
notE0
	cmpi.b #$ed, d1
	bne threeReady
	cmpi.b #$9f, d2
	bhi invalid
threeReady
	moveq #3, d0
	bra continuations
two
	cmpi.l #2, d3
	bcs invalid
	moveq #2, d0
continuations
	moveq #1, d1
loop
	move.b 0(a2, d1.l), d2
	andi.b #$c0, d2
	cmpi.b #$80, d2
	bne invalid
	addq.l #1, d1
	cmp.l d0, d1
	bcs loop
	bra done
one
	moveq #1, d0
	bra done
invalid
	clr.l d0
done
	movem.l (sp)+, d1-d3
	rts
	.bend  ; utf8Width
	.endsection
	.endmodule
