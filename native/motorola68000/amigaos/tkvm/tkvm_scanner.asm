; Native tokenizer VM token staging and scanner helpers.

	.module tkvm.amigaos.scanner
	.cpu 68020
	.pub
	.use tkvm.amigaos.demo_program
	.use tkvm.amigaos.char_predicates
	.use tkvm.amigaos.state

TK_STATUS_SUCCESS               = 0
TK_STATUS_TOKEN_OVERFLOW        = 2
TK_STATUS_LEXEME_OVERFLOW       = 3
TK_STATUS_VM_FAILURE            = 4
TK_STATUS_INVALID_PROGRAM       = 6

TK_VM_FAILURE_KIND_FAIL         = 1

TK_KIND_IDENTIFIER              = 0
TK_KIND_NUMBER                  = 2
TK_KIND_STRING                  = 3
TK_KIND_COMMA                   = 4
TK_KIND_COLON                   = 5
TK_KIND_DOLLAR                  = 6
TK_KIND_DOT                     = 7
TK_KIND_HASH                    = 8
TK_KIND_QUESTION                = 9
TK_KIND_OPEN_BRACKET            = 10
TK_KIND_CLOSE_BRACKET           = 11
TK_KIND_OPEN_BRACE              = 12
TK_KIND_CLOSE_BRACE             = 13
TK_KIND_OPEN_PAREN              = 14
TK_KIND_CLOSE_PAREN             = 15
TK_KIND_OP_RANGE                = 16
TK_KIND_OP_RANGE_INCLUSIVE      = 17
TK_KIND_OP_PLUS                 = 18
TK_KIND_OP_MINUS                = 19
TK_KIND_OP_MULTIPLY             = 20
TK_KIND_OP_POWER                = 21
TK_KIND_OP_DIVIDE               = 22
TK_KIND_OP_MOD                  = 23
TK_KIND_OP_SHL                  = 24
TK_KIND_OP_SHR                  = 25
TK_KIND_OP_BIT_NOT              = 26
TK_KIND_OP_LOGIC_NOT            = 27
TK_KIND_OP_BIT_AND              = 28
TK_KIND_OP_BIT_OR               = 29
TK_KIND_OP_BIT_XOR              = 30
TK_KIND_OP_LOGIC_AND            = 31
TK_KIND_OP_LOGIC_OR             = 32
TK_KIND_OP_LOGIC_XOR            = 33
TK_KIND_OP_EQ                   = 34
TK_KIND_OP_NE                   = 35
TK_KIND_OP_GE                   = 36
TK_KIND_OP_GT                   = 37
TK_KIND_OP_LE                   = 38
TK_KIND_OP_LT                   = 39

LOCAL_CURRENT_BYTE              = 0
LOCAL_PENDING_KIND              = 4
LOCAL_PENDING_START             = 8
LOCAL_PENDING_END               = 12
LOCAL_PENDING_LEX_LEN           = 16
LOCAL_TEMP_U32                  = 20

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Native token record staging and commit.
;
; Each committed record is 20 bytes in tokenBuffer:
; - word 0: token kind code
; - long 4: 1-based start column
; - long 8: 1-based end column
; - long 12: lexemeScratch offset
; - long 16: lexeme length in bytes
;
; This is the compact native surface that the asm tests decode back into the
; OPFORGE-TOKVM 1 report format.
; ---------------------------------------------------------------------------

; Commit the pending token metadata and staged lexeme bytes into the native token buffer.
; Inputs: A2 = LOCAL_* frame base; A5 = token buffer base; D1 = token count;
; D3 = scratch bytes used; D5 = token capacity; D6 = scratch capacity.
; Outputs: D0 = TK_STATUS_SUCCESS or an overflow status; D1 incremented on
; success; D3 advanced by the pending lexeme length on success; D2 = failing
; start column on overflow.
; Clobbers: A0-A1/CCR.
; CCR: reflects D0 on return.
commitPendingToken	.block
	cmp.l d5, d1  ; token_count < token_capacity
	bcc pendingTokenOverflow
	move.l d3, d0  ; scratch_used + pending_len must stay within scratch_capacity
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bhi pendingLexemeOverflow
	move.l d1, d0  ; compute record_index * TOKEN_RECORD_SIZE without a MUL dependency
	add.l d0, d0  ; *2
	movea.l d0, a0
	add.l d0, d0  ; *4
	add.l d0, d0  ; *8
	add.l d0, d0  ; *16
	adda.l d0, a0  ; *18
	move.l d1, d0
	add.l d0, d0  ; +2 => *20 byte stride
	adda.l d0, a0
	movea.l a5, a1
	adda.l a0, a1
	move.w LOCAL_PENDING_KIND(a2), (a1)  ; field 0: token kind code
	clr.w 2(a1)
	move.l LOCAL_PENDING_START(a2), d0  ; field 4: 1-based start column
	addq.l #1, d0
	move.l d0, 4(a1)
	move.l LOCAL_PENDING_END(a2), d0  ; field 8: 1-based end column
	addq.l #1, d0
	move.l d0, 8(a1)
	move.l d3, 12(a1)  ; field 12: lexeme offset into scratch
	move.l LOCAL_PENDING_LEX_LEN(a2), 16(a1)  ; field 16: lexeme length in bytes
	addq.l #1, d1
	add.l LOCAL_PENDING_LEX_LEN(a2), d3
	moveq #TK_STATUS_SUCCESS, d0
	rts

; Overflow exits report the start column of the token that could not be fully
; materialized. This matches the Rust-side behavior of attributing capacity
; failures to the token currently being scanned rather than the following byte.
pendingTokenOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_TOKEN_OVERFLOW, d0
	rts

pendingLexemeOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts
	.bend  ; commitPendingToken

; ---------------------------------------------------------------------------
; Scanner helpers.
;
; These are the native counterparts of the Rust tokenizer helper routines in
; tokenizer_runtime_utils.rs. Each helper advances D2 as the live source cursor,
; populates LOCAL_PENDING_* metadata, stages lexeme bytes into the scratch
; buffer, then commits a token record.
; ---------------------------------------------------------------------------
; Scan one identifier token from the current source cursor.
; Inputs: A2 = LOCAL_* frame base; A4 = source buffer base; A6 = scratch base;
; D2 = source cursor; D3 = scratch bytes used; D4 = source length; D6 =
; scratch capacity.
; Outputs: D0 = TK_STATUS_SUCCESS or an overflow status; D2 advanced past the
; identifier; D1/D3 updated by commitPendingToken on success.
; Clobbers: A0/CCR.
; CCR: reflects D0 on return.
scanIdentifierToken	.block
	; Identifier scan is the native mirror of vm_scan_identifier_token():
	; walk identifier-continue bytes, lowercase ASCII letters for the demo
	; policy, then emit one identifier record backed by scratch bytes.
	move.l d2, LOCAL_PENDING_START(a2)
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	movea.l a6, a0
	adda.l d3, a0

loop
	; D2 stays as the source cursor, A0 walks the next free scratch byte,
	; and LOCAL_PENDING_LEX_LEN grows in lockstep so commit can later write
	; both the source span and scratch payload length into the token record.
	cmp.l d4, d2
	bcc done
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr char_predicates.tkvmIsIdentifierContinue  ; mirrors vm_matches_identifier_continue_class()
	tst.l d0
	beq done
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc pendingLexemeOverflow
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'A', d0
	blo copyIdentifierByte
	cmpi.b #'Z', d0
	bhi copyIdentifierByte
	ori.b #$20, d0  ; native demo bakes in ASCII-lower identifier normalization used by the Rust bridge tests
copyIdentifierByte
	move.b d0, (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	bra loop

done
	; Match vm_scan_identifier_token(): a trailing prime belongs to the
	; identifier/register lexeme for Z80 alternate-register spellings like AF'.
	cmp.l d4, d2
	bcc commit
	cmpi.b #39, 0(a4, d2.l)
	bne commit
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc pendingLexemeOverflow
	move.b #39, (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)

commit
	; Identifier spans are half-open in cursor space and become 1-based only
	; when commitPendingToken serializes them into the native record.
	move.w #TK_KIND_IDENTIFIER, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr commitPendingToken
	rts

pendingLexemeOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts
	.bend  ; scanIdentifierToken

; Scan one permissive number token from the current source cursor.
; Inputs: A2 = LOCAL_* frame base; A4 = source buffer base; A6 = scratch base;
; D2 = source cursor; D3 = scratch bytes used; D4 = source length; D6 =
; scratch capacity.
; Outputs: D0 = TK_STATUS_SUCCESS or an overflow status; D2 advanced past the
; scanned number body; D1/D3 updated by commitPendingToken on success.
; Clobbers: A0/CCR.
; CCR: reflects D0 on return.
scanNumberToken	.block
	; Number scan accepts the same permissive body bytes as the Rust helper,
	; leaving base interpretation to downstream token consumers/report logic.
	move.l d2, LOCAL_PENDING_START(a2)
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	movea.l a6, a0
	adda.l d3, a0

loop
	; Number scanning intentionally keeps the raw source spelling, including
	; prefixes/suffixes/underscores, so later consumers can decide how to
	; interpret base markers just like the Rust runtime does.
	cmp.l d4, d2
	bcc done
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'%', d0
	bne checkBody
	cmp.l LOCAL_PENDING_START(a2), d2
	beq acceptByte
checkBody
	jsr char_predicates.tkvmIsNumberBody  ; same permissive number-body walk as vm_scan_number_token()
	tst.l d0
	beq done
acceptByte
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc pendingLexemeOverflow
	move.b 0(a4, d2.l), (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	bra loop

done
	move.w #TK_KIND_NUMBER, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr commitPendingToken
	rts

pendingLexemeOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts
	.bend  ; scanNumberToken

; Scan one quoted string token, decoding supported escape sequences into scratch bytes.
; Inputs: A2 = LOCAL_* frame base; A4 = source buffer base; A6 = scratch base;
; D2 = source cursor at the opening quote; D3 = scratch bytes used; D4 = source
; length; D6 = scratch capacity.
; Outputs: D0 = TK_STATUS_SUCCESS, TK_STATUS_LEXEME_OVERFLOW, or
; TK_STATUS_VM_FAILURE; D2 advanced past the closing delimiter on success;
; D1/D3 updated by commitPendingToken on success.
; Clobbers: A0/D1/CCR.
; CCR: reflects D0 on return.
scanStringToken	.block
	; Strings keep their raw delimiter choice for closing rules, but only the
	; decoded payload bytes are staged into scratch and exposed in LEXHEX.
	move.l d2, LOCAL_PENDING_START(a2)
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	moveq #0, d0
	move.b 0(a4, d2.l), d0  ; remember whether the string opened with ' or " so we can require the same closer
	move.l d0, LOCAL_CURRENT_BYTE(a2)
	addq.l #1, d2
	movea.l a6, a0
	adda.l d3, a0

loop
	; Strings advance one payload unit at a time. Plain bytes copy through,
	; while escape sequences normalize into their decoded payload bytes so
	; LEXHEX reflects runtime string contents rather than source spelling.
	cmp.l d4, d2
	bcc malformedString
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmp.l LOCAL_CURRENT_BYTE(a2), d0
	beq close
	cmpi.b #'\\', d0  ; decode the same escape surface exercised by vm_scan_string_token()
	bne literal
	addq.l #1, d2
	cmp.l d4, d2
	bcc malformedString
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'n', d0
	beq newLine
	cmpi.b #'r', d0
	beq return
	cmpi.b #'t', d0
	beq tab
	cmpi.b #'x', d0  ; \xHH is decoded into one payload byte, just like the Rust helper
	beq hex
	bra emitDecoded

newLine
	; The decoded escape value remains in D0 and falls through the shared
	; emit path that appends one payload byte to scratch.
	moveq #10, d0
	bra emitDecoded

return
	moveq #13, d0
	bra emitDecoded

tab
	moveq #9, d0
	bra emitDecoded

hex
	; Parse exactly two hex digits after \x and combine them into one byte,
	; mirroring tokenizer_runtime_utils::vm_scan_string_token().
	addq.l #1, d2
	cmp.l d4, d2
	bcc malformedString
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr char_predicates.tkvmHexDigitValue
	tst.l d0
	bmi malformedString
	move.l d0, LOCAL_TEMP_U32(a2)
	addq.l #1, d2
	cmp.l d4, d2
	bcc malformedString
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr char_predicates.tkvmHexDigitValue
	tst.l d0
	bmi malformedString
	move.l d1, -(sp)
	move.l LOCAL_TEMP_U32(a2), d1
	lsl.l #4, d1
	or.l d1, d0
	move.l (sp)+, d1

	bra emitDecoded

literal
	; Literal non-escape bytes use the same capacity accounting as decoded
	; escapes so both paths feed one consistent scratch payload stream.
	move.l d1, -(sp)
	move.l d3, d1
	add.l LOCAL_PENDING_LEX_LEN(a2), d1
	cmp.l d6, d1
	bcc literalOverflow
	move.l (sp)+, d1
	bra emitDecoded

literalOverflow
	move.l (sp)+, d1
	bra pendingLexemeOverflow

emitDecoded
	move.b d0, (a0)+
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	addq.l #1, d2
	bra loop

close
	; D2 is advanced past the closing delimiter before commit so the source
	; span matches the Rust token span semantics for quoted strings.
	addq.l #1, d2
	move.w #TK_KIND_STRING, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr commitPendingToken
	rts

; Unterminated strings and malformed escape sequences both collapse to the same
; VM malformedString status in this first native slice.
malformedString
	moveq #TK_STATUS_VM_FAILURE, d0
	rts

pendingLexemeOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts

	.bend  ; scanStringToken

; Symbol scan covers punctuation, operators, comments, and prefixed numeric
; forms. The structure intentionally parallels vm_scan_symbol_token() in Rust:
; dispatch by lead byte, optionally consume a longer form, then commit the
; canonical lexeme bytes through tkvmStageAndCommitSymbol.
scanSymbolToken	.block
	; The dispatch order matters. More syntactically specific lead bytes are
	; tested before generic operator fallbacks so multi-byte forms get the
	; same precedence as in the Rust helper.
	move.l d2, LOCAL_PENDING_START(a2)
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #';', d0
	beq commentToEol
	cmpi.b #'.', d0
	beq dotLike
	cmpi.b #'$', d0
	beq dollarOrPrefixedNumber
	cmpi.b #'%', d0
	beq percentOrPrefixedNumber
	cmpi.b #'#', d0
	beq stageHash
	cmpi.b #'?', d0
	beq stageQuestion
	cmpi.b #'[', d0
	beq stageOpenBracket
	cmpi.b #']', d0
	beq stageCloseBracket
	cmpi.b #'{', d0
	beq stageOpenBrace
	cmpi.b #'}', d0
	beq stageCloseBrace
	cmpi.b #',', d0
	beq stageComma
	cmpi.b #':', d0
	beq stageColon
	cmpi.b #'(', d0
	beq stageOpenParen
	cmpi.b #')', d0
	beq stageCloseParen
	cmpi.b #'+', d0
	beq stagePlus
	cmpi.b #'-', d0
	beq stageMinus
	cmpi.b #'*', d0
	beq scanStarLike
	cmpi.b #'/', d0
	beq stageDivide
	cmpi.b #'~', d0
	beq stageBitNot
	cmpi.b #'=', d0
	beq equalLike
	cmpi.b #'!', d0
	beq bangLike
	cmpi.b #'&', d0
	beq andLike
	cmpi.b #'|', d0
	beq orLike
	cmpi.b #'^', d0
	beq caretLike
	cmpi.b #'<', d0
	beq lessLike
	cmpi.b #'>', d0
	beq greaterLike
	move.w #TK_VM_FAILURE_KIND_FAIL, state.TkvmLastFailureKind
	move.w d0, state.TkvmLastFailureOperand
	moveq #TK_STATUS_VM_FAILURE, d0
	rts

commentToEol
	move.l d4, d2  ; comments consume the rest of the line and emit no token, matching vm_scan_symbol_token()
	moveq #TK_STATUS_SUCCESS, d0
	rts

dotLike
	; '.', '..', and '..=' are grouped together so the operator family stays
	; adjacent in both the native and Rust scanner implementations.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageDot
	cmpi.b #'.', 0(a4, d2.l)
	bne stageDot
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageRange
	cmpi.b #'=', 0(a4, d2.l)
	bne stageRange
	addq.l #1, d2
	move.w #TK_KIND_OP_RANGE_INCLUSIVE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexRangeInclusive, a0
	moveq #3, d0
	bra stageAndCommitSymbol

stageRange
	; '..' and '..=' share the same entry path so the inclusive form only
	; needs one extra lookahead byte and a different fixed lexeme template.
	move.w #TK_KIND_OP_RANGE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexRange, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageDot
	move.w #TK_KIND_DOT, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexDot, a0
	moveq #1, d0
	bra stageAndCommitSymbol

dollarOrPrefixedNumber
	; '$' is ambiguous by design: either a standalone dollar token or the
	; prefix for a hex-like number literal if a valid body byte follows.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageDollar
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr char_predicates.tkvmIsHexDigitOrUnderscore  ; '$' starts either a hex literal or a standalone dollar token
	tst.l d0
	beq stageDollar
	move.l LOCAL_PENDING_START(a2), d2
	jsr scanNumberToken
	rts

stageDollar
	move.w #TK_KIND_DOLLAR, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexDollar, a0
	moveq #1, d0
	bra stageAndCommitSymbol

percentOrPrefixedNumber
	; '%' is likewise split between modulo and binary-prefixed number forms.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stagePercent
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'0', d0
	beq percentAsNumber
	cmpi.b #'1', d0
	bne stagePercent

percentAsNumber
	jsr tkvmPercentHasPrefixContext
	tst.l d0
	beq stagePercent
	move.l LOCAL_PENDING_START(a2), d2  ; rewind so the number scanner sees the leading '%', like Rust prefixed-number handling
	jsr scanNumberToken
	rts

stagePercent
	move.w #TK_KIND_OP_MOD, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexMod, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageHash
	addq.l #1, d2
	move.w #TK_KIND_HASH, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexHash, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageQuestion
	; The single-byte staging labels below follow one repeated pattern:
	; advance the source cursor, choose a token kind, point at the canonical
	; lexeme bytes, set the lexeme length, then funnel through the shared
	; stage-and-commit tail.
	addq.l #1, d2
	move.w #TK_KIND_QUESTION, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexQuestion, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageOpenBracket
	addq.l #1, d2
	move.w #TK_KIND_OPEN_BRACKET, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexOpenBracket, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageCloseBracket
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_BRACKET, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexCloseBracket, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageOpenBrace
	addq.l #1, d2
	move.w #TK_KIND_OPEN_BRACE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexOpenBrace, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageCloseBrace
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_BRACE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexCloseBrace, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageComma
	addq.l #1, d2
	move.w #TK_KIND_COMMA, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexComma, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageColon
	addq.l #1, d2
	move.w #TK_KIND_COLON, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexColon, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageOpenParen
	addq.l #1, d2
	move.w #TK_KIND_OPEN_PAREN, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexOpenParen, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageCloseParen
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_PAREN, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexCloseParen, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stagePlus
	addq.l #1, d2
	move.w #TK_KIND_OP_PLUS, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexPlus, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageMinus
	addq.l #1, d2
	move.w #TK_KIND_OP_MINUS, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexMinus, a0
	moveq #1, d0
	bra stageAndCommitSymbol

scanStarLike
	; '*' and '**' match the Rust tokenizer's multiply/power split.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageMultiply
	cmpi.b #'*', 0(a4, d2.l)
	beq stagePower

stageMultiply
	move.w #TK_KIND_OP_MULTIPLY, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexMultiply, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stagePower
	addq.l #1, d2
	move.w #TK_KIND_OP_POWER, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexPower, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageDivide
	addq.l #1, d2
	move.w #TK_KIND_OP_DIVIDE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexDivide, a0
	moveq #1, d0
	bra stageAndCommitSymbol

stageBitNot
	addq.l #1, d2
	move.w #TK_KIND_OP_BIT_NOT, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexBitNot, a0
	moveq #1, d0
	bra stageAndCommitSymbol

equalLike
	; '=' and '==' both normalize to the same equality token kind, matching
	; the Rust tokenizer helper's forgiving equality parsing.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageEq
	cmpi.b #'=', 0(a4, d2.l)
	bne stageEq
	addq.l #1, d2

stageEq
	; The canonical fixed lexeme is always "==" for equality so report output
	; normalizes '=' and '==' into one operator surface.
	move.w #TK_KIND_OP_EQ, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexEq, a0
	moveq #2, d0
	bra stageAndCommitSymbol

bangLike
	; '!' stands for logical-not, while '!=' upgrades to not-equal.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageLogicNot
	cmpi.b #'=', 0(a4, d2.l)
	bne stageLogicNot
	addq.l #1, d2
	move.w #TK_KIND_OP_NE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexNe, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageLogicNot
	; Unlike equality, logical-not preserves its single-byte spelling in the
	; report/output surface because '!' and '!=' are distinct token kinds.
	move.w #TK_KIND_OP_LOGIC_NOT, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexLogicNot, a0
	moveq #1, d0
	bra stageAndCommitSymbol

andLike
	; '&' / '&&' map to bitwise and logical forms respectively.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageBitAnd
	cmpi.b #'&', 0(a4, d2.l)
	bne stageBitAnd
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_AND, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexLogicAnd, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageBitAnd
	move.w #TK_KIND_OP_BIT_AND, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexBitAnd, a0
	moveq #1, d0
	bra stageAndCommitSymbol

orLike
	; '|' / '||' map to bitwise and logical forms respectively.
	addq.l #1, d2
	cmp.l d4, d2
	bcc bitOr
	cmpi.b #'|', 0(a4, d2.l)
	bne bitOr
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_OR, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexLogicOr, a0
	moveq #2, d0
	bra stageAndCommitSymbol

bitOr
	move.w #TK_KIND_OP_BIT_OR, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexBitOr, a0
	moveq #1, d0
	bra stageAndCommitSymbol

caretLike
	; '^' is bitwise xor, while '^^' is promoted to the logical xor
	; token used by the native report-name table.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageBitXor
	cmpi.b #'^', 0(a4, d2.l)
	bne stageBitXor
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_XOR, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexLogicXor, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageBitXor
	move.w #TK_KIND_OP_BIT_XOR, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexBitXor, a0
	moveq #1, d0
	bra stageAndCommitSymbol

lessLike
	; '<' expands into four related operators: <, <<, <=, and <>/!=.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageLt
	cmpi.b #'<', 0(a4, d2.l)
	beq stageShl
	cmpi.b #'=', 0(a4, d2.l)
	beq stageLe
	cmpi.b #'>', 0(a4, d2.l)
	beq stageAltNe
	bra stageLt

stageShl
	addq.l #1, d2
	move.w #TK_KIND_OP_SHL, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexShl, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageLe
	addq.l #1, d2
	move.w #TK_KIND_OP_LE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexLe, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageAltNe
	addq.l #1, d2
	move.w #TK_KIND_OP_NE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexNe, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageLt
	; The family labels above all converge here with a fully-chosen token
	; kind and lexeme template, so the commit tail can stay generic.
	move.w #TK_KIND_OP_LT, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexLt, a0
	moveq #1, d0
	bra stageAndCommitSymbol

greaterLike
	; '>' expands into >, >>, and >=.
	addq.l #1, d2
	cmp.l d4, d2
	bcc stageGt
	cmpi.b #'>', 0(a4, d2.l)
	beq stageShr
	cmpi.b #'=', 0(a4, d2.l)
	beq stageGe
	bra stageGt

stageShr
	addq.l #1, d2
	move.w #TK_KIND_OP_SHR, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexShr, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageGe
	addq.l #1, d2
	move.w #TK_KIND_OP_GE, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexGe, a0
	moveq #2, d0
	bra stageAndCommitSymbol

stageGt
	move.w #TK_KIND_OP_GT, LOCAL_PENDING_KIND(a2)
	lea demo_program.LexGt, a0
	moveq #1, d0

stageAndCommitSymbol
	; At this point LOCAL_PENDING_START already marks the source span start,
	; D2 already points just past the consumed source bytes, and A0/D0 name
	; the canonical lexeme bytes to materialize into scratch.
	jsr stageFixedLexeme  ; stage the canonical lexeme bytes before committing the token metadata
	tst.l d0
	bne stageAndCommitSymbolDone
	move.l d2, LOCAL_PENDING_END(a2)
	jsr commitPendingToken
stageAndCommitSymbolDone
	rts

; Used when symbol lookahead discovers a shape the native bytecode contract does
; not allow, such as the unsupported '**' power operator.
symbolInvalidProgram
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_INVALID_PROGRAM, d0
	rts
	.bend  ; scanSymbolToken

.priv

; Stage a fixed lexeme literal from the static data table into scratch.
; This is used for punctuation and operator tokens whose lexeme spelling is
; known upfront and does not need to be copied from the source buffer.
stageFixedLexeme	.block
	move.l d0, LOCAL_PENDING_LEX_LEN(a2)  ; fixed operator/punctuation lexeme length from the inline template string
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bhi pendingLexemeOverflow
	movea.l a6, a1
	adda.l d3, a1
	move.l LOCAL_PENDING_LEX_LEN(a2), d0
stageFixedLexemeLoop
	tst.l d0
	beq stageFixedLexemeDone
	move.b (a0)+, (a1)+  ; copy the canonical lexeme bytes that Rust would expose in PortableToken text/raw
	subq.l #1, d0
	bra stageFixedLexemeLoop

stageFixedLexemeDone
	moveq #TK_STATUS_SUCCESS, d0
	rts

pendingLexemeOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts
	.bend  ; stageFixedLexeme

; Rust treats '%' as a binary-number prefix only when the byte appears where an
; expression can start. Without that context, % remains the modulo operator.
tkvmPercentHasPrefixContext	.block
	move.l LOCAL_PENDING_START(a2), d0
	beq prefixTrue

; Reuses pending-kind/temp slots as local scratch before symbol staging begins.
	clr.w LOCAL_PENDING_KIND(a2)
	clr.l LOCAL_TEMP_U32(a2)
	subq.l #1, d0
	move.l d0, LOCAL_TEMP_U32(a2)
	lea 0(a4, d0.l), a1
	moveq #0, d0
	move.b (a1), d0
	cmpi.b #' ', d0
	beq markLeadingSpace
	cmpi.b #9, d0
	bne checkPrevNonSpaceByte

markLeadingSpace
	moveq #1, d0
	move.w d0, LOCAL_PENDING_KIND(a2)

loop
	move.l LOCAL_TEMP_U32(a2), d0
	tst.l d0
	beq prefixTrue
	subq.l #1, d0
	move.l d0, LOCAL_TEMP_U32(a2)
	lea 0(a4, d0.l), a1
	moveq #0, d0
	move.b (a1), d0

checkPrevNonSpaceByte
	cmpi.b #' ', d0
	beq loop
	cmpi.b #9, d0
	beq loop
	cmpi.b #'(', d0
	beq prefixTrue
	cmpi.b #',', d0
	beq prefixTrue
	cmpi.b #'+', d0
	beq prefixTrue
	cmpi.b #'-', d0
	beq prefixTrue
	cmpi.b #'*', d0
	beq prefixTrue
	cmpi.b #'/', d0
	beq prefixTrue
	cmpi.b #'%', d0
	beq prefixTrue
	cmpi.b #'&', d0
	beq prefixTrue
	cmpi.b #'|', d0
	beq prefixTrue
	cmpi.b #'^', d0
	beq prefixTrue
	cmpi.b #'~', d0
	beq prefixTrue
	cmpi.b #'!', d0
	beq prefixTrue
	cmpi.b #'<', d0
	beq prefixTrue
	cmpi.b #'>', d0
	beq prefixTrue
	cmpi.b #'=', d0
	beq prefixTrue
	cmpi.b #'?', d0
	beq prefixTrue
	cmpi.b #':', d0
	beq prefixTrue

	tst.w LOCAL_PENDING_KIND(a2)
	beq prefixFalse
	jsr char_predicates.tkvmIsIdentifierContinue
	tst.l d0
	bne prefixTrue

prefixFalse
	moveq #0, d0
	rts

prefixTrue
	moveq #1, d0
	rts
	.bend  ; tkvmPercentHasPrefixContext

	.endsection
	.endmodule
