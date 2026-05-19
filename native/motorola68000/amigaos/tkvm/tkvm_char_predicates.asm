; Native tokenizer VM byte classification helpers.

	.module tkvm.amigaos.char_predicates
	.cpu 68020
	.pub

; ---------------------------------------------------------------------------
; Character-class and byte-shape predicates.
;
; These intentionally parallel vm_char_class_matches, vm_matches_identifier_*,
; and related helper logic in tokenizer_runtime_utils.rs. The native demo loop
; calls them through JumpIfClass and the scan helpers reuse them while walking
; identifiers, number bodies, strings, and prefixed constants.
; ---------------------------------------------------------------------------
tkvmIsWhitespace	.block
	cmpi.b #' ', d0  ; this line-input slice only treats space and tab as intra-line whitespace
	beq tkvmPredicateTrue
	cmpi.b #9, d0
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsWhitespace

tkvmIsIdentifierStart	.block
	; These predicate chains intentionally avoid lookup tables so the native
	; implementation stays easy to audit against the Rust helper masks.
	cmpi.b #'A', d0
	blo tkvmCheckIdentStartLower
	cmpi.b #'Z', d0
	bls tkvmPredicateTrue
tkvmCheckIdentStartLower
	cmpi.b #'a', d0
	blo tkvmCheckIdentStartPunct
	cmpi.b #'z', d0
	bls tkvmPredicateTrue
tkvmCheckIdentStartPunct
	cmpi.b #'_', d0
	beq tkvmPredicateTrue
	cmpi.b #'.', d0  ; '.' remains identifier-start-capable because the runtime class mask includes it
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsIdentifierStart

tkvmIsIdentifierContinue	.block
	cmpi.b #'A', d0
	blo tkvmCheckIdentContinueLower
	cmpi.b #'Z', d0
	bls tkvmPredicateTrue
tkvmCheckIdentContinueLower
	cmpi.b #'a', d0
	blo tkvmCheckIdentContinueDigit
	cmpi.b #'z', d0
	bls tkvmPredicateTrue
tkvmCheckIdentContinueDigit
	cmpi.b #'0', d0
	blo tkvmCheckIdentExtra
	cmpi.b #'9', d0
	bls tkvmPredicateTrue
tkvmCheckIdentExtra
	cmpi.b #'_', d0
	beq tkvmPredicateTrue
	cmpi.b #'.', d0
	beq tkvmPredicateTrue
	cmpi.b #'$', d0  ; '$' and '@' stay valid continue bytes per tokenizer_runtime_utils.rs masks
	beq tkvmPredicateTrue
	cmpi.b #'@', d0
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsIdentifierContinue

tkvmIsQuoteChar	.block
	cmpi.b #'"', d0  ; demo program accepts both quote styles, matching the Rust helper's quote-char set
	beq tkvmPredicateTrue
	cmpi.b #39, d0
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsQuoteChar

tkvmIsNumberBody	.block
	; Number bodies are deliberately permissive at scan time. Validation of
	; bases and suffix meaning is deferred to later consumers, matching the
	; Rust tokenizer helper contract.
	cmpi.b #'0', d0
	blo tkvmCheckNumberLetters
	cmpi.b #'9', d0
	bls tkvmPredicateTrue
tkvmCheckNumberLetters
	cmpi.b #'A', d0
	blo tkvmCheckNumberLower
	cmpi.b #'Z', d0
	bls tkvmPredicateTrue
tkvmCheckNumberLower
	cmpi.b #'a', d0
	blo tkvmCheckNumberExtra
	cmpi.b #'z', d0
	bls tkvmPredicateTrue
tkvmCheckNumberExtra
	cmpi.b #'_', d0
	beq tkvmPredicateTrue
	cmpi.b #'$', d0
	beq tkvmPredicateTrue
	cmpi.b #'%', d0
	beq tkvmPredicateTrue
	cmpi.b #'@', d0
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsNumberBody

tkvmIsHexDigitOrUnderscore	.block
	; Used only as a fast probe for deciding whether '$' begins a number or
	; remains a standalone token.
	cmpi.b #'_', d0
	beq tkvmPredicateTrue
	jsr tkvmHexDigitValue
	tst.l d0
	bmi tkvmPredicateFalse
	bra tkvmPredicateTrue
	.bend  ; tkvmIsHexDigitOrUnderscore

tkvmHexDigitValue	.block
	cmpi.b #'0', d0  ; shared nibble decoder for \xHH strings and '$'-prefixed number probing
	blo tkvmHexUpper
	cmpi.b #'9', d0
	bhi tkvmHexUpper
	subi.b #'0', d0
	andi.l #$FF, d0
	rts

tkvmHexUpper
	cmpi.b #'A', d0
	blo tkvmHexLower
	cmpi.b #'F', d0
	bhi tkvmHexLower
	subi.b #'A', d0
	addi.b #10, d0
	andi.l #$FF, d0
	rts

tkvmHexLower
	cmpi.b #'a', d0
	blo tkvmHexInvalid
	cmpi.b #'f', d0
	bhi tkvmHexInvalid
	subi.b #'a', d0
	addi.b #10, d0
	andi.l #$FF, d0
	rts

tkvmHexInvalid
	moveq #-1, d0
	rts
	.bend  ; tkvmHexDigitValue

	.priv

tkvmPredicateTrue
	moveq #1, d0
	rts

tkvmPredicateFalse
	moveq #0, d0
	rts

	.endmodule
