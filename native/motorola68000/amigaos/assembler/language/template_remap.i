; Remap only numeric identities in closed packed template storage. The ordinary
; expander still owns substitution, hygiene, generated spelling and evaluation.
	.pub
RemapFrame	.struct
Image	.long ?
Bytes	.long ?
Resolve	.long ?
Context	.long ?
	.endstruct
REMAP_FRAME_BYTES = RemapFrame.Context+4
CLOSED_VALUE = 0
CLOSED_MEMBER = 1
	.priv
ClosedWordLe	.struct
Low	.byte ?
High	.byte ?
	.endstruct
CLOSED_WORD_BYTES = ClosedWordLe.High+1
	.pub

; A0=RemapFrame. Resolver receives A0=Context,D0=old ID,D2=namespace;
; returns D0=status,D1=new u16 ID, preserving other registers. It must preserve
; package identities. An identity-returning resolver can collect the lexicon.
; D0/CCR=status; all other registers preserved. Failure may partially rewrite
; the owned capsule: discard it. Spelling and closed plans contain no source IDs.
remapClosed	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	tst.l RemapFrame.Resolve(a6)
	beq.w bad
	move.l RemapFrame.Bytes(a6), d0
	movea.l RemapFrame.Image(a6), a0
	bsr.w validateClosed
	bne.w bad
	movea.l RemapFrame.Image(a6), a5
	movea.l a5, a4
	adda.l ClosedHeader.Definition(a5), a4
	lea Def.Name(a4), a0
	moveq #CLOSED_VALUE, d2
	bsr.w closedMapBe
	bne.w bad
	moveq #0, d7
formal
	cmp.w Def.ParamCount(a4), d7
	bhs.w body
	move.l d7, d0
	add.w d0, d0
	lea Def.Parameter(a4, d0.w), a0
	moveq #CLOSED_VALUE, d2
	bsr.w closedMapBe
	bne.w bad
	move.l d7, d0
	lsl.w #2, d0
	movea.l a5, a0
	adda.l ClosedHeader.Defaults(a5), a0
	movea.l a0, a1
	adda.l Def.DefaultStart0(a4, d0.w), a0
	adda.l Def.DefaultEnd0(a4, d0.w), a1
	bsr.w closedTokens
	bne.w bad
	addq.w #1, d7
	bra.w formal
body
	movea.l a5, a4
	adda.l ClosedHeader.Body(a5), a4
	movea.l a4, a3
	adda.l ClosedHeader.BodyBytes(a5), a3
record
	cmpa.l a3, a4
	beq.w good
	bhi.w bad
	moveq #0, d7
	move.b source.RecordHeader.Length(a4), d7
	addq.l #1, d7
	lea source.RECORD_HEADER_BYTES(a4), a0
	movea.l a4, a1
	adda.l d7, a1
	moveq #0, d0
	move.b source.RecordHeader.Flags(a4), d0
	andi.w #source.FLAG_PLAN+LINE_PLAN_FLAG, d0
	beq.w tokens
	suba.w #source.PLAN_TOKEN_BYTES, a1
tokens
	bsr.w closedTokens
	bne.w bad
	adda.l d7, a4
	bra.w record
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; remapClosed
	.priv

; A0/A1=packed token span,A6=RemapFrame. D0=status,A0 advances; others kept.
closedTokens	.block
	movem.l d1-d7/a1-a5, -(sp)
	movea.l a1, a4
next
	cmpa.l a4, a0
	beq.w good
	bhi.w bad
	moveq #0, d4
	move.b (a0)+, d4
	cmpi.b #1, d4
	bls.w name
	cmpi.b #2, d4
	beq.w number
	cmpi.b #source.TOKEN_WIDE_NUMBER, d4
	beq.w wideNumber
	cmpi.b #3, d4
	beq.w string
	cmpi.b #TOKEN_COMPOSITE, d4
	beq.w composite
	cmpi.b #source.TOKEN_BINARY_BYTES, d4
	beq.w binary
	cmpi.b #closedExpression.COMPILED_TAG, d4
	beq.w compact
	cmpi.b #closedExpression.VALUE_TAG, d4
	beq.w typed
	cmpi.b #source.TOKEN_ITERATOR_BINDING, d4
	beq.w iterator
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, d4
	beq.w schema
	cmpi.b #4, d4
	blo.w bad
	cmpi.b #source.TOKEN_UPDATE_LAST, d4
	bhi.w bad
	bra.w next
name
	moveq #source.NAME_TOKEN_BYTES-source.NameToken.Id, d0
	bsr.w closedAvailable
	bne.w bad
	moveq #CLOSED_VALUE, d2
	cmpi.b #source.FIELD_KEY_QUALIFIER, source.NameToken.Qualifier-source.NameToken.Id(a0)
	bne.w nameReady
	moveq #CLOSED_MEMBER, d2
nameReady
	bsr.w closedMapBe
	bne.w bad
	addq.l #1, a0
	bra.w next
iterator
	moveq #CLOSED_WORD_BYTES, d0
	bsr.w closedAvailable
	bne.w bad
	moveq #CLOSED_VALUE, d2
	bsr.w closedMapBe
	bne.w bad
	bra.w next
number
	moveq #source.NUMBER_BYTES-1, d0
	bra.w skip
wideNumber
	moveq #source.WIDE_NUMBER_BYTES-1, d0
	bra.w skip
schema
	; Closed bodies are captured before struct lowering. A numeric value-seed
	; offset would borrow a different owner, so it cannot cross this boundary.
	bra.w bad
string
composite
	moveq #1, d0
	bsr.w closedAvailable
	bne.w bad
	moveq #0, d0
	move.b (a0)+, d0
	; Composite payloads contain positional/literal recipe bytes, never IDs.
	bra.w skip
binary
	subq.l #1, a0
	movea.l a4, a1
	jsr payloads.binary
	bne.w bad
	move.l d2, d0
	bra.w skip
compact
	moveq #0, d5
	bra.w expression
typed
	moveq #1, d5
expression
	moveq #1, d0
	bsr.w closedAvailable
	bne.w bad
	moveq #0, d0
	move.b (a0)+, d0
	beq.w bad
	bsr.w closedAvailable
	bne.w bad
	movea.l a0, a3
	adda.l d0, a3
opcode
	cmpa.l a3, a0
	bhs.w bad
	moveq #0, d4
	move.b (a0)+, d4
	cmpi.b #closedVm.EXPRVM_OPCODE_END, d4
	beq.w expressionEnd
	cmpi.b #closedVm.SELECT, d4
	beq.w opcode
	cmpi.b #closedVm.EXPRVM_OPCODE_PUSH_CURRENT_ADDR, d4
	beq.w opcode
	cmpi.b #closedVm.EXPRVM_OPCODE_PUSH_SYMBOL, d4
	beq.w symbol
	cmpi.b #closedVm.EXPRVM_OPCODE_APPLY_UNARY, d4
	beq.w expressionByte
	cmpi.b #closedVm.EXPRVM_OPCODE_APPLY_BINARY, d4
	beq.w expressionByte
	tst.l d5
	bne.w typedOpcode
	cmpi.b #closedVm.COMPACT_I8, d4
	beq.w expressionByte
	cmpi.b #closedVm.COMPACT_I16, d4
	beq.w expressionWord
	cmpi.b #closedVm.COMPACT_I32, d4
	beq.w expressionLong
	cmpi.b #closedVm.COMPACT_U32, d4
	beq.w expressionLong
	cmpi.b #closedVm.COMPACT_I64, d4
	beq.w expressionPair
	cmpi.b #closedVm.COMPACT_NEGATE, d4
	beq.w opcode
	cmpi.b #closedVm.COMPACT_ADD, d4
	beq.w opcode
	cmpi.b #closedVm.COMPACT_SUBTRACT, d4
	beq.w opcode
	cmpi.b #closedVm.COMPACT_MULTIPLY, d4
	beq.w opcode
	bra.w bad
typedOpcode
	cmpi.b #closedVm.EXPRVM_OPCODE_PUSH_LITERAL, d4
	beq.w expressionPair
	cmpi.b #closedVm.EXPRVM_OPCODE_PUSH_STRING_LITERAL, d4
	beq.w typedString
	cmpi.b #closedVm.EXPRVM_OPCODE_BUILD_LIST, d4
	beq.w expressionWord
	cmpi.b #closedVm.EXPRVM_OPCODE_BUILD_RANGE, d4
	beq.w expressionByte
	cmpi.b #closedVm.EXPRVM_OPCODE_BUILD_STRUCT, d4
	beq.w struct
	cmpi.b #closedVm.EXPRVM_OPCODE_GET_MEMBER, d4
	beq.w member
	cmpi.b #closedVm.EXPRVM_OPCODE_INDEX_VALUE, d4
	beq.w opcode
	cmpi.b #closedVm.EXPRVM_OPCODE_CALL_BUILTIN, d4
	beq.w expressionThree
	cmpi.b #closedVm.EXPRVM_OPCODE_REQUIRE_SCALAR, d4
	beq.w opcode
	bra.w bad
symbol
	moveq #CLOSED_VALUE, d2
	bra.w expressionIdentity
member
	moveq #CLOSED_MEMBER, d2
expressionIdentity
	moveq #CLOSED_WORD_BYTES, d0
	bsr.w closedExpressionAvailable
	bne.w bad
	bsr.w closedMapLe
	bne.w bad
	bra.w opcode
struct
	moveq #4, d0
	bsr.w closedExpressionAvailable
	bne.w bad
	moveq #CLOSED_VALUE, d2
	bsr.w closedMapLe
	bne.w bad
	moveq #0, d6
	move.b ClosedWordLe.High(a0), d6
	lsl.w #8, d6
	move.b ClosedWordLe.Low(a0), d6
	addq.l #CLOSED_WORD_BYTES, a0
field
	tst.w d6
	beq.w opcode
	moveq #CLOSED_WORD_BYTES, d0
	bsr.w closedExpressionAvailable
	bne.w bad
	moveq #CLOSED_MEMBER, d2
	bsr.w closedMapLe
	bne.w bad
	subq.w #1, d6
	bra.w field
typedString
	moveq #CLOSED_WORD_BYTES, d0
	bsr.w closedExpressionAvailable
	bne.w bad
	moveq #0, d0
	move.b ClosedWordLe.High(a0), d0
	lsl.w #8, d0
	move.b ClosedWordLe.Low(a0), d0
	addq.l #CLOSED_WORD_BYTES, a0
	bra.w expressionSkip
expressionByte
	moveq #1, d0
	bra.w expressionSkip
expressionWord
	moveq #2, d0
	bra.w expressionSkip
expressionThree
	moveq #3, d0
	bra.w expressionSkip
expressionLong
	moveq #4, d0
	bra.w expressionSkip
expressionPair
	moveq #8, d0
expressionSkip
	bsr.w closedExpressionAvailable
	bne.w bad
	adda.l d0, a0
	bra.w opcode
expressionEnd
	cmpa.l a3, a0
	bne.w bad
	bra.w next
skip
	bsr.w closedAvailable
	bne.w bad
	adda.l d0, a0
	bra.w next
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a1-a5
	tst.l d0
	rts
	.bend  ; closedTokens

; D0=needed bytes,A0=cursor,A4=token end. Retain count on success;
; CCR Z=success, NZ=failure. Callers consume the count only after that test.
closedAvailable	.block
	move.l d1, -(sp)
	move.l a4, d1
	sub.l a0, d1
	bcs.w bad
	cmp.l d0, d1
	blo.w bad
	move.l (sp)+, d1
	cmp.l d0, d0
	rts
bad
	move.l (sp)+, d1
	moveq #STATUS_BAD, d0
	tst.l d0
	rts
	.bend  ; closedAvailable

; D0=needed bytes,A0=cursor,A3=compiled expression end.
closedExpressionAvailable	.block
	move.l a4, -(sp)
	movea.l a3, a4
	bsr.w closedAvailable
	movea.l (sp)+, a4
	rts
	.bend  ; closedExpressionAvailable

; A0=BE/LE u16,D2=namespace,A6=RemapFrame. D0=status,A0 advances;
; other registers preserved. No callback receives a borrowed source pointer.
closedMapBe	.block
	movem.l d1/a1, -(sp)
	moveq #0, d0
	move.w (a0), d0
	bsr.w closedIdentity
	bne.w done
	move.w d1, (a0)+
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; closedMapBe
closedMapLe	.block
	movem.l d1/a1, -(sp)
	moveq #0, d0
	move.b ClosedWordLe.High(a0), d0
	lsl.w #8, d0
	move.b ClosedWordLe.Low(a0), d0
	bsr.w closedIdentity
	bne.w done
	move.b d1, (a0)+
	lsr.w #8, d1
	move.b d1, (a0)+
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; closedMapLe
closedIdentity	.block
	movem.l a0-a2, -(sp)
	movea.l RemapFrame.Context(a6), a0
	movea.l RemapFrame.Resolve(a6), a2
	jsr (a2)
	tst.l d0
	bne.w done
	cmpi.l #65535, d1
	bls.w ready
	moveq #STATUS_BAD, d0
	bra.w done
ready
	moveq #STATUS_OK, d0
done
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; closedIdentity
