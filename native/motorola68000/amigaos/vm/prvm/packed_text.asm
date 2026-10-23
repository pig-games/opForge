; Shared text directive envelopes over immutable compact source.
; Encoding and evaluation belong to callers. No name spelling lookup occurs here.
; @opforge-owner: opforge.vm.prvm.packed_text
	.module opforge.vm.prvm.packed_text
	.cpu 68020
	.use opforge.vm.prvm.abi as abi
	.include "telemetry_macros.i"
	.pub
MAX_OPERANDS = 128; bounded by the 256-byte compact record envelope
MAX_DEPTH = 16
HEAD_COUNT = 10
PROGRAM_HEADER = 2
DIRECTIVE_ID = 0
DIRECTIVE_ACTION = 2
DIRECTIVE_BYTES = 3
PROGRAM_TRAILER_BYTES = 2
PROGRAM_BYTES = PROGRAM_HEADER+HEAD_COUNT*DIRECTIVE_BYTES+PROGRAM_TRAILER_BYTES
NAME = 1
STRING = 2
EXPRESSION = 3
SELECT = 1
BEGIN = 2
END = 3
RANGE = 4
TABLE = 5
ESCAPE = 6
TEXT = 7
NULL = 8
PTEXT = 9
Span	.struct
Offset	.word ?
Bytes	.word ?
Kind	.word ?
Reserved	.word ?
	.endstruct
SPAN_BYTES = 8
State	.struct
Budget	.long ?
Prefix	.long ?
Start	.long ?
Count	.long ?
Depth	.long ?
Action	.long ?
Token	.long ?
Stack	.res MAX_DEPTH
Spans	.res MAX_OPERANDS*SPAN_BYTES
	.endstruct
STATE_BYTES = State.Spans+MAX_OPERANDS*SPAN_BYTES
	.section code, kind=code
; A0=request,D0=available frame bytes. Standard PRVM status/count/offset/bytes.
; Validate and stage bounded spans before writing any caller result.
run	.block
	movem.l d4-d7/a3-a6, -(sp)
	move.l a0, d1
	beq.w invalidFrame
	btst #0, d1
	bne.w invalidFrame
	cmpi.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	blo.w invalidFrame
	movea.l a0, a4
	cmpi.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a4)
	blo.w invalidFrame
	cmpi.w #abi.PRVM_ENTRY_KIND_PACKED_TEXT, abi.PRVM_FRAME_ENTRY_KIND(a4)
	bne.w invalidFrame
	tst.w abi.PRVM_FRAME_CALL_MODE(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_FLAGS(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_TOKEN_PTR(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_TOKEN_COUNT(a4)
	bne.w invalidFrame
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), d0
	bmi.w invalidFrame
	move.l abi.PRVM_FRAME_RESULT_CAPACITY(a4), d0
	bmi.w invalidFrame
	move.l abi.PRVM_FRAME_SOURCE_PTR(a4), d0
	beq.w invalidFrame
	movea.l d0, a6
	move.l abi.PRVM_FRAME_SOURCE_LEN(a4), d6
	cmpi.l #abi.PRVM_PACKED_RECORD_BYTES, d6
	blo.w invalidFrame
	cmpi.l #abi.PRVM_PACKED_MAX_RECORD_BYTES, d6
	bhi.w invalidFrame
	moveq #0, d0
	move.b (a6), d0
	addq.l #1, d0
	cmp.l d6, d0
	bne.w invalidFrame
	move.b abi.PackedRecord.Flags(a6), d0
	andi.b #255-abi.PRVM_PACKED_FLAG_ALLOWED, d0
	bne.w invalidFrame
	move.l abi.PRVM_FRAME_PROGRAM_PTR(a4), d0
	beq.w invalidFrame
	movea.l d0, a5
	move.l abi.PRVM_FRAME_PROGRAM_LEN(a4), d1
	bsr.w validateProgram
	bne.w invalidProgram
	suba.w #STATE_BYTES, sp
	movea.l sp, a3
	movea.l a3, a0
	move.w #STATE_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), State.Budget(a3)
	clr.l d2
	bsr.w tick
	bne.w failure
	move.b abi.PackedRecord.Flags(a6), d0
	andi.b #abi.PRVM_PACKED_FLAG_INACTIVE, d0
	bne.w noMatch
	cmpi.l #abi.PRVM_PACKED_RECORD_BYTES, d6
	beq.w noMatch
	moveq #abi.PRVM_PACKED_RECORD_BYTES, d2
	cmpi.l #abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_LABEL_BYTES, d6
	blo.w matchHead
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.PRVM_PACKED_RECORD_BYTES+abi.PackedName.Kind(a6)
	bhi.w matchHead
	cmpi.b #abi.PRVM_TOKEN_KIND_COLON, abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_LABEL_COLON(a6)
	bne.w matchHead
	moveq #abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_LABEL_BYTES, d2
	move.l #abi.PRVM_PACKED_LABEL_BYTES, State.Prefix(a3)
matchHead
	bsr.w tick
	bne.w failure
	move.l d6, d0
	sub.l d2, d0
	cmpi.l #abi.PRVM_PACKED_DOT_BYTES, d0
	blo.w noMatch
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, abi.PackedDot.Kind(a6, d2.l)
	bne.w noMatch
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.PackedDot.Name+abi.PackedName.Kind(a6, d2.l)
	bhi.w noMatch
	tst.b abi.PackedDot.Name+abi.PackedName.Qualifier(a6, d2.l)
	bne.w noMatch
	moveq #0, d0
	move.b abi.PackedDot.Name+abi.PackedName.Id(a6, d2.l), d0
	lsl.w #8, d0
	move.b abi.PackedDot.Name+abi.PackedName.Id+1(a6, d2.l), d0
	moveq #2, d5
findHead
	bsr.w programId
	cmp.w d1, d0
	beq.w foundHead
	addq.l #3, d5
	cmpi.l #32, d5
	blo.w findHead
	bra.w noMatch
foundHead
	moveq #0, d0
	move.b 2(a5, d5.l), d0
	move.l d0, State.Action(a3)
	tst.l State.Prefix(a3)
	beq.w operands
	tst.b abi.PRVM_PACKED_RECORD_BYTES+abi.PackedName.Qualifier(a6)
	beq.w operands
	moveq #abi.PRVM_PACKED_RECORD_BYTES+abi.PackedName.Qualifier, d2
	bra.w tokenFailure
operands
	addq.l #abi.PRVM_PACKED_DOT_BYTES, d2
	move.l d2, State.Start(a3)
	move.l d2, d7  ; initial operand start
scan
	cmp.l d6, d2
	bcc.w scanned
	bsr.w tick
	bne.w failure
	move.l d2, State.Token(a3)
	moveq #0, d0
	move.b 0(a6, d2.l), d0
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, d0
	bls.w nameToken
	cmpi.b #abi.PRVM_TOKEN_KIND_NUMBER, d0
	beq.w numberToken
	cmpi.b #abi.PRVM_TOKEN_KIND_WIDE_NUMBER, d0
	beq.w wideToken
	cmpi.b #abi.PRVM_TOKEN_KIND_STRING, d0
	beq.w payloadToken
	cmpi.b #abi.PRVM_TOKEN_KIND_COMPILED_EXPRESSION, d0
	beq.w compiledToken
	cmpi.b #abi.PRVM_TOKEN_KIND_TYPED_EXPRESSION, d0
	beq.w compiledToken
	cmpi.b #abi.PRVM_TOKEN_KIND_COMMA, d0
	beq.w commaToken
	cmpi.b #abi.PRVM_TOKEN_KIND_OPEN_BRACKET, d0
	beq.w openToken
	cmpi.b #abi.PRVM_TOKEN_KIND_OPEN_BRACE, d0
	beq.w openToken
	cmpi.b #abi.PRVM_TOKEN_KIND_OPEN_PAREN, d0
	beq.w openToken
	cmpi.b #abi.PRVM_TOKEN_KIND_CLOSE_BRACKET, d0
	beq.w closeToken
	cmpi.b #abi.PRVM_TOKEN_KIND_CLOSE_BRACE, d0
	beq.w closeToken
	cmpi.b #abi.PRVM_TOKEN_KIND_CLOSE_PAREN, d0
	beq.w closeToken
	cmpi.b #abi.PRVM_TOKEN_KIND_EQUAL, d0
	beq.w singleToken
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, d0
	beq.w singleToken
	cmpi.b #abi.PRVM_TOKEN_KIND_OPERATOR, d0
	beq.w singleToken
	cmpi.b #abi.PRVM_TOKEN_KIND_OPERATOR_FIRST, d0
	blo.w tokenFailure
	cmpi.b #abi.PRVM_TOKEN_KIND_OPERATOR_LAST, d0
	bhi.w tokenFailure
singleToken
	addq.l #1, d2
	bra.w extent
nameToken
	addq.l #abi.PRVM_PACKED_NAME_BYTES, d2
	bra.w extent
numberToken
	addq.l #abi.PRVM_NUMBER_BYTES, d2
	bra.w extent
wideToken
	addi.l #abi.PRVM_WIDE_NUMBER_BYTES, d2
	bra.w extent
compiledToken
	move.l d2, d1
	addq.l #abi.PRVM_PACKED_STRING_HEADER_BYTES, d1
	cmp.l d6, d1
	bhi.w tokenFailure
	tst.b abi.PackedString.Length(a6, d2.l)
	beq.w tokenFailure
payloadToken
	move.l d2, d1
	addq.l #abi.PRVM_PACKED_STRING_HEADER_BYTES, d1
	cmp.l d6, d1
	bhi.w tokenFailure
	moveq #0, d0
	move.b abi.PackedString.Length(a6, d2.l), d0
	add.l d0, d1
	move.l d1, d2
	bra.w extent
commaToken
	tst.l State.Depth(a3)
	bne.w singleToken
	bsr.w appendSpan
	bne.w failure
	addq.l #1, d2
	move.l d2, State.Start(a3)
	bra.w scan
openToken
	move.l State.Depth(a3), d1
	cmpi.l #MAX_DEPTH, d1
	bcc.w tokenFailure
	addq.b #1, d0
	move.b d0, State.Stack(a3, d1.l)
	addq.l #1, State.Depth(a3)
	bra.w singleToken
closeToken
	tst.l State.Depth(a3)
	beq.w tokenFailure
	subq.l #1, State.Depth(a3)
	move.l State.Depth(a3), d1
	cmp.b State.Stack(a3, d1.l), d0
	bne.w tokenFailure
	bra.w singleToken
extent
	cmp.l d6, d2
	bls.w scan
	move.l State.Token(a3), d2
	bra.w tokenFailure
scanned
	tst.l State.Depth(a3)
	bne.w tokenFailure
	cmp.l State.Start(a3), d2
	bne.w finalSpan
	cmp.l d7, d2
	beq.w countCheck
	bra.w tokenFailure
finalSpan
	bsr.w appendSpan
	bne.w failure
countCheck
	move.l State.Count(a3), d4
	move.l State.Action(a3), d5
	cmpi.l #END, d5
	beq.w zeroCount
	cmpi.l #BEGIN, d5
	beq.w beginCount
	cmpi.l #RANGE, d5
	beq.w rangeCount
	cmpi.l #TABLE, d5
	beq.w multipleCount
	cmpi.l #ESCAPE, d5
	beq.w multipleCount
	cmpi.l #TEXT, d5
	beq.w nonzeroCount
	cmpi.l #1, d4
	bne.w tokenFailure
	bra.w types
zeroCount
	tst.l d4
	bne.w tokenFailure
	bra.w types
beginCount
	cmpi.l #2, d4
	bhi.w tokenFailure
	bra.w nonzeroCount
rangeCount
	cmpi.l #3, d4
	bne.w tokenFailure
	bra.w types
multipleCount
	cmpi.l #2, d4
	blo.w tokenFailure
nonzeroCount
	tst.l d4
	beq.w tokenFailure
types
	clr.l d7
	lea State.Spans(a3), a2
nextType
	cmp.l d4, d7
	bcc.w capacity
	moveq #0, d2
	move.w Span.Offset(a2), d2
	bsr.w tick
	bne.w failure
	moveq #0, d1  ; singleton decoded string flag
	cmpi.b #abi.PRVM_TOKEN_KIND_STRING, 0(a6, d2.l)
	bne.w typed
	moveq #0, d0
	move.b abi.PackedString.Length(a6, d2.l), d0
	addq.w #abi.PRVM_PACKED_STRING_HEADER_BYTES, d0
	cmp.w Span.Bytes(a2), d0
	bne.w typed
	moveq #1, d1
typed
	move.w #EXPRESSION, Span.Kind(a2)
	cmpi.l #BEGIN, d5
	bls.w nameType
	cmpi.l #TEXT, d5
	bcc.w stringType
	cmpi.l #TABLE, d5
	beq.w firstString
	cmpi.l #ESCAPE, d5
	beq.w escapeType
	cmpi.l #RANGE, d5
	bne.w typeDone
	cmpi.l #2, d7
	bcc.w typeDone
	tst.l d1
	beq.w typeDone
	cmpi.w #3, Span.Bytes(a2)
	bne.w tokenFailure
	bra.w stringType
firstString
	tst.l d7
	bne.w typeDone
	bra.w stringType
escapeType
	tst.l d7
	beq.w stringType
	tst.l d1
	beq.w typeDone
stringType
	tst.l d1
	beq.w tokenFailure
	move.w #STRING, Span.Kind(a2)
	bra.w typeDone
nameType
	cmpi.w #abi.PRVM_PACKED_NAME_BYTES, Span.Bytes(a2)
	bne.w tokenFailure
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, 0(a6, d2.l)
	bhi.w tokenFailure
	tst.b abi.PackedName.Qualifier(a6, d2.l)
	bne.w tokenFailure
	move.b abi.PackedName.Id(a6, d2.l), d0
	or.b abi.PackedName.Id+1(a6, d2.l), d0
	beq.w tokenFailure
	move.w #NAME, Span.Kind(a2)
typeDone
	adda.w #SPAN_BYTES, a2
	addq.l #1, d7
	bra.w nextType
capacity
	move.l d6, d2
	bsr.w tick
	bne.w failure
	move.l d4, d3
	addq.l #1, d3
	lsl.l #5, d3
	cmp.l abi.PRVM_FRAME_RESULT_CAPACITY(a4), d3
	bhi.w overflow
	move.l abi.PRVM_FRAME_RESULT_PTR(a4), d0
	beq.w argumentFailure
	btst #0, d0
	bne.w argumentFailure
	bsr.w tick
	bne.w failure
	movea.l abi.PRVM_FRAME_RESULT_PTR(a4), a1
	movea.l a1, a0
	move.l d3, d0
	lsr.l #2, d0
	subq.l #1, d0
clearOutput
	clr.l (a0)+
	dbra d0, clearOutput
	move.w #abi.PRVM_RESULT_PACKED_TEXT_HEADER, (a1)
	move.w d5, abi.PRVM_TEXT_ACTION(a1)
	move.l State.Prefix(a3), abi.PRVM_TEXT_PREFIX_BYTES(a1)
	move.l d4, abi.PRVM_TEXT_OPERAND_COUNT(a1)
	adda.w #32, a1
	lea State.Spans(a3), a2
	clr.l d7
publish
	cmp.l d4, d7
	bcc.w success
	move.w #abi.PRVM_RESULT_PACKED_TEXT_OPERAND, (a1)
	move.w d7, abi.PRVM_TEXT_OPERAND_SLOT(a1)
	move.w Span.Kind(a2), abi.PRVM_TEXT_OPERAND_TYPE(a1)
	moveq #0, d0
	move.w Span.Offset(a2), d0
	move.l d0, abi.PRVM_TEXT_OPERAND_OFFSET(a1)
	move.w Span.Bytes(a2), d0
	move.l d0, abi.PRVM_TEXT_OPERAND_BYTES(a1)
	adda.w #32, a1
	adda.w #SPAN_BYTES, a2
	addq.l #1, d7
	bra.w publish
success
	move.l d4, d1
	addq.l #1, d1
	clr.l d2
	moveq #0, d0
	bra.w finish
noMatch
	clr.l d2
	moveq #0, d0
	bra.w failure
argumentFailure
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	bra.w failure
tokenFailure
	moveq #abi.PRVM_STATUS_INVALID_TOKEN, d0
	bra.w failure
overflow
	moveq #abi.PRVM_STATUS_OUTPUT_OVERFLOW, d0
failure
	clr.l d1
	clr.l d3
finish
	adda.w #STATE_BYTES, sp
	bra.w done
invalidProgram
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	bra.w invalid
invalidFrame
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
invalid
	clr.l d1
	clr.l d2
	clr.l d3
done
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
	.bend  ; run
; A0=compiled descriptor,D0=descriptor bytes,D1=numeric head.
; D0/CCR=status,D1=action (zero for an ordinary head); other registers kept.
; Preparation uses this same ownership query before choosing template recipes.
headAction	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l d1, d7
	move.l d0, d1
	move.l a5, d0
	beq.w invalid
	bsr.w validateProgram
	bne.w failure
	moveq #PROGRAM_HEADER, d5
find
	bsr.w programId
	cmp.l d1, d7
	beq.w found
	addq.l #DIRECTIVE_BYTES, d5
	cmpi.l #PROGRAM_HEADER+HEAD_COUNT*DIRECTIVE_BYTES, d5
	blo.w find
	clr.l d1
	bra.w success
found
	moveq #0, d1
	move.b DIRECTIVE_ACTION(a5, d5.l), d1
success
	moveq #0, d0
	bra.w done
invalid
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
failure
	clr.l d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; headAction

; Tokenizer-boundary contextual name role from already-emitted bounded prefix.
; A0=prefix,D0=bytes,A1=compiled text descriptor,D1=descriptor bytes.
; D0=status,D1=next encoding-name slot (0 ordinary,1 first,2 base).
; Preserves D2-D7/A0-A6. Caller binds decoded raw name once in global namespace.
nameRole	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d6
	move.l a5, d0
	beq.w invalid
	bsr.w validateProgram
	bne.w programFailure
	clr.l d1
	cmpi.l #abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_LABEL_BYTES, d6
	blo.w ordinary
	cmpi.l #abi.PRVM_PACKED_MAX_RECORD_BYTES, d6
	bhi.w ordinary
	move.l a6, d0
	beq.w invalid
	moveq #abi.PRVM_PACKED_RECORD_BYTES, d2
	cmpi.l #abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_NAME_BYTES+abi.PRVM_PACKED_DOT_BYTES, d6
	blo.w head
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.PRVM_PACKED_RECORD_BYTES+abi.PackedName.Kind(a6)
	bhi.w head
	cmpi.b #abi.PRVM_TOKEN_KIND_COLON, abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_LABEL_COLON(a6)
	beq.w colon
	move.b abi.PackedRecord.Flags(a6), d0
	andi.b #abi.PRVM_PACKED_FLAG_INDENT, d0
	bne.w head
	moveq #abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_NAME_BYTES, d2
	bra.w head
colon
	moveq #abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_LABEL_BYTES, d2
head
	move.l d6, d0
	sub.l d2, d0
	cmpi.l #abi.PRVM_PACKED_DOT_BYTES, d0
	blo.w ordinary
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, abi.PackedDot.Kind(a6, d2.l)
	bne.w ordinary
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.PackedDot.Name+abi.PackedName.Kind(a6, d2.l)
	bhi.w ordinary
	tst.b abi.PackedDot.Name+abi.PackedName.Qualifier(a6, d2.l)
	bne.w ordinary
	moveq #0, d0
	move.b abi.PackedDot.Name+abi.PackedName.Id(a6, d2.l), d0
	lsl.w #8, d0
	move.b abi.PackedDot.Name+abi.PackedName.Id+1(a6, d2.l), d0
	moveq #2, d5
find
	bsr.w programId
	cmp.w d1, d0
	beq.w found
	addq.l #3, d5
	cmpi.l #32, d5
	blo.w find
	bra.w ordinary
found
	cmpi.b #BEGIN, 2(a5, d5.l)
	bhi.w ordinary
	addq.l #abi.PRVM_PACKED_DOT_BYTES, d2
	cmp.l d6, d2
	beq.w first
	cmpi.b #BEGIN, 2(a5, d5.l)
	bne.w ordinary
	move.l d6, d0
	sub.l d2, d0
	cmpi.l #abi.PRVM_PACKED_NAME_BYTES+1, d0
	bne.w ordinary
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.PackedName.Kind(a6, d2.l)
	bhi.w ordinary
	tst.b abi.PackedName.Qualifier(a6, d2.l)
	bne.w ordinary
	cmpi.b #abi.PRVM_TOKEN_KIND_COMMA, abi.PRVM_PACKED_NAME_BYTES(a6, d2.l)
	bne.w ordinary
	move.b abi.PackedName.Id(a6, d2.l), d0
	or.b abi.PackedName.Id+1(a6, d2.l), d0
	beq.w ordinary
	moveq #2, d1
	bra.w success
first
	moveq #1, d1
success
	moveq #0, d0
	bra.w done
ordinary
	clr.l d1
	moveq #0, d0
	bra.w done
invalid
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
programFailure
	clr.l d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; nameRole
	.priv
; D1=descriptor bytes,A5=descriptor. Clobbers D0-D2/D4-D5.
validateProgram	.block
	cmpi.l #PROGRAM_BYTES, d1
	bne.w invalid
	cmpi.b #$a5, (a5)
	bne.w invalid
	cmpi.b #HEAD_COUNT, 1(a5)
	bne.w invalid
	cmpi.b #$83, 32(a5)
	bne.w invalid
	tst.b 33(a5)
	bne.w invalid
	moveq #2, d5
	moveq #0, d4
programRow
	bsr.w programId
	tst.w d1
	beq.w invalid
	moveq #2, d2
previousRow
	cmp.l d5, d2
	bcc.w actionCheck
	moveq #0, d0
	move.b 0(a5, d2.l), d0
	lsl.w #8, d0
	move.b 1(a5, d2.l), d0
	cmp.w d1, d0
	beq.w invalid
	addq.l #3, d2
	bra.w previousRow
actionCheck
	move.l d4, d0
	beq.w firstAction
	bra.w compareAction
firstAction
	moveq #1, d0
compareAction
	cmp.b 2(a5, d5.l), d0
	bne.w invalid
	addq.l #1, d4
	addq.l #3, d5
	cmpi.l #32, d5
	blo.w programRow
	moveq #0, d0
	rts
invalid
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	rts
	.bend  ; validateProgram
; Append a nonempty span ending at D2. D2 and D4-D7 remain unchanged.
appendSpan	.block
	move.l State.Start(a3), d0
	cmp.l d2, d0
	beq.w invalid
	move.l State.Count(a3), d1
	cmpi.l #MAX_OPERANDS, d1
	bcc.w invalid
	lsl.l #3, d1
	lea State.Spans(a3, d1.l), a0
	move.w d0, Span.Offset(a0)
	move.l d2, d1
	sub.l d0, d1
	move.w d1, Span.Bytes(a0)
	addq.l #1, State.Count(a3)
	moveq #0, d0
	rts
invalid
	moveq #abi.PRVM_STATUS_INVALID_TOKEN, d0
	rts
	.bend  ; appendSpan
; D5 descriptor offset -> D1 canonical numeric ID, preserves D0.
programId	.block
	moveq #0, d1
	move.b 0(a5, d5.l), d1
	lsl.w #8, d1
	move.b 1(a5, d5.l), d1
	rts
	.bend  ; programId
; Same budget points as the Rust packed_text_vm executor.
tick	.block
	tst.l State.Budget(a3)
	beq.w exhausted
	subq.l #1, State.Budget(a3)
	moveq #0, d0
	rts
exhausted
	moveq #abi.PRVM_STATUS_BUDGET_EXCEEDED, d0
	rts
	.bend  ; tick
	.endsection
	.endmodule
