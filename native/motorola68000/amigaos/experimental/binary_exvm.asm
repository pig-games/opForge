; Current EXVM control over bound numeric tokens. No source text parser.
; @opforge-owner: experimental.amigaos.binary_exvm
	.module experimental.amigaos.binary_exvm
	.cpu 68020
	.use tkvm.amigaos.runtime as tokens
	.include "telemetry_macros.i"
	.pub
STATUS_OK = 0
STATUS_SYNTAX = 1
STATUS_PROGRAM = 2
STATUS_ARENA = 3
STATUS_DEPTH = 4
STATUS_STEPS = 5
STATUS_UNSUPPORTED = 6
; Canonical package::ExvmOpcode wire values.
OP_END = $00
OP_JUMP = $01
OP_JUMP_IF_TRUE = $02
OP_CALL = $03
OP_RETURN = $04
OP_PEEK_KIND = $10
OP_PEEK_OPERATOR = $11
OP_ADVANCE = $20
OP_CONSUME_OPERATOR = $21
OP_CONSUME_KIND = $22
OP_LOAD_TOKEN_TEXT = $32
OP_BUILD_UNARY = $40
OP_BUILD_BINARY = $41
OP_BUILD_TERNARY = $42
OP_BUILD_RANGE = $43
OP_BUILD_IDENTIFIER = $60
OP_BUILD_NUMBER = $61
OP_BUILD_CURRENT_ADDRESS = $62
OP_PARSE_GROUPING = $63
OP_PARSE_LIST = $64
OP_PARSE_STRUCT_LITERAL_IF_PRESENT = $65
OP_PARSE_POSTFIX_CHAIN = $66
OP_BUILD_STRING = $67
OP_BUILD_PLACEHOLDER = $68
OP_BUILD_REGISTER = $69
OP_PARSE_CALL = $6a
OP_EMIT_DIAG = $70
OP_FAIL = $72
MAX_DEPTH = 16
CALL_LIMIT = MAX_DEPTH*32
OUTPUT_LIMIT = 64
; Each canonical subroutine permits the root plus sixteen recursive calls.
; Calls pack target in high16 and return PC in low16.
NONE = $ffffffff
Request	.struct
Tokens	.long ?
TokenBytes	.long ?
Program	.long ?
ProgramBytes	.long ?
Arena	.long ?
ArenaBytes	.long ?
StepBudget	.long ?
Status	.long ?
Consumed	.long ?
Root	.long ?
Used	.long ?
Scratch	.long ?
ScratchBytes	.long ?
	.endstruct
REQUEST_BYTES = Request.ScratchBytes+4
Node	.struct
Kind	.byte ?
Operator	.byte ?
Count	.word ?
First	.long ?
Second	.long ?
Third	.long ?
Next	.long ?
SpanStart	.word ?
SpanEnd	.word ?
	.endstruct
NODE_BYTES = Node.SpanEnd+2
KIND_LITERAL = 1
KIND_SYMBOL = 2
KIND_CURRENT = 3
KIND_UNARY = 4
KIND_BINARY = 5
	.priv
State	.struct
Cursor	.long ?
Pc	.long ?
Steps	.long ?
Used	.long ?
TokenCursor	.long ?
TokenKind	.word ?
TokenBytes	.word ?
Calls	.word ?
Outputs	.word ?
Depth	.word ?
Predicate	.word ?
Spans	.word ?
Reserved	.word ?
	.endstruct
CALLS = State.Reserved+2
OUTPUTS = CALLS+CALL_LIMIT*4
SPANS = OUTPUTS+OUTPUT_LIMIT*4
	.pub
SCRATCH_BYTES = SPANS+OUTPUT_LIMIT*4
	.section code, kind=code
	.pub
; A0=Request, transient pointers to disjoint bounded input/program/
; arena/scratch buffers. Token/program lengths <=65535; arena/scratch even.
; D0/CCR=status; all other registers preserved. Results always initialized;
; failure Root=NONE, Used=0; Consumed is the exact bounded numeric cursor.
; Success returns the consumed prefix; owners validate required delimiters/end.
; Packed DOT7 is a package-wrapper delimiter; bracket indexing is unsupported.
; Success nodes contain arena offsets only. Literal First=u32, symbol First=u16
; ID; unary First=child; binary First/Second=children. Count=child arity.
; Request.Scratch owns SCRATCH_BYTES reusable work; only call frames use stack.
compile	.block
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_EXVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_FRONTEND
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Request.Consumed(a5)
	clr.l Request.Used(a5)
	move.l #NONE, Request.Root(a5)
	movea.l Request.Scratch(a5), a6
	move.l a6, d0
	beq.w invalidScratch
	btst #0, d0
	bne.w invalidScratch
	move.l Request.ScratchBytes(a5), d1
	cmpi.l #SCRATCH_BYTES, d1
	blo.w invalidScratch
	add.l d1, d0
	bcs.w invalidScratch
	movea.l a6, a1
	moveq #State.Reserved/4, d0
clear
	clr.l (a1)+
	dbra d0, clear
	move.l #NONE, State.TokenCursor(a6)
	clr.l Request.Status(a5)
	move.l Request.TokenBytes(a5), d0
	cmpi.l #65535, d0
	bhi.w programBad
	move.l Request.ProgramBytes(a5), d0
	beq.w programBad
	cmpi.l #65535, d0
	bhi.w programBad
	move.l Request.Tokens(a5), d0
	add.l Request.TokenBytes(a5), d0
	bcs.w programBad
	move.l Request.Program(a5), d0
	add.l Request.ProgramBytes(a5), d0
	bcs.w programBad
	move.l Request.Arena(a5), d0
	add.l Request.ArenaBytes(a5), d0
	bcs.w programBad
	move.l Request.Arena(a5), d0
	btst #0, d0
	bne.w programBad
	bsr.w execute
	bne.w finish
	move.l OUTPUTS(a6), Request.Root(a5)
	move.l State.Used(a6), Request.Used(a5)
	moveq #STATUS_OK, d0
	bra.w finish
programBad
	moveq #STATUS_PROGRAM, d0
	bra.w finish
syntaxBad
	moveq #STATUS_SYNTAX, d0
finish
	move.l d0, Request.Status(a5)
	move.l State.Cursor(a6), Request.Consumed(a5)
	bra.w restore
invalidScratch
	moveq #STATUS_ARENA, d0
	move.l d0, Request.Status(a5)
restore
	movem.l (sp)+, d1-d7/a0-a6
	.TELEMETRY_VM_LEAVE
	tst.l d0
	rts
	.bend  ; compile
	.priv
; Execute the same canonical program at PC0, with separate output/call barriers.
; A5=request,A6=scratch. D0=status; other working registers private/clobbered.
execute	.block
	move.l State.Pc(a6), -(sp)
	move.w State.Calls(a6), -(sp)
	move.w State.Outputs(a6), -(sp)
	clr.l State.Pc(a6)
next
	move.l State.Steps(a6), d0
	cmp.l Request.StepBudget(a5), d0
	bhs.w stepsBad
	addq.l #1, State.Steps(a6)
	bsr.w programByte
	bne.w done
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_EXVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_FRONTEND
	cmpi.w #OP_FAIL, d1
	bhi.w programBad
	add.w d1, d1
	lea Dispatch, a0
	move.w 0(a0, d1.w), d0
	jmp 0(a0, d0.w)
end
	move.w State.Calls(a6), d0
	cmp.w 2(sp), d0
	bne.w programBad
	move.w State.Outputs(a6), d0
	sub.w (sp), d0
	cmpi.w #1, d0
	bne.w syntaxBad
	moveq #STATUS_OK, d0
	bra.w done
jump
	bsr.w target
	bne.w done
	move.l d2, State.Pc(a6)
	bra.w next
conditional
	bsr.w target
	bne.w done
	tst.w State.Predicate(a6)
	beq.w next
	move.l d2, State.Pc(a6)
	bra.w next
call
	bsr.w target
	bne.w done
	moveq #0, d3
	move.w State.Calls(a6), d3
	cmpi.w #CALL_LIMIT, d3
	bhs.w depthBad
	move.l d3, d4
	lsl.l #2, d4
	moveq #0, d5
	moveq #0, d0
countRecursion
	cmp.l d4, d0
	bhs.w recursionReady
	move.l CALLS(a6, d0.l), d1
	swap d1
	cmp.w d2, d1
	bne.w recursionNext
	addq.w #1, d5
	cmpi.w #MAX_DEPTH+1, d5
	bhs.w depthBad
recursionNext
	addq.l #4, d0
	bra.w countRecursion
recursionReady
	lsl.l #2, d3
	move.l d2, d1
	swap d1
	move.w State.Pc+2(a6), d1
	move.l d1, CALLS(a6, d3.l)
	addq.w #1, State.Calls(a6)
	move.l d2, State.Pc(a6)
	bra.w next
returning
	moveq #0, d3
	move.w State.Calls(a6), d3
	cmp.w 2(sp), d3
	bls.w programBad
	subq.w #1, State.Calls(a6)
	subq.w #1, d3
	lsl.l #2, d3
	move.l CALLS(a6, d3.l), d1
	andi.l #65535, d1
	move.l d1, State.Pc(a6)
	bra.w next
peekKind
	bsr.w programByte
	bne.w done
	bsr.w kindMatch
	bne.w done
	bra.w next
peekOperator
	bsr.w programByte
	bne.w done
	bsr.w operatorMatch
	bne.w done
	bra.w next
consumeKind
	bsr.w programByte
	bne.w done
	bsr.w kindMatch
	bne.w done
	tst.w State.Predicate(a6)
	beq.w syntaxBad
	bra.w advance
consumeOperator
	bsr.w programByte
	bne.w done
	bsr.w operatorMatch
	bne.w done
	tst.w State.Predicate(a6)
	beq.w syntaxBad
	moveq #0, d0
	move.w State.Spans(a6), d0
	cmpi.w #OUTPUT_LIMIT, d0
	bhs.w depthBad
	lsl.l #2, d0
	lea SPANS(a6), a0
	move.l State.Cursor(a6), 0(a0, d0.l)
	addq.w #1, State.Spans(a6)
advance
	bsr.w token
	bne.w done
	tst.l d2
	beq.w syntaxBad
	add.l d2, State.Cursor(a6)
	bra.w next
loadPayload
	bsr.w token
	bne.w done
	tst.l d2
	beq.w syntaxBad
	bra.w next
identifier
	bsr.w token
	bne.w done
	cmpi.w #1, d1
	bhi.w syntaxBad
	tst.b 3(a0)
	bne.w syntaxBad
	moveq #0, d4
	move.w 1(a0), d4
	moveq #KIND_SYMBOL, d3
	bra.w leaf
number
	bsr.w token
	bne.w done
	cmpi.w #tokens.TK_KIND_NUMBER, d1
	bne.w syntaxBad
	move.l 1(a0), d4
	bra.w literalReady
string
	bsr.w token
	bne.w done
	cmpi.w #tokens.TK_KIND_STRING, d1
	bne.w syntaxBad
	moveq #0, d4
	move.b 1(a0), d4
	beq.w syntaxBad
	cmpi.w #2, d4
	bhi.w syntaxBad
	moveq #0, d4
	move.b 2(a0), d4
	cmpi.w #4, d2
	bne.w literalReady
	lsl.w #8, d4
	move.b 3(a0), d4
	bra.w literalReady
literalReady
	moveq #KIND_LITERAL, d3
	bra.w leaf
current
	bsr.w token
	bne.w done
	cmpi.w #tokens.TK_KIND_DOLLAR, d1
	bne.w syntaxBad
	moveq #0, d4
	moveq #KIND_CURRENT, d3
leaf
	bsr.w allocate
	bne.w done
	move.b d3, Node.Kind(a1)
	move.l d4, Node.First(a1)
	move.l State.Cursor(a6), d4
	move.w d4, Node.SpanStart(a1)
	add.l d2, d4
	move.w d4, Node.SpanEnd(a1)
	bsr.w push
	bne.w done
	bra.w next
unary
	moveq #1, d5
	moveq #KIND_UNARY, d3
	bra.w operation
binary
	moveq #2, d5
	moveq #KIND_BINARY, d3
operation
	bsr.w programByte
	bne.w done
	cmpi.w #1, d1
	blo.w programBad
	cmpi.w #24, d1
	bhi.w programBad
	cmpi.w #1, d5
	bne.w binaryOperator
	cmpi.w #2, d1
	bls.w operatorReady
	cmpi.w #7, d1
	blo.w programBad
	cmpi.w #10, d1
	bhi.w programBad
	bra.w operatorReady
binaryOperator
	cmpi.w #23, d1
	bhs.w programBad
	cmpi.w #7, d1
	beq.w programBad
	cmpi.w #8, d1
	beq.w programBad
operatorReady
	move.w d1, d4
	tst.w State.Spans(a6)
	beq.w programBad
	moveq #0, d2
	move.w State.Outputs(a6), d2
	sub.w (sp), d2
	cmp.w d5, d2
	blo.w syntaxBad
	bsr.w allocate
	bne.w done
	move.b d3, Node.Kind(a1)
	move.b d4, Node.Operator(a1)
	move.w d5, Node.Count(a1)
	move.l d7, d6
	bsr.w pop
	move.l d1, Node.First(a1)
	cmpi.w #2, d5
	bne.w childReady
	move.l d1, Node.Second(a1)
	bsr.w pop
	move.l d1, Node.First(a1)
childReady
	subq.w #1, State.Spans(a6)
	moveq #0, d0
	move.w State.Spans(a6), d0
	lsl.l #2, d0
	lea SPANS(a6), a0
	move.l 0(a0, d0.l), d1
	move.w d1, Node.SpanStart(a1)
	addq.w #1, d1
	move.w d1, Node.SpanEnd(a1)
	move.l d6, d7
	bsr.w push
	bne.w done
	bra.w next
grouping
	bsr.w token
	bne.w done
	cmpi.w #tokens.TK_KIND_OPEN_PAREN, d1
	bne.w syntaxBad
	cmpi.w #MAX_DEPTH, State.Depth(a6)
	bhs.w depthBad
	addq.w #1, State.Depth(a6)
	add.l d2, State.Cursor(a6)
	bsr.w execute
	subq.w #1, State.Depth(a6)
	tst.l d0
	bne.w done
	bsr.w token
	bne.w done
	cmpi.w #tokens.TK_KIND_CLOSE_PAREN, d1
	bne.w syntaxBad
	add.l d2, State.Cursor(a6)
	bra.w next
structCheck
	bsr.w token
	bne.w done
	cmpi.w #tokens.TK_KIND_OPEN_BRACE, d1
	beq.w unsupported
	bra.w next
postfixCheck
	bsr.w token
	bne.w done
	cmpi.w #tokens.TK_KIND_OPEN_BRACKET, d1
	beq.w unsupported
	bra.w next
unsupported
	moveq #STATUS_UNSUPPORTED, d0
	bra.w done
stepsBad
	moveq #STATUS_STEPS, d0
	bra.w done
depthBad
	moveq #STATUS_DEPTH, d0
	bra.w done
programBad
	moveq #STATUS_PROGRAM, d0
	bra.w done
syntaxBad
	moveq #STATUS_SYNTAX, d0
done
	addq.l #4, sp
	move.l (sp)+, State.Pc(a6)
	tst.l d0
	rts
; Relative handler offsets for the current canonical opcodes. Holes fail;
; the table contains no executable pointers or version selection.
	.align 2
Dispatch
	.word end-Dispatch, jump-Dispatch, conditional-Dispatch, call-Dispatch, returning-Dispatch
	.for OP_PEEK_KIND-OP_RETURN-1
	.word programBad-Dispatch
	.endfor
	.word peekKind-Dispatch, peekOperator-Dispatch
	.for OP_ADVANCE-OP_PEEK_OPERATOR-1
	.word programBad-Dispatch
	.endfor
	.word advance-Dispatch, consumeOperator-Dispatch, consumeKind-Dispatch
	.for OP_LOAD_TOKEN_TEXT-OP_CONSUME_KIND-1
	.word programBad-Dispatch
	.endfor
	.word loadPayload-Dispatch
	.for OP_BUILD_UNARY-OP_LOAD_TOKEN_TEXT-1
	.word programBad-Dispatch
	.endfor
	.word unary-Dispatch, binary-Dispatch, unsupported-Dispatch, unsupported-Dispatch
	.for OP_BUILD_IDENTIFIER-OP_BUILD_RANGE-1
	.word programBad-Dispatch
	.endfor
	.word identifier-Dispatch, number-Dispatch, current-Dispatch, grouping-Dispatch
	.word unsupported-Dispatch, structCheck-Dispatch, postfixCheck-Dispatch
	.word string-Dispatch, unsupported-Dispatch, identifier-Dispatch, unsupported-Dispatch
	.for OP_EMIT_DIAG-OP_PARSE_CALL-1
	.word programBad-Dispatch
	.endfor
	.word syntaxBad-Dispatch, programBad-Dispatch, syntaxBad-Dispatch
	.bend  ; execute
; Fetch one canonical program byte; D1=byte, D0/CCR=status.
programByte	.block
	move.l State.Pc(a6), d0
	cmp.l Request.ProgramBytes(a5), d0
	bhs.w bad
	movea.l Request.Program(a5), a0
	moveq #0, d1
	move.b 0(a0, d0.l), d1
	addq.l #1, State.Pc(a6)
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_PROGRAM, d0
	rts
	.bend  ; programByte

; Read and validate LE u16 control target. D2=offset; D0/CCR=status.
target	.block
	bsr.w programByte
	bne.w done
	move.l d1, d2
	bsr.w programByte
	bne.w done
	lsl.w #8, d1
	or.w d1, d2
	cmp.l Request.ProgramBytes(a5), d2
	bhs.w bad
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_PROGRAM, d0
done
	rts
	.bend  ; target

; Validate current numeric token without advancing. A0=token,D1=kind (-1 end),
; D2=bytes (0 end),D0/CCR=status. Preserve D3-D7/A1-A6.
token	.block
	move.l State.Cursor(a6), d0
	movea.l Request.Tokens(a5), a0
	adda.l d0, a0
	; The packed input is immutable during compilation. Reuse the validated
	; kind/width while grammar predicates revisit the same numeric cursor.
	cmp.l State.TokenCursor(a6), d0
	bne.w decode
	move.w State.TokenKind(a6), d1
	ext.l d1
	moveq #0, d2
	move.w State.TokenBytes(a6), d2
	moveq #STATUS_OK, d0
	rts
decode
	move.l Request.TokenBytes(a5), d2
	sub.l d0, d2
	bcs.w bad
	beq.w end
	moveq #0, d1
	move.b (a0), d1
	cmpi.w #40, d1
	bhi.w bad
	cmpi.w #1, d1
	bls.w name
	cmpi.w #tokens.TK_KIND_NUMBER, d1
	beq.w number
	cmpi.w #tokens.TK_KIND_STRING, d1
	beq.w string
	moveq #1, d0
	bra.w bounded
name
	moveq #4, d0
	bra.w bounded
number
	moveq #5, d0
	bra.w bounded
string
	cmpi.l #2, d2
	blo.w bad
	moveq #0, d0
	move.b 1(a0), d0
	addq.l #2, d0
bounded
	cmp.l d2, d0
	bhi.w bad
	move.l d0, d2
	bra.w cached
end
	moveq #-1, d1
cached
	move.l State.Cursor(a6), State.TokenCursor(a6)
	move.w d1, State.TokenKind(a6)
	move.w d2, State.TokenBytes(a6)
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_SYNTAX, d0
	rts
	.bend  ; token

; D1=canonical EXVM token kind. Map numeric categories, not grammar.
; D0/CCR=status; State.Predicate=match. Preserve D3-D7/A1-A6.
kindMatch	.block
	move.w d1, -(sp)
	cmpi.w #1, d1
	blo.w bad
	cmpi.w #11, d1
	bhi.w bad
	bsr.w token
	bne.w done
	clr.w State.Predicate(a6)
	cmpi.w #1, (sp)
	bne.w ordinary
	cmpi.w #tokens.TK_KIND_NUMBER, d1
	beq.w matched
	bra.w good
ordinary
	cmpi.w #2, (sp)
	bne.w table
	cmpi.w #1, d1
	bls.w matched
	bra.w good
table
	moveq #0, d0
	move.w (sp), d0
	lea KindMap, a0
	moveq #0, d2
	move.b 0(a0, d0.w), d2
	cmp.w d2, d1
	bne.w good
matched
	move.w #1, State.Predicate(a6)
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_PROGRAM, d0
done
	addq.l #2, sp
	tst.l d0
	rts
	.bend  ; kindMatch

; D1=canonical EXVM operator; D0/CCR=status, Predicate=match.
operatorMatch	.block
	move.w d1, -(sp)
	cmpi.w #1, d1
	blo.w bad
	cmpi.w #24, d1
	bhi.w bad
	bsr.w token
	bne.w done
	clr.w State.Predicate(a6)
	moveq #0, d0
	move.w (sp), d0
	lea OperatorMap, a0
	moveq #0, d2
	move.b 0(a0, d0.w), d2
	cmp.w d2, d1
	bne.w good
	move.w #1, State.Predicate(a6)
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_PROGRAM, d0
done
	addq.l #2, sp
	tst.l d0
	rts
	.bend  ; operatorMatch

; Allocate one zeroed node. A1=node,D7=arena offset,D0/CCR=status.
; Preserve D1-D6/A0/A2-A6. Failed allocation does not advance Used.
allocate	.block
	move.l State.Used(a6), d7
	move.l Request.ArenaBytes(a5), d0
	sub.l d7, d0
	bcs.w bad
	cmpi.l #NODE_BYTES, d0
	blo.w bad
	movea.l Request.Arena(a5), a1
	adda.l d7, a1
	clr.l (a1)
	clr.l 4(a1)
	clr.l 8(a1)
	clr.l 12(a1)
	move.l #NONE, Node.Next(a1)
	clr.l 20(a1)
	addi.l #NODE_BYTES, State.Used(a6)
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_ARENA, d0
	rts
	.bend  ; allocate

; D7=offset. Push a node offset; D0/CCR=status, D1/A0 scratch.
push	.block
	moveq #0, d1
	move.w State.Outputs(a6), d1
	cmpi.w #OUTPUT_LIMIT, d1
	bhs.w bad
	lsl.l #2, d1
	lea OUTPUTS(a6), a0
	move.l d7, 0(a0, d1.l)
	addq.w #1, State.Outputs(a6)
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_DEPTH, d0
	rts
	.bend  ; push

; Checked by operation's output barrier. D1=node offset; D0/A0 scratch.
pop	.block
	subq.w #1, State.Outputs(a6)
	moveq #0, d0
	move.w State.Outputs(a6), d0
	lsl.l #2, d0
	lea OUTPUTS(a6), a0
	move.l 0(a0, d0.l), d1
	rts
	.bend  ; pop
	.endsection
	.section data, kind=data
; Indexed by canonical package::ExvmTokenKind/ExvmOperatorKind.
; These are representation adapters only; EXVM bytecode owns precedence.
KindMap
	.byte 255, tokens.TK_KIND_NUMBER, tokens.TK_KIND_IDENTIFIER, tokens.TK_KIND_DOLLAR
	.byte tokens.TK_KIND_OPEN_PAREN, tokens.TK_KIND_CLOSE_PAREN
	.byte tokens.TK_KIND_QUESTION, tokens.TK_KIND_COLON, tokens.TK_KIND_OPEN_BRACE
	.byte tokens.TK_KIND_STRING, 255, tokens.TK_KIND_DOT
OperatorMap
	.byte 255
	.byte tokens.TK_KIND_OP_PLUS, tokens.TK_KIND_OP_MINUS, tokens.TK_KIND_OP_MULTIPLY
	.byte tokens.TK_KIND_OP_DIVIDE, tokens.TK_KIND_OP_MOD, tokens.TK_KIND_OP_POWER
	.byte tokens.TK_KIND_OP_BIT_NOT, tokens.TK_KIND_OP_LOGIC_NOT, tokens.TK_KIND_OP_LT
	.byte tokens.TK_KIND_OP_GT, tokens.TK_KIND_OP_SHL, tokens.TK_KIND_OP_SHR
	.byte tokens.TK_KIND_OP_EQ, tokens.TK_KIND_OP_NE, tokens.TK_KIND_OP_GE
	.byte tokens.TK_KIND_OP_LE, tokens.TK_KIND_OP_BIT_AND, tokens.TK_KIND_OP_BIT_OR
	.byte tokens.TK_KIND_OP_BIT_XOR, tokens.TK_KIND_OP_LOGIC_AND, tokens.TK_KIND_OP_LOGIC_OR
	.byte tokens.TK_KIND_OP_LOGIC_XOR, tokens.TK_KIND_OP_RANGE, tokens.TK_KIND_OP_RANGE_INCLUSIVE
	.endsection
	.endmodule
