; Numeric instruction selection for experimental binary source records.
; @opforge-owner: experimental.amigaos.binary_encoding

	.module experimental.amigaos.binary_encoding
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_shapes as shapes
	.use experimental.amigaos.binary_mask_unary as mask_unary
	.use opasm.amigaos.binary_expression as expression
	.use tkpkg.amigaos.encoding_execution as encoding
	.use tkpkg.amigaos.value_execution as value

TOKEN_SYMBOL_0 = 0
TOKEN_SYMBOL_1 = 1
TOKEN_COMMA = 4
TOKEN_DOT = 7
TOKEN_HASH = 8
TOKEN_OPEN_PAREN = 14
TOKEN_CLOSE_PAREN = 15
TOKEN_PLUS = 18
TOKEN_MINUS = 19
TOKEN_DIVIDE = 22

SHAPE_EMPTY = 0
SHAPE_SINGLE = 1
SHAPE_PREFIXED = 2
SHAPE_PREFIXED_PAIR = 3
SHAPE_PAIR = 4
SHAPE_REGISTER_PAIR = 5
SHAPE_VALUE_REGISTER = 6
SHAPE_STRUCTURED_PAIR = 7
SHAPE_PREFIXED_DIRECT = 8
SHAPE_REGISTER = 9
SHAPE_DIRECT_PAIR = 10

RECIPE_NONE = 0
RECIPE_U8 = 1
RECIPE_U16 = 2
RECIPE_REL8 = 3
RECIPE_SEMANTIC_INPUTS = 4
RECIPE_SEMANTIC_BRANCH = 5
RECIPE_UNSUPPORTED = 6
RECIPE_SEMANTIC_TABLE = 7
RECIPE_PACKED_MASK_UNARY = 8

PROGRAM_TABLE = 1
PROGRAM_SEMANTIC = 2
PROGRAM_VALUE = 3
MISSING_PROGRAM = $ffff
HEADER_BYTES = 76
ROW_BYTES = 32
PROJECTION_BYTES = 12
PROGRAM_BYTES = 12

	.section bss, kind=bss
	.priv
OperandStart	.res long, 2
OperandEnd	.res long, 2
OperandCount	.res word, 1
OperandShape	.res word, 1
MemberMask	.res word, 1
Unresolved	.res word, 1
Records	.res byte, 24
Execution	.res byte, encoding.Context.FixupTargets+4
Output	.res byte, 4096
	.endsection

	.section code, kind=code
	.pub

; A0=operand token cursor, A1=bounded end, A2=package.Context,
; D0.W=base mnemonic id, D1.B=0 or qualifier-index+1.
; Returns D0=0 on success, D1=byte count, A1=output. A0 may change;
; D2-D7/A2-A6 are preserved. No source spelling is consulted.
encode	.block
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_SELECTION
	movem.l d2-d7/a2-a6, -(sp)
	move.w d0, d6
	moveq #0, d7
	move.b d1, d7
	bsr.w splitOperands
	tst.l d0
	bne.w fail
	clr.w MemberMask
	tst.w OperandCount
	beq.w shapesReady
	movea.l OperandStart, a0
	movea.l OperandEnd, a1
	jsr shapes.isMember
	move.w d0, MemberMask
	cmpi.w #2, OperandCount
	bne.w shapesReady
	movea.l OperandStart+4, a0
	movea.l OperandEnd+4, a1
	jsr shapes.isMember
	add.w d0, d0
	or.w d0, MemberMask
shapesReady
	movea.l package.Context.Package(a2), a5
	move.l a5, d0
	beq.w fail
	move.l package.Header.Bytes(a5), d0
	cmpi.l #HEADER_BYTES, d0
	blo.w fail
	move.l package.Header.Rows(a5), d2
	move.l package.Header.RowCount(a5), d3
	cmpi.l #$ffff, d3
	bhi.w fail
	move.l d3, d0
	mulu.w #ROW_BYTES, d0
	add.l d2, d0
	bcs.w fail
	cmp.l package.Header.Bytes(a5), d0
	bhi.w fail
	adda.l d2, a5
rowLoop
	tst.l d3
	beq.w fail
	cmp.w package.Row.Name(a5), d6
	bne.w nextRow
	cmp.b package.Row.Qualifier(a5), d7
	bne.w nextRow
	move.w OperandShape, d0
	cmp.b package.Row.Shape(a5), d0
	bne.w nextRow
	moveq #0, d0
	move.b package.Row.MemberExcluded(a5), d0
	and.w MemberMask, d0
	bne.w nextRow  ; a necessary package match predicate is conclusively false
	bsr.w excludedName
	cmpi.l #2, d0
	beq.w fail
	tst.l d0
	bne.w nextRow
	bsr.w requiredForms
	cmpi.l #2, d0
	beq.w fail
	tst.l d0
	bne.w nextRow
	cmpi.b #RECIPE_UNSUPPORTED, package.Row.Recipe(a5)
	beq.w fail
	movem.l d3/d6-d7/a2/a5, -(sp)
	bsr.w tryRow
	movem.l (sp)+, d3/d6-d7/a2/a5
	tst.l d0
	beq.w success
nextRow
	adda.w #ROW_BYTES, a5
	subq.l #1, d3
	bra.w rowLoop
success
	movem.l (sp)+, d2-d7/a2-a6
	.TELEMETRY_SERVICE_LEAVE
	tst.l d0
	rts
fail
	moveq #1, d0
	moveq #0, d1
	suba.l a1, a1
	movem.l (sp)+, d2-d7/a2-a6
	.TELEMETRY_SERVICE_LEAVE
	tst.l d0
	rts
	.bend  ; encode

	.priv

; Record at most two top-level operands and derive their numeric shape.
splitOperands	.block
	move.l a0, OperandStart
	move.l a1, OperandEnd
	clr.w OperandCount
	clr.w OperandShape
	cmpa.l a1, a0
	beq.w emptyOperands
	moveq #0, d2
	movea.l a0, a3
scan
	cmpa.l a1, a3
	bhs.w one
	moveq #0, d0
	move.b (a3), d0
	cmpi.b #TOKEN_OPEN_PAREN, d0
	bne.w close
	addq.w #1, d2
	bra.w step
close
	cmpi.b #TOKEN_CLOSE_PAREN, d0
	bne.w comma
	tst.w d2
	beq.w malformed
	subq.w #1, d2
	bra.w step
comma
	cmpi.b #TOKEN_COMMA, d0
	bne.w step
	tst.w d2
	bne.w step
	cmpa.l a0, a3
	beq.w malformed
	move.l a3, OperandEnd
	addq.l #1, a3
	cmpa.l a1, a3
	bhs.w malformed
	move.l a3, OperandStart+4
	move.l a1, OperandEnd+4
	move.w #2, OperandCount
	cmpi.b #TOKEN_HASH, (a0)
	beq.w prefixedPair
	move.w #SHAPE_PAIR, OperandShape
	movea.l OperandStart+4, a0
	movea.l OperandEnd+4, a1
	bsr.w knownRegister
	cmpi.l #2, d0
	beq.w malformed
	tst.l d0
	bne.w secondDirect
	move.w #SHAPE_VALUE_REGISTER, OperandShape
	movea.l OperandStart, a0
	movea.l OperandEnd, a1
	bsr.w knownRegister
	cmpi.l #2, d0
	beq.w malformed
	tst.l d0
	bne.w pairReady
	move.w #SHAPE_REGISTER_PAIR, OperandShape
	bra.w pairReady
secondDirect
	movea.l OperandStart, a0
	movea.l OperandEnd, a1
	bsr.w knownRegister
	cmpi.l #2, d0
	beq.w malformed
	tst.l d0
	beq.w pairReady
	move.w #SHAPE_DIRECT_PAIR, OperandShape
pairReady
	; Recognize either operand order for the package-owned mask/indirect fragment.
	movea.l OperandStart+4, a3
	movea.l OperandEnd+4, a4
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #7, d0
	bne.w firstIndirect
	cmpi.b #TOKEN_MINUS, (a3)
	bne.w firstIndirect
	cmpi.b #TOKEN_OPEN_PAREN, 1(a3)
	bne.w firstIndirect
	cmpi.b #TOKEN_CLOSE_PAREN, 6(a3)
	bne.w firstIndirect
	movea.l OperandStart, a0
	movea.l OperandEnd, a1
	bra.w maskList
firstIndirect
	movea.l OperandStart, a3
	movea.l OperandEnd, a4
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #7, d0
	bne.w pairComplete
	cmpi.b #TOKEN_OPEN_PAREN, (a3)
	bne.w pairComplete
	cmpi.b #TOKEN_CLOSE_PAREN, 5(a3)
	bne.w pairComplete
	cmpi.b #TOKEN_PLUS, 6(a3)
	bne.w pairComplete
	movea.l OperandStart+4, a0
	movea.l OperandEnd+4, a1
maskList
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w pairComplete
	cmpi.b #1, (a0)
	bhi.w pairComplete
	cmpi.l #4, d0
	beq.w structuredPair
	cmpi.l #9, d0
	blo.w pairComplete
	cmpi.b #TOKEN_MINUS, 4(a0)
	beq.w structuredPair
	cmpi.b #TOKEN_DIVIDE, 4(a0)
	bne.w pairComplete
structuredPair
	move.w #SHAPE_STRUCTURED_PAIR, OperandShape
pairComplete
	moveq #0, d0
	rts
step
	bsr.w skipToken
	tst.l d0
	bne.w malformed
	bra.w scan
one
	tst.w d2
	bne.w malformed
	move.w #1, OperandCount
	cmpi.b #TOKEN_HASH, (a0)
	bne.w singleOperand
	move.w #SHAPE_PREFIXED, OperandShape
	moveq #0, d0
	rts
singleOperand
	bsr.w knownRegister
	cmpi.l #2, d0
	beq.w malformed
	tst.l d0
	bne.w directOperand
	move.w #SHAPE_REGISTER, OperandShape
	moveq #0, d0
	rts
directOperand
	move.w #SHAPE_SINGLE, OperandShape
	moveq #0, d0
	rts
prefixedPair
	movea.l OperandStart+4, a0
	movea.l OperandEnd+4, a1
	bsr.w knownRegister
	cmpi.l #2, d0
	beq.w malformed
	tst.l d0
	beq.w prefixedRegister
	move.w #SHAPE_PREFIXED_DIRECT, OperandShape
	moveq #0, d0
	rts
prefixedRegister
	move.w #SHAPE_PREFIXED_PAIR, OperandShape
	moveq #0, d0
	rts
emptyOperands
	moveq #0, d0
	rts
malformed
	moveq #1, d0
	rts
	.bend  ; splitOperands

; D0=0 for an active package register, 1 for another operand, 2 for bad metadata.
; Preserves all other registers; CCR reflects D0.
knownRegister	.block
	movem.l d1-d4/a0-a1/a6, -(sp)
	bsr.w exactName
	tst.l d0
	bne.w done
	movea.l package.Context.Package(a2), a6
	move.l package.Header.RegisterRows(a6), d0
	move.l package.Header.RegisterCount(a6), d2
	cmpi.l #$ffff, d2
	bhi.w malformed
	move.l d2, d4
	mulu.w #6, d4
	add.l d0, d4
	bcs.w malformed
	cmp.l package.Header.Bytes(a6), d4
	bhi.w malformed
	adda.l d0, a6
loop
	tst.l d2
	beq.w no
	cmp.w (a6), d1
	beq.w yes
	addq.l #6, a6
	subq.l #1, d2
	bra.w loop
yes
	moveq #0, d0
	bra.w done
no
	moveq #1, d0
	bra.w done
malformed
	moveq #2, d0
done
	movem.l (sp)+, d1-d4/a0-a1/a6
	tst.l d0
	rts
	.bend  ; knownRegister

; D0=1 only when an exact package name disproves this row's match predicate;
; 0 is unknown/no exclusion, 2 is malformed metadata. Other registers preserved.
excludedName	.block
	movem.l d1-d4/a0-a1/a3-a4/a6, -(sp)
	move.l package.Row.Exclusions(a5), d0
	beq.w no
	movea.l package.Context.Package(a2), a6
	move.l d0, d2
	addq.l #2, d2
	bcs.w malformed
	cmp.l package.Header.Bytes(a6), d2
	bhi.w malformed
	movea.l a6, a3
	adda.l d0, a3
	moveq #0, d3
	move.w (a3)+, d3
	move.l d3, d4
	lsl.l #2, d4
	add.l d2, d4
	bcs.w malformed
	cmp.l package.Header.Bytes(a6), d4
	bhi.w malformed
	moveq #0, d4
loop
	tst.l d3
	beq.w complete
	moveq #0, d0
	move.w (a3)+, d0
	cmpi.w #2, d0
	bhs.w malformed
	move.w (a3)+, d2
	cmp.w package.Header.NameCount(a6), d2
	bhs.w malformed
	cmp.w OperandCount, d0
	bhs.w next
	lsl.w #2, d0
	lea OperandStart, a4
	movea.l 0(a4, d0.w), a0
	lea OperandEnd, a4
	movea.l 0(a4, d0.w), a1
	bsr.w exactName
	tst.l d0
	bne.w next
	cmp.w d1, d2
	bne.w next
	moveq #1, d4
next
	subq.l #1, d3
	bra.w loop
complete
	move.l d4, d0
	bra.w done
no
	moveq #0, d0
	bra.w done
malformed
	moveq #2, d0
done
	movem.l (sp)+, d1-d4/a0-a1/a3-a4/a6
	tst.l d0
	rts
	.bend  ; excludedName

; Prove that an unsupported sequence cannot match its required packed wrapper.
; Zero permits normal handling, one skips the row, two rejects bad metadata.
requiredForms	.block
	movem.l d1-d4/a0-a1/a6, -(sp)
	moveq #0, d2
	move.b package.Row.RequiredForms(a5), d2
	moveq #0, d3
next
	move.l d2, d4
	andi.l #15, d4
	cmpi.l #4, d4
	bhi.w malformed
	tst.l d4
	beq.w advance
	cmp.w OperandCount, d3
	bhs.w mismatch
	move.l d3, d1
	lsl.l #2, d1
	lea OperandStart, a0
	movea.l 0(a0, d1.l), a0
	lea OperandEnd, a1
	movea.l 0(a1, d1.l), a1
	cmpi.l #4, d4
	beq.w tuple
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #1, d4
	beq.w plain
	cmpi.l #7, d0
	bne.w mismatch
	cmpi.l #2, d4
	beq.w postincrement
	cmpi.b #TOKEN_MINUS, (a0)
	bne.w mismatch
	cmpi.b #TOKEN_OPEN_PAREN, 1(a0)
	bne.w mismatch
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a1)
	bne.w mismatch
	bra.w advance
postincrement
	cmpi.b #TOKEN_OPEN_PAREN, (a0)
	bne.w mismatch
	cmpi.b #TOKEN_CLOSE_PAREN, -2(a1)
	bne.w mismatch
	cmpi.b #TOKEN_PLUS, -1(a1)
	bne.w mismatch
	bra.w advance
plain
	cmpi.l #6, d0
	bne.w mismatch
	cmpi.b #TOKEN_OPEN_PAREN, (a0)
	bne.w mismatch
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a1)
	bne.w mismatch
	bra.w advance
tuple
	; Tuple arity belongs to the full candidate match. A compiled prefix
	; is only a necessary condition and also admits indexed 3-item tuples.
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #9, d0
	blo.w mismatch
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w mismatch
advance
	lsr.l #4, d2
	addq.l #1, d3
	cmpi.l #2, d3
	blo.w next
	moveq #0, d0
	bra.w done
mismatch
	moveq #1, d0
	bra.w done
malformed
	moveq #2, d0
done
	movem.l (sp)+, d1-d4/a0-a1/a6
	tst.l d0
	rts
	.bend  ; requiredForms

; Advance A3 over one bounded binary token.
skipToken	.block
	cmpa.l a1, a3
	bhs.w bad
	moveq #0, d0
	move.b (a3)+, d0
	cmpi.b #expression.COMPILED_TAG, d0
	beq.w compiled
	cmpi.b #3, d0
	beq.w string
	cmpi.b #2, d0
	bhi.w ok
	moveq #3, d1
	cmpi.b #2, d0
	blo.w sized
	moveq #4, d1
	bra.w sized
compiled
	cmpa.l a1, a3
	bhs.w bad
	moveq #0, d1
	move.b (a3)+, d1
	tst.l d1
	beq.w bad
	bra.w sized
string
	cmpa.l a1, a3
	bhs.w bad
	moveq #0, d1
	move.b (a3)+, d1
sized
	move.l a1, d0
	sub.l a3, d0
	cmp.l d1, d0
	blo.w bad
	adda.l d1, a3
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; skipToken

tryRow	.block
	clr.w Unresolved
	moveq #0, d0
	move.b package.Row.Recipe(a5), d0
	cmpi.b #RECIPE_NONE, d0
	beq.w tableNone
	cmpi.b #RECIPE_U8, d0
	beq.w tableU8
	cmpi.b #RECIPE_U16, d0
	beq.w tableU16
	cmpi.b #RECIPE_REL8, d0
	beq.w tableRel8
	cmpi.b #RECIPE_SEMANTIC_INPUTS, d0
	beq.w semantic
	cmpi.b #RECIPE_SEMANTIC_BRANCH, d0
	beq.w semantic
	cmpi.b #RECIPE_SEMANTIC_TABLE, d0
	beq.w semantic
	cmpi.b #RECIPE_PACKED_MASK_UNARY, d0
	beq.w packedMaskUnary
	bra.w bad
packedMaskUnary
	movea.l package.Context.Package(a2), a4
	move.l package.Row.Inputs(a5), d0
	move.l d0, d2
	addi.l #16, d2
	bcs.w bad
	cmp.l package.Header.Bytes(a4), d2
	bhi.w bad
	adda.l d0, a4
	movem.l a2/a5-a6, -(sp)
	movea.l package.Context.Package(a2), a6
	tst.b 2(a4)
	beq.w maskFirst
	cmpi.b #1, 2(a4)
	bne.w packedBad
	tst.b 3(a4)
	bne.w packedBad
	movea.l OperandStart+4, a0
	movea.l OperandEnd+4, a1
	movea.l OperandStart, a2
	movea.l OperandEnd, a3
	bra.w packedEncode
maskFirst
	cmpi.b #1, 3(a4)
	bne.w packedBad
	movea.l OperandStart, a0
	movea.l OperandEnd, a1
	movea.l OperandStart+4, a2
	movea.l OperandEnd+4, a3
packedEncode
	movea.l a6, a5
	jsr mask_unary.encode
	movem.l (sp)+, a2/a5-a6
	rts
packedBad
	movem.l (sp)+, a2/a5-a6
	bra.w bad
tableU8
	bsr.w evaluateOperandZero
	tst.l d0
	bne.w bad
	tst.w Unresolved
	bne.w unresolvedFixed
	tst.l d1
	bmi.w bad
	cmpi.l #255, d1
	bhi.w bad
	move.b d1, Records
	moveq #1, d5
	moveq #1, d6
	bra.w table
tableU16
	bsr.w evaluateOperandZero
	tst.l d0
	bne.w bad
	tst.w Unresolved
	bne.w unresolvedFixed
	tst.l d1
	bmi.w bad
	cmpi.l #65535, d1
	bhi.w bad
	bsr.w storeWord
	moveq #1, d5
	moveq #2, d6
	bra.w table
tableRel8
	bsr.w evaluateOperandZero
	tst.l d0
	bne.w bad
	tst.w Unresolved
	bne.w unresolvedFixed
	sub.l package.Context.Pc(a2), d1
	subq.l #2, d1
	cmpi.l #-128, d1
	blt.w bad
	cmpi.l #127, d1
	bgt.w bad
	move.b d1, Records
	moveq #1, d5
	moveq #1, d6
	bra.w table
unresolvedFixed
	cmpi.w #1, package.Context.Pass(a2)
	bne.w bad
	clr.l d1
	cmpi.b #RECIPE_U16, package.Row.Recipe(a5)
	beq.w unresolvedWord
	clr.b Records
	moveq #1, d5
	moveq #1, d6
	bra.w table
unresolvedWord
	clr.w Records
	moveq #1, d5
	moveq #2, d6
	bra.w table
tableNone
	moveq #0, d5
	moveq #0, d6
table
	bsr.w program
	tst.l d0
	bne.w bad
	cmpi.w #PROGRAM_TABLE, d2
	bne.w bad
	lea Records, a3
	bsr.w prepareExecution
	jsr encoding.table
	rts
semantic
	bsr.w project
	tst.l d0
	bne.w bad
	; Indexed payload width can change with the resolved address. Without
	; layout convergence, a deferred short candidate could invalidate labels.
	cmpi.b #RECIPE_SEMANTIC_TABLE, package.Row.Recipe(a5)
	bne.w resolvedInputs
	tst.w Unresolved
	bne.w bad
resolvedInputs
	bsr.w program
	tst.l d0
	bne.w bad
	cmpi.w #PROGRAM_SEMANTIC, d2
	bne.w bad
	bsr.w prepareExecution
	move.l package.Context.Pc(a2), encoding.Context.Pc(a6)
	move.w package.Context.Pass(a2), encoding.Context.Pass(a6)
	move.b package.Row.Unstable(a5), encoding.Context.Unstable(a6)
	clr.b encoding.Context.Defer(a6)
	clr.b encoding.Context.HasSymbol(a6)
	tst.w Unresolved
	beq.w branchStateReady
	move.b #1, encoding.Context.Defer(a6)
	move.b #1, encoding.Context.HasSymbol(a6)
branchStateReady
	move.l #Records, encoding.Context.Input(a6)
	move.w package.Row.InputCount(a5), encoding.Context.InputCount(a6)
	move.w #4, encoding.Context.FirstInputLen(a6)
	jsr encoding.semantic
	tst.l d0
	bne.w bad
	cmpi.b #RECIPE_SEMANTIC_TABLE, package.Row.Recipe(a5)
	bne.w return
	; Semantic inputs are dead. Reuse their storage so TABL output cannot
	; overwrite its own source while inserting the package opcode prefix.
	cmpi.l #24, d1
	bhi.w bad
	move.l d1, d6
	lea Records, a3
	move.l d1, d0
copyPayload
	tst.l d0
	beq.w payloadReady
	move.b (a1)+, (a3)+
	subq.l #1, d0
	bra.w copyPayload
payloadReady
	moveq #0, d0
	move.w package.Row.TableProgram(a5), d0
	bsr.w programId
	tst.l d0
	bne.w bad
	cmpi.w #PROGRAM_TABLE, d2
	bne.w bad
	moveq #1, d5
	lea Records, a3
	bsr.w prepareExecution
	jsr encoding.table
return
	rts
bad
	moveq #1, d0
	rts
	.bend  ; tryRow

evaluateOperandZero	.block
	movea.l OperandStart, a0
	cmpi.b #TOKEN_HASH, (a0)
	bne.w cursorReady
	addq.l #1, a0
cursorReady
	movea.l OperandEnd, a1
	jsr expression.evaluate
	tst.l d0
	bne.w return
	cmpa.l a1, a0
	bne.w malformed
	tst.l d2
	beq.w return
	move.w #1, Unresolved
return
	rts
malformed
	moveq #1, d0
	rts
	.bend  ; evaluateOperandZero

storeWord	.block
	movea.l package.Context.Package(a2), a4
	tst.w package.Header.LittleEndian(a4)
	beq.w big
	move.b d1, Records
	lsr.w #8, d1
	move.b d1, Records+1
	rts
big
	move.w d1, Records
	rts
	.bend  ; storeWord

; Resolve Row.Program. Returns D0 status, D2 kind, D4 version, A1/D1 bytes.
program	.block
	moveq #0, d0
	move.w package.Row.Program(a5), d0
	bra.w programId
	.bend  ; program

; D0=program ID; same outputs as program. D5 is preserved.
programId	.block
	move.l d5, -(sp)
	movea.l package.Context.Package(a2), a4
	cmp.l package.Header.ProgramCount(a4), d0
	bhs.w bad
	mulu.w #PROGRAM_BYTES, d0
	add.l package.Header.Programs(a4), d0
	bcs.w bad
	move.l d0, d1
	add.l #PROGRAM_BYTES, d1
	bcs.w bad
	cmp.l package.Header.Bytes(a4), d1
	bhi.w bad
	movea.l a4, a3
	adda.l d0, a3
	moveq #0, d2
	move.w package.Program.Kind(a3), d2
	moveq #0, d4
	move.w package.Program.Version(a3), d4
	move.l package.Program.Offset(a3), d1
	move.l d1, d5
	add.l package.Program.Bytes(a3), d5
	bcs.w bad
	cmp.l package.Header.Bytes(a4), d5
	bhi.w bad
	movea.l a4, a1
	adda.l d1, a1
	move.l package.Program.Bytes(a3), d1
	moveq #0, d0
	move.l (sp)+, d5
	rts
bad
	moveq #1, d0
	move.l (sp)+, d5
	tst.l d0
	rts
	.bend  ; programId

; Materialize Row projections in the existing CSEM little-endian scalar ABI.
project	.block
	moveq #0, d7
	move.w package.Row.InputCount(a5), d7
	cmpi.w #4, d7
	bhi.w bad
	move.l d7, d0
	mulu.w #PROJECTION_BYTES, d0
	move.l package.Row.Inputs(a5), d1
	add.l d1, d0
	bcs.w bad
	movea.l package.Context.Package(a2), a4
	cmp.l package.Header.Bytes(a4), d0
	bhi.w bad
	adda.l d1, a4
	lea Records, a3
	moveq #0, d6
loop
	cmp.w d7, d6
	bhs.w done
	tst.w d6
	beq.w recordReady
	move.b #4, (a3)+
recordReady
	moveq #0, d0
	move.b package.Projection.Kind(a4), d0
	cmpi.b #0, d0
	beq.w expressionValue
	cmpi.b #1, d0
	beq.w registerValue
	cmpi.b #2, d0
	beq.w memberValue
	cmpi.b #3, d0
	beq.w constantValue
	cmpi.b #4, d0
	beq.w namedValue
	cmpi.b #5, d0
	beq.w tupleRegister
	cmpi.b #6, d0
	beq.w tupleValue
	cmpi.b #8, d0
	beq.w wrappedRegister
	cmpi.b #9, d0
	beq.w wrappedRegister
	cmpi.b #10, d0
	beq.w wrappedRegister
	bra.w bad
expressionValue
	bsr.w projectionExpression
	bra.w valueReady
registerValue
	bsr.w projectionRegister
	bra.w valueReady
namedValue
	bsr.w projectionNamed
	bra.w valueReady
memberValue
	bsr.w projectionMember
	bra.w valueReady
tupleRegister
	bsr.w projectionTupleRegister
	bra.w valueReady
tupleValue
	bsr.w projectionTupleValue
	bra.w valueReady
wrappedRegister
	bsr.w projectionWrappedRegister
	bra.w valueReady
constantValue
	move.l package.Projection.Literal(a4), d3
	moveq #0, d0
valueReady
	tst.l d0
	bne.w bad
	move.w package.Projection.ValueProgram(a4), d0
	cmpi.w #MISSING_PROGRAM, d0
	beq.w store
	bsr.w valueProgram
	tst.l d0
	bne.w bad
store
	move.b d3, (a3)
	lsr.l #8, d3
	move.b d3, 1(a3)
	lsr.l #8, d3
	move.b d3, 2(a3)
	lsr.l #8, d3
	move.b d3, 3(a3)
	adda.w #4, a3
	adda.w #PROJECTION_BYTES, a4
	addq.w #1, d6
	bra.w loop
done
	tst.w Unresolved
	beq.w ok
	cmpi.w #1, package.Context.Pass(a2)
	bne.w bad
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; project

projectionExpression	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	cmpi.b #TOKEN_HASH, (a0)
	bne.w ready
	addq.l #1, a0
ready
	jsr expression.evaluate
	tst.l d0
	bne.w return
	cmpa.l a1, a0
	bne.w bad
	move.l d1, d3
	tst.l d2
	beq.w return
	move.w #1, Unresolved
return
	rts
bad
	moveq #1, d0
	rts
	.bend  ; projectionExpression

projectionRegister	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	bsr.w register
return
	rts
	.bend  ; projectionRegister

projectionNamed	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	bsr.w exactName
	tst.l d0
	bne.w return
	cmp.w package.Projection.Class(a4), d1
	bne.w bad
	moveq #0, d3
	moveq #0, d0
return
	rts
bad
	moveq #1, d0
	rts
	.bend  ; projectionNamed

projectionMember	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	cmpi.b #TOKEN_OPEN_PAREN, (a0)+
	bne.w bad
	movea.l a1, a6
	subq.l #6, a6
	cmpa.l a0, a6
	blo.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, (a6)
	bne.w bad
	cmpi.b #TOKEN_DOT, 1(a6)
	bne.w bad
	moveq #0, d0
	move.b 2(a6), d0
	cmpi.b #TOKEN_SYMBOL_0, d0
	beq.w word
	cmpi.b #TOKEN_SYMBOL_1, d0
	bne.w bad
word
	moveq #0, d0
	move.b 3(a6), d0
	lsl.w #8, d0
	move.b 4(a6), d0
	cmp.w package.Projection.Class(a4), d0
	bne.w bad
	tst.b 5(a6)
	bne.w bad
	movea.l a6, a1
	jsr expression.evaluate
	tst.l d0
	bne.w return
	cmpa.l a6, a0
	bne.w bad
	move.l d1, d3
	tst.l d2
	beq.w return
	move.w #1, Unresolved
return
	rts
bad
	moveq #1, d0
	rts
	.bend  ; projectionMember

projectionWrappedRegister	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	move.l a1, d0
	sub.l a0, d0
	cmpi.b #9, package.Projection.Kind(a4)
	beq.w postincrement
	cmpi.b #10, package.Projection.Kind(a4)
	beq.w predecrement
	cmpi.l #6, d0
	bne.w bad
	cmpi.b #TOKEN_OPEN_PAREN, (a0)+
	bne.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a1)
	bne.w bad
	subq.l #1, a1
	bra.w projectRegister
postincrement
	cmpi.l #7, d0
	bne.w bad
	cmpi.b #TOKEN_OPEN_PAREN, (a0)+
	bne.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, -2(a1)
	bne.w bad
	cmpi.b #TOKEN_PLUS, -1(a1)
	bne.w bad
	subq.l #2, a1
	bra.w projectRegister
predecrement
	cmpi.l #7, d0
	bne.w bad
	cmpi.b #TOKEN_MINUS, (a0)+
	bne.w bad
	cmpi.b #TOKEN_OPEN_PAREN, (a0)+
	bne.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a1)
	bne.w bad
	subq.l #1, a1
projectRegister
	bsr.w register
return
	rts
bad
	moveq #1, d0
	rts
	.bend  ; projectionWrappedRegister

; The two-item packed tuple is [compiled displacement] '(' [numeric name] ')'.
; A0/A1 bound the operand; returns A6 at '(' or D0=1. No source text is read.
tupleBounds	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #9, d0
	blo.w bad
	movea.l a1, a6
	suba.w #6, a6
	cmpi.b #TOKEN_OPEN_PAREN, (a6)
	bne.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, 5(a6)
	bne.w bad
	cmpi.b #TOKEN_SYMBOL_1, 1(a6)
	bhi.w bad
	tst.b 4(a6)
	bne.w bad
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w bad
	moveq #0, d0
	move.b 1(a0), d0
	beq.w bad
	addq.l #2, d0
	move.l a0, d1
	add.l d1, d0
	cmpa.l d0, a6
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; tupleBounds

projectionTupleRegister	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	bsr.w tupleBounds
	tst.l d0
	bne.w return
	lea 1(a6), a0
	subq.l #1, a1
	bsr.w register
return
	rts
	.bend  ; projectionTupleRegister

projectionTupleValue	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	bsr.w tupleBounds
	tst.l d0
	bne.w return
	movea.l a6, a1
	jsr expression.evaluate
	tst.l d0
	bne.w return
	cmpa.l a1, a0
	bne.w bad
	move.l d1, d3
	tst.l d2
	beq.w return
	move.w #1, Unresolved
return
	rts
bad
	moveq #1, d0
	rts
	.bend  ; projectionTupleValue

operandSpan	.block
	moveq #0, d0
	move.b package.Projection.Operand(a4), d0
	cmp.w OperandCount, d0
	bhs.w bad
	lsl.w #2, d0
	lea OperandStart, a1
	movea.l 0(a1, d0.w), a0
	lea OperandEnd, a1
	movea.l 0(a1, d0.w), a1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; operandSpan

; A0/A1=operand; D0=status, D1=name ID on success. Advances A0; CCR=D0.
; Only an exact unqualified numeric name can prove a package predicate.
exactName	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	moveq #0, d0
	move.b (a0)+, d0
	cmpi.b #TOKEN_SYMBOL_0, d0
	beq.w symbol
	cmpi.b #TOKEN_SYMBOL_1, d0
	bne.w bad
symbol
	moveq #0, d1
	move.b (a0)+, d1
	lsl.w #8, d1
	move.b (a0)+, d1
	tst.b (a0)+
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; exactName

; Exact numeric name plus package register-class lookup.
register	.block
	bsr.w exactName
	tst.l d0
	bne.w bad
	movea.l package.Context.Package(a2), a6
	move.l package.Header.RegisterRows(a6), d0
	move.l package.Header.RegisterCount(a6), d2
	cmpi.l #$ffff, d2
	bhi.w bad  ; MULU.W must cover the complete count used by the scan
	move.l d2, d4
	mulu.w #6, d4
	add.l d0, d4
	bcs.w bad
	cmp.l package.Header.Bytes(a6), d4
	bhi.w bad
	adda.l d0, a6
loop
	tst.l d2
	beq.w bad
	cmp.w (a6), d1
	bne.w next
	move.w package.Projection.Class(a4), d0
	cmp.w 2(a6), d0
	bne.w next
	moveq #0, d3
	move.w 4(a6), d3
	moveq #0, d0
	rts
next
	addq.l #6, a6
	subq.l #1, d2
	bra.w loop
bad
	moveq #1, d0
	rts
	.bend  ; register

valueProgram	.block
	move.w d0, -(sp)
	movea.l package.Context.Package(a2), a6
	moveq #0, d1
	move.w d0, d1
	cmp.l package.Header.ProgramCount(a6), d1
	bhs.w badPop
	mulu.w #PROGRAM_BYTES, d1
	add.l package.Header.Programs(a6), d1
	bcs.w badPop
	move.l d1, d2
	addi.l #PROGRAM_BYTES, d2
	bcs.w badPop
	cmp.l package.Header.Bytes(a6), d2
	bhi.w badPop  ; validate the descriptor before reading any of its fields
	movea.l a6, a0
	adda.l d1, a0
	cmpi.w #PROGRAM_VALUE, package.Program.Kind(a0)
	bne.w badPop
	move.l package.Program.Offset(a0), d1
	move.l package.Program.Bytes(a0), d2
	move.l d1, d4
	add.l d2, d4
	bcs.w badPop
	cmp.l package.Header.Bytes(a6), d4
	bhi.w badPop
	moveq #0, d0
	move.w package.Program.Version(a0), d0
	movea.l a6, a1
	adda.l d1, a1
	move.l d2, d1
	movem.l d1-d2/d4-d7/a0-a6, -(sp)
	jsr value.execute
	movem.l (sp)+, d1-d2/d4-d7/a0-a6
	addq.l #2, sp
	rts
badPop
	addq.l #2, sp
	moveq #1, d0
	rts
	.bend  ; valueProgram

; Own the minimal execution state; no service or assembler globals are imported.
; A6=context, all other registers preserved. This subset emits no output fixups.
prepareExecution	.block
	lea Execution, a6
	move.l #Output, encoding.Context.Output(a6)
	move.l #4096, encoding.Context.Capacity(a6)
	clr.w encoding.Context.WriteOffset(a6)
	clr.w encoding.Context.FixupCount(a6)
	clr.w encoding.Context.FixupCapacity(a6)
	clr.w encoding.Context.MnemonicLength(a6)
	rts
	.bend  ; prepareExecution
	.endsection
	.endmodule
