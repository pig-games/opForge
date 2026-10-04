; Numeric instruction selection for experimental binary source records.
; @opforge-owner: experimental.amigaos.binary_encoding

	.module experimental.amigaos.binary_encoding
	.cpu 68020
	.include "telemetry_macros.i"
	.include "memory_telemetry.i"
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_shapes as shapes
	.use experimental.amigaos.binary_operand_wrappers as wrappers
	.use experimental.amigaos.binary_dependencies as dependencies
	.use experimental.amigaos.binary_mutable as mutable
	.use experimental.amigaos.binary_hunk_references as references
	.use experimental.amigaos.binary_register_mask as register_mask
	.use opasm.amigaos.binary_expression as expression
	.use exprvm.amigaos.runtime as exprvm
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

SHAPE_EMPTY = 0
SHAPE_SINGLE = 1
SHAPE_PREFIXED = 2
SHAPE_PREFIXED_PAIR = 3
SHAPE_PAIR = 4
SHAPE_REGISTER_PAIR = 5
SHAPE_VALUE_REGISTER = 6
SHAPE_PREFIXED_VALUE = 8
SHAPE_REGISTER = 9
SHAPE_VALUE_PAIR = 10

RECIPE_NONE = 0
RECIPE_U8 = 1
RECIPE_U16 = 2
RECIPE_REL8 = 3
RECIPE_SEMANTIC_INPUTS = 4
RECIPE_SEMANTIC_BRANCH = 5
RECIPE_UNSUPPORTED = 6
RECIPE_SEMANTIC_TABLE = 7
RECIPE_SEMANTIC_SEQUENCE = 9

PROGRAM_TABLE = 1
PROGRAM_SEMANTIC = 2
PROGRAM_VALUE = 3
PROJECTION_EXPRESSION = 0
PROJECTION_TARGET_MEMBER = 16
PROJECTION_MEMBER_SHAPE = 19
PROJECTION_WRAPPED_SCALAR = 20
PROJECTION_TUPLE_NAMED = 21
PROJECTION_SCALAR_EXPRESSION = 22
MISSING_PROGRAM = $ffff
HEADER_BYTES = package.HEADER_BYTES
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
Records	.res byte, 128
Execution	.res byte, encoding.Context.InputTarget+2
Output	.res byte, 4096
FixupCount	.res word, 1
FixupOffsets	.res long, 16
FixupAddends	.res long, 16
FixupWidths	.res word, 16
FixupTargets	.res word, 16
PositionProofCount	.res word, 1
PositionProofTarget	.res word, 1
ProjectedTarget	.res word, 1
ScalarTarget	.res word, 1
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.pub
Selection	.res byte, SelectionPosition.Projection+4
	.priv
.endif
.endif
.endif
	.endsection

	.section code, kind=code
	.pub

; A0=operand token cursor, A1=bounded end, A2=package.Context,
; D0.W=base mnemonic id, D1.B=0 or qualifier-index+1.
; Returns D0=0 on success, D1=byte count, A1=output. A0 may change;
; D2-D7/A2-A6 are preserved. No source spelling is consulted.
encode	.block
	.SELECTION_POSITION_CLEAR Selection
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_SELECTION
	movem.l d2-d7/a2-a6, -(sp)
	clr.w FixupCount
	clr.w PositionProofCount
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
	moveq #0, d0
	move.b package.Row.Shape(a5), d0
	cmp.w OperandShape, d0
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
	bsr.w tupleClasses
	cmpi.l #2, d0
	beq.w fail
	tst.l d0
	bne.w nextRow
	.SELECTION_POSITION_CANDIDATE Selection, package.Row.Priority(a5), package.Row.Recipe(a5)
	cmpi.b #RECIPE_UNSUPPORTED, package.Row.Recipe(a5)
	beq.w fail
	movem.l d3/d6-d7/a2/a5, -(sp)
	clr.w FixupCount
	clr.w PositionProofCount
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

; Numeric side channel from the most recent successful encode. Hunk projection
; binds opaque targets to canonical sections; package execution only transports IDs.
outputFixupCount	.block
	moveq #0, d0
	move.w FixupCount, d0
	rts
	.bend  ; outputFixupCount

; Return the successful package VM's positional cancellation proof.
; D0.W=count (saturated at two), D1.W=one-based Hunk section ID (flat: symbol ID). Other registers
; preserved; CCR reflects D0.
outputPositionProof	.block
	moveq #0, d0
	move.w PositionProofCount, d0
	moveq #0, d1
	move.w PositionProofTarget, d1
	tst.l d0
	rts
	.bend  ; outputPositionProof

; D0.W=index. Returns D0=status,D1=byte offset,D2=width,D3=target
; one-based Hunk section ID (flat: symbol ID),D4=encoded addend.
; Preserves D5-D7/A0-A6.
outputFixup	.block
	movem.l d5/a0, -(sp)
	moveq #0, d5
	move.w d0, d5
	cmp.w FixupCount, d5
	bhs.w badFixup
	move.l d5, d0
	lsl.l #2, d0
	lea FixupOffsets, a0
	move.l 0(a0, d0.l), d1
	lea FixupAddends, a0
	move.l 0(a0, d0.l), d4
	move.l d5, d0
	add.w d0, d0
	lea FixupWidths, a0
	moveq #0, d2
	move.w 0(a0, d0.w), d2
	lea FixupTargets, a0
	moveq #0, d3
	move.w 0(a0, d0.w), d3
	moveq #0, d0
	bra.w fixupReturn
badFixup
	moveq #1, d0
fixupReturn
	movem.l (sp)+, d5/a0
	tst.l d0
	rts
	.bend  ; outputFixup

; Replace a proven absolute-long fixup field after placement normalization.
; D0=index,D1=section-relative addend,A2=Context. D0/CCR=status; others preserved.
; The field offset/width and byte order come from package execution and metadata.
patchOutputFixup	.block
	movem.l d1-d5/a0-a1, -(sp)
	move.l d1, d5
	move.l d0, -(sp)
	bsr.w outputFixup
	tst.l d0
	bne.w bad
	cmpi.l #4, d2
	bne.w bad
	cmpi.l #4092, d1
	bhi.w bad
	lea Output, a0
	adda.l d1, a0
	movea.l package.Context.Package(a2), a1
	tst.w package.Header.LittleEndian(a1)
	beq.w bigEndian
	moveq #3, d2
littleLoop
	move.b d5, (a0)+
	lsr.l #8, d5
	dbra d2, littleLoop
	bra.w patched
bigEndian
	move.l d5, (a0)
patched
	move.l (sp), d0
	lsl.l #2, d0
	lea FixupAddends, a0
	move.l 4(sp), 0(a0, d0.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	addq.l #4, sp
	movem.l (sp)+, d1-d5/a0-a1
	tst.l d0
	rts
	.bend  ; patchOutputFixup

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
	bne.w secondValue
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
secondValue
	movea.l OperandStart, a0
	movea.l OperandEnd, a1
	bsr.w knownRegister
	cmpi.l #2, d0
	beq.w malformed
	tst.l d0
	beq.w pairReady
	move.w #SHAPE_VALUE_PAIR, OperandShape
pairReady
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
	bne.w valueOperand
	move.w #SHAPE_REGISTER, OperandShape
	moveq #0, d0
	rts
valueOperand
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
	move.w #SHAPE_PREFIXED_VALUE, OperandShape
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
	bsr.w findRegisterClass
done
	movem.l (sp)+, d1-d4/a0-a1/a6
	tst.l d0
	rts
	.bend  ; knownRegister

; D1.W=name ID. Returns D0=0 and D2.W=class for the first register row,
; 1 for an unknown name, or 2 for malformed table metadata. Clobbers D4/A6.
findRegisterClass	.block
	movea.l package.Context.Package(a2), a6
	move.l package.Header.RegisterRows(a6), d0
	move.l package.Header.RegisterCount(a6), d4
	cmpi.l #$ffff, d4
	bhi.w malformed
	move.l d4, d2
	mulu.w #6, d2
	add.l d0, d2
	bcs.w malformed
	cmp.l package.Header.Bytes(a6), d2
	bhi.w malformed
	adda.l d0, a6
loop
	tst.l d4
	beq.w no
	cmp.w (a6), d1
	beq.w yes
	addq.l #6, a6
	subq.l #1, d4
	bra.w loop
yes
	moveq #0, d2
	move.w 2(a6), d2
	moveq #0, d0
	rts
no
	moveq #1, d0
	rts
malformed
	moveq #2, d0
	rts
	.bend  ; findRegisterClass

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
	cmpi.l #9, d4
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
	cmpi.l #5, d4
	bhs.w wrappedFirstItem
	cmpi.l #4, d4
	beq.w tuple
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #1, d4
	beq.w plain
	cmpi.l #7, d0
	bne.w mismatch
	cmpi.l #2, d4
	beq.w tailUpdate
	cmpi.b #TOKEN_MINUS, (a0)
	bne.w mismatch
	cmpi.b #TOKEN_OPEN_PAREN, 1(a0)
	bne.w mismatch
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a1)
	bne.w mismatch
	bra.w advance
tailUpdate
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
wrappedFirstItem
	; A named register/range cannot match a compiled scalar expression.
	; Preserve the unsupported barrier for names and other unknown forms.
	cmpi.l #7, d4
	bne.w otherStructuredRoot
	cmpi.b #expression.COMPILED_TAG, (a0)
	beq.w mismatch
	; A complete indirect/update wrapper is not a bare named root either.
	jsr shapes.isWrappedName
	tst.l d0
	bne.w mismatch
	bra.w advance
otherStructuredRoot
	; A complete scalar has no member or indirect path root. Form 6
	; requires the scalar itself and must retain its matching barrier.
	cmpi.l #6, d4
	beq.w scalarRoot
	jsr shapes.isScalar
	tst.l d0
	bne.w mismatch
	cmpi.l #5, d4
	bne.w notMemberForm
	; A complete raw name/range list cannot satisfy a member root.
	jsr shapes.isNameSequence
	tst.l d0
	bne.w mismatch
	; A complete indirect/update wrapper also has no member tail.
	jsr shapes.isWrappedName
	tst.l d0
	bne.w mismatch
notMemberForm

	cmpi.l #8, d4
	blo.w scalarRoot
	; Path forms 8/9 require a nested first tuple item. A complete
	; wrapper around one name cannot match that required structure.
	jsr shapes.isWrappedName
	tst.l d0
	bne.w mismatch
	; A complete member root likewise has no outer indirect tuple. Its
	; scalar payload remains opaque; only the complete wrapper proves this.
	jsr shapes.isMember
	tst.l d0
	bne.w mismatch
scalarRoot
	bsr.w scalarTupleArity
	tst.l d0
	bne.w mismatch
	bra.w advance
tuple
	; A complete compiled scalar has no tuple tail, even when its payload
	; is long enough to pass the minimum-length check for a tuple prefix.
	jsr shapes.isScalar
	tst.l d0
	bne.w mismatch
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

; Disprove a row only when a complete packed tuple contains a known register
; with a different class. D0=0 unknown/matches, 1 mismatch, 2 bad metadata.
; All other caller state is preserved; CCR reflects D0.
tupleClasses	.block
	move.w package.Row.TupleClasses(a5), d0
	beq.w no
	movem.l d1-d4/a0-a1/a4, -(sp)
	lea package.Row.TupleClasses(a5), a4
	moveq #0, d3
next
	moveq #0, d4
	move.b 0(a4, d3.w), d4
	beq.w advance
	cmp.w OperandCount, d3
	bhs.w malformed
	move.w d3, d1
	lsl.w #2, d1
	lea OperandStart, a0
	movea.l 0(a0, d1.w), a0
	lea OperandEnd, a1
	movea.l 0(a1, d1.w), a1
	bsr.w tupleOperandClass
	tst.l d0
	bne.w done
advance
	addq.w #1, d3
	cmpi.w #2, d3
	blo.w next
	moveq #0, d0
	bra.w done
malformed
	moveq #2, d0
done
	movem.l (sp)+, d1-d4/a0-a1/a4
	tst.l d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; tupleClasses

; A0/A1 bound one operand; D4.B is required class + 1. Only the exact
; compiled-scalar/one-or-two-name wrapper can prove a base-register mismatch.
; D0=0 unknown/matches, 1 mismatch, 2 bad register metadata; others preserved.
tupleOperandClass	.block
	movem.l d1-d5/a0-a1/a3/a6, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #9, d0
	blo.w unknown
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w unknown
	moveq #0, d1
	move.b 1(a0), d1
	beq.w unknown
	addq.l #2, d1
	sub.l d1, d0
	cmpi.l #6, d0
	beq.w oneName
	cmpi.l #11, d0
	bne.w unknown
	cmpi.b #TOKEN_COMMA, 5(a0, d1.l)
	bne.w unknown
	cmpi.b #TOKEN_CLOSE_PAREN, 10(a0, d1.l)
	bne.w unknown
	bra.w baseName
oneName
	cmpi.b #TOKEN_CLOSE_PAREN, 5(a0, d1.l)
	bne.w unknown
baseName
	lea 0(a0, d1.l), a3
	cmpi.b #TOKEN_OPEN_PAREN, (a3)
	bne.w unknown
	lea 1(a3), a0
	lea 5(a3), a1
	bsr.w exactName
	tst.l d0
	bne.w unknown
	move.w d1, d5
	cmpi.b #TOKEN_COMMA, 5(a3)
	bne.w lookup
	; The second complete name token may carry a package qualifier. Its
	; meaning is irrelevant to the necessary class of the first name.
	cmpi.b #TOKEN_SYMBOL_0, 6(a3)
	beq.w lookup
	cmpi.b #TOKEN_SYMBOL_1, 6(a3)
	bne.w unknown
lookup
	move.w d5, d1
	move.w d4, d3
	bsr.w findRegisterClass
	cmpi.l #2, d0
	beq.w result
	tst.l d0
	bne.w unknown
	subq.w #1, d3
	cmp.w d2, d3
	beq.w unknown
	moveq #1, d0
	bra.w result
unknown
	moveq #0, d0
result
	movem.l (sp)+, d1-d5/a0-a1/a3/a6
	tst.l d0
	rts
	.bend  ; tupleOperandClass

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
	move.w #$ffff, ScalarTarget
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
	cmpi.b #RECIPE_SEMANTIC_SEQUENCE, d0
	beq.w sequence
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
; Conservative metadata scan; no register spellings or target widths.
hasWiderRow	.block
	movem.l d1-d4/a0-a1, -(sp)
	movea.l package.Context.Package(a2), a0
	move.l package.Header.RowCount(a0), d1
	movea.l a0, a1
	adda.l package.Header.Rows(a0), a1
next
	tst.l d1
	beq.w no
	move.w package.Row.Name(a5), d2
	cmp.w package.Row.Name(a1), d2
	bne.w advance
	move.b package.Row.Qualifier(a5), d2
	cmp.b package.Row.Qualifier(a1), d2
	bne.w advance
	move.b package.Row.Shape(a5), d2
	cmp.b package.Row.Shape(a1), d2
	bne.w advance
	move.b package.Row.Owner(a5), d2
	cmp.b package.Row.Owner(a1), d2
	bne.w advance
	move.b package.Row.WidthRank(a5), d2
	cmp.b package.Row.WidthRank(a1), d2
	bhs.w advance
	cmpi.b #RECIPE_UNSUPPORTED, package.Row.Recipe(a1)
	beq.w advance
	moveq #1, d0
	bra.w done
advance
	adda.w #ROW_BYTES, a1
	subq.l #1, d1
	bra.w next
no
	moveq #0, d0
done
	movem.l (sp)+, d1-d4/a0-a1
	rts
	.bend

semantic
	bsr.w project
	tst.l d0
	bne.w bad
	; Deferred scalar inputs use a fixed-width placeholder. An unstable
	; narrow row must yield to a supported wider row from the same owner.
	cmpi.b #RECIPE_SEMANTIC_TABLE, package.Row.Recipe(a5)
	bne.w resolvedInputs
	tst.w Unresolved
	beq.w resolvedInputs
	tst.b package.Row.Unstable(a5)
	beq.w resolvedInputs
	bsr.w hasWiderRow
	tst.l d0
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
	move.w encoding.Context.PositionProofCount(a6), PositionProofCount
	move.w encoding.Context.PositionProofTarget(a6), PositionProofTarget
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
	moveq #0, d5
	tst.l d6
	beq.w tablePayloadReady
	moveq #1, d5
tablePayloadReady
	lea Records, a3
	bsr.w prepareExecution
	jsr encoding.table
return
	rts
bad
	moveq #1, d0
	rts
	.bend  ; tryRow

; Execute a bounded package-owned sequence using the ordinary projection and
; SEMV interfaces. Match stages validate projections; encode stages append bytes.
sequence	.block
	movem.l d2-d7/a2-a5, -(sp)
	suba.w #32, sp
	movea.l sp, a0
	movea.l a5, a1
	moveq #7, d0
copyRow
	move.l (a1)+, (a0)+
	dbf d0, copyRow
	moveq #0, d7
	move.w package.Row.InputCount(a5), d7
	beq.w bad
	cmpi.w #8, d7
	bhi.w bad
	movea.l package.Context.Package(a2), a4
	move.l package.Row.Inputs(a5), d0
	move.l d7, d1
	mulu.w #12, d1
	add.l d0, d1
	bcs.w bad
	cmp.l package.Header.Bytes(a4), d1
	bhi.w bad
	adda.l d0, a4
	movea.l sp, a5
	; The private row copy uses its reserved word to track the encoding phase.
	clr.w package.Row.Reserved2(a5)
	moveq #0, d6
	moveq #0, d0
	move.w package.Row.TableProgram(a5), d0
	cmpi.w #MISSING_PROGRAM, d0
	beq.w loop
	; Package table prefix precedes operand-only semantic stages. Programs
	; that emit the instruction themselves carry no table prefix descriptor.
	movem.l d7/a4, -(sp)
	bsr.w programId
	tst.l d0
	bne.w prefixBad
	cmpi.w #PROGRAM_TABLE, d2
	bne.w prefixBad
	; The table owns the opcode and its operand slot. The sequence supplies
	; that slot later, so bind one empty operand for prefix emission.
	moveq #1, d5
	lea Records, a3
	bsr.w prepareExecution
	jsr encoding.table
	tst.l d0
	bne.w prefixBad
	move.l d1, d6
	movem.l (sp)+, d7/a4
	bra.w loop
prefixBad
	movem.l (sp)+, d7/a4
	bra.w bad
loop
	movem.l d6-d7/a4, -(sp)
	moveq #0, d0
	move.b package.SequenceStage.Kind(a4), d0
	cmpi.b #2, d0
	bhi.w stageBad
	tst.b package.SequenceStage.Reserved(a4)
	bne.w stageBad
	tst.w package.SequenceStage.Reserved2(a4)
	bne.w stageBad
	tst.w package.SequenceStage.InputCount(a4)
	beq.w stageBad
	move.w package.SequenceStage.Program(a4), package.Row.Program(a5)
	move.w package.SequenceStage.InputCount(a4), package.Row.InputCount(a5)
	move.l package.SequenceStage.Inputs(a4), package.Row.Inputs(a5)
	move.w d0, -(sp)
	cmpi.w #2, d0
	beq.w fixupProjection
	bsr.w project
	tst.l d0
	bne.w projectionBad
	tst.w Unresolved
	beq.w projected
	cmpi.w #1, package.Context.Pass(a2)
	bne.w projectionBad
projected
	move.w (sp)+, d0
	tst.w d0
	beq.w match
	bra.w encodeStage
fixupProjection
	bsr.w projectFixup
	tst.l d0
	bne.w projectionBad
	addq.w #2, sp
	bra.w fixupStage
encodeStage
	bsr.w program
	tst.l d0
	bne.w stageBad
	cmpi.w #PROGRAM_SEMANTIC, d2
	bne.w stageBad
	cmpi.w #2, d4
	beq.w encodingVersion
	cmpi.w #6, d4
	bne.w stageBad
encodingVersion
	move.w #1, package.Row.Reserved2(a5)
	bsr.w prepareExecution
	; Saved sequence output length is at the top of the stage frame.
	move.w 2(sp), encoding.Context.WriteOffset(a6)
	move.l package.Context.Pc(a2), encoding.Context.Pc(a6)
	move.w package.Context.Pass(a2), encoding.Context.Pass(a6)
	clr.b encoding.Context.Unstable(a6)
	clr.b encoding.Context.Defer(a6)
	clr.b encoding.Context.HasSymbol(a6)
	move.l #Records, encoding.Context.Input(a6)
	move.w package.Row.InputCount(a5), encoding.Context.InputCount(a6)
	move.w #4, encoding.Context.FirstInputLen(a6)
	jsr encoding.semantic
	tst.l d0
	bne.w stageBad
	move.l d1, (sp)
	bra.w next
fixupStage
	tst.w package.Row.Reserved2(a5)
	beq.w stageBad
	bsr.w program
	tst.l d0
	bne.w stageBad
	cmpi.w #PROGRAM_SEMANTIC, d2
	bne.w stageBad
	cmpi.w #4, d4
	beq.w fixupVersion
	cmpi.w #7, d4
	bne.w stageBad
fixupVersion
	bsr.w prepareExecution
	move.w 2(sp), encoding.Context.WriteOffset(a6)
	move.l package.Context.Pc(a2), encoding.Context.Pc(a6)
	move.w package.Context.Pass(a2), encoding.Context.Pass(a6)
	move.l #Records, encoding.Context.Input(a6)
	move.w package.Row.InputCount(a5), encoding.Context.InputCount(a6)
	move.w #7, encoding.Context.FirstInputLen(a6)
	jsr encoding.semantic
	tst.l d0
	bne.w stageBad
	move.w encoding.Context.FixupCount(a6), FixupCount
	move.w encoding.Context.PositionProofCount(a6), PositionProofCount
	move.w encoding.Context.PositionProofTarget(a6), PositionProofTarget
	move.l d1, (sp)
	bra.w next
match
	tst.w package.Row.Reserved2(a5)
	bne.w stageBad
	cmpi.w #MISSING_PROGRAM, package.Row.Program(a5)
	bne.w stageBad
next
	movem.l (sp)+, d6-d7/a4
	adda.w #12, a4
	subq.w #1, d7
	bne.w loop
	tst.w package.Row.Reserved2(a5)
	beq.w bad
	move.l d6, d1
	lea Output, a1
	moveq #0, d0
	bra.w done
projectionBad
	addq.l #2, sp
stageBad
	movem.l (sp)+, d6-d7/a4
bad
	moveq #1, d0
done
	adda.w #32, sp
	movem.l (sp)+, d2-d7/a2-a5
	rts
	.bend  ; sequence

; Project a known scalar through the package's bounded 32-bit bridge.
; A0/A1=expression, A2=package.Context. Expression ABI and registers retained.
; D0/CCR=status; reject values outside signed-i32/unsigned-u32 representation.
evaluateScalar	.block
	jsr expression.evaluate
	tst.l d0
	bne.w done
	tst.l d2
	bne.w done
	tst.l package.Context.High(a2)
	beq.w done
	cmpi.l #-1, package.Context.High(a2)
	bne.w bad
	tst.l d1
	bmi.w ready
bad
	moveq #1, d0
	bra.w done
ready
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; evaluateScalar

evaluateOperandZero	.block
	movea.l OperandStart, a0
	cmpi.b #TOKEN_HASH, (a0)
	bne.w cursorReady
	addq.l #1, a0
cursorReady
	movea.l OperandEnd, a1
	bsr.w evaluateScalar
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
	cmpi.w #16, d7
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
	.SELECTION_POSITION_PROJECTION Selection, package.Projection.Kind(a4)
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
	cmpi.b #11, d0
	beq.w tupleItem
	cmpi.b #12, d0
	beq.w tupleItem
	cmpi.b #13, d0
	beq.w tupleItem
	cmpi.b #14, d0
	beq.w tupleItem
	cmpi.b #15, d0
	beq.w targetExpression
	cmpi.b #PROJECTION_TARGET_MEMBER, d0
	beq.w memberValue
	cmpi.b #17, d0
	beq.w atomicTarget
	cmpi.b #18, d0
	beq.w registerMask
	cmpi.b #PROJECTION_MEMBER_SHAPE, d0
	beq.w memberShape
	cmpi.b #PROJECTION_WRAPPED_SCALAR, d0
	beq.w wrappedScalar
	cmpi.b #PROJECTION_TUPLE_NAMED, d0
	beq.w tupleNamed
	cmpi.b #PROJECTION_SCALAR_EXPRESSION, d0
	beq.w scalarExpression
	bra.w bad
expressionValue
	move.w package.ScalarProjection.Flags(a4), d0
	beq.w plainExpression
	cmpi.w #package.SCALAR_ADDRESS_IDENTITY, d0
	bne.w bad
	cmpi.b #RECIPE_SEMANTIC_BRANCH, package.Row.Recipe(a5)
	bne.w bad
	cmpi.w #1, d6
	bne.w bad
	tst.b package.Projection.Operand(a4)
	bne.w bad
	bsr.w projectionExpression
	tst.l d0
	bne.w bad
	bsr.w scalarIdentity
	bra.w valueReady
plainExpression
	bsr.w projectionExpression
	bra.w valueReady
targetExpression
	bsr.w projectionTargetExpression
	bra.w valueReady
atomicTarget
	bsr.w projectionAtomicTarget
	bra.w valueReady
registerMask
	bsr.w projectionRegisterMask
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
memberShape
	bsr.w projectionMemberShape
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
tupleItem
	bsr.w projectionTupleItem
	bra.w valueReady
wrappedScalar
	bsr.w projectionWrappedScalar
	bra.w valueReady
tupleNamed
	bsr.w projectionTupleNamed
	bra.w valueReady
scalarExpression
	bsr.w projectionScalarExpression
	bra.w valueReady
constantValue
	tst.w package.Projection.Reserved(a4)
	bne.w bad  ; constants have no scalar identity metadata
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

; Match a scalar expression containing a symbol, including absolute constants.
; ExprVM supplies symbol presence. Relocatable current-PC expressions also name
; an address base, classified by the shared bounded reference proof.
projectionTargetExpression	.block
	movem.l d1-d2/d4/a0-a1/a6, -(sp)
	bsr.w operandSpan
	tst.l d0
	bne.w done
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #TOKEN_HASH, (a0)
	bne.w ready
	addq.l #1, a0
ready
	movea.l a0, a6
	jsr expression.evaluateWithSymbols
	tst.l d0
	bne.w done
	cmpa.l a1, a0
	bne.w bad
	tst.l d4
	bne.w targetPresent
	tst.w package.Context.Relocatable(a2)
	beq.w bad
	movea.l a6, a0
	jsr references.targets
	cmpi.l #references.STATUS_SECTION, d0
	bne.w bad
targetPresent
	tst.l d2
	beq.w matched
	move.w #1, Unresolved
matched
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/d4/a0-a1/a6
	moveq #0, d3
	tst.l d0
	rts
	.bend  ; projectionTargetExpression

; Match an immediate or direct operand that names exactly one relocation
; target. This is a package predicate, so it projects zero rather than the
; current target address; the later fixup stage resolves and records the ID.
projectionAtomicTarget	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	cmpa.l a1, a0
	bhs.w invalid
	cmpi.b #TOKEN_HASH, (a0)
	bne.w target
	addq.l #1, a0
target
	bsr.w exactTarget
	tst.l d0
	bne.w return
	moveq #0, d3
return
	rts
invalid
	moveq #1, d0
	rts
	.bend  ; projectionAtomicTarget

; Build the package fixup VM's seven-byte numeric input records. Scalar
; expressions transport identity only when the generic affine proof succeeds.
; Hunk targets are canonical section IDs, so a PC base needs no fabricated symbol.
; The Hunk caller rejects section-bearing operands without a package fixup.
projectFixup	.block
	moveq #0, d7
	move.w package.Row.InputCount(a5), d7
	beq.w bad
	cmpi.w #16, d7
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
nextFixupInput
	cmp.w d7, d6
	bhs.w good
	tst.w d6
	beq.w inputReady
	move.b #7, (a3)+
inputReady
	.SELECTION_POSITION_PROJECTION Selection, package.Projection.Kind(a4)
	cmpi.b #15, package.Projection.Kind(a4)
	beq.w scalarTarget
	; Scalar fixups share the same bounded identity proof; literals have no target.
	cmpi.b #PROJECTION_EXPRESSION, package.Projection.Kind(a4)
	beq.w scalarTarget
	cmpi.b #PROJECTION_TARGET_MEMBER, package.Projection.Kind(a4)
	beq.w memberTarget
	cmpi.b #6, package.Projection.Kind(a4)
	bne.w bad
	bsr.w operandSpan
	tst.l d0
	bne.w bad
	bsr.w projectedTupleBounds
	tst.l d0
	bne.w bad
	movea.l a6, a1  ; first tuple item is the bounded scalar target
	bra.w targetSpanReady
memberTarget
	bsr.w operandSpan
	tst.l d0
	bne.w bad
	bsr.w memberBase
	bne.w bad
	bra.w targetSpanReady
scalarTarget
	bsr.w operandSpan
	tst.l d0
	bne.w bad
targetSpanReady
	move.w #$ffff, ProjectedTarget
	cmpa.l a1, a0
	bhs.w bad
	; Validate the scalar before proving relocation identity. In pass one an
	; undefined label has no usable section provenance yet; ExprVM owns that
	; unresolved decision. Preserve the bounded span for resolved-value proof.
	movem.l a0-a1, -(sp)
	cmpi.b #6, package.Projection.Kind(a4)
	bne.w scalarValue
	bsr.w projectionTupleValue
	bra.w valueProjected
scalarValue
	cmpi.b #PROJECTION_TARGET_MEMBER, package.Projection.Kind(a4)
	bne.w expressionValue
	bsr.w projectionMember
	bra.w valueProjected
expressionValue
	bsr.w projectionExpression
valueProjected
	movem.l (sp)+, a0-a1
	tst.l d0
	bne.w bad
	tst.w Unresolved
	bne.w targetReady
	; The immediate marker is syntax, not part of the target identity.
	cmpi.b #TOKEN_HASH, (a0)
	bne.w targetName
	addq.l #1, a0
targetName
	cmpa.l a1, a0
	bhs.w bad
	; Exact-name probing may advance even on failure. The bounded proof starts
	; at the original operand, not at the probe's failure cursor.
	move.l a0, -(sp)
	bsr.w exactTarget
	movea.l (sp)+, a0
	tst.l d0
	beq.w exactIdentity
	; The generic postfix proof transports one base through absolute addends.
	; Numeric evaluation still belongs to ExprVM; unsafe address algebra fails.
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w targetReady
	jsr references.affineTarget
	cmpi.l #references.STATUS_BAD, d0
	beq.w bad
	cmpi.l #references.STATUS_SECTION, d0
	bne.w targetReady
	bra.w bindTarget
exactIdentity
	cmpi.l #references.CURRENT_PC_BASE, d1
	beq.w bindTarget
	cmp.l package.Context.Count(a2), d1
	bhs.w bad
	movea.l package.Context.Defined(a2), a0
	tst.b 0(a0, d1.l)
	beq.w targetReady
	cmpi.b #dependencies.ABSOLUTE, 0(a0, d1.l)
	beq.w targetReady
	cmpi.b #mutable.DEFINED, 0(a0, d1.l)
	beq.w targetReady
	cmpi.b #mutable.SNAPSHOT_ABSOLUTE, 0(a0, d1.l)
	beq.w targetReady
bindTarget
	tst.w package.Context.Relocatable(a2)
	beq.w storeTarget
	jsr references.baseSection
	bne.w bad
storeTarget
	move.w d1, ProjectedTarget
targetReady
	moveq #0, d0
	cmpi.w #$ffff, ProjectedTarget
	beq.w targetFlagReady
	moveq #1, d0
targetFlagReady
	tst.w Unresolved
	beq.w valueReady
	cmpi.w #1, package.Context.Pass(a2)
	bne.w bad
	ori.b #2, d0
	move.w #$ffff, ProjectedTarget
valueReady
	move.b d0, (a3)+
	move.b d3, (a3)+
	lsr.l #8, d3
	move.b d3, (a3)+
	lsr.l #8, d3
	move.b d3, (a3)+
	lsr.l #8, d3
	move.b d3, (a3)+
	move.w ProjectedTarget, d0
	lsr.w #8, d0
	move.b d0, (a3)+
	move.b ProjectedTarget+1, (a3)+
	adda.w #PROJECTION_BYTES, a4
	addq.w #1, d6
	bra.w nextFixupInput
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; projectFixup

projectionExpression	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	cmpi.b #TOKEN_HASH, (a0)
	bne.w ready
	addq.l #1, a0
ready
	bsr.w evaluateScalar
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

; Bind optional scalar address identity after expression validation. In Hunk
; mode the bounded affine proof supplies a canonical section for symbols or PC.
; Flat mode retains exact symbol identity; unresolved and absolute values have none.
; D0/CCR=status; all projected values and caller registers are retained.
scalarIdentity	.block
	movem.l d1-d3/a0-a1, -(sp)
	bsr.w operandSpan
	tst.l d0
	bne.w done
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #TOKEN_HASH, (a0)
	bne.w target
	addq.l #1, a0
target
	tst.w package.Context.Relocatable(a2)
	beq.w exact
	tst.w Unresolved
	bne.w absent
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w exact
	jsr references.affineTarget
	cmpi.l #references.STATUS_BAD, d0
	beq.w bad
	cmpi.l #references.STATUS_SECTION, d0
	bne.w absent
	bra.w bind
exact
	bsr.w exactTarget
	tst.l d0
	bne.w absent
	cmp.l package.Context.Count(a2), d1
	bhs.w bad
	movea.l package.Context.Defined(a2), a0
	tst.b 0(a0, d1.l)
	beq.w absent
	cmpi.b #dependencies.ABSOLUTE, 0(a0, d1.l)
	beq.w absent
	cmpi.b #mutable.DEFINED, 0(a0, d1.l)
	beq.w absent
	cmpi.b #mutable.SNAPSHOT_ABSOLUTE, 0(a0, d1.l)
	beq.w absent
bind
	tst.w package.Context.Relocatable(a2)
	beq.w store
	jsr references.baseSection
	bne.w bad
store
	move.w d1, ScalarTarget
absent
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; scalarIdentity

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

; A0/A1=bounded member wrapper, A4=package projection. Return the compiled
; scalar base in A0/A1 after validating the package's numeric field identity.
; D0/CCR=status; clobbers A0/A1/A6. No field spelling or value is interpreted.
memberBase	.block
	jsr shapes.isMember
	beq.w bad
	movea.l a1, a6
	subq.l #6, a6
	moveq #0, d0
	move.b 3(a6), d0
	lsl.w #8, d0
	move.b 4(a6), d0
	cmp.w package.Projection.Class(a4), d0
	bne.w bad
	addq.l #1, a0
	movea.l a6, a1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; memberBase

; Shape-only package predicates validate the wrapper without evaluating its
; base. Forward and section-bearing names therefore remain valid matches.
projectionMemberShape	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	bsr.w memberBase
return
	moveq #0, d3
	tst.l d0
	rts
	.bend  ; projectionMemberShape

projectionMember	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	bsr.w memberBase
	bne.w return
	bsr.w evaluateScalar
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
	.bend  ; projectionMember

projectionWrappedRegister	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	move.l a1, d0
	sub.l a0, d0
	cmpi.b #9, package.Projection.Kind(a4)
	beq.w tailUpdate
	cmpi.b #10, package.Projection.Kind(a4)
	beq.w headUpdate
	cmpi.l #6, d0
	bne.w bad
	cmpi.b #TOKEN_OPEN_PAREN, (a0)+
	bne.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a1)
	bne.w bad
	subq.l #1, a1
	bra.w projectRegister
tailUpdate
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
headUpdate
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

; Generic three-item tuple projections. The first item is compiled scalar data;
; the remaining items retain numeric names and an optional numeric qualifier.
; A4 selects class/qualifier; no target register bits or spelling are consulted.
projectionTupleItem	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	cmpi.b #14, package.Projection.Kind(a4)
	bne.w triple
	cmpi.w #2, package.Projection.Class(a4)
	bne.w triple
	bsr.w projectedTupleBounds
	moveq #0, d3
	rts
triple
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w bad
	moveq #0, d0
	move.b 1(a0), d0
	beq.w bad
	addq.l #2, d0
	lea 0(a0, d0.l), a6
	move.l a1, d0
	sub.l a6, d0
	cmpi.l #11, d0
	bne.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, 10(a6)
	bne.w bad

bounds
	cmpi.b #TOKEN_OPEN_PAREN, (a6)
	bne.w bad
	cmpi.b #TOKEN_SYMBOL_1, 1(a6)
	bhi.w bad
	tst.b 4(a6)
	bne.w bad
	cmpi.b #TOKEN_COMMA, 5(a6)
	bne.w bad
	cmpi.b #TOKEN_SYMBOL_1, 6(a6)
	bhi.w bad
	; Numeric names occupy four bytes: symbol, id word, qualifier byte.
	cmpi.b #14, package.Projection.Kind(a4)
	beq.w arity
	cmpi.b #3, package.Projection.Reserved(a4)
	bne.w bad
	cmpi.b #12, package.Projection.Kind(a4)
	beq.w scalar
	cmpi.b #11, package.Projection.Kind(a4)
	beq.w base
	cmpi.b #13, package.Projection.Kind(a4)
	bne.w bad
	cmpi.b #2, package.Projection.Reserved+1(a4)
	bne.w bad
	moveq #0, d0
	move.b 9(a6), d0
	cmp.l package.Projection.Literal(a4), d0
	bne.w bad
	moveq #0, d1
	move.b 7(a6), d1
	lsl.w #8, d1
	move.b 8(a6), d1
	bra.w lookupRegister

base
	cmpi.b #1, package.Projection.Reserved+1(a4)
	bne.w bad
	lea 1(a6), a0
	lea 4(a0), a1
	bra.w register
scalar
	tst.b package.Projection.Reserved+1(a4)
	bne.w bad
	movea.l a6, a1
	bsr.w evaluateScalar
	tst.l d0
	bne.w return
	cmpa.l a1, a0
	bne.w bad
	move.l d1, d3
	tst.l d2
	beq.w return
	move.w #1, Unresolved
	bra.w return
arity
	cmpi.w #3, package.Projection.Class(a4)
	bne.w bad
	moveq #0, d3
	moveq #0, d0
return
	rts
bad
	moveq #1, d0
	rts
	.bend  ; projectionTupleItem

; Recognize a complete tuple whose first item is a valid compiled scalar and
; whose remaining items are numeric names. Returns D0=2/3, or zero unknown.
; Malformed structures/programs return unknown and never disprove a match;
; unresolved values can still prove scalar structure after successful validation.
; Preserves D1-D4/A0-A1. A6 is scratch; no package semantics are consulted.
scalarTupleArity	.block
	movem.l d1-d4/a0-a1, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #9, d0
	blo.w unknown
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w unknown
	moveq #0, d0
	move.b 1(a0), d0
	beq.w unknown
	addq.l #2, d0
	lea 0(a0, d0.l), a6
	move.l a1, d0
	sub.l a6, d0
	moveq #2, d4
	cmpi.l #6, d0
	beq.w tail
	cmpi.l #11, d0
	bne.w unknown
	moveq #3, d4
	cmpi.b #TOKEN_COMMA, 5(a6)
	bne.w unknown
	cmpi.b #TOKEN_SYMBOL_1, 6(a6)
	bhi.w unknown
	cmpi.b #TOKEN_CLOSE_PAREN, 10(a6)
	bne.w unknown
	bra.w tail
tail
	cmpi.b #TOKEN_OPEN_PAREN, (a6)
	bne.w unknown
	cmpi.b #TOKEN_SYMBOL_1, 1(a6)
	bhi.w unknown
	tst.b 4(a6)
	bne.w unknown
	cmpi.w #2, d4
	bne.w scalar
	cmpi.b #TOKEN_CLOSE_PAREN, 5(a6)
	bne.w unknown
scalar
	movea.l a6, a1
	bsr.w evaluateScalar
	tst.l d0
	bne.w unknown
	cmpa.l a1, a0
	bne.w unknown
	move.l d4, d0
	bra.w done
unknown
	moveq #0, d0
done
	movem.l (sp)+, d1-d4/a0-a1
	rts
	.bend  ; scalarTupleArity

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

; Both structural encodings expose the exact first scalar and named item.
projectedTupleBounds	.block
	cmpi.b #TOKEN_OPEN_PAREN, (a0)
	beq.w wrapped
	bra.w tupleBounds
wrapped
	jsr wrappers.tuple
	; Existing displacement consumers use A6 as the scalar-end delimiter.
	; The new named projection uses the helper directly for its name cursor.
	tst.l d0
	bne.w done
	movea.l a1, a6
done
	rts
	.bend

projectionScalarExpression	.block
	bsr.w operandSpan
	bne.w done
	jsr shapes.isScalar
	cmpi.l #1, d0
	bne.w bad
	bra.w projectionExpression
bad
	moveq #1, d0
done
	rts
	.bend

projectionWrappedScalar	.block
	bsr.w operandSpan
	bne.w done
	jsr wrappers.scalar
	bne.w done
	bsr.w evaluateScalar
	tst.l d0
	bne.w done
	cmpa.l a1, a0
	bne.w bad
	move.l d1, d3
	tst.l d2
	beq.w done
	move.w #1, Unresolved
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	rts
	.bend

projectionTupleNamed	.block
	bsr.w operandSpan
	bne.w done
	jsr wrappers.tuple
	bne.w done
	movea.l a6, a0
	lea 4(a0), a1
	bsr.w exactName
	bne.w done
	cmp.w package.Projection.Class(a4), d1
	bne.w bad
	moveq #0, d3
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	rts
	.bend

projectionTupleRegister	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	bsr.w projectedTupleBounds
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
	bsr.w projectedTupleBounds
	tst.l d0
	bne.w return
	movea.l a6, a1
	bsr.w evaluateScalar
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

; Project a package-mapped register mask through the reusable bounded list
; parser. The map and reversal reside in this row's projection metadata.
projectionRegisterMask	.block
	bsr.w operandSpan
	tst.l d0
	bne.w return
	movem.l a4-a5, -(sp)
	movea.l package.Context.Package(a2), a5
	jsr register_mask.project
	movem.l (sp)+, a4-a5
return
	rts
	.bend  ; projectionRegisterMask

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

; A0/A1=operand; D0=status,D1=numeric symbol ID or CURRENT_PC_BASE.
; Exact ExprVM leaves retain their identity without evaluating source syntax.
; The PC leaf is a target only inside relocatable output; flat PC is absolute.
exactTarget	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w symbol
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w plain
	cmpi.b #2, 1(a0)
	bne.w bad
	cmpi.b #exprvm.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, 2(a0)
	bne.w bad
	tst.b 3(a0)
	bne.w bad
	tst.w package.Context.Relocatable(a2)
	beq.w bad
	tst.w package.Context.CurrentSection(a2)
	beq.w bad
	move.l #references.CURRENT_PC_BASE, d1
	bra.w ready
symbol
	cmpi.l #6, d0
	bne.w plain
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w plain
	cmpi.b #4, 1(a0)
	bne.w bad
	cmpi.b #exprvm.EXPRVM_V2_OPCODE_PUSH_SYMBOL, 2(a0)
	bne.w bad
	tst.b 5(a0)
	bne.w bad
	moveq #0, d1
	move.b 4(a0), d1
	lsl.w #8, d1
	move.b 3(a0), d1
ready
	movea.l a1, a0
	moveq #0, d0
	rts
plain
	bsr.w exactName
	rts
bad
	moveq #1, d0
	rts
	.bend  ; exactTarget

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
	beq.w lookup
	rts
lookup
	bra.w lookupRegister
	.bend  ; register

lookupRegister	.block
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
	.bend  ; lookupRegister

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
; A6=context, all other registers preserved. Fixups are a bounded numeric
; side channel owned by this encoder and consumed by the assembly layer.
prepareExecution	.block
	lea Execution, a6
	move.l #Output, encoding.Context.Output(a6)
	move.l #4096, encoding.Context.Capacity(a6)
	clr.w encoding.Context.WriteOffset(a6)
	move.w FixupCount, encoding.Context.FixupCount(a6)
	move.w #16, encoding.Context.FixupCapacity(a6)
	move.l #FixupOffsets, encoding.Context.FixupOffsets(a6)
	move.l #FixupAddends, encoding.Context.FixupAddends(a6)
	move.l #FixupWidths, encoding.Context.FixupWidths(a6)
	move.l #FixupTargets, encoding.Context.FixupTargets(a6)
	move.w PositionProofCount, encoding.Context.PositionProofCount(a6)
	move.w PositionProofTarget, encoding.Context.PositionProofTarget(a6)
	move.w ScalarTarget, encoding.Context.InputTarget(a6)
	clr.w encoding.Context.MnemonicLength(a6)
	rts
	.bend  ; prepareExecution
	.endsection
	.endmodule
