; Resolve numeric register candidates in their consuming package projections.
; @opforge-owner: experimental.amigaos.binary_operand_binding
	.module experimental.amigaos.binary_operand_binding
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_nested_operands as nested
	.use experimental.amigaos.binary_register_mask as masks
	.use opasm.amigaos.binary_expression as expression
	.use prvm.amigaos.abi as abi
	.priv
STATUS_OK = 0
STATUS_MISMATCH = 1
STATUS_INVALID = 2
PROJECTION_BYTES = 12
SEQUENCE_BYTES = 12
MAX_OPERANDS = 3
TOKEN_HASH = 8
TOKEN_MINUS = 19
TOKEN_LIST = 22
NameToken	.struct
Kind	.byte ?
Name	.word ?
Qualifier	.byte ?
	.endstruct
NAME_BYTES = NameToken.Qualifier+1
Work	.struct
Record	.long ?
Package	.long ?
Scopes	.long ?
Bytes	.word ?
Name	.word ?
Qualifier	.byte ?
Shape	.byte ?
Count	.word ?
Starts	.res MAX_OPERANDS*2
Ends	.res MAX_OPERANDS*2
Trial	.res source.MAX_LINE
Result	.res source.MAX_LINE
	.endstruct
WORK_BYTES = Work.Result+source.MAX_LINE
	.section code, kind=code
	.pub

; A0=mutable raw writer record,A1=validated package,A2=scope state.
; D0/CCR=status (zero success); all other registers preserved. Scratch is
; stack-owned. Every row probes a private copy; failed projections cannot
; publish names or lose source origins. Scalar projections never promote.
line	.block
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_OPERAND
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #WORK_BYTES, sp
	movea.l sp, a6
	move.l a0, Work.Record(a6)
	move.l a1, Work.Package(a6)
	move.l a2, Work.Scopes(a6)
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	move.w d0, Work.Bytes(a6)
	cmpi.w #4, d0
	blo.w invalid
	movea.l a0, a1
	adda.w d0, a1
	move.b 1(a0), d0
	andi.b #source.FLAG_OMIT+source.FLAG_LAYOUT+source.FLAG_PLAN+scopes.LEXICAL_BLOCK, d0
	bne.w done
	lea 4(a0), a0
	cmpa.l a1, a0
	beq.w done
	; Declarations and dot statements belong to shared preparation.
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, (a0)
	bhi.w done
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w invalid
	cmpi.l #5, d0
	blo.w head
	cmpi.b #abi.PRVM_TOKEN_KIND_COLON, 4(a0)
	bne.w declaration
	addq.l #5, a0
	bra.w head
declaration
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, 4(a0)
	beq.w done
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a0)
	beq.w done
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a0)
	beq.w done
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, 4(a0)
	beq.w done
head
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w done
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, (a0)
	bhi.w done
	move.w NameToken.Name(a0), Work.Name(a6)
	move.b NameToken.Qualifier(a0), Work.Qualifier(a6)
	addq.l #4, a0
	bsr.w needsCandidates
	cmpi.l #STATUS_INVALID, d0
	beq.w invalid
	tst.l d0
	bne.w done
	bsr.w split
	bne.w invalid
	bsr.w shape
	bne.w invalid
	movea.l Work.Record(a6), a0
	lea Work.Result(a6), a1
	bsr.w copyRecord
	movea.l Work.Package(a6), a5
	move.l package.Header.Rows(a5), d0
	move.l package.Header.RowCount(a5), d5
	cmpi.l #$ffff, d5
	bhi.w invalid
	move.l d5, d1
	mulu.w #package.ROW_BYTES, d1
	bsr.w table
	bne.w invalid
	movea.l a0, a5
row
	tst.l d5
	beq.w publish
	move.w Work.Name(a6), d0
	cmp.w package.Row.Name(a5), d0
	bne.w nextRow
	move.b Work.Qualifier(a6), d0
	cmp.b package.Row.Qualifier(a5), d0
	bne.w nextRow
	move.b Work.Shape(a6), d0
	cmp.b package.Row.Shape(a5), d0
	bne.w nextRow
	; Binding proves structural register roles, not candidate acceptance.
	; Ordered STVM state and value constraints belong to later selection.
	movea.l Work.Record(a6), a0
	lea Work.Trial(a6), a1
	bsr.w copyRecord
	bsr.w rowProjections
	cmpi.l #STATUS_INVALID, d0
	beq.w invalid
	tst.l d0
	bne.w nextRow
	; Merge complete row proofs. Roots already use the package's numeric
	; shape precedence, so a scalar row cannot override accumulator/mask roles.
	lea Work.Trial(a6), a0
	lea Work.Result(a6), a1
	bsr.w merge
nextRow
	adda.w #package.ROW_BYTES, a5
	subq.l #1, d5
	bra.w row
publish
	lea Work.Result(a6), a0
	movea.l Work.Record(a6), a1
	bsr.w copyRecord
done
	moveq #STATUS_OK, d0
	bra.w return
invalid
	moveq #STATUS_MISMATCH, d0
return
	adda.w #WORK_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	.TELEMETRY_SERVICE_LEAVE
	tst.l d0
	rts
	.bend  ; line
	.priv

; Copy the bounded whole record. D0-D1/A0-A1 scratch.
copyRecord	.block
	moveq #0, d0
	move.w Work.Bytes(a6), d0
copy
	move.b (a0)+, (a1)+
	subq.w #1, d0
	bne.w copy
	rts
	.bend  ; copyRecord

; Return zero only if a raw operand carries a source register candidate.
; Ordinary instruction lines avoid the package row scan entirely.
needsCandidates	.block
	movem.l d1-d4/a0-a3, -(sp)
	movea.l Work.Package(a6), a3
next
	cmpa.l a1, a0
	bhs.w absent
	jsr nested.rawTokenBytes
	bne.w invalid
	move.l d1, d4
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, (a0)
	bhi.w step
	moveq #0, d0
	move.w NameToken.Name(a0), d0
	cmp.w package.Header.NameCount(a3), d0
	blo.w step
	movem.l a0-a1, -(sp)
	movea.l Work.Scopes(a6), a0
	jsr scopes.registerCandidate
	movem.l (sp)+, a0-a1
	cmpi.l #STATUS_INVALID, d0
	beq.w invalid
	tst.l d0
	beq.w done
step
	adda.l d4, a0
	bra.w next
absent
	moveq #STATUS_MISMATCH, d0
	bra.w done
invalid
	moveq #STATUS_INVALID, d0
done
	movem.l (sp)+, d1-d4/a0-a3
	tst.l d0
	rts
	.bend  ; needsCandidates

; Offsets are relative to the immutable source record, also valid in Trial.
split	.block
	movem.l d2-d4/a2-a5, -(sp)
	clr.w Work.Count(a6)
	moveq #0, d4
next
	cmpa.l a1, a0
	beq.w complete
	cmpi.w #MAX_OPERANDS, d4
	bhs.w bad
	move.l a0, d2
	sub.l Work.Record(a6), d2
	move.w d4, d3
	add.w d3, d3
	move.w d2, Work.Starts(a6, d3.w)
	move.l a6, -(sp)
	jsr nested.rawSpan
	move.l a6, d2
	movea.l (sp)+, a6
	tst.l d0
	bne.w bad
	move.l d2, d0
	sub.l Work.Record(a6), d0
	move.w d0, Work.Ends(a6, d3.w)
	cmp.l a0, d2
	beq.w bad
	movea.l d2, a0
	addq.w #1, d4
	cmpa.l a1, a0
	beq.w complete
	cmpi.b #abi.PRVM_TOKEN_KIND_COMMA, (a0)+
	bne.w bad
	cmpa.l a1, a0
	beq.w bad
	bra.w next
complete
	move.w d4, Work.Count(a6)
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_MISMATCH, d0
done
	movem.l (sp)+, d2-d4/a2-a5
	tst.l d0
	rts
	.bend  ; split

; Reuse the package numeric shape contract. Only exact register roots affect
; the register mask; names in scalar expressions or containers do not.
shape	.block
	movem.l d2-d7/a0-a5, -(sp)
	moveq #0, d5  ; exact root register bits
	moveq #0, d6  ; hash bits
	moveq #0, d7
next
	cmp.w Work.Count(a6), d7
	bhs.w choose
	move.l d7, d0
	bsr.w originalSpan
	cmpi.b #TOKEN_HASH, (a0)
	bne.w root
	bset d7, d6
	bra.w advance
root
	moveq #-1, d4
	moveq #0, d3
	bsr.w registerName
	cmpi.l #STATUS_INVALID, d0
	beq.w bad
	tst.l d0
	bne.w advance
	bset d7, d5
advance
	addq.w #1, d7
	bra.w next
choose
	moveq #0, d0
	tst.w d7
	beq.w selected
	cmpi.w #1, d7
	beq.w one
	cmpi.w #3, d7
	beq.w three
	lea PairShapes, a0
	lsl.w #2, d6
	add.w d6, d5
	bra.w lookup
one
	lea SingleShapes, a0
	add.w d6, d6
	add.w d6, d5
	bra.w lookup
three
	lea TripleShapes, a0
	andi.w #4, d6
	add.w d6, d6
	add.w d6, d5
lookup
	move.b 0(a0, d5.w), d0
selected
	move.b d0, Work.Shape(a6)
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_INVALID, d0
done
	movem.l (sp)+, d2-d7/a0-a5
	tst.l d0
	rts
	.bend  ; shape

; Indexed by exact root-register bits, then immediate wrapper bits. These are
; the existing BS32 shapes, independent of target names, classes or values.
SingleShapes
	.byte package.SHAPE_SINGLE, package.SHAPE_REGISTER, package.SHAPE_PREFIXED, package.SHAPE_PREFIXED
PairShapes
	.byte package.SHAPE_VALUE_PAIR, package.SHAPE_PAIR, package.SHAPE_VALUE_REGISTER, package.SHAPE_REGISTER_PAIR
	.byte package.SHAPE_PREFIXED_VALUE, package.SHAPE_PREFIXED_VALUE, package.SHAPE_PREFIXED_PAIR, package.SHAPE_PREFIXED_PAIR
	.byte package.SHAPE_VALUE_PAIR, package.SHAPE_REGISTER_PREFIX_PAIR, package.SHAPE_VALUE_PAIR, package.SHAPE_REGISTER_PREFIX_PAIR
	.byte package.SHAPE_PREFIXED_VALUE, package.SHAPE_PREFIXED_VALUE, package.SHAPE_PREFIXED_PAIR, package.SHAPE_PREFIXED_PAIR
TripleShapes
	.byte package.SHAPE_VALUE_TRIPLE, package.SHAPE_VALUE_TRIPLE, package.SHAPE_VALUE_TRIPLE, package.SHAPE_REGISTER_VALUE_TRIPLE, package.SHAPE_VALUE_TRIPLE, package.SHAPE_VALUE_TRIPLE, package.SHAPE_VALUE_TRIPLE, package.SHAPE_REGISTER_TRIPLE
	.byte package.SHAPE_VALUE_PREFIX_TRIPLE, package.SHAPE_VALUE_PREFIX_TRIPLE, package.SHAPE_VALUE_PREFIX_TRIPLE, package.SHAPE_REGISTER_PREFIX_TRIPLE, package.SHAPE_VALUE_PREFIX_TRIPLE, package.SHAPE_VALUE_PREFIX_TRIPLE, package.SHAPE_VALUE_PREFIX_TRIPLE, package.SHAPE_REGISTER_PREFIX_TRIPLE
	.align 2

; D0=operand index -> A0/A1=raw immutable operand; other registers retained.
originalSpan	.block
	movea.l Work.Record(a6), a0
	bra.w span
	.bend  ; originalSpan
trialSpan	.block
	lea Work.Trial(a6), a0
	bra.w span
	.bend  ; trialSpan
span	.block
	add.w d0, d0
	movea.l a0, a1
	adda.w Work.Starts(a6, d0.w), a0
	adda.w Work.Ends(a6, d0.w), a1
	rts
	.bend  ; span

; D0=package-relative offset,D1=extent -> A0=table. D0=invalid or zero.
table	.block
	movea.l Work.Package(a6), a0
	move.l d0, d2
	add.l d1, d2
	bcs.w bad
	cmp.l package.Header.Bytes(a0), d2
	bhi.w bad
	adda.l d0, a0
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_INVALID, d0
	rts
	.bend  ; table

rowProjections	.block
	movem.l d3-d4/d6-d7/a3-a5, -(sp)
	moveq #0, d7
	move.w package.Row.RoleCount(a5), d7
	move.l package.Row.RoleInputs(a5), d0
	bsr.w projections
	bra.w done
done
	movem.l (sp)+, d3-d4/d6-d7/a3-a5
	tst.l d0
	rts
	.bend  ; rowProjections

; D0=projection offset,D7=count. Keep stage/row cursors and counters.
projections	.block
	movem.l d3-d4/d6-d7/a3-a5, -(sp)
	cmpi.w #16, d7
	bhi.w bad
	move.l d7, d1
	mulu.w #PROJECTION_BYTES, d1
	bsr.w table
	bne.w done
	movea.l a0, a4
next
	tst.l d7
	beq.w ok
	bsr.w projection
	bne.w done
	adda.w #PROJECTION_BYTES, a4
	subq.w #1, d7
	bra.w next
ok
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_INVALID, d0
done
	movem.l (sp)+, d3-d4/d6-d7/a3-a5
	tst.l d0
	rts
	.bend  ; projections

; A4=projection. Only proven numeric register views mutate Trial.
projection	.block
	moveq #0, d0
	move.b package.Projection.Kind(a4), d0
	cmpi.b #package.PROJECTION_CONSTANT, d0
	beq.w ok  ; literal has no operand
	moveq #0, d0
	move.b package.Projection.Operand(a4), d0
	cmp.w Work.Count(a6), d0
	bhs.w mismatch
	bsr.w trialSpan
	moveq #0, d0
	move.b package.Projection.Kind(a4), d0
	cmpi.b #package.PROJECTION_REGISTER, d0
	beq.w plain
	cmpi.b #package.PROJECTION_NAMED, d0
	beq.w named
	cmpi.b #package.PROJECTION_WRAPPED_REGISTER, d0
	beq.w wrapped
	cmpi.b #package.PROJECTION_POST_REGISTER, d0
	beq.w wrapped
	cmpi.b #package.PROJECTION_PRE_REGISTER, d0
	beq.w wrapped
	cmpi.b #package.PROJECTION_TUPLE_REGISTER, d0
	beq.w tupleRegister
	cmpi.b #package.PROJECTION_QUALIFIED_TUPLE_REGISTER, d0
	beq.w tupleRegister
	cmpi.b #package.PROJECTION_TUPLE_VALUE, d0
	beq.w tupleValue
	cmpi.b #package.PROJECTION_TUPLE_ARITY, d0
	beq.w tupleArity
	cmpi.b #package.PROJECTION_MASK, d0
	beq.w mask
	cmpi.b #package.PROJECTION_TUPLE_NAMED, d0
	beq.w tupleNamed
	cmpi.b #package.PROJECTION_CALL_REGISTER, d0
	beq.w callRegister
	cmpi.b #package.PROJECTION_CALL_WRAPPED_REGISTER, d0
	beq.w callWrapped
	cmpi.b #package.PROJECTION_HASH_SCALAR, d0
	beq.w hashScalar
	cmpi.b #package.PROJECTION_PATH, d0
	beq.w mismatch  ; value-dependent path/product terminals remain unresolved
	cmpi.b #package.PROJECTION_EXPRESSION, d0
	beq.w scalar
	cmpi.b #package.PROJECTION_TARGET, d0
	beq.w scalar
	cmpi.b #package.PROJECTION_ATOMIC_TARGET, d0
	beq.w scalar
	cmpi.b #package.PROJECTION_SCALAR, d0
	beq.w scalarExact
	cmpi.b #package.PROJECTION_WRAPPED_SCALAR, d0
	beq.w wrappedScalar
	cmpi.b #package.PROJECTION_MEMBER, d0
	beq.w member
	cmpi.b #package.PROJECTION_TARGET_MEMBER, d0
	beq.w member
	cmpi.b #package.PROJECTION_MEMBER_SHAPE, d0
	beq.w member
	bra.w mismatch  ; unknown predicates cannot authorize a partial row
ok
	moveq #STATUS_OK, d0
	rts
plain
	moveq #0, d3
	bra.w className
named
	moveq #0, d3
	moveq #0, d4
	move.w package.Projection.Class(a4), d4
	bsr.w exactName
	bne.w return
	cmp.w d4, d1
	bne.w mismatch
	bra.w promote
wrapped
	move.l a1, d1
	sub.l a0, d1
	cmpi.b #package.PROJECTION_WRAPPED_REGISTER, d0
	bne.w updated
	cmpi.l #6, d1
	bne.w mismatch
	cmpi.b #abi.PRVM_TOKEN_KIND_OPEN_PAREN, (a0)+
	bne.w mismatch
	cmpi.b #abi.PRVM_TOKEN_KIND_CLOSE_PAREN, -1(a1)
	bne.w mismatch
	subq.l #1, a1
	bra.w plain
updated
	cmpi.l #7, d1
	bne.w mismatch
	cmpi.b #package.PROJECTION_POST_REGISTER, d0
	bne.w before
	cmpi.b #abi.PRVM_TOKEN_KIND_OPEN_PAREN, (a0)+
	bne.w mismatch
	cmpi.b #abi.PRVM_TOKEN_KIND_CLOSE_PAREN, -2(a1)
	bne.w mismatch
	cmpi.b #abi.PRVM_TOKEN_KIND_OP_PLUS, -1(a1)
	bne.w mismatch
	subq.l #2, a1
	bra.w plain
before
	cmpi.b #TOKEN_MINUS, (a0)+
	bne.w mismatch
	cmpi.b #abi.PRVM_TOKEN_KIND_OPEN_PAREN, (a0)+
	bne.w mismatch
	cmpi.b #abi.PRVM_TOKEN_KIND_CLOSE_PAREN, -1(a1)
	bne.w mismatch
	subq.l #1, a1
	bra.w plain
tupleRegister
	bsr.w tupleLeaf
	bne.w return
	moveq #0, d3
	cmpi.b #package.PROJECTION_QUALIFIED_TUPLE_REGISTER, package.Projection.Kind(a4)
	bne.w className
	move.b package.Projection.Literal+3(a4), d3
className
	moveq #0, d4
	move.w package.Projection.Class(a4), d4
	bsr.w registerName
	bne.w return
promote
	move.w d1, NameToken.Name(a0)
	move.b d2, NameToken.Qualifier(a0)
	moveq #STATUS_OK, d0
return
	tst.l d0
	rts
tupleValue
	bsr.w tupleLeaf
	bne.w return
	bra.w scalarExact
tupleArity
	moveq #0, d0
	move.w package.Projection.Class(a4), d0
	moveq #0, d1
	jsr nested.rawTuple
	bra.w return
tupleNamed
	moveq #2, d0
	moveq #1, d1
	jsr nested.rawTuple
	bne.w return
	bra.w named
callRegister
	moveq #0, d1
	move.w package.Projection.Reserved(a4), d1
	jsr nested.rawCall
	bne.w return
	bra.w plain
callWrapped
	moveq #0, d1
	move.w package.Projection.Reserved(a4), d1
	jsr nested.rawCall
	bne.w return
	moveq #package.PROJECTION_WRAPPED_REGISTER, d0
	bra.w wrapped
hashScalar
	cmpi.b #TOKEN_HASH, (a0)
	bne.w mismatch
	addq.l #1, a0
	bra.w scalarExact
scalar
	cmpi.b #TOKEN_HASH, (a0)
	bne.w scalarExact
	addq.l #1, a0
scalarExact
	bsr.w scalarSyntax
	bra.w return
wrappedScalar
	moveq #nested.RAW_PAREN_OPEN, d1
	jsr nested.unwrap
	bne.w return
	bra.w scalarExact
member
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #NAME_BYTES+1, d0
	bls.w mismatch
	lea -NAME_BYTES-1(a1), a2
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, (a2)
	bne.w mismatch
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, 1(a2)
	bhi.w mismatch
	tst.b 1+NameToken.Qualifier(a2)
	bne.w mismatch
	move.w 1+NameToken.Name(a2), d0
	cmp.w package.Projection.Class(a4), d0
	bne.w mismatch
	movea.l a2, a1
	bra.w scalarExact
mask
	; Candidate names are changed only in this row's private operand copy.
	; The existing mask parser proves separators, ordering and mapped classes.
	movem.l a0-a1/a5, -(sp)
	bsr.w candidateNames
	bne.w maskDone
	movea.l Work.Package(a6), a5
	jsr masks.project
maskDone
	movem.l (sp)+, a0-a1/a5
	bra.w return
mismatch
	moveq #STATUS_MISMATCH, d0
	rts
	.bend  ; projection

tupleLeaf	.block
	moveq #0, d0
	move.b package.Projection.Reserved(a4), d0
	moveq #0, d1
	move.b package.Projection.Reserved+1(a4), d1
	jsr nested.rawTuple
	rts
	.bend  ; tupleLeaf

; Check an exact scalar with the installed shared EXVM grammar. No value is
; evaluated and no bytes persist; a row cannot borrow a malformed value view
; to authorize a different register projection. Origins remain unchanged.
scalarSyntax	.block
	movem.l d1/a0-a4, -(sp)
	; Scalar checking always sees original source IDs, even if an earlier
	; projection in this private row has already proven another register leaf.
	lea Work.Trial(a6), a3
	suba.l a3, a0
	suba.l a3, a1
	adda.l Work.Record(a6), a0
	adda.l Work.Record(a6), a1
	suba.w #source.MAX_LINE, sp
	movea.l sp, a3
	lea source.MAX_LINE(sp), a4
	movea.l Work.Package(a6), a2
	jsr expression.compileUnfolded
	tst.l d0
	bne.w mismatch
	cmpa.l a1, a0
	bne.w mismatch
	moveq #STATUS_OK, d0
	bra.w done
mismatch
	moveq #STATUS_MISMATCH, d0
done
	adda.w #source.MAX_LINE, sp
	movem.l (sp)+, d1/a0-a4
	tst.l d0
	rts
	.bend  ; scalarSyntax

; Exact name A0/A1 -> package candidate D1/D2 without following aliases.
; D3=expected qualifier. Existing package names remain valid numeric leaves.
exactName	.block
	movem.l a0-a1, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w mismatch
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, (a0)
	bhi.w mismatch
	moveq #0, d0
	move.w NameToken.Name(a0), d0
	moveq #0, d2
	move.b NameToken.Qualifier(a0), d2
	move.l d0, d1
	movea.l Work.Package(a6), a1
	cmp.w package.Header.NameCount(a1), d0
	blo.w qualifier
	movea.l Work.Scopes(a6), a0
	jsr scopes.registerCandidate
	tst.l d0
	bne.w done
qualifier
	cmp.b d3, d2
	bne.w mismatch
	moveq #STATUS_OK, d0
	bra.w done
mismatch
	moveq #STATUS_MISMATCH, d0
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; exactName

; A0/A1=exact name,D4=class (-1 any),D3=qualifier. Metadata is package owned.
; D0=status,D1/D2=package name/qualifier. Other registers retained.
registerName	.block
	movem.l d5-d7/a2-a3, -(sp)
	bsr.w exactName
	bne.w done
	move.l d1, d7
	move.l d2, d6
	movea.l Work.Package(a6), a3
	move.l package.Header.RegisterRows(a3), d0
	move.l package.Header.RegisterCount(a3), d5
	cmpi.l #$ffff, d5
	bhi.w invalid
	move.l d5, d1
	mulu.w #6, d1
	movem.l a0, -(sp)
	bsr.w table
	movea.l a0, a2
	movea.l (sp)+, a0
	bne.w done
next
	tst.l d5
	beq.w mismatch
	cmp.w (a2), d7
	bne.w advance
	tst.l d4
	bmi.w matched
	cmp.w 2(a2), d4
	beq.w matched
advance
	addq.l #6, a2
	subq.w #1, d5
	bra.w next
matched
	move.l d7, d1
	move.l d6, d2
	moveq #STATUS_OK, d0
	bra.w done
mismatch
	moveq #STATUS_MISMATCH, d0
	bra.w done
invalid
	moveq #STATUS_INVALID, d0
done
	movem.l (sp)+, d5-d7/a2-a3
	tst.l d0
	rts
	.bend  ; registerName

; Promote candidate names in a private mask view; masks.project owns grammar.
candidateNames	.block
	movem.l d3-d4/a0-a2, -(sp)
	movea.l a0, a2
next
	cmpa.l a1, a2
	beq.w ok
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, (a2)
	bhi.w separator
	move.l a1, d0
	sub.l a2, d0
	cmpi.l #4, d0
	blo.w mismatch
	movea.l a2, a0
	movem.l a1, -(sp)
	lea 4(a0), a1
	moveq #0, d3
	moveq #-1, d4
	bsr.w registerName
	movea.l (sp)+, a1
	bne.w done
	move.w d1, NameToken.Name(a2)
	move.b d2, NameToken.Qualifier(a2)
	addq.l #4, a2
	bra.w next
separator
	; Separators are still untrusted; mask grammar validates them afterward.
	cmpi.b #TOKEN_MINUS, (a2)
	beq.w step
	cmpi.b #TOKEN_LIST, (a2)
	bne.w mismatch
step
	addq.l #1, a2
	bra.w next
ok
	moveq #STATUS_OK, d0
	bra.w done
mismatch
	moveq #STATUS_MISMATCH, d0
done
	movem.l (sp)+, d3-d4/a0-a2
	tst.l d0
	rts
	.bend  ; candidateNames

; Compare raw framed tokens, merging only changed numeric name payloads.
; A0=Trial,A1=Result; source token framing was checked by nested.rawSpan.
merge	.block
	movem.l d1-d4/a0-a3, -(sp)
	movea.l Work.Record(a6), a2
	movea.l a2, a3
	adda.w Work.Bytes(a6), a3
	addq.l #4, a0
	addq.l #4, a1
	addq.l #4, a2
next
	cmpa.l a3, a2
	bhs.w done
	movem.l a0-a1, -(sp)
	movea.l a2, a0
	movea.l a3, a1
	jsr nested.rawTokenBytes
	movem.l (sp)+, a0-a1
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, (a2)
	bhi.w advance
	move.l (a0), d0
	cmp.l (a2), d0
	beq.w advance
	move.l d0, (a1)
advance
	adda.l d1, a0
	adda.l d1, a1
	adda.l d1, a2
	bra.w next
done
	movem.l (sp)+, d1-d4/a0-a3
	rts
	.bend  ; merge
	.endsection
	.endmodule
