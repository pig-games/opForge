; Temporary numeric certificates for preparation-time collectible ownership.
; @opforge-owner: experimental.amigaos.binary_owner_guards
	.module experimental.amigaos.binary_owner_guards
	.cpu 68020
	.use experimental.amigaos.binary_lexical_store as lexical
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_record_view as views
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_branches as branches
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as runtime
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.packed_loop as loopParser
	.use experimental.amigaos.binary_materialized_scope as materialized
	.pub
ABSENT = 0
ACTIVE = 1
INACTIVE = 2
UNKNOWN = 3
PROOF_VISITING = 1
PROOF_PURE = 2
PROOF_UNCERTIFIED = 3
DEPTH_LIMIT = 32
RECORD_CAPACITY = 256; encoded byte length excludes its own byte
Decl	.struct
Rhs	.long ?  ; numeric record offset+1; zero has no eligible initializer
Proof	.byte ?
Activity	.byte ?
Reserved	.word ?
.endstruct
DECL_BYTES = Decl.Reserved+2
Boundary	.struct
Offset	.long ?  ; first following record
Activity	.word ?
Reserved	.word ?
.endstruct
BOUNDARY_BYTES = Boundary.Reserved+2
State	.struct
Declarations	.res memory.Block.Used+4
Boundaries	.res memory.Block.Used+4
Values	.res memory.Block.Used+4
Defined	.res memory.Block.Used+4
Sections	.res memory.Block.Used+4
Records	.long ?  ; borrowed record-view Frame for this preparation call
End	.long ?  ; logical stream bytes
Cursor	.long ?
Next	.long ?
Record	.res RECORD_CAPACITY  ; one packed record, independent of recursive view seeks
BufferView	.res views.FRAME_BYTES
Scope	.long ?
Base	.long ?
Count	.long ?
UnknownDepth	.word ?
ProofDepth	.word ?
LoopDepth	.word ?
Veto	.word ?
Reserved	.word ?
Context	.res pkg.CONTEXT_BYTES
Branches	.res branches.STATE_BYTES
.endstruct
STATE_BYTES = State.Branches+branches.STATE_BYTES
	.section bss, kind=bss
	.priv
Current	.res long, 1  ; synchronous proof callback; cleared before return
	.endsection
	.section code, kind=code
	.pub
; A0=State,A1=bounded compiled records,D0=bytes,A2=scope,A3=package.
; Contiguous adapter for prepareView. D0/CCR=status; others preserved.
; Release remains required on every exit for certificate-owned allocations.
prepare	.block
	movem.l d1/a0-a1/a4, -(sp)
	movea.l a0, a4
	move.l d0, d1
	lea State.BufferView(a4), a0
	jsr views.initBuffer
	bne.w done
	lea State.BufferView(a4), a1
	movea.l a4, a0
	move.l d1, d0
	bsr.w prepareView
	move.l d0, -(sp)
	lea State.BufferView(a4), a0
	jsr views.finish
	move.l (sp)+, d0
done
	movem.l (sp)+, d1/a0-a1/a4
	tst.l d0
	rts
	.bend  ; prepare

; A0=State,A1=record-view Frame,D0=bytes,A2=scope,A3=package.
; Only unconditional immutable scalar provenance can certify a guard.
; The shared branch evaluator sees one copied record; recursive provenance
; proofs copy each initializer onto their bounded stack before another seek.
; Unknown proofs retain conservative owner alternatives. No candidate code
; executes, and no source-visible scalar cells change.
; D0/CCR=status; other registers preserved. Caller retains/finishes the view.
; Release is required on every exit for certificate-owned allocations.
prepareView	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l a1, State.Records(a6)
	cmp.l views.Frame.Bytes(a1), d0
	bne.w bad
	move.l d0, State.End(a6)
	clr.l State.Cursor(a6)
	clr.l State.Next(a6)
	move.l a2, State.Scope(a6)
	moveq #0, d0
	move.w layout.State.Base(a2), d0
	move.l d0, State.Base(a6)
	moveq #0, d1
	move.w layout.State.Count(a2), d1
	move.l d1, State.Count(a6)
	clr.w State.UnknownDepth(a6)
	clr.w State.ProofDepth(a6)
	clr.w State.LoopDepth(a6)
	clr.l State.Boundaries+memory.Block.Used(a6)
	lsl.l #3, d1
	move.l d1, d0
	lea State.Declarations(a6), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
clear
	tst.l d1
	beq.w context
	clr.l (a1)+
	subq.l #4, d1
	bra.w clear
context
	lea State.Context(a6), a1
	moveq #pkg.CONTEXT_BYTES/4-1, d0
clearContext
	clr.l (a1)+
	dbf d0, clearContext
	move.l a3, State.Context+pkg.Context.Package(a6)
	lea State.Values(a6), a0
	move.l State.Count(a6), d0
	lsl.l #3, d0
	bsr.w allocate
	bne.w bad
	lea State.Defined(a6), a0
	move.l State.Count(a6), d0
	bsr.w allocate
	bne.w bad
	lea State.Sections(a6), a0
	move.l State.Count(a6), d0
	bsr.w allocate
	bne.w bad
	movea.l State.Values+memory.Block.Pointer(a6), a1
	move.l State.Base(a6), d0
	lsl.l #3, d0
	suba.l d0, a1
	move.l a1, State.Context+pkg.Context.Values(a6)
	movea.l State.Defined+memory.Block.Pointer(a6), a1
	suba.l State.Base(a6), a1
	move.l a1, State.Context+pkg.Context.Defined(a6)
	movea.l State.Sections+memory.Block.Pointer(a6), a1
	suba.l State.Base(a6), a1
	move.l a1, State.Context+pkg.Context.SectionIds(a6)
	move.l State.Base(a6), d0
	add.l State.Count(a6), d0
	move.l d0, State.Context+pkg.Context.Count(a6)
inventory
	move.l State.Cursor(a6), d0
	cmp.l State.End(a6), d0
	beq.w acquire
	bsr.w recordEnd
	bne.w bad
	bsr.w scanLoop
	bne.w bad
	tst.w State.LoopDepth(a6)
	bne.w inventoryNext
	cmpi.l #source.DECLARATION_HEADER_BYTES+1, d7
	blo.w inventoryNext
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, source.DeclarationRecord.Name+source.NameToken.Kind(a4)
	bhi.w inventoryNext
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, source.DeclarationRecord.Marker(a4)
	bne.w inventoryNext
	moveq #0, d0
	move.w source.DeclarationRecord.Name+source.NameToken.Id(a4), d0
	bsr.w row
	bne.w bad
	movea.l State.Scope(a6), a2
	movem.l a0-a1, -(sp)
	lea layout.LEXICAL_STATE(a2), a0
	lea LexicalEntry, a1
	jsr lexical.readEntry
	movem.l (sp)+, a0-a1
	bne.w bad
	lea LexicalEntry, a2
	move.w records.Entry.Flags(a2), d1
	andi.w #records.MUTABLE+records.ITERATOR+records.LABEL_VALUE+records.CONDITIONAL_VALUE, d1
	bne.w inventoryNext
	tst.l Decl.Rhs(a0)
	bne.w duplicate
	move.l State.Cursor(a6), d0
	addq.l #1, d0
	move.l d0, Decl.Rhs(a0)
	bra.w inventoryNext
duplicate
	move.b #PROOF_UNCERTIFIED, Decl.Proof(a0)
inventoryNext
	move.l State.Next(a6), State.Cursor(a6)
	bra.w inventory
acquire
	tst.w State.LoopDepth(a6)
	bne.w bad
	lea State.Branches(a6), a0
	moveq #0, d0
	movea.l d0, a1
	lea State.Record(a6), a2
	moveq #1, d0
	jsr branches.beginPass
	jsr branches.begin
	lea proofExpression, a1
	move.l a1, branches.State.Proof(a0)
	move.l a6, Current
	clr.l State.Cursor(a6)
line
	move.l State.Cursor(a6), d0
	cmp.l State.End(a6), d0
	beq.w complete
	bsr.w recordEnd
	bne.w bad
	bsr.w scanLoop
	bne.w bad
	; Branch trace uses this temporary anchor only to subtract a logical offset.
	move.l a4, d0
	sub.l State.Cursor(a6), d0
	move.l d0, State.Branches+branches.State.Records(a6)
	movea.l a4, a0
	movea.l a5, a1
	lea State.Context(a6), a2
	lea State.Branches(a6), a3
	clr.w State.Veto(a6)
	jsr branches.step
	cmpi.l #branches.RESULT_INVALID, d0
	bne.w branchReady
	tst.w State.Veto(a6)
	beq.w bad  ; malformed controls are not uncertified ownership
	; A vetoed opening guard has already pushed its shared branch slot.
	; A vetoed elseif retains that slot. Mark the entire remaining chain unknown.
	move.w State.Branches+branches.State.Depth(a6), d0
	beq.w bad
	move.w d0, State.UnknownDepth(a6)
branchReady
	tst.w State.UnknownDepth(a6)
	beq.w certified
	move.w State.Branches+branches.State.Depth(a6), d0
	cmp.w State.UnknownDepth(a6), d0
	blo.w endedUnknown
	clr.w State.Branches+branches.State.Active(a6)
	moveq #UNKNOWN, d6
	bra.w statusReady
endedUnknown
	clr.w State.UnknownDepth(a6)
certified
	moveq #ACTIVE, d6
	tst.w State.Branches+branches.State.Active(a6)
	bne.w statusReady
	moveq #INACTIVE, d6
statusReady
	btst #source.FLAG_OMIT_BIT, source.RecordHeader.Flags(a4)
	beq.w regionReady
	moveq #INACTIVE, d6
regionReady
	cmpi.l #ACTIVE, d6
	bne.w retainBoundary
	tst.w State.LoopDepth(a6)
	beq.w retainBoundary
	moveq #UNKNOWN, d6  ; unmaterialized loops have no executed declaration yet
retainBoundary
	; Only changes retain a boundary; offsets are independent of memory growth.
	lea State.Boundaries(a6), a0
	move.l memory.Block.Used(a0), d2
	beq.w saveBoundary
	movea.l memory.Block.Pointer(a0), a1
	suba.w #BOUNDARY_BYTES, a1
	cmp.w Boundary.Activity(a1, d2.l), d6
	beq.w declarationRecord
saveBoundary
	move.l d2, d0
	addi.l #BOUNDARY_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	move.l State.Next(a6), Boundary.Offset(a1)
	move.w d6, Boundary.Activity(a1)
	clr.w Boundary.Reserved(a1)
	addi.l #BOUNDARY_BYTES, memory.Block.Used(a0)
declarationRecord
	cmpi.l #source.DECLARATION_HEADER_BYTES, d7
	blo.w next
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, source.DeclarationRecord.Name+source.NameToken.Kind(a4)
	bhi.w next
	moveq #0, d0
	move.b source.DeclarationRecord.Marker(a4), d0
	cmpi.b #source.TOKEN_LABEL_DECLARATION, d0
	beq.w declarationReady
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, d0
	beq.w declarationReady
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, d0
	beq.w declarationReady
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, d0
	beq.w declarationReady
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, d0
	bne.w next
declarationReady
	moveq #0, d0
	move.w source.DeclarationRecord.Name+source.NameToken.Id(a4), d0
	bsr.w row
	bne.w bad
	moveq #0, d0
	move.b Decl.Activity(a0), d0
	cmpi.b #UNKNOWN, d0
	beq.w next
	cmpi.b #UNKNOWN, d6
	beq.w saveActivity
	cmpi.b #ACTIVE, d0
	beq.w next
saveActivity
	move.b d6, Decl.Activity(a0)
next
	move.l State.Next(a6), State.Cursor(a6)
	bra.w line
complete
	tst.w State.LoopDepth(a6)
	bne.w bad
	lea State.Branches(a6), a0
	jsr branches.end
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	clr.l Current
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepareView

; A0=State,D0=record offset. D0=ACTIVE/INACTIVE/UNKNOWN; others preserved.
; Boundaries are sorted offsets. Binary search gives the preceding activity.
activity	.block
	movem.l d1-d5/a1, -(sp)
	move.l d0, d5
	moveq #0, d1
	move.l State.Boundaries+memory.Block.Used(a0), d2
	lsr.l #3, d2
	movea.l State.Boundaries+memory.Block.Pointer(a0), a1
	moveq #ACTIVE, d0
search
	cmp.l d2, d1
	bhs.w done
	move.l d1, d3
	add.l d2, d3
	lsr.l #1, d3
	move.l d3, d4
	lsl.l #3, d4
	cmp.l Boundary.Offset(a1, d4.l), d5
	blo.w earlier
	moveq #0, d0
	move.w Boundary.Activity(a1, d4.l), d0
	move.l d3, d1
	addq.l #1, d1
	bra.w search
earlier
	move.l d3, d2
	bra.w search
done
	movem.l (sp)+, d1-d5/a1
	rts
	.bend  ; activity
; A0=State,D0=canonical declaration ID. D0=ABSENT/ACTIVE/INACTIVE/UNKNOWN.
declaration	.block
	movem.l d1/a0/a6, -(sp)
	movea.l a0, a6
	bsr.w row
	bne.w noRow
	moveq #0, d0
	move.b Decl.Activity(a0), d0
	bra.w done
noRow
	moveq #ABSENT, d0
done
	movem.l (sp)+, d1/a0/a6
	rts
	.bend  ; declaration
; A0=State. Free temporary certificates and branch trace on all exits.
release	.block
	movem.l d0/a0/a1, -(sp)
	lea State.Declarations(a0), a0
	jsr memory.release
	movea.l 4(sp), a0
	lea State.Boundaries(a0), a0
	jsr memory.release
	movea.l 4(sp), a0
	lea State.Values(a0), a0
	jsr memory.release
	movea.l 4(sp), a0
	lea State.Defined(a0), a0
	jsr memory.release
	movea.l 4(sp), a0
	lea State.Sections(a0), a0
	jsr memory.release
	movea.l 4(sp), a0
	lea State.Branches(a0), a0
	jsr branches.release
	movea.l 4(sp), a0
	clr.l State.Declarations+memory.Block.Used(a0)
	clr.l State.Boundaries+memory.Block.Used(a0)
	clr.l State.Values+memory.Block.Used(a0)
	clr.l State.Defined+memory.Block.Used(a0)
	clr.l State.Sections+memory.Block.Used(a0)
	clr.l State.Count(a0)
	clr.l State.Base(a0)
	clr.l State.Records(a0)
	clr.l State.End(a0)
	clr.l State.Cursor(a0)
	clr.l State.Next(a0)
	clr.l State.Scope(a0)
	clr.l State.Branches+branches.State.Proof(a0)
	lea State.Context(a0), a1
	move.w #(pkg.CONTEXT_BYTES/4)-1, d0
clearContext
	clr.l (a1)+
	dbf d0, clearContext
	movem.l (sp)+, d0/a0/a1
	tst.l d0
	rts
	.bend  ; release
	.priv
; A0=Block,D0=bytes. Zero a private scalar view; D0/CCR=status.
; Other registers preserved. Values keep full signed i64 pairs and EXVM provenance.
allocate	.block
	movem.l d1/a1, -(sp)
	move.l d0, d1
	jsr memory.reserveExact
	bne.w done
	move.l d1, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
clear
	tst.l d1
	beq.w done
	clr.b (a1)+
	subq.l #1, d1
	bra.w clear
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; allocate

; A4/D7=bounded record,A6=State. Shared PRVM classifies unmaterialized loops.
; D0/CCR=status; D1/A0-A1 clobbered. Materialized envelopes already represent
; preparation-established iterations, so they do not open an unknown region.
scanLoop	.block
	cmpi.l #materialized.Record.Kind+1, d7
	blo.w classify
	cmpi.b #materialized.TOKEN, materialized.Record.Kind(a4)
	beq.w good
classify
	movea.l a4, a0
	move.l d7, d0
	movea.l State.Context+pkg.Context.Package(a6), a1
	move.l pkg.Header.LoopPlanBytes(a1), d1
	adda.l pkg.Header.LoopPlan(a1), a1
	jsr loopParser.classify
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w bad
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w closing
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w closing
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	beq.w good
	cmpi.w #DEPTH_LIMIT, State.LoopDepth(a6)
	bhs.w bad
	addq.w #1, State.LoopDepth(a6)
	bra.w good
closing
	tst.w State.LoopDepth(a6)
	beq.w bad
	subq.w #1, State.LoopDepth(a6)
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; scanLoop

; A6=State. Copy the logical cursor's record before recursive proofs seek.
; Return D7=width,A4=record,A5=end,D0/CCR=status. Clobbers D1-D2/A0-A1.
recordEnd	.block
	movea.l State.Records(a6), a0
	move.l State.Cursor(a6), d1
	jsr views.seek
	bne.w bad
	cmpi.l #source.RECORD_HEADER_BYTES, d2
	blo.w bad
	moveq #0, d7
	move.b source.RecordHeader.Length(a1), d7
	addq.w #1, d7
	cmpi.l #source.RECORD_HEADER_BYTES, d7
	blo.w bad
	cmp.l d2, d7
	bhi.w bad
	add.l d7, d1
	bcs.w bad
	cmp.l State.End(a6), d1
	bhi.w bad
	move.l d1, State.Next(a6)
	lea State.Record(a6), a4
	movea.l a4, a5
	move.l d7, d2
copy
	move.b (a1)+, (a5)+
	subq.w #1, d2
	bne.w copy
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; recordEnd

; A6=State,D0=initializer record offset,A0=RECORD_CAPACITY-byte destination.
; Return A0=RHS,A1=end,D0/CCR=status; other registers preserved.
; Each recursive caller owns its copy until its proof/evaluation returns.
initializer	.block
	movem.l d1-d3/a2, -(sp)
	movea.l a0, a2
	move.l d0, d1
	movea.l State.Records(a6), a0
	jsr views.seek
	bne.w bad
	cmpi.l #source.DECLARATION_HEADER_BYTES, d2
	blo.w bad
	moveq #0, d3
	move.b source.RecordHeader.Length(a1), d3
	addq.w #1, d3
	cmpi.l #source.DECLARATION_HEADER_BYTES, d3
	blo.w bad
	cmp.l d2, d3
	bhi.w bad
	add.l d3, d1
	bcs.w bad
	cmp.l State.End(a6), d1
	bhi.w bad
	movea.l a2, a0
copy
	move.b (a1)+, (a2)+
	subq.w #1, d3
	bne.w copy
	movea.l a2, a1
	adda.w #source.DeclarationRecord.Rhs, a0
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a2
	tst.l d0
	rts
	.bend  ; initializer
; A6=State,D0=ID. D0=index,A0=row,CCR=status.
row	.block
	sub.l State.Base(a6), d0
	bcs.w bad
	cmp.l State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	lsl.l #3, d1
	movea.l State.Declarations+memory.Block.Pointer(a6), a0
	adda.l d1, a0
	cmp.l d0, d0
	rts
bad
	moveq #1, d0
	tst.l d0
	rts
	.bend  ; row
; Shared branches.evaluate invokes this before reading any guard value.
; A0..A1=compiled operand; D0/CCR=0 certified,1 uncertified; others preserved.
proofExpression	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l Current, a6
	bsr.w expressionPure
	tst.l d0
	beq.w done
	move.w #1, State.Veto(a6)
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; proofExpression
; A0..A1=compact scalar wrapper,A6=State. Recursive symbol provenance proof.
expressionPure	.block
	movem.l d1-d7/a0-a5, -(sp)
	move.l a1, d0
	sub.l a0, d0
	beq.w unknown
	cmpi.b #abi.PRVM_TOKEN_KIND_NUMBER, (a0)
	beq.w foldedNumber
	cmpi.b #source.TOKEN_WIDE_NUMBER, (a0)
	beq.w foldedWide
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, (a0)
	bls.w foldedSymbol
	cmpi.l #3, d0
	blo.w unknown
	cmpi.b #expr.COMPILED_TAG, (a0)+
	bne.w unknown
	moveq #0, d0
	move.b (a0)+, d0
	movea.l a0, a5
	adda.l d0, a5
	cmpa.l a1, a5
	bne.w unknown
	bra.w opcode
foldedNumber
	cmpi.l #source.NUMBER_BYTES, d0
	bne.w unknown
	bra.w pure
foldedWide
	cmpi.l #source.WIDE_NUMBER_BYTES, d0
	bne.w unknown
	bra.w pure
foldedSymbol
	cmpi.l #source.NAME_TOKEN_BYTES, d0
	bne.w unknown
	cmpi.b #1, source.NameToken.Qualifier(a0)
	bhi.w unknown
	moveq #0, d0
	move.w source.NameToken.Id(a0), d0
	bsr.w symbolPure
	bra.w done
opcode
	cmpa.l a5, a0
	bhs.w unknown
	moveq #0, d0
	move.b (a0)+, d0
	beq.w end
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, d0
	beq.w symbol
	cmpi.b #runtime.COMPACT_I8, d0
	beq.w one
	cmpi.b #runtime.COMPACT_I16, d0
	beq.w two
	cmpi.b #runtime.COMPACT_I32, d0
	beq.w four
	cmpi.b #runtime.COMPACT_U32, d0
	beq.w four
	cmpi.b #runtime.COMPACT_I64, d0
	beq.w eight
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_UNARY, d0
	beq.w one
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_BINARY, d0
	beq.w one
	cmpi.b #runtime.SELECT, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_NEGATE, d0
	blo.w unknown
	cmpi.b #runtime.COMPACT_MULTIPLY, d0
	bls.w opcode
	bra.w unknown
one
	addq.l #1, a0
	bra.w operand
two
	addq.l #2, a0
	bra.w operand
four
	addq.l #4, a0
	bra.w operand
eight
	addq.l #8, a0
operand
	cmpa.l a5, a0
	bhi.w unknown
	bra.w opcode
symbol
	move.l a5, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w unknown
	moveq #0, d0
	move.b 1(a0), d0
	lsl.w #8, d0
	move.b (a0), d0
	addq.l #2, a0
	movem.l a0/a5, -(sp)
	bsr.w symbolPure
	movem.l (sp)+, a0/a5
	tst.l d0
	bne.w unknown
	bra.w opcode
end
	cmpa.l a5, a0
	bne.w unknown
pure
	moveq #0, d0
	bra.w done
unknown
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; expressionPure
; D0=ID,A6=State. A declaration's whole dependency closure must be pure.
symbolPure	.block
	movem.l d1-d3/a0-a3, -(sp)
	suba.w #RECORD_CAPACITY, sp
	move.l d0, d3
	bsr.w row
	bne.w unknown
	cmpi.b #PROOF_PURE, Decl.Proof(a0)
	beq.w valueAvailable
	tst.b Decl.Proof(a0)
	bne.w unknown
	tst.l Decl.Rhs(a0)
	beq.w unknown
	cmpi.w #DEPTH_LIMIT, State.ProofDepth(a6)
	bhs.w unknown
	movea.l a0, a3
	move.b #PROOF_VISITING, Decl.Proof(a3)
	move.l Decl.Rhs(a3), d0
	subq.l #1, d0
	movea.l sp, a0
	bsr.w initializer
	bne.w impure
	addq.w #1, State.ProofDepth(a6)
	bsr.w expressionPure
	subq.w #1, State.ProofDepth(a6)
	tst.l d0
	bne.w impure
	; The stack copy remains valid after all recursively proven dependencies
	; acquire their scalar cells; evaluate this exact initializer once.
	lea State.Context(a6), a2
	clr.l pkg.Context.Kind(a2)
	clr.l pkg.Context.High(a2)
	clr.l pkg.Context.Provenance(a2)
	jsr expr.evaluate
	bne.w impure
	cmpa.l a1, a0
	bne.w impure
	tst.l d2
	bne.w impure
	tst.l pkg.Context.Kind(a2)
	bne.w impure
	move.l d3, d0
	lsl.l #3, d0
	movea.l pkg.Context.Values(a2), a0
	move.l d1, runtime.Value.Low(a0, d0.l)
	move.l pkg.Context.High(a2), runtime.Value.High(a0, d0.l)
	movea.l pkg.Context.SectionIds(a2), a0
	move.l pkg.Context.Provenance(a2), d0
	move.b d0, 0(a0, d3.l)
	movea.l pkg.Context.Defined(a2), a0
	move.b #1, 0(a0, d3.l)
	move.b #PROOF_PURE, Decl.Proof(a3)
valueAvailable
	movea.l State.Context+pkg.Context.Defined(a6), a1
	tst.b 0(a1, d3.l)
	beq.w unknown
	moveq #0, d0
	bra.w done
impure
	move.b #PROOF_UNCERTIFIED, Decl.Proof(a3)
unknown
	moveq #1, d0
done
	adda.w #RECORD_CAPACITY, sp
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; symbolPure
	.endsection
	.section bss, kind=bss
; Accessor scratch is consumed before any callback; no Entry view escapes.
LexicalEntry	.res byte, records.ENTRY_BYTES
	.endsection
	.endmodule
