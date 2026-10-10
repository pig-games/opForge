; Resolve absolute constant graphs before layout, using only packed records.
; @opforge-owner: experimental.amigaos.binary_dependencies

	.module experimental.amigaos.binary_dependencies
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_numeric_bindings as bindings
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as runtime
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_repeat_inventory as repeatInventory
	.use experimental.amigaos.binary_repeat_dependencies as localGraph
	.use experimental.amigaos.binary_repeat_activation as activation
	.use experimental.amigaos.binary_dependency_layout as graph
	.use prvm.amigaos.packed_branch as branch
	.pub
ABSOLUTE = graph.ABSOLUTE
	.priv
PENDING = graph.PENDING
VISITING = graph.VISITING
LAYOUT = graph.LAYOUT
LABEL = graph.LABEL
MUTABLE_SNAPSHOT = graph.SNAPSHOT
MODE_LOCAL = 1
MODE_BRANCH = 2
MODE_GLOBAL_BRANCH = 3
Entry	.struct
Id	.word ?
Cursor	.word ?
Flags	.word ?
Reserved	.word ?
.endstruct
ENTRY_BYTES = 8
	.section bss, kind=bss
Base	.res long, 1
Limit	.res long, 1
Depth	.res long, 1
Constants	.res long, 1
Locals	.res long, 1
GlobalCandidates	.res long, 1
States	.res byte, memory.Block.Used+4
Offsets	.res byte, memory.Block.Used+4
Local	.res long, 1
RootCursor	.res long, 1
Mode	.res word, 1
BranchDepth	.res word, 1
	.align 4
Scratch	.res byte, memory.Block.Used+4
	.endsection
	.section code, kind=code
	.pub
; A0=packed records,D0=record bytes,A1=optional declaration map,
; D1=global conditional ID count,A2=Context with zeroed symbol arrays.
; Scoped declarations belong to their iteration owner, outside the eager graph.
; D0/CCR=status; other registers preserved. Graph states and expression offsets
; stay in private storage. Absolute values retain Defined=ABSOLUTE; other entries
; are cleared before return. Scratch is bounded by source IDs and always freed.
resolve	.block
	clr.w Mode
start
	movem.l d1-d7/a0-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	tst.w Mode
	bne.w localInputs
	move.l a0, Base
	move.l a1, Locals
	move.l d1, GlobalCandidates
	add.l a0, d0
	bcs.w bad
	move.l d0, Limit
	clr.l Depth
	clr.l Constants
	movea.l a2, a6
	move.l pkg.Context.Count(a6), d0
	beq.w bad
	cmpi.l #65536, d0
	bhi.w bad
	movea.l pkg.Context.Package(a6), a0
	moveq #0, d1
	move.w pkg.Header.NameCount(a0), d1
	sub.l d1, d0
	bcs.w bad
	bsr.w graphStorage
	bne.w bad
	; Source IDs are bounded by Context.Count; dependency scratch is owned and
	; grows from the actual number of constants, independently of preparation.
	bsr.w index
	tst.l d0
	bne.w clearFailure
rootsReady
	tst.l Constants
	beq.w success
	move.l Constants, d0
	lsl.l #3, d0
	lea Scratch, a0
	jsr memory.reserve
	bne.w clearFailure
	movea.l Offsets+memory.Block.Pointer, a4
	movea.l States+memory.Block.Pointer, a5
	movea.l pkg.Context.Package(a6), a0
	moveq #0, d7
	move.w pkg.Header.NameCount(a0), d7
root
	tst.w Mode
	beq.w globalRoot
	cmpi.w #MODE_GLOBAL_BRANCH, Mode
	beq.w globalRoot
	movea.l Local, a0
	move.l RootCursor, d0
	cmp.l graph.Frame.Count(a0), d0
	bhs.w success
	add.l d0, d0
	movea.l graph.Frame.Ids(a0), a0
	moveq #0, d7
	move.w 0(a0, d0.l), d7
	bra.w rootState
globalRoot
	cmp.l pkg.Context.Count(a6), d7
	bhs.w success
rootState
	cmpi.b #PENDING, 0(a5, d7.l)
	bne.w nextRoot
	move.l d7, d0
	bsr.w push
	bne.w clearFailure
visit
	move.l Depth, d0
	beq.w nextRoot
	subq.l #1, d0
	lsl.l #3, d0
	lea Scratch, a0
	movea.l memory.Block.Pointer(a0), a3
	adda.l d0, a3
	moveq #0, d4
	move.w Entry.Id(a3), d4
	move.l d4, d5
	lsl.l #3, d5
	move.l d4, d0
	lsl.l #graph.OFFSET_SHIFT, d0
	movea.l Base, a0
	adda.l graph.Offset.Expression(a4, d0.l), a0
	moveq #0, d6
	move.b 1(a0), d6
	addq.w #2, d6
	moveq #0, d3
	move.w Entry.Cursor(a3), d3
	cmp.w d6, d3
	bhs.w clearFailure
	moveq #0, d0
	move.b 0(a0, d3.w), d0
	beq.w completed
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, d0
	beq.w dependency
	moveq #1, d1
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_CURRENT_ADDR, d0
	beq.w current
	cmpi.b #runtime.COMPACT_NEGATE, d0
	beq.w advance
	cmpi.b #runtime.COMPACT_ADD, d0
	beq.w advance
	cmpi.b #runtime.COMPACT_SUBTRACT, d0
	beq.w advance
	cmpi.b #runtime.COMPACT_MULTIPLY, d0
	beq.w advance
	cmpi.b #runtime.SELECT, d0
	beq.w advance
	moveq #2, d1
	cmpi.b #runtime.COMPACT_I8, d0
	beq.w advance
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_UNARY, d0
	beq.w advance
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_BINARY, d0
	beq.w advance
	moveq #3, d1
	cmpi.b #runtime.COMPACT_I16, d0
	beq.w advance
	moveq #5, d1
	cmpi.b #runtime.COMPACT_I32, d0
	beq.w advance
	cmpi.b #runtime.COMPACT_U32, d0
	beq.w advance
	moveq #9, d1
	cmpi.b #runtime.COMPACT_I64, d0
	bne.w clearFailure
advance
	add.w d1, d3
	cmp.w d6, d3
	bhs.w clearFailure
	move.w d3, Entry.Cursor(a3)
	bra.w visit
current
	ori.w #1, Entry.Flags(a3)
	bra.w advance
dependency
	move.w d3, d1
	addq.w #3, d1
	cmp.w d6, d1
	bhs.w clearFailure
	moveq #0, d0
	move.b 2(a0, d3.w), d0
	lsl.w #8, d0
	move.b 1(a0, d3.w), d0
	cmp.l pkg.Context.Count(a6), d0
	bhs.w clearFailure
	; A member proxy depends on a layout-produced record, outside the scalar DAG.
	move.l d0, -(sp)
	bsr.w memberPlan
	cmpi.l #values.BOUNDS, d0
	beq.w ordinaryDependency
	tst.l d0
	beq.w memberDependency
	addq.l #4, sp
	bra.w clearFailure
memberDependency
	move.l (sp)+, d0
	bra.w dependentLayout
ordinaryDependency
	move.l (sp)+, d0
	moveq #0, d2
	move.b 0(a5, d0.l), d2
	tst.w Mode
	beq.w dependencyState
	movem.l a0, -(sp)
	movea.l Local, a0
	jsr localGraph.state
	movem.l (sp)+, a0
dependencyState
	cmpi.b #PENDING, d2
	beq.w descend
	cmpi.b #ABSOLUTE, d2
	beq.w absoluteDependency
	cmpi.b #LAYOUT, d2
	beq.w dependentLayout
	cmpi.b #graph.MUTABLE, d2
	beq.w dependentMutable
	cmpi.b #MUTABLE_SNAPSHOT, d2
	beq.w dependentMutable
	cmpi.b #LABEL, d2
	bne.w clearFailure  ; missing names and visiting nodes are errors
	bra.w dependentLayout
absoluteDependency
	; The scalar DAG must never interpret a compound arena offset as an i64.
	; Bare aliases retain their kind when replayed by expression.evaluateValue.
	movem.l d1/a0, -(sp)
	move.l d0, d1
	movea.l pkg.Context.Owner(a6), a0
	move.l a0, d0
	beq.w absoluteScalar
	jsr values.getKind
	tst.l d0
	bne.w absoluteFailed
	tst.l d2
	beq.w absoluteScalar
	movem.l (sp)+, d1/a0
	bra.w dependentLayout
absoluteScalar
	movem.l (sp)+, d1/a0
	bra.w consumed
absoluteFailed
	movem.l (sp)+, d1/a0
	bra.w clearFailure
dependentLayout
	ori.w #1, Entry.Flags(a3)
	bra.w consumed
dependentMutable
	ori.w #2, Entry.Flags(a3)
consumed
	move.w d1, Entry.Cursor(a3)
	bra.w visit
descend
	; Parent stays on this symbol until its child has completed.
	bsr.w push
	bne.w clearFailure
	bra.w visit
completed
	addq.w #1, d3
	cmp.w d6, d3
	bne.w clearFailure
	tst.w Entry.Flags(a3)
	bne.w deferred
	movea.l a0, a1
	adda.l d6, a1
	movea.l a6, a2
	jsr expr.evaluate
	tst.l d0
	bne.w clearFailure
	tst.l d2
	bne.w clearFailure
	cmpa.l a1, a0
	bne.w clearFailure
	movea.l pkg.Context.Values(a6), a0
	move.l d1, runtime.Value.Low(a0, d5.l)
	move.l pkg.Context.High(a6), runtime.Value.High(a0, d5.l)
	movea.l pkg.Context.Defined(a6), a0
	move.b #ABSOLUTE, 0(a0, d4.l)
	move.b #ABSOLUTE, 0(a5, d4.l)
	bra.w pop
deferred
	move.b #LAYOUT, 0(a5, d4.l)
	btst #1, Entry.Flags+1(a3)
	beq.w pop
	; A0 is the compiled expression, nine bytes after its record header.
	; Only the dependency owner marks readonly snapshots; bodies stay immutable.
	ori.b #source.FLAG_MUTABLE_SNAPSHOT, -8(a0)
	move.b #MUTABLE_SNAPSHOT, 0(a5, d4.l)
pop
	subq.l #1, Depth
	bra.w visit
nextRoot
	tst.w Mode
	beq.w nextGlobalRoot
	cmpi.w #MODE_GLOBAL_BRANCH, Mode
	beq.w nextGlobalRoot
	addq.l #1, RootCursor
	bra.w root
nextGlobalRoot
	addq.l #1, d7
	bra.w root
success
	moveq #0, d7
	bra.w normalize
clearFailure
	moveq #1, d7
normalize
	tst.w Mode
	bne.w normalized
	movea.l pkg.Context.Package(a6), a0
	moveq #0, d2
	move.w pkg.Header.NameCount(a0), d2
	move.l pkg.Context.Count(a6), d1
	sub.l d2, d1
	beq.w normalized
	movea.l pkg.Context.Values(a6), a0
	movea.l pkg.Context.Defined(a6), a1
	adda.l d2, a1
	lsl.l #3, d2
	adda.l d2, a0
clearLoop
	tst.l d7
	bne.w erase
	cmpi.b #ABSOLUTE, (a1)
	beq.w keep
erase
	clr.l (a0)
	clr.l 4(a0)
	clr.b (a1)
keep
	addq.l #8, a0
	addq.l #1, a1
	subq.l #1, d1
	bne.w clearLoop
normalized
	move.l d7, d0
	bra.w release
bad
	moveq #1, d0
	bra.w release
localInputs
	move.l a0, Local
	movea.l graph.Frame.Context(a0), a6
	move.l graph.Frame.Records(a0), Base
	move.l Base, d0
	add.l graph.Frame.RecordBytes(a0), d0
	bcs.w bad
	move.l d0, Limit
	move.l graph.Frame.Declarations(a0), Locals
	move.l States+memory.Block.Pointer, graph.Frame.States(a0)
	move.l Offsets+memory.Block.Pointer, graph.Frame.Offsets(a0)
	clr.l RootCursor
	clr.l Depth
	cmpi.w #MODE_LOCAL, Mode
	bne.w activateLocal
	jsr localGraph.init
	bra.w localReady
activateLocal
	jsr activation.select
localReady
	tst.l d0
	bne.w bad
	move.l d1, Constants
	bra.w rootsReady
release
	lea Scratch, a0
	jsr memory.release
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; resolve

; A0=graph.Frame whose IDs are one prepared owner's directly-owned range.
; D0/CCR=status; other registers preserved. Uses the same scalar DFS/evaluator
; as resolve; external mutable/label dependencies remain declaration-time work.
resolveLocal	.block
	move.w #MODE_LOCAL, Mode
	bra.w resolve.start
	.bend  ; resolveLocal

; A0=graph.Frame with selected branch control offset. Activate only directly
; owned scalar roots in this branch, then reuse the same DFS/evaluator.
; D0/CCR=status; other registers preserved. No inactive RHS is evaluated.
activateLocal	.block
	move.w #MODE_BRANCH, Mode
	bra.w resolve.start
	.bend  ; activateLocal

; A0=global owner Frame with selected control offset. Selected roots share the
; scalar DFS, while root traversal covers numeric source IDs rather than a loop.
; D0/CCR=status; other registers preserved.
activateGlobal	.block
	move.w #MODE_GLOBAL_BRANCH, Mode
	bra.w resolve.start
	.bend  ; activateGlobal

; A0=global owner Frame. Begin one source sweep: discard selected ownership
; offsets without discarding prior live values needed by forward guards.
; D0/CCR=0; other registers preserved.
beginGlobal	.block
	movem.l d1-d3/a0-a3, -(sp)
	movea.l graph.Frame.Declarations(a0), a1
	move.l a1, d0
	beq.w done
	movea.l graph.Frame.Context(a0), a2
	move.l pkg.Context.Count(a2), d2
	movea.l States+memory.Block.Pointer, a2
	movea.l Offsets+memory.Block.Pointer, a3
	moveq #0, d1
next
	cmpi.w #repeatInventory.IMMUTABLE+repeatInventory.CONDITIONAL, repeatInventory.Decl.Kind(a1)
	bne.w advance
	tst.w repeatInventory.Decl.Owner(a1)
	bne.w advance
	move.b #LAYOUT, 0(a2, d1.l)
	move.l d1, d3
	lsl.l #graph.OFFSET_SHIFT, d3
	clr.l graph.Offset.Expression(a3, d3.l)
advance
	addq.l #repeatInventory.DECL_BYTES, a1
	addq.l #1, d1
	cmp.l d2, d1
	blo.w next
done
	moveq #0, d0
	movem.l (sp)+, d1-d3/a0-a3
	rts
	.bend  ; beginGlobal

; Release graph storage after assembly/retry ownership has ended.
; Registers preserved; CCR reflects the caller's D0 status.
release	.block
	movem.l a0, -(sp)
	lea States, a0
	jsr memory.release
	lea Offsets, a0
	jsr memory.release
	lea Scratch, a0
	jsr memory.release
	movem.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; release

	.priv
; D0=source ID. D0/CCR=OK for a numeric member plan, BOUNDS absent,
; MALFORMED invalid. No values are evaluated during scalar dependency discovery.
memberPlan	.block
	movem.l d1-d2/a0-a2, -(sp)
	move.l d0, d1
	movea.l pkg.Context.Owner(a6), a0
	move.l a0, d0
	beq.w absent
	jsr bindings.getBinding
	bra.w done
absent
	moveq #values.BOUNDS, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; memberPlan

; D0=source ID. Mark visiting and append a bounded frame. Other registers saved.
push	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.l Depth, d1
	cmp.l Constants, d1
	bhs.w bad
	lsl.l #3, d1
	lea Scratch, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d1, a0
	move.w d0, Entry.Id(a0)
	move.w #2, Entry.Cursor(a0)
	clr.w Entry.Flags(a0)
	clr.w Entry.Reserved(a0)
	movea.l States+memory.Block.Pointer, a1
	move.b #VISITING, 0(a1, d0.l)
	addq.l #1, Depth
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; push

; Index declaration ownership and constant offsets; leave expression evaluation
; to the graph walk. Record bodies remain immutable; resolve owns the snapshot
; role flag on immutable declarations that depend on mutable values.
; Other registers preserved.
index	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l Base, a0
	movea.l Limit, a1
	movea.l pkg.Context.Package(a6), a2
	movea.l Offsets+memory.Block.Pointer, a4
	movea.l States+memory.Block.Pointer, a5
	clr.w BranchDepth
line
	cmpa.l a1, a0
	beq.w good
	bhi.w bad
	moveq #0, d6
	move.b (a0), d6
	addq.w #1, d6
	cmpi.w #4, d6
	blo.w bad
	move.l a1, d0
	sub.l a0, d0
	cmp.l d0, d6
	bhi.w bad
	cmpi.b #source.FLAG_ALLOWED, 1(a0)
	bhi.w bad
	btst #3, 1(a0)
	bne.w next
	tst.l GlobalCandidates
	beq.w rolesReady
	bsr.w indexBranch
	bne.w bad
rolesReady
	cmpi.w #9, d6
	blo.w next
	lea 4(a0), a3
	moveq #9, d2
	cmpi.b #7, (a3)
	beq.w directive
	cmpi.b #1, (a3)
	bhi.w next
	moveq #LABEL, d7
	cmpi.b #5, 4(a3)
	beq.w declaration
	moveq #PENDING, d7
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, 4(a3)
	beq.w declaration
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, 4(a3)
	beq.w declaration
	moveq #graph.MUTABLE, d7
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a3)
	beq.w declaration
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a3)
	bne.w next
declaration
	tst.b 3(a3)
	bne.w bad
	moveq #0, d4
	move.w 1(a3), d4
	cmp.w pkg.Header.NameCount(a2), d4
	blo.w bad
	cmp.l pkg.Context.Count(a6), d4
	bhs.w bad
	move.l Locals, d0
	beq.w enclosingDeclaration
	movea.l d0, a3
	move.l d4, d0
	lsl.l #repeatInventory.DECL_SHIFT, d0
	tst.w repeatInventory.Decl.Owner(a3, d0.l)
	bne.w next
	move.w repeatInventory.Decl.Kind(a3, d0.l), d1
	andi.w #repeatInventory.CONDITIONAL, d1
	beq.w knownOwnership
	tst.w BranchDepth
	beq.w unconditionalOwner
	tst.b 0(a5, d4.l)
	bne.w next
	move.b #LAYOUT, 0(a5, d4.l)
	bra.w next
unconditionalOwner
	move.w repeatInventory.Decl.Kind(a3, d0.l), d1
	andi.w #repeatInventory.UNCONDITIONAL, d1
	bne.w knownOwnership
	cmpi.b #LAYOUT, 0(a5, d4.l)
	bne.w eagerOwner
	clr.b 0(a5, d4.l)
eagerOwner
	ori.w #repeatInventory.UNCONDITIONAL, repeatInventory.Decl.Kind(a3, d0.l)
knownOwnership
	lea 4(a0), a3
enclosingDeclaration
	tst.b 0(a5, d4.l)
	beq.w firstDeclaration
	cmpi.b #graph.MUTABLE, d7
	bne.w bad
	cmpi.b #graph.MUTABLE, 0(a5, d4.l)
	; A retained branch may replace a readonly identity. Preserve its original
	; dependency ownership; selected runtime mutable execution rejects the write.
	; Still validate the replacement's numeric expression envelope below.
	bne.w declarationKind
firstDeclaration
	move.b d7, 0(a5, d4.l)
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, 4(a3)
	bne.w declarationKind
	cmpi.w #13, d6
	bne.w bad
	move.b #LAYOUT, 0(a5, d4.l)
	bra.w next
declarationKind
	cmpi.b #graph.MUTABLE, d7
	beq.w scalarDeclaration
	cmpi.b #PENDING, d7
	bne.w labelTail
scalarDeclaration
	cmpi.w #12, d6
	blo.w bad
	addq.l #5, a3
	cmpi.b #expr.VALUE_TAG, (a3)
	beq.w compoundDeclaration
	cmpi.b #expr.COMPILED_TAG, (a3)
	bne.w bad
	moveq #0, d0
	move.b 1(a3), d0
	beq.w bad
	addi.w #11, d0
	cmp.w d6, d0
	bne.w bad
	cmpi.b #runtime.EXPRVM_OPCODE_END, -1(a0, d6.w)
	bne.w bad
	cmpi.b #graph.MUTABLE, d7
	beq.w next
	move.l a3, d0
	sub.l Base, d0
	lsl.l #graph.OFFSET_SHIFT, d4
	move.l d0, graph.Offset.Expression(a4, d4.l)
	addq.l #1, Constants
	bra.w next
compoundDeclaration
	; Compound declarations are source-order snapshots, outside the scalar
	; immutable DAG. Scalar aliases inherit layout dependence through this ID.
	moveq #0, d0
	move.b 1(a3), d0
	beq.w bad
	addi.w #11, d0
	cmp.w d6, d0
	bne.w bad
	cmpi.b #runtime.EXPRVM_OPCODE_END, -1(a0, d6.w)
	bne.w bad
	cmpi.b #graph.MUTABLE, d7
	beq.w next
	move.b #LAYOUT, 0(a5, d4.l)
	bra.w next
labelTail
	; An explicit label may precede .end on the same record.
	cmpi.w #14, d6
	blo.w next
	addq.l #5, a3
	moveq #14, d2
	cmpi.b #7, (a3)
	bne.w next
directive
	cmpi.b #1, 1(a3)
	bhi.w next
	move.w 2(a3), d0
	cmp.w pkg.Header.EndDirective(a2), d0
	bne.w next
	tst.b 4(a3)
	bne.w bad
	cmp.w d2, d6
	bne.w bad
	bra.w good
next
	adda.l d6, a0
	bra.w line
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; index

; A0=bounded record,D6=extent,A2=package. Track branch ancestry through PRVM.
; D0/CCR=status; other registers preserved.
indexBranch	.block
	movem.l d1/a0-a1, -(sp)
	move.l d6, d0
	movea.l a2, a1
	adda.l pkg.Header.BranchPlan(a1), a1
	jsr branch.classify
	bne.w done
	cmpi.l #branch.IF, d1
	beq.w opening
	cmpi.l #branch.ENDIF, d1
	bne.w good
	tst.w BranchDepth
	beq.w bad
	subq.w #1, BranchDepth
	bra.w good
opening
	addq.w #1, BranchDepth
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; indexBranch

	.pub
; A1=Context. Discard provisional label/alias and mutable values before a fresh
; layout replay. Compile-time constants and incoming parameters are retained.
; Incoming compound parameters occupy a retained immutable owner prefix.
; Later compound declarations are discarded with their provisional cells.
; D0/CCR=status; other registers preserved. Owner validation failure leaves
; cells intact; caller must propagate failure before replaying layout.
resetLayout	.block
	movem.l d3-d4/a0/a2-a4, -(sp)
	movea.l pkg.Context.Owner(a1), a0
	move.l a0, d0
	beq.w ownerReady
	move.l pkg.Context.RetainedValues(a1), d0
	jsr values.rewind
	bne.w done
	; Rewind validated Count and the complete kinds extent. Clear provisional
	; tags alongside their cells; scalar-only sessions have no kinds allocation.
	movea.l values.Owner.Kinds+memory.Block.Pointer(a0), a0
ownerReady
	move.l pkg.Context.Count(a1), d3
	movea.l pkg.Context.Values(a1), a2
	movea.l pkg.Context.Defined(a1), a3
	movea.l pkg.Context.SectionIds(a1), a4
	moveq #0, d4
symbol
	cmpi.b #ABSOLUTE, (a3)
	beq.w retained
	clr.l (a2)
	clr.l 4(a2)
	clr.b (a3)
	clr.b (a4)
	move.l a0, d0
	beq.w retained
	clr.b 0(a0, d4.l)
retained
	addq.l #8, a2
	addq.l #1, a3
	addq.l #1, a4
	addq.l #1, d4
	subq.l #1, d3
	bne.w symbol
	moveq #0, d0
done
	movem.l (sp)+, d3-d4/a0/a2-a4
	tst.l d0
	rts
	.bend  ; resetLayout
; A6=context. Allocate private graph state and expression offsets once.
; Runtime Defined bytes remain exclusively numeric availability state.
; D0/CCR=status; other registers preserved.
graphStorage	.block
	movem.l d1/a0-a1, -(sp)
	move.l pkg.Context.Count(a6), d1
	lea States, a0
	move.l d1, d0
	jsr memory.reserve
	bne.w done
	move.l d1, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a0
	movea.l pkg.Context.Defined(a6), a1
copy
	move.b (a1)+, (a0)+
	subq.l #1, d1
	bne.w copy
	move.l pkg.Context.Count(a6), d0
	lsl.l #graph.OFFSET_SHIFT, d0
	lea Offsets, a0
	move.l d0, d1
	jsr memory.reserve
	bne.w done
	move.l d1, memory.Block.Used(a0)
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; graphStorage
	.endsection
	.endmodule
