; Preparation-owned declaration identities for scoped repetitions and selected globals.
; Replay consumes numeric IDs and offsets; lexical spelling stays in preparation.
; @opforge-owner: experimental.amigaos.binary_repeat_inventory
	.module experimental.amigaos.binary_repeat_inventory
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_materialized_scope as materialized
	.use prvm.amigaos.packed_branch as branch
	.use prvm.amigaos.packed_loop as parser
	.use prvm.amigaos.abi as abi
	.pub
LABEL = 1
IMMUTABLE = 2
MUTABLE = 3
COLLECTION = 4
UNCONDITIONAL = $200; eager global owner also exists; selected replacement rejects
CONDITIONAL = $100; immutable root requires an executed branch
LOOP_CONTROLLED = $400; global immutable root requires ordinary-loop execution
KIND_MASK = $ff
Decl	.struct
Owner	.word ?  ; one-based lexical loop identity; zero is enclosing/global
Kind	.word ?
Record	.long ?
	.endstruct
DECL_BYTES = Decl.Record+4
DECL_SHIFT = 3
PackedRecord	.struct
Length	.byte ?
Flags	.byte ?
SourceLine	.word ?
NameKind	.byte ?
NameId	.word ?
NameQualifier	.byte ?
Marker	.byte ?
Rhs	.byte ?
	.endstruct
Owner	.struct
Header	.long ?
First	.long ?  ; byte offset in contiguous declaration-ID storage
Count	.long ?
Cursor	.long ?  ; preparation-only scatter cursor
	.endstruct
OWNER_BYTES = Owner.Cursor+4
OWNER_SHIFT = 4
Frame	.struct
Parent	.word ?
Kind	.word ?
BranchBase	.word ?
Reserved	.word ?
	.endstruct
FRAME_BYTES = Frame.Reserved+2
FRAME_SHIFT = 3
DEPTH_LIMIT = 16
MaterializedFrame	.struct
Label	.word ?
Active	.word ?
Index	.long ?
OwnerDepth	.word ?
Padding	.word ?
.endstruct
MATERIALIZED_FRAME_BYTES = MaterializedFrame.Padding+2
State	.struct
Declarations	.res memory.Block.Used+4
Owners	.res memory.Block.Used+4
Ids	.res memory.Block.Used+4
SourceBase	.long ?
MemberKeys	.long ?  ; borrowed u16 canonical member key per actual symbol ID
Symbols	.long ?
Count	.long ?
Globals	.long ?  ; global IDs with conditional immutable candidates
DynamicGlobals	.long ?  ; global IDs requiring executed ordinary-loop activity
Current	.word ?
Depth	.word ?
BranchDepth	.word ?
BranchBase	.word ?
Frames	.res DEPTH_LIMIT*FRAME_BYTES
CollectionDepth	.word ?
CollectionPadding	.word ?
Collections	.res DEPTH_LIMIT*MATERIALIZED_FRAME_BYTES
	.endstruct
STATE_BYTES = State.Collections+DEPTH_LIMIT*MATERIALIZED_FRAME_BYTES
	.section code, kind=code
; A0=state,A1=compiled records,D0=record bytes,A2=context,A3=MemberKeys.
; D0/CCR=status; other registers preserved. Build once after lexical binding.
; Direct declarations include nested blocks/namespaces, but a nested scoped
; repetition owns its own IDs. This creates no collectible block markers.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a4
	movea.l a1, a5
	adda.l d0, a5
	move.l a1, State.SourceBase(a6)
	move.l a3, State.MemberKeys(a6)
	clr.l State.Count(a6)
	clr.l State.Globals(a6)
	clr.l State.DynamicGlobals(a6)
	clr.w State.Current(a6)
	clr.w State.Depth(a6)
	clr.w State.CollectionDepth(a6)
	clr.w State.BranchDepth(a6)
	clr.w State.BranchBase(a6)
	clr.l State.Owners+memory.Block.Used(a6)
	clr.l State.Ids+memory.Block.Used(a6)
	clr.l State.Declarations+memory.Block.Used(a6)
	move.l pkg.Context.Count(a2), d0
	cmpi.l #65536, d0
	bhi.w bad
	move.l d0, State.Symbols(a6)
scan
	cmpa.l a5, a4
	beq.w scanned
	bhi.w bad
	moveq #0, d4
	move.b PackedRecord.Length(a4), d4
	addq.w #1, d4
	cmpi.w #4, d4
	blo.w bad
	move.l a5, d0
	sub.l a4, d0
	cmp.l d0, d4
	bhi.w bad
	btst #source.FLAG_OMIT_BIT, PackedRecord.Flags(a4)
	bne.w next
	cmpi.w #materialized.Record.Kind+1, d4
	blo.w next
	cmpi.b #materialized.TOKEN, materialized.Record.Kind(a4)
	beq.w materializedRecord
	cmpi.w #PackedRecord.Rhs, d4
	blo.w next
	bsr.w branchDepth
	bne.w done
	movea.l a4, a0
	move.l d4, d0
	movea.l pkg.Context.Package(a2), a1
	move.l pkg.Header.LoopPlanBytes(a1), d1
	adda.l pkg.Header.LoopPlan(a1), a1
	jsr parser.classify
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w bad
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w closing
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w closing
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	beq.w declaration
	move.l d0, d5
	cmpi.l #abi.PRVM_LOOP_BFOR, d5
	beq.w collection
	cmpi.l #abi.PRVM_LOOP_BWHILE, d5
	bne.w frameReady
collection
	bsr.w bindCollection
	bne.w done
frameReady
	moveq #0, d6
	move.w State.Depth(a6), d6
	cmpi.w #DEPTH_LIMIT, d6
	bhs.w bad
	lsl.w #FRAME_SHIFT, d6
	lea State.Frames(a6), a3
	adda.w d6, a3
	move.w State.Current(a6), Frame.Parent(a3)
	move.w d5, Frame.Kind(a3)
	move.w State.BranchBase(a6), Frame.BranchBase(a3)
	addq.w #1, State.Depth(a6)
	cmpi.l #abi.PRVM_LOOP_BFOR, d5
	beq.w owner
	cmpi.l #abi.PRVM_LOOP_BWHILE, d5
	bne.w next
owner
	bsr.w declarationMap
	bne.w done
	lea State.Owners(a6), a0
	move.l memory.Block.Used(a0), d6
	move.l d6, d0
	addi.l #OWNER_BYTES, d0
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a3
	adda.l d6, a3
	move.l d6, d0
	addi.l #OWNER_BYTES, d0
	move.l d0, memory.Block.Used(a0)
	move.l a4, d0
	sub.l State.SourceBase(a6), d0
	move.l d0, Owner.Header(a3)
	clr.l Owner.First(a3)
	clr.l Owner.Count(a3)
	clr.l Owner.Cursor(a3)
	lsr.l #OWNER_SHIFT, d6
	addq.l #1, d6
	cmpi.l #65535, d6
	bhi.w bad
	move.w d6, State.Current(a6)
	move.w State.BranchDepth(a6), State.BranchBase(a6)
	bra.w next
materializedRecord
	movea.l a4, a0
	movea.l a5, a1
	jsr materialized.validate
	bne.w bad
	moveq #0, d0
	move.b materialized.Record.Action(a4), d0
	moveq #0, d6
	move.w State.CollectionDepth(a6), d6
	cmpi.l #materialized.COLLECTION_OPEN, d0
	beq.w materializedCollection
	tst.l d6
	beq.w bad
	subq.l #1, d6
	mulu.w #MATERIALIZED_FRAME_BYTES, d6
	lea State.Collections(a6), a3
	adda.w d6, a3
	move.w materialized.Record.Label(a4), d1
	cmp.w MaterializedFrame.Label(a3), d1
	bne.w bad
	move.l materialized.Record.Index(a4), d1
	cmp.l MaterializedFrame.Index(a3), d1
	bne.w bad
	moveq #0, d6
	move.w MaterializedFrame.OwnerDepth(a3), d6
	cmpi.l #materialized.ITERATION_OPEN, d0
	beq.w materializedOwner
	cmpi.l #materialized.ITERATION_CLOSE, d0
	beq.w materializedClosing
	tst.w MaterializedFrame.Active(a3)
	bne.w bad
	cmp.w State.Depth(a6), d6
	bne.w bad
	subq.w #1, State.CollectionDepth(a6)
	bra.w next
materializedOwner
	tst.w MaterializedFrame.Active(a3)
	bne.w bad
	cmp.w State.Depth(a6), d6
	bne.w bad
	move.w #1, MaterializedFrame.Active(a3)
	moveq #abi.PRVM_LOOP_BWHILE, d5
	bra.w frameReady
materializedClosing
	tst.w MaterializedFrame.Active(a3)
	beq.w bad
	addq.w #1, d6
	cmp.w State.Depth(a6), d6
	bne.w bad
	clr.w MaterializedFrame.Active(a3)
	addq.l #1, MaterializedFrame.Index(a3)
	moveq #abi.PRVM_LOOP_ENDWHILE, d0
	bra.w closing
materializedCollection
	cmpi.w #DEPTH_LIMIT, d6
	bhs.w bad
	mulu.w #MATERIALIZED_FRAME_BYTES, d6
	lea State.Collections(a6), a3
	adda.w d6, a3
	move.w materialized.Record.Label(a4), MaterializedFrame.Label(a3)
	clr.w MaterializedFrame.Active(a3)
	clr.l MaterializedFrame.Index(a3)
	move.w State.Depth(a6), MaterializedFrame.OwnerDepth(a3)
	addq.w #1, State.CollectionDepth(a6)
	moveq #0, d1
	move.w materialized.Record.Label(a4), d1
	cmpi.l #materialized.ANONYMOUS, d1
	beq.w next
	bsr.w bindCollectionId
	bne.w done
	bra.w next

closing
	moveq #0, d6
	move.w State.Depth(a6), d6
	beq.w bad
	subq.w #1, d6
	move.w d6, State.Depth(a6)
	lsl.w #FRAME_SHIFT, d6
	lea State.Frames(a6), a3
	adda.w d6, a3
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w closeWhile
	cmpi.w #abi.PRVM_LOOP_MATCHED, Frame.Kind(a3)
	beq.w closed
	cmpi.w #abi.PRVM_LOOP_BFOR, Frame.Kind(a3)
	bne.w bad
	bra.w closed
closeWhile
	cmpi.w #abi.PRVM_LOOP_WHILE, Frame.Kind(a3)
	beq.w closed
	cmpi.w #abi.PRVM_LOOP_BWHILE, Frame.Kind(a3)
	bne.w bad
closed
	move.w Frame.Parent(a3), State.Current(a6)
	move.w Frame.BranchBase(a3), State.BranchBase(a6)
	bra.w next
declaration
	tst.w State.Current(a6)
	bne.w declarationOwned
	tst.w State.BranchDepth(a6)
	bne.w globalCandidate
	tst.w State.Depth(a6)
	beq.w next
globalCandidate
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, PackedRecord.Marker(a4)
	bne.w next
	bsr.w declarationMap
	bne.w done
declarationOwned
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, PackedRecord.NameKind(a4)
	bhi.w next
	tst.b PackedRecord.NameQualifier(a4)
	bne.w bad
	moveq #LABEL, d5
	cmpi.b #abi.PRVM_TOKEN_KIND_COLON, PackedRecord.Marker(a4)
	beq.w bind
	moveq #IMMUTABLE, d5
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, PackedRecord.Marker(a4)
	beq.w bind
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, PackedRecord.Marker(a4)
	beq.w bind
	moveq #MUTABLE, d5
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, PackedRecord.Marker(a4)
	beq.w bind
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, PackedRecord.Marker(a4)
	bne.w next
bind
	moveq #0, d6
	move.w PackedRecord.NameId(a4), d6
	cmp.l State.Symbols(a6), d6
	bhs.w bad
	lsl.l #DECL_SHIFT, d6
	movea.l State.Declarations+memory.Block.Pointer(a6), a3
	adda.l d6, a3
	tst.w Decl.Kind(a3)
	beq.w first
	move.w State.Current(a6), d0
	cmp.w Decl.Owner(a3), d0
	beq.w sameOwner
	; Qualified updates retain the enclosing mutable owner. They are not
	; fresh declarations in the child iteration and cannot be restored by it.
	cmpi.w #MUTABLE, d5
	bne.w bad
	cmp.w Decl.Kind(a3), d5
	bne.w bad
	bra.w next
sameOwner
	cmpi.w #MUTABLE, d5
	beq.w repeatedMutable
	cmpi.w #IMMUTABLE, d5
	bne.w bad
	move.w Decl.Kind(a3), d0
	andi.w #KIND_MASK, d0
	cmpi.w #IMMUTABLE, d0
	bne.w bad
	tst.w State.Current(a6)
	bne.w branchAlternative
	tst.w State.Depth(a6)
	beq.w branchAlternative
	move.w Decl.Kind(a3), d0
	andi.w #LOOP_CONTROLLED, d0
	bne.w alternative
	ori.w #LOOP_CONTROLLED+CONDITIONAL, Decl.Kind(a3)
	addq.l #1, State.DynamicGlobals(a6)
	bra.w alternative
branchAlternative
	move.w State.BranchDepth(a6), d0
	cmp.w State.BranchBase(a6), d0
	bhi.w alternative
	move.w Decl.Kind(a3), d0
	andi.w #CONDITIONAL, d0
	beq.w bad
	; A later unconditional definition is the eager root; earlier conditional
	; candidates remain deferred and will reject if they are also selected.
	move.w Decl.Kind(a3), d0
	andi.w #LOOP_CONTROLLED, d0
	beq.w eagerKind
	ori.w #IMMUTABLE+CONDITIONAL, d0
	bra.w storeEagerKind
eagerKind
	moveq #IMMUTABLE, d0
storeEagerKind
	move.w d0, Decl.Kind(a3)
	move.l a4, d0
	sub.l State.SourceBase(a6), d0
	move.l d0, Decl.Record(a3)
	bra.w next
alternative
	; Alternatives share a lexical ID. Replay selects one RHS, and rejects
	; more than one active definition rather than resolving every alternative.
	bra.w next
repeatedMutable
	cmp.w Decl.Kind(a3), d5
	bne.w bad
	bra.w next
first
	cmpi.w #MUTABLE, d5
	bne.w claim
	move.l State.MemberKeys(a6), d0
	beq.w claim
	movea.l d0, a0
	move.l d6, d0
	lsr.l #DECL_SHIFT-1, d0
	; Fresh scoped declarations acquire a member key during lexical binding.
	; An outer mutable update keeps the existing noncollectible identity.
	cmpi.w #$ffff, 0(a0, d0.l)
	beq.w next
claim
	move.w State.Current(a6), Decl.Owner(a3)
	move.w d5, Decl.Kind(a3)
	cmpi.w #IMMUTABLE, d5
	bne.w owned
	tst.w State.Current(a6)
	bne.w branchOwned
	tst.w State.Depth(a6)
	beq.w branchOwned
	ori.w #LOOP_CONTROLLED+CONDITIONAL, Decl.Kind(a3)
	addq.l #1, State.DynamicGlobals(a6)
branchOwned
	move.w State.BranchDepth(a6), d0
	cmp.w State.BranchBase(a6), d0
	bls.w owned
	ori.w #CONDITIONAL, Decl.Kind(a3)
owned
	move.l a4, d0
	sub.l State.SourceBase(a6), d0
	move.l d0, Decl.Record(a3)
	tst.w State.Current(a6)
	bne.w scopedClaim
	addq.l #1, State.Globals(a6)
	bra.w next
scopedClaim
	addq.l #1, State.Count(a6)
	moveq #0, d0
	move.w State.Current(a6), d0
	subq.l #1, d0
	lsl.l #OWNER_SHIFT, d0
	movea.l State.Owners+memory.Block.Pointer(a6), a3
	addq.l #1, Owner.Count(a3, d0.l)
next
	adda.w d4, a4
	bra.w scan
scanned
	tst.w State.CollectionDepth(a6)
	bne.w bad
	tst.w State.Depth(a6)
	bne.w bad
	tst.w State.BranchDepth(a6)
	bne.w bad
	bsr.w scatter
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepare

; A0=state. Release only preparation inventory, independent of runtime cells.
; Registers preserved; CCR reflects the caller's D0 status after cleanup.
release	.block
	movem.l a0, -(sp)
	jsr memory.release
	lea State.Owners(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea State.Ids(a0), a0
	jsr memory.release
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; release

; A0=inventory,A1=current records,A2=context,A3=current member keys,D0=bytes.
; Refresh borrowed views only; declaration identities/count remain sealed.
; Publication of new symbols requires the later append API, not this hook.
; Other registers preserved; D0/CCR=status.
refresh	.block
	movem.l d1, -(sp)
	move.l a1, d1
	tst.l d0
	beq.w baseReady
	tst.l d1
	beq.w bad
baseReady
	add.l d0, d1
	bcs.w bad
	move.l pkg.Context.Count(a2), d1
	cmp.l State.Symbols(a0), d1
	bne.w bad
	tst.l d1
	beq.w publish
	move.l a3, d1
	beq.w bad
publish
	move.l a1, State.SourceBase(a0)
	move.l a3, State.MemberKeys(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; refresh

; A0=state,D0=compiled header offset. D0/CCR=status,D1=owner ordinal,
; D2=direct declaration count,D3=ID byte offset; other registers preserved.
; Preparation source order makes owner headers strictly increasing.
find	.block
	movem.l d4-d7/a0-a2, -(sp)
	movea.l State.Owners+memory.Block.Pointer(a0), a1
	move.l State.Owners+memory.Block.Used(a0), d5
	lsr.l #OWNER_SHIFT, d5
	moveq #0, d4
search
	cmp.l d5, d4
	bhs.w bad
	move.l d4, d6
	add.l d5, d6
	lsr.l #1, d6
	move.l d6, d7
	lsl.l #OWNER_SHIFT, d7
	movea.l a1, a2
	adda.l d7, a2
	cmp.l Owner.Header(a2), d0
	beq.w found
	blo.w lower
	move.l d6, d4
	addq.l #1, d4
	bra.w search
lower
	move.l d6, d5
	bra.w search
found
	move.l d6, d1
	addq.l #1, d1
	move.l Owner.Count(a2), d2
	move.l Owner.First(a2), d3
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d4-d7/a0-a2
	tst.l d0
	rts
	.bend  ; find
	.priv
; Register a nested labeled scoped repetition as a direct collection in its enclosing owner.
; A4=header,A6=inventory. D0/CCR=status; other registers preserved.
bindCollection	.block
	movem.l d1-d2/a0-a1, -(sp)
	tst.w State.Current(a6)
	beq.w good
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, PackedRecord.NameKind(a4)
	bhi.w good
	cmpi.b #abi.PRVM_TOKEN_KIND_COLON, PackedRecord.Marker(a4)
	bne.w good
	tst.b PackedRecord.NameQualifier(a4)
	bne.w invalid
	bsr.w declarationMap
	bne.w done
	moveq #0, d1
	move.w PackedRecord.NameId(a4), d1
	bsr.w bindCollectionId
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
invalid
	moveq #1, d0
	bra.w done
	.bend  ; bindCollection

; D1=collection identity; register the same direct inventory ownership as loops.
bindCollectionId	.block
	movem.l d1-d2/a0-a1, -(sp)
	tst.w State.Current(a6)
	beq.w good
	bsr.w declarationMap
	bne.w done
	cmp.l State.Symbols(a6), d1
	bhs.w bad
	lsl.l #DECL_SHIFT, d1
	movea.l State.Declarations+memory.Block.Pointer(a6), a0
	adda.l d1, a0
	tst.w Decl.Owner(a0)
	bne.w bad
	move.w State.Current(a6), Decl.Owner(a0)
	move.w #COLLECTION, Decl.Kind(a0)
	move.l a4, d0
	sub.l State.SourceBase(a6), d0
	move.l d0, Decl.Record(a0)
	addq.l #1, State.Count(a6)
	moveq #0, d1
	move.w State.Current(a6), d1
	subq.l #1, d1
	lsl.l #OWNER_SHIFT, d1
	movea.l State.Owners+memory.Block.Pointer(a6), a0
	addq.l #1, Owner.Count(a0, d1.l)
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; bindCollectionId
; A4=bounded compiled record,D4=extent,A2=context,A6=inventory.
; Track shared PRVM branch ancestry, without evaluating expressions.
; An enclosing branch is outside a newly opened scoped owner's branch base.
; D0/CCR=status; other registers preserved.
branchDepth	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a4, a0
	move.l d4, d0
	movea.l pkg.Context.Package(a2), a1
	adda.l pkg.Header.BranchPlan(a1), a1
	jsr branch.classify
	bne.w done
	cmpi.l #branch.MATCH, d1
	beq.w opening
	cmpi.l #branch.IF, d1
	beq.w opening
	cmpi.l #branch.ENDMATCH, d1
	beq.w closing
	cmpi.l #branch.ENDIF, d1
	bne.w good
closing
	tst.w State.BranchDepth(a6)
	beq.w bad
	subq.w #1, State.BranchDepth(a6)
	bra.w good
opening
	cmpi.w #DEPTH_LIMIT, State.BranchDepth(a6)
	bhs.w bad
	addq.w #1, State.BranchDepth(a6)
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; branchDepth

; A6=state. Prefix counts and scatter IDs once into directly-owned ranges.
; Clobbers D0-D3/A0-A3; D0/CCR=status.
scatter	.block
	tst.l State.Owners+memory.Block.Used(a6)
	beq.w good
	move.l State.Count(a6), d0
	add.l d0, d0
	lea State.Ids(a6), a0
	jsr memory.reserve
	bne.w done
	move.l State.Count(a6), d0
	add.l d0, d0
	move.l d0, memory.Block.Used(a0)
	movea.l State.Owners+memory.Block.Pointer(a6), a1
	move.l State.Owners+memory.Block.Used(a6), d1
	moveq #0, d2
prefix
	tst.l d1
	beq.w ids
	move.l d2, Owner.First(a1)
	move.l d2, Owner.Cursor(a1)
	move.l Owner.Count(a1), d0
	add.l d0, d0
	add.l d0, d2
	adda.w #OWNER_BYTES, a1
	subi.l #OWNER_BYTES, d1
	bra.w prefix
ids
	movea.l State.Declarations+memory.Block.Pointer(a6), a0
	movea.l State.Ids+memory.Block.Pointer(a6), a2
	moveq #0, d1
id
	cmp.l State.Symbols(a6), d1
	bhs.w good
	moveq #0, d0
	move.w Decl.Owner(a0), d0
	beq.w next
	subq.l #1, d0
	lsl.l #OWNER_SHIFT, d0
	movea.l State.Owners+memory.Block.Pointer(a6), a1
	adda.l d0, a1
	move.l Owner.Cursor(a1), d3
	move.w d1, 0(a2, d3.l)
	addq.l #2, Owner.Cursor(a1)
next
	addq.l #1, d1
	addq.l #DECL_BYTES, a0
	bra.w id
good
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; scatter

; A6=state. Allocate the direct-ID map at the first scoped header or global
; conditional immutable candidate. Ordinary programs need no inventory map.
; D0/CCR=status; other registers preserved.
declarationMap	.block
	movem.l d1-d2/a0-a1, -(sp)
	tst.l State.Declarations+memory.Block.Used(a6)
	bne.w good
	move.l State.Symbols(a6), d1
	beq.w bad
	move.l d1, d2
	lsl.l #DECL_SHIFT, d2
	move.l d2, d0
	lea State.Declarations(a6), a0
	jsr memory.reserve
	bne.w done
	move.l d2, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
clear
	clr.l (a1)+
	clr.l (a1)+
	subq.l #1, d1
	bne.w clear
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; declarationMap
	.endsection
	.endmodule
