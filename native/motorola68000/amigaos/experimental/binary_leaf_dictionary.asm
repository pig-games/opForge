; Relocatable spelling closure for dependency-free prepared declaration leaves.
; @opforge-owner: experimental.amigaos.binary_leaf_dictionary
	.module experimental.amigaos.binary_leaf_dictionary
	.cpu 68020
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_memory as memory
	.pub
MAGIC = $4c444331
OK = 0
MALFORMED = 1
FATAL = 2
Header	.struct
Magic	.long ?
Bytes	.long ?
Count	.word ?
Root	.word ?
Names	.long ?
.endstruct
HEADER_BYTES = Header.Names+4
Item	.struct
Name	.long ?
Length	.word ?
Owner	.word ?
Flags	.word ?
Member	.word ?
Leaf	.word ?
Exposure	.word ?
Kind	.word ?
Reserved	.word ?
.endstruct
ITEM_BYTES = Item.Reserved+2
	.priv
Work	.struct
Scope	.long ?
Output	.long ?
Map	.long ?
Root	.long ?
Count	.long ?
Selected	.long ?
NameBytes	.long ?
.endstruct
WORK_BYTES = Work.NameBytes+4
	.section code, kind=code
	.pub
; A0=scope,D1=module entry index+1,A1=private output Block,A2=private map Block.
; D0/CCR=status; other registers preserved. Output contains only this module's
; declarations, lexical owners and canonical bare field keys. Map has Count long
; source-index slots: -1 absent, otherwise archive-local ID. Failure never changes
; scope semantics; caller owns and releases both blocks, including on failure.
export	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #WORK_BYTES, sp
	movea.l sp, a6
	move.l a0, Work.Scope(a6)
	move.l a1, Work.Output(a6)
	move.l a2, Work.Map(a6)
	move.l d1, Work.Root(a6)
	clr.l Work.Selected(a6)
	clr.l Work.NameBytes(a6)
	moveq #0, d0
	move.w layout.State.Count(a0), d0
	move.l d0, Work.Count(a6)
	tst.l d1
	beq.w bad
	cmp.l d0, d1
	bhi.w bad
	lsl.l #2, d0
	movea.l a2, a0
	jsr memory.reserve
	bne.w bad
	move.l Work.Count(a6), d0
	lsl.l #2, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a4
	move.l Work.Count(a6), d7
	movea.l a4, a1
clear
	move.l #-1, (a1)+
	subq.l #1, d7
	bne.w clear
	move.l Work.Root(a6), d0
	subq.l #1, d0
	lsl.l #2, d0
	clr.l 0(a4, d0.l)
	movea.l Work.Scope(a6), a5
	movea.l layout.MODULE_STATE+modules.OWNERS_POINTER(a5), a3
	moveq #0, d7
seed
	cmp.l Work.Count(a6), d7
	bhs.w closure
	move.l d7, d0
	add.l d0, d0
	moveq #0, d1
	move.w 0(a3, d0.l), d1
	cmp.l Work.Root(a6), d1
	bne.w seedNext
	lsl.l #2, d7
	clr.l 0(a4, d7.l)
	lsr.l #2, d7
seedNext
	addq.l #1, d7
	bra.w seed
closure
	moveq #0, d6
	moveq #0, d7
scan
	cmp.l Work.Count(a6), d7
	bhs.w scanDone
	move.l d7, d0
	lsl.l #2, d0
	tst.l 0(a4, d0.l)
	bmi.w scanNext
	move.l d7, d0
	add.l d0, d0
	moveq #0, d1
	move.w 0(a3, d0.l), d1
	cmp.l Work.Root(a6), d1
	bne.w scanNext  ; bare field keys do not import their declaration facets
	move.l d7, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a5), a2
	adda.l d0, a2
	moveq #0, d0
	move.w records.Entry.Owner(a2), d0
	beq.w member
	subq.l #1, d0
	bsr.w mark
	bne.w bad
member
	moveq #0, d0
	move.w records.Entry.MemberId(a2), d0
	cmpi.w #$ffff, d0
	beq.w scanNext
	sub.w layout.State.Base(a5), d0
	bcs.w bad
	andi.l #$ffff, d0
	bsr.w mark
	bne.w bad
scanNext
	addq.l #1, d7
	bra.w scan
scanDone
	tst.l d6
	bne.w closure
	moveq #0, d7
number
	cmp.l Work.Count(a6), d7
	bhs.w allocate
	move.l d7, d0
	lsl.l #2, d0
	tst.l 0(a4, d0.l)
	bmi.w numberNext
	move.l Work.Selected(a6), 0(a4, d0.l)
	addq.l #1, Work.Selected(a6)
	move.l d7, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a5), a2
	adda.l d0, a2
	moveq #0, d0
	move.w records.Entry.Length(a2), d0
	add.l d0, Work.NameBytes(a6)
numberNext
	addq.l #1, d7
	bra.w number
allocate
	move.l Work.Selected(a6), d0
	mulu.w #ITEM_BYTES, d0
	addi.l #HEADER_BYTES, d0
	move.l d0, d5
	add.l Work.NameBytes(a6), d0
	addq.l #1, d0
	andi.l #$fffffffe, d0
	move.l d0, d6
	movea.l Work.Output(a6), a0
	jsr memory.reserve
	bne.w bad
	move.l d6, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a3
	move.l #MAGIC, Header.Magic(a3)
	move.l d6, Header.Bytes(a3)
	move.w Work.Selected+2(a6), Header.Count(a3)
	move.l Work.Root(a6), d0
	subq.l #1, d0
	lsl.l #2, d0
	move.l 0(a4, d0.l), d0
	move.w d0, Header.Root(a3)
	move.l d5, Header.Names(a3)
	clr.b -1(a3, d6.l)
	moveq #0, d7
	lea HEADER_BYTES(a3), a1
write
	cmp.l Work.Count(a6), d7
	bhs.w good
	move.l d7, d0
	lsl.l #2, d0
	tst.l 0(a4, d0.l)
	bmi.w writeNext
	move.l d7, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a5), a2
	adda.l d0, a2
	move.l d5, Item.Name(a1)
	move.w records.Entry.Length(a2), Item.Length(a1)
	move.w records.Entry.Leaf(a2), Item.Leaf(a1)
	move.w records.Entry.ScopeKind(a2), Item.Kind(a1)
	clr.w Item.Reserved(a1)
	clr.w Item.Flags(a1)
	clr.w Item.Exposure(a1)
	move.l d7, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.OWNERS_POINTER(a5), a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	cmp.l Work.Root(a6), d1
	bne.w bare
	move.w records.Entry.Flags(a2), d1
	andi.w #$ffff-(scopes.DECLARED+records.REFERENCED+scopes.EXPLICIT+records.STRUCT_TYPE), d1
	bne.w bad
	move.w records.Entry.Flags(a2), Item.Flags(a1)
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a5), a0
	move.w 0(a0, d0.l), d1
	andi.w #modules.PUBLIC, d1
	move.w d1, Item.Exposure(a1)
	moveq #0, d1
	move.w records.Entry.Target(a2), d1
	sub.w layout.State.Base(a5), d1
	andi.l #$ffff, d1
	cmp.l d7, d1
	bne.w bad
	bra.w links
bare
	clr.w Item.Owner(a1)
	move.w #$ffff, Item.Member(a1)
	clr.w Item.Leaf(a1)
	; The already-open module is the sole nondeclaration lexical owner.
	move.l d7, d1
	addq.l #1, d1
	cmp.l Work.Root(a6), d1
	beq.w copySpelling
	tst.w records.Entry.Owner(a2)
	bne.w bad
	tst.w records.Entry.ScopeKind(a2)
	bne.w bad
	clr.w Item.Kind(a1)
	bra.w copySpelling
links
	moveq #0, d0
	move.w records.Entry.Owner(a2), d0
	beq.w ownerReady
	subq.l #1, d0
	lsl.l #2, d0
	move.l 0(a4, d0.l), d0
	addq.l #1, d0
ownerReady
	move.w d0, Item.Owner(a1)
	moveq #0, d0
	move.w records.Entry.MemberId(a2), d0
	cmpi.w #$ffff, d0
	beq.w memberReady
	sub.w layout.State.Base(a5), d0
	andi.l #$ffff, d0
	lsl.l #2, d0
	move.l 0(a4, d0.l), d0
memberReady
	move.w d0, Item.Member(a1)
copySpelling
	movea.l layout.ARENA_POINTER(a5), a0
	adda.l records.Entry.Name(a2), a0
	moveq #0, d1
	move.w Item.Length(a1), d1
copyName
	move.b (a0)+, d0
	move.b d0, 0(a3, d5.l)
	addq.l #1, d5
	subq.l #1, d1
	bne.w copyName
	adda.w #ITEM_BYTES, a1
writeNext
	addq.l #1, d7
	bra.w write
good
	movea.l a3, a0
	move.l d6, d0
	bsr.w validate
	bra.w done
bad
	moveq #MALFORMED, d0
done
	adda.w #WORK_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; export
	.priv
; D0=source entry index,A4=map,A6=Work. Mark closure; D6 becomes nonzero if new.
; D0/CCR=status; other registers preserved.
mark	.block
	cmp.l Work.Count(a6), d0
	bhs.w bad
	lsl.l #2, d0
	tst.l 0(a4, d0.l)
	bpl.w good
	clr.l 0(a4, d0.l)
	moveq #1, d6
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; mark
	.pub
; A0=portable dictionary,D0=exact bytes. D0/CCR=status; others preserved.
; Complete read-only validation precedes any publication. Original spellings
; are printable ASCII, unique under case folding and contiguous; owner links point strictly earlier.
validate	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l a0, a5
	move.l a0, d1
	beq.w bad
	btst #0, d1
	bne.w bad
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	cmpi.l #MAGIC, Header.Magic(a5)
	bne.w bad
	cmp.l Header.Bytes(a5), d0
	bne.w bad
	move.l d0, d6
	add.l d1, d0
	bcs.w bad
	moveq #0, d7
	move.w Header.Count(a5), d7
	beq.w bad
	moveq #0, d0
	move.w Header.Root(a5), d0
	cmp.l d7, d0
	bhs.w bad
	move.l d7, d5
	mulu.w #ITEM_BYTES, d5
	addi.l #HEADER_BYTES, d5
	cmp.l Header.Names(a5), d5
	bne.w bad
	cmp.l d6, d5
	bhs.w bad
	moveq #0, d0
	move.w Header.Root(a5), d0
	mulu.w #ITEM_BYTES, d0
	lea HEADER_BYTES(a5, d0.l), a1
	cmpi.w #modules.KIND_MODULE, Item.Kind(a1)
	bne.w bad
	tst.w Item.Owner(a1)
	bne.w bad
	tst.w Item.Flags(a1)
	bne.w bad
	lea HEADER_BYTES(a5), a4
	moveq #0, d4
item
	cmp.l d7, d4
	bhs.w tail
	cmp.l Item.Name(a4), d5
	bne.w bad
	moveq #0, d3
	move.w Item.Length(a4), d3
	beq.w bad
	cmpi.w #records.NAME_BYTES, d3
	bhs.w bad
	move.l d5, d0
	add.l d3, d0
	cmp.l d6, d0
	bhi.w bad
	moveq #0, d1
	move.w Item.Owner(a4), d1
	cmp.l d4, d1
	bhi.w bad
	move.w Item.Member(a4), d1
	cmpi.w #$ffff, d1
	beq.w flags
	cmp.w d7, d1
	bhs.w bad
flags
	move.w Item.Flags(a4), d1
	andi.w #$ffff-(scopes.DECLARED+records.REFERENCED+scopes.EXPLICIT+records.STRUCT_TYPE), d1
	bne.w bad
	move.w Item.Exposure(a4), d1
	andi.w #$ffff-modules.PUBLIC, d1
	bne.w bad
	cmpi.w #modules.KIND_MODULE, Item.Kind(a4)
	bhi.w bad
	tst.w Item.Reserved(a4)
	bne.w bad
	cmp.w Item.Leaf(a4), d3
	bls.w bad
	moveq #0, d1
	move.w Item.Owner(a4), d1
	beq.w lexicalReady
	subq.l #1, d1
	mulu.w #ITEM_BYTES, d1
	lea HEADER_BYTES(a5, d1.l), a2
	moveq #0, d2
	move.w Item.Length(a2), d2
	addq.l #1, d2
	cmp.w Item.Leaf(a4), d2
	bne.w bad
	cmp.l d3, d2
	bhs.w bad
	subq.l #1, d2
	movea.l a5, a0
	adda.l Item.Name(a2), a0
	lea 0(a5, d5.l), a1
ownerByte
	cmpm.b (a0)+, (a1)+
	bne.w bad
	subq.l #1, d2
	bne.w ownerByte
	cmpi.b #'.', (a1)
	bne.w bad
lexicalReady
	lea 0(a5, d5.l), a0
	move.l d3, d2
name
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #33, d1
	blo.w bad
	cmpi.b #127, d1
	bhs.w bad
	subq.l #1, d2
	bne.w name
	; Ordinary source binding folds ASCII case; preserve original spelling.
	lea HEADER_BYTES(a5), a2
unique
	cmpa.l a4, a2
	beq.w itemNext
	cmp.w Item.Length(a2), d3
	bne.w uniqueNext
	movea.l a5, a0
	adda.l Item.Name(a2), a0
	lea 0(a5, d5.l), a1
	move.l d3, d2
equalByte
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.l d1, d0
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d0, d1
	bne.w uniqueNext
	subq.l #1, d2
	bne.w equalByte
	bra.w bad
uniqueNext
	adda.w #ITEM_BYTES, a2
	bra.w unique
itemNext
	add.l d3, d5
	adda.w #ITEM_BYTES, a4
	addq.l #1, d4
	bra.w item
tail
	move.l d5, d0
	addq.l #1, d0
	andi.l #$fffffffe, d0
	cmp.l d6, d0
	bne.w bad
	cmp.l d5, d6
	beq.w good
	tst.b 0(a5, d5.l)
	bne.w bad
good
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; validate

; A0=scope currently inside the matching opened module,A1=portable dictionary,
; D0=exact bytes,A2=private output Block for long local-to-current source IDs.
; D0/CCR=OK, MALFORMED before semantic mutation, or FATAL after binding begins.
; Other registers preserved. FATAL requires aborting preparation, never fallback.
; Caller validates all accompanying records/schema payloads before calling here.
; Names bind through the ordinary binder; no serialized ID becomes a live ID.
import	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	movea.l a2, a4
	movea.l a1, a0
	bsr.w validate
	bne.w bad
	moveq #0, d7
	move.w Header.Count(a5), d7
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	beq.w bad
	cmp.w layout.MODULE_STATE+modules.State.Active(a6), d0
	bne.w bad
	subq.l #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	cmpi.w #modules.KIND_MODULE, records.Entry.ScopeKind(a3)
	bne.w bad
	moveq #0, d0
	move.w Header.Root(a5), d0
	mulu.w #ITEM_BYTES, d0
	lea HEADER_BYTES(a5, d0.l), a2
	move.w Item.Length(a2), d0
	cmp.w records.Entry.Length(a3), d0
	bne.w bad
	movea.l layout.ARENA_POINTER(a6), a0
	adda.l records.Entry.Name(a3), a0
	movea.l a5, a1
	adda.l Item.Name(a2), a1
rootByte
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.l d1, d2
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d2, d1
	bne.w bad
	subq.w #1, d0
	bne.w rootByte
	move.l d7, d0
	lsl.l #2, d0
	movea.l a4, a0
	jsr memory.reserve
	bne.w bad
	move.l d7, d0
	lsl.l #2, d0
	move.l d0, memory.Block.Used(a4)
	movea.l memory.Block.Pointer(a4), a4
	move.w layout.State.Current(a6), -(sp)
	move.w layout.State.FirstBound(a6), -(sp)
	move.w layout.State.FirstExplicit(a6), -(sp)
	clr.w layout.State.Current(a6)
	lea HEADER_BYTES(a5), a3
	moveq #0, d6
bind
	cmp.l d7, d6
	bhs.w bound
	movea.l a5, a0
	adda.l Item.Name(a3), a0
	moveq #0, d0
	move.w Item.Length(a3), d0
	movea.l a6, a1
	jsr scopes.bindOrigin
	bne.w failedBind
	move.l d6, d0
	lsl.l #2, d0
	move.l d1, 0(a4, d0.l)
	adda.w #ITEM_BYTES, a3
	addq.l #1, d6
	bra.w bind
failedBind
	move.w (sp)+, layout.State.FirstExplicit(a6)
	move.w (sp)+, layout.State.FirstBound(a6)
	move.w (sp)+, layout.State.Current(a6)
	bra.w fatalFailure
bound
	move.w (sp)+, layout.State.FirstExplicit(a6)
	move.w (sp)+, layout.State.FirstBound(a6)
	move.w (sp)+, layout.State.Current(a6)
	lea HEADER_BYTES(a5), a3
	moveq #0, d6
publish
	cmp.l d7, d6
	bhs.w good
	move.l d6, d0
	lsl.l #2, d0
	move.l 0(a4, d0.l), d5
	move.l d5, d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	move.l d0, d4
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a2
	adda.l d0, a2
	btst #0, Item.Flags+1(a3)
	beq.w publishNext
	btst #0, records.Entry.Flags+1(a2)
	bne.w fatalFailure
	move.w Item.Flags(a3), d0
	or.w d0, records.Entry.Flags(a2)
	move.w Item.Leaf(a3), records.Entry.Leaf(a2)
	move.w Item.Kind(a3), records.Entry.ScopeKind(a2)
	moveq #0, d0
	move.w Item.Owner(a3), d0
	beq.w ownerReady
	subq.l #1, d0
	lsl.l #2, d0
	move.l 0(a4, d0.l), d0
	sub.w layout.State.Base(a6), d0
	addq.w #1, d0
ownerReady
	move.w d0, records.Entry.Owner(a2)
	moveq #0, d0
	move.w Item.Member(a3), d0
	cmpi.w #$ffff, d0
	beq.w memberReady
	lsl.l #2, d0
	move.l 0(a4, d0.l), d0
memberReady
	move.w d0, records.Entry.MemberId(a2)
	move.l d4, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.OWNERS_POINTER(a6), a0
	move.w layout.State.Current(a6), 0(a0, d0.l)
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	adda.l d0, a0
	andi.w #$ffff-modules.PUBLIC, (a0)
	move.w Item.Exposure(a3), d1
	or.w d1, (a0)
publishNext
	adda.w #ITEM_BYTES, a3
	addq.l #1, d6
	bra.w publish
good
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
	bra.w done
fatalFailure
	moveq #FATAL, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; import
	.priv
; D1=ASCII byte; fold source-binding case. Other registers preserved.
fold	.block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; fold
	.endsection
	.endmodule
