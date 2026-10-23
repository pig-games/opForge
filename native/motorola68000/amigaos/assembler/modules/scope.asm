; Install one canonical interface through ordinary module/name ownership.
; @opforge-owner: opforge.assembler.modules.scope
	.module opforge.assembler.modules.scope
	.cpu 68020
	.use opforge.platform.memory as memory
	.use opforge.assembler.modules.semantic_exports as interface
	.use opforge.assembler.modules.remap as remap
	.use opforge.assembler.symbols.scope_layout as layout
	.use opforge.assembler.symbols.scopes as scopes
	.use opforge.assembler.symbols.lexical_store as lexical
	.use opforge.assembler.symbols.binding_records as records
	.use opforge.assembler.modules.bindings as modules
	.use opforge.assembler.symbols.imports as imports
	.use opforge.assembler.values.values as values
	.use opforge.vm.exprvm.runtime as runtime
	.pub
Frame	.struct
Scope	.long ?
Image	.long ?
Bytes	.long ?
ModuleName	.long ?
ModuleNameBytes	.long ?
Map	.res memory.Block.Used+4
Address	.long ?
Context	.long ?
Module	.long ?  ; destination module lexical index+1, not absolute source ID
Reuse	.long ?  ; optional nameless schema lookup: A0=Context,D1=local ID
	.endstruct
FRAME_BYTES = Frame.Reuse+4
DemandFrame	.struct
Scope	.long ?
Image	.long ?
Bytes	.long ?
Export	.long ?
Global	.long ?
Module	.long ?
Resolve	.long ?
Context	.long ?
Address	.long ?
	.endstruct
DEMAND_BYTES = DemandFrame.Address+4
MapRow	.struct
Local	.long ?
Global	.long ?
Kind	.long ?
	.endstruct
MAP_BYTES = MapRow.Kind+4
VALUE = 0
MEMBER = 1
OK = 0
INVALID = 1
NOT_FOUND = 2
	.priv
Local	.struct
Copy	.res remap.FRAME_BYTES
Entry	.res records.ENTRY_BYTES
ModuleEntry	.res records.ENTRY_BYTES
Opened	.long ?
Global	.long ?
	.endstruct
LOCAL_BYTES = Local.Global+4
	.section code, kind=code
	.pub
; A0=consumer scope,D0=module lexical index+1. Allocate one undeclared,
; unindexed provider entity. D0/CCR=status,D1=absolute consumer identity;
; other registers preserved. The module's name handle is shared immutably.
allocateEntity	.block
	movem.l d2-d4/a0-a3, -(sp)
	suba.w #records.ENTRY_BYTES, sp
	movea.l a0, a3
	move.l d0, d4
	beq.w bad
	subq.l #1, d0
	moveq #0, d2
	move.w layout.State.Count(a3), d2
	cmp.l d2, d0
	bhs.w bad
	lea layout.LEXICAL_STATE(a3), a0
	movea.l sp, a1
	jsr lexical.readEntry
	bne.w bad
	movea.l records.Entry.Name(sp), a2
	moveq #0, d3
	move.w records.Entry.Length(sp), d3
	movea.l a3, a0
	move.l d2, d0
	addq.l #1, d0
	jsr scopes.reserveIdentities
	bne.w bad
	movea.l sp, a1
	moveq #records.ENTRY_BYTES/2-1, d0
clear
	clr.w (a1)+
	dbra d0, clear
	movea.l sp, a1
	move.l a2, records.Entry.Name(a1)
	move.w d3, records.Entry.Length(a1)
	move.w d4, records.Entry.Owner(a1)
	move.w #$ffff, records.Entry.MemberId(a1)
	moveq #0, d1
	move.w layout.State.Base(a3), d1
	add.l d2, d1
	move.w d1, records.Entry.Target(a1)
	move.l d2, d0
	lea layout.LEXICAL_STATE(a3), a0
	jsr lexical.appendEntry
	bne.w bad
	addq.w #1, layout.State.Count(a3)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #records.ENTRY_BYTES, sp
	movem.l (sp)+, d2-d4/a0-a3
	tst.l d0
	rts
	.bend  ; allocateEntity

; A0=scope,D0=existing spelling,D1=canonical value,D2=provider module.
; Declare only the spelling's value alias, preserving independent templates.
; D0/CCR=status; other registers preserved. Numeric consumers follow Target.
alias	.block
	movem.l d1-d7/a0-a3, -(sp)
	suba.w #records.ENTRY_BYTES, sp
	movea.l a0, a3
	move.l d0, d7
	move.l d1, d6
	move.l d2, d5
	move.l d1, d0
	sub.w layout.State.Base(a3), d0
	bcs.w bad
	andi.l #$ffff, d0
	lea layout.LEXICAL_STATE(a3), a0
	movea.l sp, a1
	jsr lexical.readEntry
	bne.w bad
	move.w records.Entry.Flags(sp), d4
	andi.w #records.STRUCT_TYPE+records.LABEL_VALUE, d4
	move.l d7, d0
	sub.w layout.State.Base(a3), d0
	bcs.w bad
	andi.l #$ffff, d0
	jsr lexical.readEntry
	bne.w bad
	btst #0, records.Entry.Flags+1(sp)
	bne.w bad
	ori.w #scopes.DECLARED, d4
	or.w d4, records.Entry.Flags(sp)
	move.w d6, records.Entry.Target(sp)
	move.w d5, records.Entry.Owner(sp)
	move.l d7, d0
	sub.w layout.State.Base(a3), d0
	andi.l #$ffff, d0
	jsr lexical.writeEntry
	bne.w bad
	lea layout.MODULE_STATE(a3), a0
	move.w modules.State.Active(a0), -(sp)
	move.w modules.State.Visibility(a0), -(sp)
	move.w d5, modules.State.Active(a0)
	move.w #modules.PUBLIC, modules.State.Visibility(a0)
	move.l d7, d0
	sub.w layout.State.Base(a3), d0
	andi.l #$ffff, d0
	jsr modules.claim
	move.w (sp)+, modules.State.Visibility(a0)
	move.w (sp)+, modules.State.Active(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #records.ENTRY_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; alias

; A0=DemandFrame. Install exactly one demanded entity into an existing binding.
; Image is a validated immutable OFMS and Export one of its exact rows. The
; caller binds required nominal/member closure before this call; Resolve has
; remap.copy's ABI and cannot grow its destination arena. Address has install's
; ABI. Global is absolute; Module is the consumer lexical module index+1.
; D0/CCR=status; others preserved. No image pointer or lease escapes. Failure
; may leave local declaration/value work and requires ordinary owner cleanup.
demand	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l sp, a4
	movea.l a0, a5
	movea.l DemandFrame.Scope(a5), a6
	cmpa.w #0, a6
	beq.w bad
	move.l DemandFrame.Module(a5), d0
	beq.w bad
	cmpi.l #65535, d0
	bhi.w bad
	cmpi.l #65535, DemandFrame.Global(a5)
	bhi.w bad
	movea.l DemandFrame.Export(a5), a3
	; The leased interface is validated by its owner; still check that this
	; request selects an exact row, including the symbolic-address fast path.
	movea.l DemandFrame.Image(a5), a0
	cmpa.w #0, a0
	beq.w bad
	move.l DemandFrame.Bytes(a5), d0
	cmpi.l #interface.HEADER_BYTES, d0
	blo.w bad
	cmp.l interface.Header.Bytes(a0), d0
	bne.w bad
	move.l a3, d0
	sub.l a0, d0
	bcs.w bad
	sub.l interface.Header.Exports(a0), d0
	bcs.w bad
	move.l d0, d2
	moveq #interface.EXPORT_BYTES, d1
	divu.l d1, d0
	cmp.l interface.Header.ExportCount(a0), d0
	bhs.w bad
	mulu.l #interface.EXPORT_BYTES, d0
	cmp.l d2, d0
	bne.w bad
	move.l DemandFrame.Global(a5), d1
	move.l d1, Local.Global(a4)
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	andi.l #$ffff, d0
	moveq #0, d2
	move.w layout.State.Count(a6), d2
	cmp.l d2, d0
	bhs.w bad
	lea layout.LEXICAL_STATE(a6), a0
	lea Local.Entry(a4), a1
	move.l Local.Global(a4), d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	jsr lexical.readEntry
	bne.w bad
	move.w records.Entry.Flags(a1), d2
	andi.w #scopes.DECLARED+records.IMPORT_PROXY+records.BINDING_PROXY, d2
	bne.w bad
	move.l DemandFrame.Image(a5), Local.Copy+remap.Frame.Image(a4)
	move.l DemandFrame.Bytes(a5), Local.Copy+remap.Frame.Bytes(a4)
	move.l a3, Local.Copy+remap.Frame.Export(a4)
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a6), a0
	move.l a0, Local.Copy+remap.Frame.Owner(a4)
	move.l DemandFrame.Resolve(a5), Local.Copy+remap.Frame.Resolve(a4)
	move.l DemandFrame.Context(a5), Local.Copy+remap.Frame.Context(a4)
	cmpi.l #interface.FRAGMENT_ADDRESS, interface.Export.Kind(a3)
	beq.w address
	tst.l interface.Export.Provenance(a3)
	bne.w address
	lea Local.Copy(a4), a0
	jsr remap.copy
	bne.w bad
	move.l Local.Copy+remap.Frame.Low(a4), d2
	move.l Local.Copy+remap.Frame.High(a4), d3
	move.l Local.Copy+remap.Frame.Kind(a4), d4
	moveq #1, d5
	bra.w store
address
	movea.l DemandFrame.Address(a5), a1
	cmpa.w #0, a1
	beq.w bad
	movea.l DemandFrame.Context(a5), a0
	move.l interface.Export.Id(a3), d0
	move.l Local.Global(a4), d1
	move.l interface.Export.Low(a3), d2
	move.l interface.Export.Provenance(a3), d3
	cmpi.l #interface.FRAGMENT_ADDRESS, interface.Export.Kind(a3)
	bne.w callAddress
	move.l interface.Export.High(a3), d3
callAddress
	jsr (a1)
	tst.l d0
	bne.w bad
	moveq #0, d2
	moveq #0, d3
	moveq #values.SCALAR, d4
	moveq #0, d5
store
	move.l Local.Global(a4), d1
	bsr.w storeKnown
	bne.w bad
	lea Local.Entry(a4), a1
	ori.w #scopes.DECLARED, records.Entry.Flags(a1)
	move.l DemandFrame.Module(a5), d0
	move.w d0, records.Entry.Owner(a1)
	cmpi.l #values.STRUCT_TYPE, interface.Export.Kind(a3)
	bne.w label
	ori.w #records.STRUCT_TYPE, records.Entry.Flags(a1)
label
	cmpi.l #interface.FRAGMENT_ADDRESS, interface.Export.Kind(a3)
	beq.w labelFlag
	tst.l interface.Export.Provenance(a3)
	beq.w publish
labelFlag
	ori.w #records.LABEL_VALUE, records.Entry.Flags(a1)
publish
	move.l Local.Global(a4), d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	lea layout.LEXICAL_STATE(a6), a0
	jsr lexical.writeEntry
	bne.w bad
	lea layout.MODULE_STATE(a6), a0
	move.w modules.State.Active(a0), -(sp)
	move.w modules.State.Visibility(a0), -(sp)
	move.l DemandFrame.Module(a5), d0
	move.w d0, modules.State.Active(a0)
	clr.w modules.State.Visibility(a0)
	tst.l interface.Export.NameBytes(a3)
	beq.w claim
	move.w #modules.PUBLIC, modules.State.Visibility(a0)
claim
	move.l Local.Global(a4), d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	jsr modules.claim
	move.w (sp)+, modules.State.Visibility(a0)
	move.w (sp)+, modules.State.Active(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #LOCAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; demand

; A0=Frame, Map empty, Scope at file boundary. D0/CCR=status; others kept.
; Binds public names and bare member keys, then copies values once into the
; destination import owner. Hidden nominal IDs share only the public module
; spelling and are never inserted in the name buckets. No image borrow survives.
; The validated image must remain immutable/live for this entire installation,
; including Address/Reuse callbacks; callbacks must not replace its ownership.
; Address callback: A0=Context,D0=local value ID,D1=global value ID,D2=Low,
; D3=fragment ID (FRAGMENT_ADDRESS) or section1..8 (snapshot placeholder)
; -> D0=status; all other registers kept. The caller knows the image boundary.
; It records a runtime
; recipe; addresses stay declared LABEL_VALUE with an undefined known snapshot.
; Compound address cells reject because their original label identity is absent.
; Failure owns partial Map/scope allocations; caller releases them normally.
; Installation closes the opened module even on failure, but does not roll back
; definitions or permit retry in the same scope. One install per module/scope.
install	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l sp, a4
	movea.l a0, a5
	movea.l Frame.Scope(a5), a6
	clr.l Frame.Module(a5)
	clr.l Local.Opened(a4)
	move.l a6, d0
	beq.w bad
	tst.w layout.State.Current(a6)
	bne.w bad
	lea Frame.Map(a5), a0
	move.l memory.Block.Pointer(a0), d0
	or.l memory.Block.Capacity(a0), d0
	or.l memory.Block.Used(a0), d0
	bne.w bad
	movea.l Frame.Image(a5), a0
	move.l Frame.Bytes(a5), d0
	jsr interface.validate
	bne.w bad
	move.l interface.Header.ExportCount(a0), d0
	add.l interface.Header.MemberCount(a0), d0
	bcs.w bad
	cmpi.l #memory.LIMIT/MAP_BYTES, d0
	bhi.w bad
	mulu.l #MAP_BYTES, d0
	lea Frame.Map(a5), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l a6, a0
	movea.l Frame.ModuleName(a5), a1
	move.l Frame.ModuleNameBytes(a5), d0
	jsr scopes.beginFileDerived
	bne.w bad
	move.l #1, Local.Opened(a4)
	move.l d1, Frame.Module(a5)
	subq.l #1, d1
	move.l d1, d0
	lea layout.LEXICAL_STATE(a6), a0
	lea Local.ModuleEntry(a4), a1
	jsr lexical.readEntry
	bne.w bad
	movea.l Frame.Image(a5), a3
	move.l interface.Header.MemberCount(a3), d7
	adda.l interface.Header.Members(a3), a3
memberRow
	tst.l d7
	beq.w exportsBegin
	clr.w layout.State.Current(a6)
	movea.l Frame.Image(a5), a0
	adda.l interface.Member.Name(a3), a0
	move.l interface.Member.NameBytes(a3), d0
	movea.l a6, a1
	jsr scopes.bindOrigin
	tst.l d0
	bne.w memberRestore
	; Retain the immutable key role so later snapshots can recover closure
	; through ordinary member acquisition without the original declarations.
	move.l d1, d0
	movea.l a6, a0
	jsr scopes.memberIdentity
memberRestore
	move.l Frame.Module(a5), d2
	move.w d2, layout.State.Current(a6)
	tst.l d0
	bne.w bad
	move.l interface.Member.Id(a3), d0
	moveq #MEMBER, d2
	bsr.w appendMap
	adda.w #interface.MEMBER_BYTES, a3
	subq.l #1, d7
	bra.w memberRow
exportsBegin
	movea.l Frame.Image(a5), a3
	move.l interface.Header.ExportCount(a3), d7
	adda.l interface.Header.Exports(a3), a3
export
	tst.l d7
	beq.w valuesBegin
	tst.l interface.Export.NameBytes(a3)
	beq.w hidden
	movea.l Frame.Image(a5), a0
	adda.l interface.Export.Name(a3), a0
	move.l interface.Export.NameBytes(a3), d0
	movea.l a6, a1
	jsr scopes.bindOrigin
	bne.w bad
	bra.w declare
hidden
	movea.l Frame.Reuse(a5), a2
	cmpa.w #0, a2
	beq.w newHidden
	movea.l Frame.Context(a5), a0
	move.l interface.Export.Id(a3), d1
	jsr (a2)
	tst.l d0
	beq.w reused
	cmpi.l #NOT_FOUND, d0
	bne.w bad
newHidden
	bsr.w hiddenIdentity
	bne.w bad
declare
	move.l d1, Local.Global(a4)
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	lea layout.LEXICAL_STATE(a6), a0
	lea Local.Entry(a4), a1
	jsr lexical.readEntry
	bne.w bad
	move.w records.Entry.Flags(a1), d2
	andi.w #scopes.DECLARED+records.IMPORT_PROXY+records.BINDING_PROXY, d2
	bne.w bad
	ori.w #scopes.DECLARED, records.Entry.Flags(a1)
	move.l Frame.Module(a5), d2
	move.w d2, records.Entry.Owner(a1)
	cmpi.l #values.STRUCT_TYPE, interface.Export.Kind(a3)
	bne.w scalarFlag
	ori.w #records.STRUCT_TYPE, records.Entry.Flags(a1)
scalarFlag
	cmpi.l #interface.FRAGMENT_ADDRESS, interface.Export.Kind(a3)
	beq.w labelFlag
	tst.l interface.Export.Provenance(a3)
	beq.w publishEntry
labelFlag
	ori.w #records.LABEL_VALUE, records.Entry.Flags(a1)
publishEntry
	move.l Local.Global(a4), d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	lea layout.LEXICAL_STATE(a6), a0
	jsr lexical.writeEntry
	bne.w bad
	lea layout.MODULE_STATE(a6), a0
	clr.w modules.State.Visibility(a0)
	tst.l interface.Export.NameBytes(a3)
	beq.w claim
	move.w #modules.PUBLIC, modules.State.Visibility(a0)
claim
	move.l Local.Global(a4), d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	jsr modules.claim
reused
	move.l interface.Export.Id(a3), d0
	tst.l interface.Export.NameBytes(a3)
	beq.w mapHidden
	move.l Local.Global(a4), d1
mapHidden
	moveq #VALUE, d2
	bsr.w appendMap
	adda.w #interface.EXPORT_BYTES, a3
	subq.l #1, d7
	bra.w export
valuesBegin
	move.l Frame.Image(a5), Local.Copy+remap.Frame.Image(a4)
	move.l Frame.Bytes(a5), Local.Copy+remap.Frame.Bytes(a4)
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a6), a0
	move.l a0, Local.Copy+remap.Frame.Owner(a4)
	lea resolve, a0
	move.l a0, Local.Copy+remap.Frame.Resolve(a4)
	move.l a5, Local.Copy+remap.Frame.Context(a4)
	movea.l Frame.Image(a5), a3
	move.l interface.Header.ExportCount(a3), d7
	adda.l interface.Header.Exports(a3), a3
valueRow
	tst.l d7
	beq.w good
	move.l interface.Export.Id(a3), d1
	moveq #VALUE, d0
	movea.l a5, a0
	bsr.w find
	bne.w bad
	move.l d1, Local.Global(a4)
	tst.l interface.Export.NameBytes(a3)
	bne.w valueCopy
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	movea.l layout.IMPORT_STATE+imports.KNOWN_DEFINED_POINTER(a6), a0
	tst.b 0(a0, d0.l)
	beq.w valueCopy
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a6), a0
	jsr values.getKind
	bne.w bad
	cmpi.l #values.STRUCT_TYPE, d2
	bne.w bad
	bra.w nextValue
valueCopy
	cmpi.l #interface.FRAGMENT_ADDRESS, interface.Export.Kind(a3)
	beq.w address
	tst.l interface.Export.Provenance(a3)
	bne.w address
	move.l a3, Local.Copy+remap.Frame.Export(a4)
	lea Local.Copy(a4), a0
	jsr remap.copy
	bne.w bad
	move.l Local.Copy+remap.Frame.Low(a4), d2
	move.l Local.Copy+remap.Frame.High(a4), d3
	move.l Local.Copy+remap.Frame.Kind(a4), d4
	moveq #1, d5
	bra.w store
address
	movea.l Frame.Address(a5), a1
	move.l a1, d0
	beq.w bad
	movea.l Frame.Context(a5), a0
	move.l interface.Export.Id(a3), d0
	move.l interface.Export.Low(a3), d2
	move.l interface.Export.Provenance(a3), d3
	cmpi.l #interface.FRAGMENT_ADDRESS, interface.Export.Kind(a3)
	bne.w callAddress
	move.l interface.Export.High(a3), d3
callAddress
	jsr (a1)
	tst.l d0
	bne.w bad
	moveq #0, d2
	moveq #0, d3
	moveq #values.SCALAR, d4
	moveq #0, d5
store
	move.l Local.Global(a4), d1
	bsr.w storeKnown
	bne.w bad
nextValue
	adda.w #interface.EXPORT_BYTES, a3
	subq.l #1, d7
	bra.w valueRow
good
	moveq #OK, d7
	bra.w close
bad
	moveq #INVALID, d7
close
	tst.l Local.Opened(a4)
	beq.w done
	movea.l a6, a0
	jsr scopes.endFileDerived
	beq.w ended
	moveq #INVALID, d7
ended
	movea.l a6, a0
	moveq #0, d0
	jsr scopes.endFile
	beq.w done
	moveq #INVALID, d7
done
	move.l d7, d0
	adda.w #LOCAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; install

; A0=Frame,D0=VALUE/MEMBER,D1=source ID. D0/CCR=status,D1=mapped ID.
; Other registers preserved. Map rows retain numeric identities only.
find	.block
	movem.l d2-d4/a0-a1, -(sp)
	move.l d0, d4
	move.l Frame.Map+memory.Block.Used(a0), d3
	movea.l Frame.Map+memory.Block.Pointer(a0), a1
next
	tst.l d3
	beq.w absent
	cmp.l MapRow.Kind(a1), d4
	bne.w advance
	cmp.l MapRow.Local(a1), d1
	bne.w advance
	move.l MapRow.Global(a1), d1
	moveq #OK, d0
	bra.w done
advance
	adda.w #MAP_BYTES, a1
	subi.l #MAP_BYTES, d3
	bra.w next
absent
	moveq #NOT_FOUND, d0
done
	movem.l (sp)+, d2-d4/a0-a1
	tst.l d0
	rts
	.bend  ; find
; A0=scope,D0=owning module lexical index+1. D0/CCR=status,D1=unindexed
; wrapper identity; other registers preserved. Shares only the public module
; spelling. Wrapper labels are separate from all public fragment addresses.
allocateOwner	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l sp, a4
	movea.l a0, a6
	move.l d0, d7
	tst.l d0
	beq.w bad
	moveq #0, d2
	move.w layout.State.Count(a6), d2
	cmp.l d2, d0
	bhi.w bad
	subq.l #1, d0
	lea Local.ModuleEntry(a4), a1
	lea layout.LEXICAL_STATE(a6), a0
	jsr lexical.readEntry
	bne.w bad
	bsr.w hiddenIdentity
	bne.w bad
	move.w #scopes.DECLARED+records.LABEL_VALUE, Local.Entry+records.Entry.Flags(a4)
	move.w #scopes.KIND_BLOCK, Local.Entry+records.Entry.ScopeKind(a4)
	move.w d7, Local.Entry+records.Entry.Owner(a4)
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	lea Local.Entry(a4), a1
	lea layout.LEXICAL_STATE(a6), a0
	jsr lexical.writeEntry
	bne.w bad
	lea layout.MODULE_STATE(a6), a0
	move.w modules.State.Active(a0), d2
	move.w modules.State.Visibility(a0), d3
	move.w d7, modules.State.Active(a0)
	clr.w modules.State.Visibility(a0)
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	jsr modules.claim
	move.w d2, modules.State.Active(a0)
	move.w d3, modules.State.Visibility(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #LOCAL_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; allocateOwner

	.priv
; A5=Frame,D0=local ID,D1=global ID,D2=kind. Reserved once before binding.
appendMap	.block
	movem.l a0-a1, -(sp)
	lea Frame.Map(a5), a0
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d0, MapRow.Local(a1)
	move.l d1, MapRow.Global(a1)
	move.l d2, MapRow.Kind(a1)
	addi.l #MAP_BYTES, memory.Block.Used(a0)
	movem.l (sp)+, a0-a1
	rts
	.bend  ; appendMap

; A4=Local,A6=scope. Allocate an unindexed numeric schema identity. The name
; handle aliases the existing public module name solely for lexical lifecycle.
; D0/CCR=status,D1=absolute ID; other registers kept.
hiddenIdentity	.block
	movem.l d2-d4/a0-a2, -(sp)
	moveq #0, d4
	move.w layout.State.Count(a6), d4
	move.l d4, d0
	addq.l #1, d0
	movea.l a6, a0
	jsr scopes.reserveIdentities
	bne.w done
	lea Local.Entry(a4), a1
	moveq #records.ENTRY_BYTES/2-1, d0
clear
	clr.w (a1)+
	dbra d0, clear
	lea Local.Entry(a4), a1
	move.l Local.ModuleEntry+records.Entry.Name(a4), records.Entry.Name(a1)
	move.w Local.ModuleEntry+records.Entry.Length(a4), records.Entry.Length(a1)
	move.w #$ffff, records.Entry.MemberId(a1)
	moveq #0, d1
	move.w layout.State.Base(a6), d1
	add.l d4, d1
	move.w d1, records.Entry.Target(a1)
	move.l d4, d0
	lea layout.LEXICAL_STATE(a6), a0
	jsr lexical.appendEntry
	bne.w done
	addq.w #1, layout.State.Count(a6)
	moveq #OK, d0
done
	movem.l (sp)+, d2-d4/a0-a2
	tst.l d0
	rts
	.bend  ; hiddenIdentity

; remap resolver ABI. Compound addresses reject until origin IDs are retained.
resolve	.block
	cmpi.l #remap.NOMINAL, d0
	beq.w mapNominal
	cmpi.l #remap.MEMBER, d0
	beq.w mapMember
	moveq #INVALID, d0
	rts
mapNominal
	moveq #VALUE, d0
	bra.w find
mapMember
	moveq #MEMBER, d0
	bra.w find
	.bend  ; resolve

; A6=scope,D1=absolute ID,D2/D3=value,D4=kind,D5=known. Others kept.
storeKnown	.block
	movem.l d1-d6/a0-a2, -(sp)
	move.l d1, d6
	sub.w layout.State.Base(a6), d6
	andi.l #$ffff, d6
	movea.l layout.IMPORT_STATE+imports.KNOWN_DEFINED_POINTER(a6), a0
	move.b d5, 0(a0, d6.l)
	lsl.l #3, d6
	movea.l layout.IMPORT_STATE+imports.KNOWN_VALUES_POINTER(a6), a0
	move.l d2, runtime.Value.Low(a0, d6.l)
	move.l d3, runtime.Value.High(a0, d6.l)
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a6), a0
	move.l d4, d2
	jsr values.setKind
	movem.l (sp)+, d1-d6/a0-a2
	tst.l d0
	rts
	.bend  ; storeKnown
	.endsection
	.endmodule
