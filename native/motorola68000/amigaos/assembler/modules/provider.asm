; Active consumer bindings over immutable stored dependency interfaces.
; @opforge-owner: opforge.assembler.modules.provider
	.module opforge.assembler.modules.provider
	.cpu 68020
	.use opforge.assembler.prepare.frontend as frontend
	.use opforge.assembler.symbols.scope_layout as layout
	.use opforge.assembler.symbols.lexical_store as lexical
	.use opforge.assembler.symbols.binding_records as records
	.use opforge.assembler.modules.interface_store as reader
	.use opforge.assembler.modules.registry as registry
	.use opforge.assembler.modules.semantic_exports as interface
	.use opforge.assembler.modules.scope as scope
	.use opforge.platform.memory as memory
	.use opforge.assembler.values.values as values
	.use opforge.assembler.layout.assembly as assembly
	.use opforge.packages.format as package
	.use opforge.assembler.layout.dependencies as dependencies
	.use opforge.assembler.symbols.scopes as scopes
	.use opforge.assembler.modules.remap as remap
	.use opforge.assembler.modules.templates as templates
	.use opforge.assembler.modules.template_names as names
	.use opforge.vm.exprvm.runtime as runtime
	.pub
State	.struct
Frontend	.long ?
Registry	.long ?
Scope	.long ?
Base	.long ?
Count	.long ?
Depth	.long ?
FallbackModule	.long ?
Address	.long ?
AddressContext	.long ?
ActiveProvider	.long ?
ActiveWrapper	.long ?
Export	.res interface.EXPORT_BYTES
RecordBytes	.long ?
Remap	.res remap.FRAME_BYTES
TemplateImage	.res memory.Block.Used+4
TemplateFrame	.res templates.DEMAND_BYTES
TemplateProvider	.long ?
TemplateModule	.long ?
TemplateSource	.long ?
TemplateFound	.long ?
LocalValue	.long ?
LocalTemplate	.long ?
ResolveModule	.long ?
Modules	.res memory.Block.Used+4
Bindings	.res memory.Block.Used+4
Reader	.res reader.STATE_BYTES
Demand	.res scope.DEMAND_BYTES
Image	.res memory.Block.Used+4
	.endstruct
STATE_BYTES = State.Image+memory.Block.Used+4
Module	.struct
Local	.long ?
Stored	.long ?
	.endstruct
MODULE_BYTES = Module.Stored+4
Binding	.struct
Provider	.long ?
Entity	.long ?
Consumer	.long ?
Kind	.long ?
Wrapper	.long ?
Ready	.long ?
	.endstruct
BINDING_BYTES = Binding.Ready+4
OK = 0
INVALID = 1
NOT_FOUND = 2
VALUE = 0
MEMBER = 1
TEMPLATE = 2
ORIGIN_VALUE = 3
ORIGIN_MEMBER = 4
	.section code, kind=code
	.pub
; A0=zero State,A1=consumer frontend,A2=stored module registry.
; D0/CCR=status; others preserved. The registry outlives this active consumer.
begin	.block
	movem.l a0-a3, -(sp)
	movea.l a0, a3
	move.l a1, State.Frontend(a3)
	move.l a2, State.Registry(a3)
	movea.l frontend.Frame.Scratch(a1), a0
	cmpa.w #0, a0
	beq.w bad
	adda.l #frontend.SCOPE_STATE, a0
	move.l a0, State.Scope(a3)
	move.l #resolve, layout.State.DependencyResolve(a0)
	move.l #owners, layout.State.DependencyOwners(a0)
	move.l #scopes.bindCanonicalOrigin, layout.State.DependencyBinder(a0)
	clr.w layout.State.DependencyFailed(a0)
	move.l a3, layout.State.DependencyContext(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, a0-a3
	tst.l d0
	rts
	.bend  ; begin

; A0=State,D1=consumer lexical module index+1,D2=stored module ID.
; Register only the module handle; never enumerate its exported names.
; D0/CCR=status; other registers preserved.
add	.block
	movem.l d1-d4/a0-a2, -(sp)
	movea.l a0, a2
	tst.l d1
	beq.w bad
	move.l State.Modules+memory.Block.Used(a2), d4
	movea.l State.Modules+memory.Block.Pointer(a2), a1
	move.l d4, d3
find
	tst.l d3
	beq.w append
	cmp.l Module.Local(a1), d1
	beq.w existing
	adda.w #MODULE_BYTES, a1
	subi.l #MODULE_BYTES, d3
	bra.w find
existing
	cmp.l Module.Stored(a1), d2
	bne.w bad
	bra.w good
append
	move.l d4, d0
	addi.l #MODULE_BYTES, d0
	bcs.w bad
	lea State.Modules(a2), a0
	move.l d1, d3
	move.l #65536, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l State.Modules+memory.Block.Pointer(a2), a1
	adda.l d4, a1
	move.l d3, Module.Local(a1)
	move.l d2, Module.Stored(a1)
	addi.l #MODULE_BYTES, State.Modules+memory.Block.Used(a2)
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d4/a0-a2
	tst.l d0
	rts
	.bend  ; add

; A0=State,D1=provider,D2=entity,D3=VALUE/MEMBER. Lookup numeric uses only.
; D0/CCR=status,D1=consumer on success; other registers preserved.
lookup	.block
	movem.l d2-d4/a0-a1, -(sp)
	move.l State.Bindings+memory.Block.Used(a0), d4
	movea.l State.Bindings+memory.Block.Pointer(a0), a1
next
	tst.l d4
	beq.w absent
	cmp.l Binding.Provider(a1), d1
	bne.w advance
	cmp.l Binding.Entity(a1), d2
	bne.w advance
	cmp.l Binding.Kind(a1), d3
	bne.w advance
	move.l Binding.Consumer(a1), d1
	moveq #OK, d0
	bra.w done
advance
	adda.w #BINDING_BYTES, a1
	subi.l #BINDING_BYTES, d4
	bra.w next
absent
	moveq #NOT_FOUND, d0
done
	movem.l (sp)+, d2-d4/a0-a1
	tst.l d0
	rts
	.bend  ; lookup

; A0=State,D1=provider,D2=entity,D3=already allocated consumer ID.
; Internal exact-entity demand, including private closure; public visibility
; remains lookupName's responsibility. D0/CCR=status,D1=canonical consumer.
; Other registers preserved; does not allocate a second consumer identity.
acquire	.block
	movem.l d2-d4/a0, -(sp)
	move.l d3, d4
	moveq #VALUE, d3
	bsr.w lookup
	tst.l d0
	beq.w done
	move.l d4, d3
	cmpi.l #64, State.Depth(a0)
	bhs.w depthBad
	addq.l #1, State.Depth(a0)
	bsr.w materialize
	subq.l #1, State.Depth(a0)
	tst.l d0
	bne.w done
	move.l d1, -(sp)
	bsr.w drain
	move.l (sp)+, d1
	bra.w done
depthBad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d4/a0
	tst.l d0
	rts
	.bend  ; acquire
	.priv

materialize	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #records.ENTRY_BYTES+records.NAME_BYTES, sp
	movea.l a0, a6
	move.l d3, d6
	move.l d1, d4
	move.l d2, d5
	move.l d4, d3
	movea.l State.Modules+memory.Block.Pointer(a6), a0
	move.l State.Modules+memory.Block.Used(a6), d0
module
	tst.l d0
	beq.w fallback
	cmp.l Module.Stored(a0), d3
	beq.w found
	adda.w #MODULE_BYTES, a0
	subi.l #MODULE_BYTES, d0
	bra.w module
found
	move.l Module.Local(a0), State.Demand+scope.DemandFrame.Module(a6)
	move.l Module.Local(a0), State.FallbackModule(a6)
	bra.w scoped
fallback
	tst.l State.FallbackModule(a6)
	beq.w bad
	move.l State.FallbackModule(a6), State.Demand+scope.DemandFrame.Module(a6)
scoped
	movea.l State.Scope(a6), a4
	cmpa.w #0, a4
	beq.w bad
	move.l d6, d0
	sub.w layout.State.Base(a4), d0
	bcs.w bad
	andi.l #$ffff, d0
	lea layout.LEXICAL_STATE(a4), a0
	movea.l sp, a1
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d7
	move.w records.Entry.Length(sp), d7
	move.l records.Entry.Name(sp), d0
	move.l d7, d1
	lea layout.LEXICAL_STATE(a4), a0
	lea records.ENTRY_BYTES(sp), a1
	jsr lexical.copyName
	bne.w bad
	lea records.ENTRY_BYTES(sp), a5
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	move.l d3, d1
	move.l d3, State.ActiveProvider(a6)
	clr.l State.ActiveWrapper(a6)
	jsr reader.select
	bne.w bad
	move.l d5, d1
	jsr reader.valueRow
	bne.w bad
	cmpi.l #interface.FORWARDED_ENTITY, interface.Export.Kind(a1)
	beq.w forwarded
	bsr.w hydrate
	bra.w done
forwarded
	move.l interface.Export.High(a1), -(sp)
	move.l interface.Export.Low(a1), d1
	jsr reader.dependencyName
	bne.w forwardedBad
	move.l d1, d2
	movea.l State.Registry(a6), a0
	jsr registry.findObject
	bne.w forwardedBad
	move.l (sp)+, d2
	move.l d6, d3
	movea.l a6, a0
	bsr.w acquire
	tst.l d0
	bne.w done
	move.l d1, d6
	move.l d4, d3
	bsr.w valueSlot
	bne.w bad
	movea.l State.Bindings+memory.Block.Pointer(a6), a0
	adda.l d4, a0
	move.l d6, Binding.Consumer(a0)
	move.l #2, Binding.Ready(a0)
	move.l d6, d1
	moveq #OK, d0
	bra.w done
forwardedBad
	addq.l #4, sp
	bra.w bad
bad
	moveq #INVALID, d0
done
	adda.w #records.ENTRY_BYTES+records.NAME_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; materialize
	.pub

; Import callback. The canonical name is matched against registered module
; prefixes, longest first. A successful lookup hydrates only that one entity.
; A0/D0=name,D1=consumer ID,A1=scope,A2=State. D0/CCR=status,D1=ID;
; all other registers preserved. Local declarations retain precedence.
resolve	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #records.ENTRY_BYTES+2*records.NAME_BYTES, sp
	movea.l a2, a6
	movea.l a0, a5
	move.l d0, d7
	move.l d1, d6
	tst.l d7
	beq.w bad
	cmpi.l #records.NAME_BYTES-1, d7
	bhi.w bad
	lea records.ENTRY_BYTES+records.NAME_BYTES(sp), a1
	move.l d7, d0
copySpelling
	move.b (a5)+, (a1)+
	subq.l #1, d0
	bne.w copySpelling
	lea records.ENTRY_BYTES+records.NAME_BYTES(sp), a5
	movea.l State.Scope(a6), a4
	move.l d1, d0
	sub.w layout.State.Base(a4), d0
	bcs.w absent
	andi.l #$ffff, d0
	lea layout.LEXICAL_STATE(a4), a0
	movea.l sp, a1
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d0
	btst #0, records.Entry.Flags+1(sp)
	beq.w valueFresh
	moveq #1, d0
valueFresh
	move.l d0, State.LocalValue(a6)
	moveq #0, d0
	move.w records.Entry.TemplateFlags(sp), d0
	andi.l #records.TEMPLATE_DECLARED+records.STATEMENT_DECLARED, d0
	move.l d0, State.LocalTemplate(a6)
	movea.l State.Modules+memory.Block.Pointer(a6), a3
	move.l State.Modules+memory.Block.Used(a6), d5
	moveq #0, d4
	moveq #0, d3
module
	tst.l d5
	beq.w selected
	move.l Module.Local(a3), d0
	subq.l #1, d0
	lea layout.LEXICAL_STATE(a4), a0
	movea.l sp, a1
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d2
	move.w records.Entry.Length(sp), d2
	cmp.l d7, d2
	bhs.w next
	cmp.l d4, d2
	bls.w next
	lea layout.LEXICAL_STATE(a4), a0
	move.l records.Entry.Name(sp), d0
	move.l d2, d1
	lea records.ENTRY_BYTES(sp), a1
	jsr lexical.copyName
	bne.w bad
	movea.l a5, a0
	lea records.ENTRY_BYTES(sp), a1
	move.l d2, d0
	bsr.w equal
	bne.w next
	cmpi.b #'.', 0(a5, d2.l)
	bne.w next
	move.l d2, d4
	move.l Module.Stored(a3), d3
	move.l Module.Local(a3), State.Demand+scope.DemandFrame.Module(a6)
	move.l Module.Local(a3), State.ResolveModule(a6)
	move.l Module.Local(a3), State.FallbackModule(a6)
next
	adda.w #MODULE_BYTES, a3
	subi.l #MODULE_BYTES, d5
	bra.w module
selected
	tst.l d4
	beq.w absent
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	move.l d3, d1
	move.l d3, State.ActiveProvider(a6)
	clr.l State.ActiveWrapper(a6)
	jsr reader.select
	bne.w bad
	clr.l State.TemplateFound(a6)
	tst.l State.LocalTemplate(a6)
	bne.w templateReady
	bsr.w templateName
	cmpi.l #INVALID, d0
	beq.w bad
templateReady
	; Template closure reads can select transitive providers. Public lookup
	; resumes in the original importing module's interface directory.
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	move.l d3, d1
	jsr reader.select
	bne.w bad
	tst.l State.LocalValue(a6)
	bne.w noValue
	movea.l a5, a1
	move.l d7, d1
	lea State.Reader(a6), a0
	jsr reader.lookupName
	tst.l d0
	beq.w valueFound
	cmpi.l #NOT_FOUND, d0
	bne.w bad
	movea.l a5, a1
	adda.l d4, a1
	addq.l #1, a1
	move.l d7, d1
	sub.l d4, d1
	subq.l #1, d1
	lea State.Reader(a6), a0
	jsr reader.lookupName
	cmpi.l #NOT_FOUND, d0
	beq.w noValue
	tst.l d0
	bne.w bad
valueFound
	move.l d1, d5
	movea.l a6, a0
	move.l d3, d1
	move.l d5, d2
	move.l d6, d3
	bsr.w acquire
	tst.l d0
	bne.w done
	cmp.l d6, d1
	beq.w bound
	movea.l State.Scope(a6), a0
	move.l d6, d0
	move.l State.ResolveModule(a6), d2
	jsr scope.alias
	bne.w bad
bound
	move.l d6, d1
	moveq #OK, d0
	bra.w done
noValue
	tst.l State.LocalValue(a6)
	bne.w templateOnly
	bsr.w admitMemberBase
	cmpi.l #INVALID, d0
	beq.w bad
templateOnly
	tst.l State.TemplateFound(a6)
	beq.w absent
	bra.w bound
absent
	move.l d6, d1
	moveq #NOT_FOUND, d0
	bra.w done
bad
	move.l d6, d1
	movea.l State.Scope(a6), a0
	move.w #1, layout.State.DependencyFailed(a0)
	moveq #INVALID, d0
done
	adda.w #records.ENTRY_BYTES+2*records.NAME_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; resolve
	.priv

; A6=State,A5/D7=owned full spelling,D4=registered module prefix,D3=provider.
; Admit the longest public struct/literal base before generic dotted-value
; capture. The ordinary binding-plan owner still selects/evaluates its fields.
; D0/CCR=status; all other registers preserved.
admitMemberBase	.block
	movem.l d1-d7/a0-a5, -(sp)
	move.l d7, d5
component
	subq.l #1, d5
	cmp.l d4, d5
	bls.w absent
	cmpi.b #'.', 0(a5, d5.l)
	bne.w component
	lea State.Reader(a6), a0
	movea.l a5, a1
	move.l d5, d1
	jsr reader.lookupName
	cmpi.l #NOT_FOUND, d0
	beq.w component
	tst.l d0
	bne.w bad
	move.l d1, d6
	jsr reader.valueRow
	bne.w bad
	cmpi.l #values.STRUCT_TYPE, interface.Export.Kind(a1)
	beq.w declared
	cmpi.l #values.STRUCT_LITERAL, interface.Export.Kind(a1)
	bne.w absent
declared
	movea.l State.Scope(a6), a2
	move.w layout.State.Current(a2), -(sp)
	move.w layout.State.BindingNamespace(a2), -(sp)
	move.w layout.State.FirstBound(a2), -(sp)
	move.w layout.State.FirstExplicit(a2), -(sp)
	clr.w layout.State.Current(a2)
	clr.w layout.State.BindingNamespace(a2)
	movea.l a5, a0
	movea.l a2, a1
	move.l d5, d0
	jsr scopes.bindCanonicalOrigin
	move.w (sp)+, layout.State.FirstExplicit(a2)
	move.w (sp)+, layout.State.FirstBound(a2)
	move.w (sp)+, layout.State.BindingNamespace(a2)
	move.w (sp)+, layout.State.Current(a2)
	tst.l d0
	bne.w bad
	move.l d1, -(sp)
	move.l d3, d1
	move.l d6, d2
	move.l (sp), d3
	movea.l a6, a0
	bsr.w acquire
	move.l (sp)+, d6
	tst.l d0
	bne.w bad
	cmp.l d6, d1
	beq.w good
	movea.l State.Scope(a6), a0
	move.l d6, d0
	move.l State.ResolveModule(a6), d2
	jsr scope.alias
	bra.w done
good
	moveq #OK, d0
	bra.w done
absent
	moveq #NOT_FOUND, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; admitMemberBase

; A0=State. Drain numeric nominal closure after the prior arena borrow ends.
; D0/CCR=status; all other registers preserved. One materialization window is
; reused; pending entities cannot retain source leases or complete frontends.
drain	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	moveq #0, d7
next
	cmp.l State.Bindings+memory.Block.Used(a6), d7
	bhs.w good
	movea.l State.Bindings+memory.Block.Pointer(a6), a1
	adda.l d7, a1
	addi.l #BINDING_BYTES, d7
	tst.l Binding.Kind(a1)
	bne.w next
	tst.l Binding.Ready(a1)
	bne.w next
	move.l Binding.Provider(a1), d1
	move.l Binding.Entity(a1), d2
	move.l Binding.Consumer(a1), d3
	movea.l a6, a0
	bsr.w materialize
	tst.l d0
	bne.w done
	bra.w next
good
	moveq #OK, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; drain

; A6=State,D3=provider,D5=entity,D6=consumer,A5/D7=owned name.
; Reader is selected. D0/CCR=status,D1=canonical consumer; others preserved.
hydrate	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea State.Reader(a6), a0
	move.l d5, d1
	jsr reader.valueRow
	bne.w bad
	lea State.Export(a6), a0
	moveq #interface.EXPORT_BYTES/4-1, d0
export
	move.l (a1)+, (a0)+
	dbra d0, export
	lea State.Export(a6), a1
	clr.l State.RecordBytes(a6)
	cmpi.l #interface.FRAGMENT_ADDRESS, interface.Export.Kind(a1)
	beq.w supported
	tst.l interface.Export.Kind(a1)
	beq.w supported
	lea State.Reader(a6), a0
	move.l interface.Export.Low(a1), d2
	moveq #values.HEADER_BYTES, d3
	jsr reader.valueRecord
	bne.w bad
	jsr values.descriptorBytes
	bne.w bad
	move.l d2, State.RecordBytes(a6)
supported
	move.l d7, d4
	addi.l #interface.HEADER_BYTES+interface.EXPORT_BYTES+7, d4
	andi.l #$fffffff8, d4
	move.l d4, d0
	add.l State.RecordBytes(a6), d0
	bcs.w bad
	cmpi.l #reader.WINDOW_BYTES, d0
	bhi.w bad
	lea State.Image(a6), a0
	move.l d4, d0
	add.l State.RecordBytes(a6), d0
	jsr memory.reserveExact
	bne.w bad
	movea.l State.Image+memory.Block.Pointer(a6), a2
	move.l d4, d0
	add.l State.RecordBytes(a6), d0
	lsr.l #2, d0
	subq.l #1, d0
clear
	clr.l (a2)+
	dbra d0, clear
	movea.l State.Image+memory.Block.Pointer(a6), a2
	move.l #interface.MAGIC, interface.Header.Magic(a2)
	move.l d4, d0
	add.l State.RecordBytes(a6), d0
	move.l d0, interface.Header.Bytes(a2)
	move.l #1, interface.Header.ExportCount(a2)
	move.l #interface.HEADER_BYTES, interface.Header.Exports(a2)
	move.l #interface.HEADER_BYTES+interface.EXPORT_BYTES, interface.Header.Members(a2)
	move.l #interface.HEADER_BYTES+interface.EXPORT_BYTES, interface.Header.Names(a2)
	move.l d7, interface.Header.NameBytes(a2)
	move.l d4, interface.Header.Arena(a2)
	move.l State.RecordBytes(a6), interface.Header.ArenaBytes(a2)
	lea interface.HEADER_BYTES(a2), a0
	lea State.Export(a6), a1
	moveq #interface.EXPORT_BYTES/4-1, d0
copyRow
	move.l (a1)+, (a0)+
	dbra d0, copyRow
	tst.l State.Export+interface.Export.NameBytes(a6)
	beq.w privateName
	move.l #interface.HEADER_BYTES+interface.EXPORT_BYTES, interface.HEADER_BYTES+interface.Export.Name(a2)
	move.l d7, interface.HEADER_BYTES+interface.Export.NameBytes(a2)
	bra.w named
privateName
	clr.l interface.HEADER_BYTES+interface.Export.Name(a2)
	clr.l interface.HEADER_BYTES+interface.Export.NameBytes(a2)
named
	lea interface.HEADER_BYTES+interface.EXPORT_BYTES(a2), a0
	movea.l a5, a1
	move.l d7, d0
copyName
	move.b (a1)+, (a0)+
	subq.l #1, d0
	bne.w copyName
	tst.l State.RecordBytes(a6)
	beq.w copied
	lea State.Reader(a6), a0
	move.l State.Export+interface.Export.Low(a6), d2
	move.l State.RecordBytes(a6), d3
	jsr reader.valueRecord
	bne.w bad
	movea.l State.Image+memory.Block.Pointer(a6), a2
	movea.l a2, a0
	adda.l d4, a0
	move.l State.RecordBytes(a6), d0
copyRecord
	move.b (a1)+, (a0)+
	subq.l #1, d0
	bne.w copyRecord
	clr.l interface.HEADER_BYTES+interface.Export.Low(a2)
copied
	move.l a2, State.Demand+scope.DemandFrame.Image(a6)
	move.l d4, d0
	add.l State.RecordBytes(a6), d0
	move.l d0, State.Demand+scope.DemandFrame.Bytes(a6)
	adda.w #interface.HEADER_BYTES, a2
	move.l a2, State.Demand+scope.DemandFrame.Export(a6)
	move.l State.Scope(a6), State.Demand+scope.DemandFrame.Scope(a6)
	move.l d6, State.Demand+scope.DemandFrame.Global(a6)
	move.l #reference, State.Demand+scope.DemandFrame.Resolve(a6)
	move.l a6, State.Demand+scope.DemandFrame.Context(a6)
	move.l #address, State.Demand+scope.DemandFrame.Address(a6)
	move.l State.ActiveProvider(a6), d3
	bsr.w valueSlot
	bne.w bad
	bsr.w prepareRecord
	bne.w bad
	lea State.Demand(a6), a0
	jsr scope.demand
	bne.w bad
	movea.l State.Bindings+memory.Block.Pointer(a6), a0
	adda.l d4, a0
	move.l #1, Binding.Ready(a0)
	move.l State.ActiveWrapper(a6), Binding.Wrapper(a0)
	move.l d6, d1
	moveq #OK, d0
	bra.w done
bad
	move.l d6, d1
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; hydrate

; A6=State,D3=provider,D5=entity,D6=consumer. D4=row offset output.
valueSlot	.block
	movem.l d1-d2/a0, -(sp)
	movea.l State.Bindings+memory.Block.Pointer(a6), a0
	move.l State.Bindings+memory.Block.Used(a6), d2
	moveq #0, d4
find
	cmp.l d2, d4
	bhs.w append
	tst.l Binding.Kind(a0)
	bne.w next
	cmp.l Binding.Provider(a0), d3
	bne.w next
	cmp.l Binding.Entity(a0), d5
	beq.w good
next
	adda.w #BINDING_BYTES, a0
	addi.l #BINDING_BYTES, d4
	bra.w find
append
	move.l State.Bindings+memory.Block.Used(a6), d4
	move.l d4, d0
	addi.l #BINDING_BYTES, d0
	lea State.Bindings(a6), a0
	move.l #65536, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l State.Bindings+memory.Block.Pointer(a6), a0
	adda.l d4, a0
	move.l d3, Binding.Provider(a0)
	move.l d5, Binding.Entity(a0)
	move.l d6, Binding.Consumer(a0)
	clr.l Binding.Kind(a0)
	clr.l Binding.Wrapper(a0)
	clr.l Binding.Ready(a0)
	addi.l #BINDING_BYTES, State.Bindings+memory.Block.Used(a6)
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	rts
	.bend  ; valueSlot

; scope.demand address ABI -> root's provider-qualified sparse address bridge.
; D5 wrapper output is retained numerically; all caller registers kept.
address	.block
	movem.l d1-d5/a0-a1, -(sp)
	movea.l a0, a1
	move.l d2, d4
	move.l d1, d2
	move.l d0, d1
	move.l State.ActiveProvider(a1), d0
	movea.l State.Address(a1), a0
	cmpa.w #0, a0
	beq.w bad
	move.l a0, -(sp)
	movea.l State.AddressContext(a1), a0
	move.l a1, -(sp)
	movea.l 4(sp), a1
	jsr (a1)
	movea.l (sp)+, a1
	addq.l #4, sp
	tst.l d0
	bne.w done
	move.l d5, State.ActiveWrapper(a1)
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d5/a0-a1
	tst.l d0
	rts
	.bend  ; address

; A6=State. Prebind only numeric keys needed by the current owned record.
; Nominal values are queued, not copied while remap holds its arena borrow.
; D0/CCR=status; other registers preserved.
prepareRecord	.block
	movem.l d1-d7/a0-a5, -(sp)
	tst.l State.RecordBytes(a6)
	beq.w good
	movea.l State.Image+memory.Block.Pointer(a6), a4
	adda.l interface.Header.Arena(a4), a4
	move.l values.Record.Kind(a4), d6
	move.l values.Record.Count(a4), d7
	cmpi.l #values.STRUCT_TYPE, d6
	blo.w addressRecord
	move.l values.StructRecord.Nominal(a4), d1
	moveq #VALUE, d2
	bsr.w ensure
	bne.w bad
	adda.w #values.STRUCT_BASE_BYTES, a4
	bra.w fields
addressRecord
	cmpi.l #values.ADDRESS_RECORD, d6
	bne.w good
	adda.w #values.RECORD_BASE_BYTES, a4
fields
	tst.l d7
	beq.w good
	move.l (a4), d1
	moveq #MEMBER, d2
	bsr.w ensure
	bne.w bad
	cmpi.l #values.STRUCT_TYPE, d6
	bne.w valueField
	adda.w #values.STRUCT_FIELD_BYTES, a4
	bra.w next
valueField
	adda.w #values.FIELD_BYTES, a4
next
	subq.l #1, d7
	bra.w fields
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; prepareRecord

; A6=State,D1=provider entity/key,D2=VALUE/MEMBER.
; D0/CCR=status,D1=consumer identity. New values stay queued for later copy.
ensure	.block
	movem.l d2-d7/a0-a5, -(sp)
	move.l d1, d4
	move.l d2, d5
	movea.l a6, a0
	move.l d4, d2
	move.l d5, d3
	move.l State.ActiveProvider(a6), d1
	bsr.w lookup
	tst.l d0
	beq.w done
	tst.l d5
	bne.w member
	lea State.Reader(a6), a0
	move.l d4, d1
	jsr reader.valueRow
	bne.w bad
	cmpi.l #interface.FORWARDED_ENTITY, interface.Export.Kind(a1)
	beq.w forwarded
	movea.l State.Scope(a6), a0
	move.l State.Demand+scope.DemandFrame.Module(a6), d0
	jsr scope.allocateEntity
	bne.w bad
	move.l d1, d6
	moveq #0, d3
	bra.w append
forwarded
	cmpi.l #64, State.Depth(a6)
	bhs.w bad
	move.l interface.Export.High(a1), -(sp)
	move.l interface.Export.Low(a1), d1
	jsr reader.dependencyName
	bne.w forwardReadBad
	move.l d1, d2
	movea.l State.Registry(a6), a0
	jsr registry.findObject
	bne.w forwardReadBad
	movea.l State.ActiveProvider(a6), a5
	move.l d1, State.ActiveProvider(a6)
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	jsr reader.select
	bne.w forwardReadBad
	move.l (sp)+, d1
	moveq #VALUE, d2
	addq.l #1, State.Depth(a6)
	bsr.w ensure
	subq.l #1, State.Depth(a6)
	tst.l d0
	bne.w bad
	move.l d1, d6
	move.l a5, State.ActiveProvider(a6)
	move.l a5, d1
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	jsr reader.select
	bne.w bad
	moveq #2, d3
	bra.w append
forwardReadBad
	addq.l #4, sp
	bra.w bad
member
	lea State.Reader(a6), a0
	move.l d4, d1
	jsr reader.memberRow
	bne.w bad
	move.l interface.Member.Name(a1), d2
	move.l interface.Member.NameBytes(a1), d3
	jsr reader.valueRead
	bne.w bad
	move.l d3, d0
	bsr.w bindMember
	tst.l d0
	bne.w bad
	move.l d1, d6
	moveq #1, d3
append
	move.l State.Bindings+memory.Block.Used(a6), d7
	move.l d7, d0
	addi.l #BINDING_BYTES, d0
	lea State.Bindings(a6), a0
	move.l #65536, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l State.Bindings+memory.Block.Pointer(a6), a0
	adda.l d7, a0
	move.l State.ActiveProvider(a6), Binding.Provider(a0)
	move.l d4, Binding.Entity(a0)
	move.l d6, Binding.Consumer(a0)
	move.l d5, Binding.Kind(a0)
	clr.l Binding.Wrapper(a0)
	move.l d3, Binding.Ready(a0)
	addi.l #BINDING_BYTES, State.Bindings+memory.Block.Used(a6)
	move.l d6, d1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d7/a0-a5
	tst.l d0
	rts
	.bend  ; ensure

; A6=State,A1=stored spelling,D0=bytes. D0/CCR=status,D1=bare member ID;
; other registers preserved. Metadata binding cannot alter caller line state.
bindMember	.block
	movem.l d2-d3/a0-a3, -(sp)
	movea.l State.Scope(a6), a2
	movea.l a1, a0
	movea.l a1, a3
	move.l d0, d3
leaf
	cmpi.b #'.', (a3)+
	bne.w next
	movea.l a3, a0
	move.l d3, d0
	subq.l #1, d0
next
	subq.l #1, d3
	bne.w leaf
	tst.l d0
	beq.w bad
	move.w layout.State.Current(a2), -(sp)
	move.w layout.State.BindingNamespace(a2), -(sp)
	move.w layout.State.FirstBound(a2), -(sp)
	move.w layout.State.FirstExplicit(a2), -(sp)
	clr.w layout.State.Current(a2)
	clr.w layout.State.BindingNamespace(a2)
	movea.l a2, a1
	jsr scopes.bindOrigin
	move.w (sp)+, layout.State.FirstExplicit(a2)
	move.w (sp)+, layout.State.FirstBound(a2)
	move.w (sp)+, layout.State.BindingNamespace(a2)
	move.w (sp)+, layout.State.Current(a2)
	tst.l d0
	bne.w done
	move.l d1, d3
	move.l d1, d0
	movea.l a2, a0
	jsr scopes.memberIdentity
	move.l d3, d1
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d3/a0-a3
	tst.l d0
	rts
	.bend  ; bindMember

; remap callback. The prebound map is immutable during the arena borrow.
reference	.block
	cmpi.l #remap.SECTION, d0
	beq.w bad
	movem.l d2-d3, -(sp)
	move.l d1, d2
	move.l d0, d3
	move.l State.ActiveProvider(a0), d1
	bsr.w lookup
	movem.l (sp)+, d2-d3
	tst.l d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; reference

; A0/A1=D0 bytes. ASCII folded exact comparison; all inputs preserved.
; D0/CCR=status, other registers preserved.
equal	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l d0, d3
next
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.l d1, d2
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d1, d2
	bne.w bad
	subq.l #1, d3
	bne.w next
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; equal
fold	.block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; fold
	.include "provider_tmpl.i"
	.pub
; A0=State,A1=u16 owner array,D0=scope count. Copy whole-block ownership
; only for demanded addresses whose wrapper belongs to this consumer scope.
; Foreign private-consumer wrappers are outside its ID range and stay zero.
; D0/CCR=status; other registers preserved.
owners	.block
	movem.l d1-d7/a0-a3, -(sp)
	move.l d0, d7
	movea.l State.Scope(a0), a2
	moveq #0, d6
	move.w layout.State.Base(a2), d6
	move.l State.Bindings+memory.Block.Used(a0), d5
	movea.l State.Bindings+memory.Block.Pointer(a0), a3
binding
	tst.l d5
	beq.w good
	move.l Binding.Wrapper(a3), d1
	beq.w next
	sub.l d6, d1
	bcs.w next
	cmp.l d7, d1
	bhs.w next
	move.l Binding.Consumer(a3), d2
	sub.l d6, d2
	bcs.w bad
	cmp.l d7, d2
	bhs.w bad
	add.l d1, d1
	add.l d2, d2
	move.w 0(a1, d1.l), d0
	move.w d0, 0(a1, d2.l)
next
	adda.w #BINDING_BYTES, a3
	subi.l #BINDING_BYTES, d5
	bra.w binding
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; owners

; A0=State,A1=assembly Frame. Reread demanded immutable cells after frontend
; retirement; no preparation snapshot or complete dependency owner is retained.
; D0/CCR=status; others preserved. Scalar/address bridge only at this boundary.
seed	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l assembly.Frame.Context(a1), a5
	movea.l State.Bindings+memory.Block.Pointer(a6), a4
	move.l State.Bindings+memory.Block.Used(a6), d7
binding
	tst.l d7
	beq.w good
	tst.l Binding.Kind(a4)
	bne.w next
	cmpi.l #2, Binding.Ready(a4)
	beq.w next
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	move.l Binding.Provider(a4), d1
	move.l d1, State.ActiveProvider(a6)
	jsr reader.select
	bne.w bad
	move.l Binding.Entity(a4), d1
	jsr reader.valueRow
	bne.w bad
	moveq #0, d4
	moveq #0, d6
	moveq #dependencies.ABSOLUTE, d5
	move.l interface.Export.Kind(a1), d0
	cmpi.l #interface.FRAGMENT_ADDRESS, d0
	beq.w symbolic
	tst.l d0
	bne.w compound
	tst.l interface.Export.Provenance(a1)
	bne.w symbolic
	move.l interface.Export.Low(a1), d2
	move.l interface.Export.High(a1), d3
	bra.w store
compound
	move.l d0, d4
	move.l interface.Export.Low(a1), d2
	move.l d2, -(sp)
	moveq #values.HEADER_BYTES, d3
	jsr reader.valueRecord
	bne.w recordBad
	jsr values.descriptorBytes
	bne.w recordBad
	move.l d2, d3
	move.l (sp)+, d2
	jsr reader.valueRecord
	bne.w bad
	move.l d3, d0
	moveq #0, d1
	move.l d4, d3
	movea.l package.Context.Owner(a5), a0
	jsr values.copyRecord
	bne.w bad
	move.l #reference, State.Remap+remap.Frame.Resolve(a6)
	move.l a6, State.Remap+remap.Frame.Context(a6)
	lea State.Remap(a6), a1
	jsr remap.record
	bne.w bad
	moveq #0, d3
	bra.w store
recordBad
	addq.l #4, sp
	bra.w bad
symbolic
	moveq #0, d2
	moveq #0, d3
	moveq #values.SCALAR, d4
	moveq #1, d6
	moveq #dependencies.RETAINED_ADDRESS, d5
store
	move.l Binding.Consumer(a4), d1
	movea.l package.Context.SectionIds(a5), a0
	move.b d6, 0(a0, d1.l)
	move.l d1, d6
	move.l d6, d0
	lsl.l #3, d0
	movea.l package.Context.Values(a5), a0
	move.l d2, runtime.Value.Low(a0, d0.l)
	move.l d3, runtime.Value.High(a0, d0.l)
	movea.l package.Context.Defined(a5), a0
	move.b d5, 0(a0, d6.l)
	movea.l package.Context.Owner(a5), a0
	move.l d6, d1
	move.l d4, d2
	jsr values.setKind
	bne.w bad
next
	adda.w #BINDING_BYTES, a4
	subi.l #BINDING_BYTES, d7
	bra.w binding
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seed

; A0=State. Detach before releasing its bounded backing and binding owners.
; All registers preserved; CCR unspecified.
seal	.block
	movem.l d0/a0-a2, -(sp)
	movea.l a0, a2
	movea.l State.Scope(a2), a1
	cmpa.w #0, a1
	beq.w detached
	moveq #0, d0
	move.w layout.State.Base(a1), d0
	move.l d0, State.Base(a2)
	moveq #0, d0
	move.w layout.State.Count(a1), d0
	move.l d0, State.Count(a2)
	cmpa.l layout.State.DependencyContext(a1), a2
	bne.w detached
	clr.l layout.State.DependencyResolve(a1)
	clr.l layout.State.DependencyContext(a1)
	clr.l layout.State.DependencyOwners(a1)
	clr.l layout.State.DependencyBinder(a1)
detached
	clr.l State.Scope(a2)
	clr.l State.Frontend(a2)
	lea State.Reader(a2), a0
	jsr reader.release
	lea State.Image(a2), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.TemplateImage(a2), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.Demand(a2), a0
	moveq #scope.DEMAND_BYTES/4-1, d0
clearDemand
	clr.l (a0)+
	dbra d0, clearDemand
	lea State.TemplateFrame(a2), a0
	moveq #templates.DEMAND_BYTES/4-1, d0
clearTemplate
	clr.l (a0)+
	dbra d0, clearTemplate
	movem.l (sp)+, d0/a0-a2
	moveq #OK, d0
	rts
	.bend  ; seal

release	.block
	movem.l d0/a0-a2, -(sp)
	movea.l a0, a2
	movea.l State.Scope(a2), a1
	cmpa.w #0, a1
	beq.w owners
	cmpa.l layout.State.DependencyContext(a1), a2
	bne.w owners
	clr.l layout.State.DependencyResolve(a1)
	clr.l layout.State.DependencyContext(a1)
	clr.l layout.State.DependencyOwners(a1)
	clr.l layout.State.DependencyBinder(a1)
owners
	lea State.Reader(a2), a0
	jsr reader.release
	lea State.Modules(a2), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.Bindings(a2), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.Image(a2), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.TemplateImage(a2), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l State.Scope(a2)
	clr.l State.Frontend(a2)
	clr.l State.Registry(a2)
	movem.l (sp)+, d0/a0-a2
	rts
	.bend  ; release
	.endsection
	.endmodule
