; Capture an owned module-local semantic interface before lexical retirement.
; @opforge-owner: opforge.assembler.modules.exports
	.module opforge.assembler.modules.exports
	.cpu 68020
	.use opforge.platform.memory as memory
	.use opforge.assembler.prepare.frontend as frontend
	.use opforge.assembler.symbols.scope_layout as layout
	.use opforge.assembler.symbols.scopes as scopes
	.use opforge.assembler.symbols.lexical_store as lexical
	.use opforge.assembler.symbols.binding_records as records
	.use opforge.assembler.modules.bindings as modules
	.use opforge.assembler.symbols.imports as imports
	.use opforge.packages.format as pkg
	.use opforge.assembler.values.values as values
	.use opforge.assembler.modules.semantic_exports as interface
	.use opforge.assembler.modules.templates as moduleTemplates
	.use opforge.assembler.modules.template_names as templateNames
	.use opforge.vm.exprvm.runtime as exprvm
	.pub
State	.struct
Exports	.res memory.Block.Used+4
Members	.res memory.Block.Used+4
Names	.res memory.Block.Used+4
Bits	.res memory.Block.Used+4
Targets	.res memory.Block.Used+4
Base	.long ?
Count	.long ?
ExportCount	.long ?
MemberCount	.long ?
Module	.long ?  ; zero selects all public modules; otherwise lexical index+1
Ready	.long ?
Consumed	.long ?
	.endstruct
STATE_BYTES = State.Consumed+4
OK = 0
INVALID = 1
	.priv
PrepareLocal	.struct
Entry	.res records.ENTRY_BYTES
Target	.res records.ENTRY_BYTES
ValuesOnly	.long ?  ; transient dependency projection, never serialized
	.endstruct
PREPARE_BYTES = PrepareLocal.ValuesOnly+4
SourceView	.struct
Owner	.long ?
Values	.long ?
Defined	.long ?
Sections	.long ?
Base	.long ?  ; array indexing base, never a pointer bias
Count	.long ?  ; absolute exclusive value ID bound
Scope	.long ?  ; nonzero only for retained known-snapshot publication
	.endstruct
SOURCE_BYTES = SourceView.Scope+4
PublishLocal	.struct
Frame	.res interface.FRAME_BYTES
Source	.res SOURCE_BYTES
Entry	.res records.ENTRY_BYTES
	.endstruct
PUBLISH_BYTES = PublishLocal.Entry+records.ENTRY_BYTES
	.section code, kind=code
	.pub
; A0=zero State,A1=completed frontend.Frame with live lexical names.
; D0/CCR=status; other registers preserved. Captures public value declarations
; and all referenced bare member keys. Rows retain local source IDs and owned
; Names-relative offsets; Export.Kind temporarily holds the runtime target ID.
; No frontend, scope, lexical-page or name pointer is retained. On failure State
; owns its partial buffers; release before reuse. Templates/statements and
; deferred/mutable declarations fail closed rather than disappearing silently.
; Canonical public targets are retained once. Publication adds private nominal
; schemas as numeric, nameless dependency rows and prunes unused member keys.
prepare	.block
	moveq #0, d0
	bra.w prepareModule
	.bend  ; prepare

; Same capture contract; D0 selects one owning module lexical index+1.
; Zero retains the complete standalone component interface. Selected public
; templates/statements still reject; unrelated modules do not enter capture.
prepareModule	.block
	move.l d1, -(sp)
	moveq #0, d1
	bsr.w prepareSelected
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; prepareModule

; Dependency-snapshot projection only: same inputs, nonzero owning module D0.
; Retains every public value/address and schema/member dependency, but omits
; template/statement facets. This does not establish complete module publication.
; A private consumer needing an omitted facet must fail replay and use reference
; handling. prepare/prepareModule remain strict for final producer publication.
; D0/CCR=status; other registers and partial-owner rules are unchanged.
prepareModuleValues	.block
	tst.l d0
	bne.w selected
	moveq #INVALID, d0
	rts
selected
	move.l d1, -(sp)
	moveq #1, d1
	bsr.w prepareSelected
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; prepareModuleValues
	.priv
; D1 selects dependency value projection; public entries preserve the caller D1.
prepareSelected	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #PREPARE_BYTES, sp
	movea.l sp, a4
	move.l d1, PrepareLocal.ValuesOnly(a4)
	movea.l a0, a5
	move.l d0, d3
	moveq #STATE_BYTES/4-1, d0
fresh
	tst.l (a0)+
	bne.w bad
	dbra d0, fresh
	movea.l frontend.Frame.Scratch(a1), a6
	move.l a6, d0
	beq.w bad
	adda.l #frontend.SCOPE_STATE, a6
	tst.w layout.State.Current(a6)
	bne.w bad
	tst.w layout.State.LoopDepth(a6)
	bne.w bad
	tst.w layout.State.Deferred(a6)
	bne.w bad
	moveq #0, d5
	move.w layout.State.Base(a6), d5
	move.l d5, State.Base(a5)
	moveq #0, d6
	move.w layout.State.Count(a6), d6
	move.l d6, State.Count(a5)
	cmp.l d6, d3
	bhi.w bad
	move.l d3, State.Module(a5)
	beq.w moduleReady
	move.l d3, d0
	subq.l #1, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	move.l a0, d1
	beq.w bad
	move.w 0(a0, d0.l), d0
	andi.w #modules.MODULE_ID, d0
	beq.w bad
moduleReady
	move.l d5, d0
	add.l d6, d0
	cmpi.l #65536, d0
	bhi.w bad
	move.l d6, d0
	addq.l #7, d0
	lsr.l #3, d0
	beq.w empty
	lea State.Bits(a5), a0
	jsr memory.reserveExact
	bne.w bad
	; reserveExact returns status, so compute and clear the bit extent explicitly.
	move.l d6, d1
	addq.l #7, d1
	lsr.l #3, d1
	move.l d1, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
clearBits
	clr.b (a1)+
	subq.l #1, d1
	bne.w clearBits
	lea State.Targets(a5), a0
	move.l d6, d0
	addq.l #7, d0
	lsr.l #3, d0
	jsr memory.reserveExact
	bne.w bad
	move.l d6, d1
	addq.l #7, d1
	lsr.l #3, d1
	move.l d1, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
clearTargets
	clr.b (a1)+
	subq.l #1, d1
	bne.w clearTargets
	moveq #0, d7
entry
	cmp.l d6, d7
	bhs.w canonicalBegin
	lea layout.LEXICAL_STATE(a6), a0
	movea.l a4, a1
	move.l d7, d0
	jsr lexical.readEntry
	bne.w bad
	; Proxies carry occurrence metadata, not schema/member declarations.
	; Their zero-filled MemberId is not a published immutable field key.
	move.w records.Entry.Flags(a4), d0
	andi.w #records.IMPORT_PROXY+records.BINDING_PROXY+records.ENCODING_NAME, d0
	bne.w visibility
	; Member keys need not themselves be value declarations.
	moveq #0, d0
	move.w records.Entry.MemberId(a4), d0
	cmpi.l #$ffff, d0
	beq.w visibility
	sub.l d5, d0
	bcs.w bad
	cmp.l d6, d0
	bhs.w bad
	move.l d0, d1
	lsr.l #3, d0
	movea.l State.Bits+memory.Block.Pointer(a5), a0
	adda.l d0, a0
	bset d1, (a0)
visibility
	tst.l PrepareLocal.ValuesOnly(a4)
	bne.w valueVisibility
	move.l State.Module(a5), d0
	beq.w templateVisibility
	cmp.w records.Entry.TemplateModule(a4), d0
	bne.w statementVisibility
templateVisibility
	move.w records.Entry.TemplateFlags(a4), d0
	andi.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, d0
	cmpi.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, d0
	beq.w bad
statementVisibility
	move.l State.Module(a5), d0
	beq.w testStatement
	cmp.w records.Entry.StatementModule(a4), d0
	bne.w valueVisibility
testStatement
	move.w records.Entry.TemplateFlags(a4), d0
	andi.w #records.STATEMENT_DECLARED+records.STATEMENT_PUBLIC, d0
	cmpi.w #records.STATEMENT_DECLARED+records.STATEMENT_PUBLIC, d0
	beq.w bad
valueVisibility
	move.l d7, d0
	add.l d0, d0
	addq.l #2, d0
	cmp.l layout.MODULE_STATE+modules.FLAGS+memory.Block.Used(a6), d0
	bhi.w bad
	subq.l #2, d0
	move.l State.Module(a5), d1
	beq.w ownerSelected
	movea.l layout.MODULE_STATE+modules.OWNERS_POINTER(a6), a0
	move.l a0, d2
	beq.w bad
	cmp.w 0(a0, d0.l), d1
	bne.w next
ownerSelected
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	move.l a0, d1
	beq.w bad
	move.w 0(a0, d0.l), d0
	move.w d0, d1
	andi.w #modules.MODULE_ID, d1
	bne.w next  ; module identities are not exported values
	andi.w #modules.PUBLIC, d0
	beq.w next
	btst #records.ENCODING_NAME_HIGH_BIT, records.Entry.Flags(a4)
	bne.w next
	move.w records.Entry.Flags(a4), d0
	andi.w #scopes.DECLARED, d0
	beq.w next
	move.w records.Entry.Flags(a4), d0
	andi.w #records.MUTABLE+records.CONDITIONAL_VALUE+records.IMPORT_PROXY+records.BINDING_PROXY, d0
	bne.w bad
	moveq #0, d0
	move.w records.Entry.Target(a4), d0
	sub.l d5, d0
	bcs.w bad
	cmp.l d6, d0
	bhs.w bad
	lea layout.LEXICAL_STATE(a6), a0
	lea PrepareLocal.Target(a4), a1
	jsr lexical.readEntry
	bne.w bad
	move.w PrepareLocal.Target+records.Entry.Flags(a4), d0
	andi.w #scopes.DECLARED, d0
	beq.w bad
	move.w PrepareLocal.Target+records.Entry.Flags(a4), d0
	andi.w #records.MUTABLE+records.CONDITIONAL_VALUE+records.IMPORT_PROXY+records.BINDING_PROXY, d0
	bne.w bad
	moveq #0, d0
	move.w records.Entry.Target(a4), d0
	sub.l d5, d0
	move.l d0, d1
	lsr.l #3, d0
	movea.l State.Targets+memory.Block.Pointer(a5), a0
	adda.l d0, a0
	bset d1, (a0)
	bra.w next
canonicalBegin
	moveq #0, d7
canonical
	cmp.l d6, d7
	bhs.w members
	move.l d7, d0
	lsr.l #3, d0
	movea.l State.Targets+memory.Block.Pointer(a5), a0
	adda.l d0, a0
	btst d7, (a0)
	beq.w nextCanonical
	lea layout.LEXICAL_STATE(a6), a0
	movea.l a4, a1
	move.l d7, d0
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d0
	move.w records.Entry.Target(a4), d0
	sub.l d5, d0
	cmp.l d7, d0
	bne.w bad
	bsr.w appendName
	bne.w bad
	move.l d1, d3
	lea State.Exports(a5), a0
	move.l memory.Block.Used(a0), d0
	addi.l #interface.EXPORT_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l memory.Block.Used(a0), a2
	addi.l #interface.EXPORT_BYTES, memory.Block.Used(a0)
	move.l d7, d0
	add.l d5, d0
	move.l d0, interface.Export.Id(a2)
	move.l d3, interface.Export.Name(a2)
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	move.l d0, interface.Export.NameBytes(a2)
	moveq #0, d0
	move.w records.Entry.Target(a4), d0
	move.l d0, interface.Export.Kind(a2)
	clr.l interface.Export.Low(a2)
	clr.l interface.Export.High(a2)
	clr.l interface.Export.Provenance(a2)
	addq.l #1, State.ExportCount(a5)
nextCanonical
	addq.l #1, d7
	bra.w canonical
next
	addq.l #1, d7
	bra.w entry
members
	moveq #0, d7
member
	cmp.l d6, d7
	bhs.w empty
	move.l d7, d0
	lsr.l #3, d0
	movea.l State.Bits+memory.Block.Pointer(a5), a0
	adda.l d0, a0
	btst d7, (a0)
	beq.w nextMember
	lea layout.LEXICAL_STATE(a6), a0
	movea.l a4, a1
	move.l d7, d0
	jsr lexical.readEntry
	bne.w bad
	bsr.w appendName
	bne.w bad
	bsr.w bareName
	bne.w bad
	move.l d1, d3
	lea State.Members(a5), a0
	move.l memory.Block.Used(a0), d0
	addi.l #interface.MEMBER_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l memory.Block.Used(a0), a2
	addi.l #interface.MEMBER_BYTES, memory.Block.Used(a0)
	move.l d7, d0
	add.l d5, d0
	move.l d0, interface.Member.Id(a2)
	move.l d3, interface.Member.Name(a2)
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	move.l d0, interface.Member.NameBytes(a2)
	addq.l #1, State.MemberCount(a5)
nextMember
	addq.l #1, d7
	bra.w member
empty
	move.l #1, State.Ready(a5)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #PREPARE_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepareSelected
	.include "export_closure.i"
	.pub

; A0=prepared State,A1=assembled pkg.Context,A2=empty destination Block.
; D0/CCR=status; other registers preserved. One publish attempt per prepare:
; names materialize only after final runtime cells are checked. interface.build
; copies names/compound arena into Destination; no borrowed runtime pointer
; survives success. On failure release the consumed snapshot and destination.
publish	.block
	moveq #0, d0
	bra.w publishSource
	.bend  ; publish

; A0=prepared State,A1=retained scope,A2=empty destination Block.
; Same ownership/status contract as publish, using base-relative known tables.
; Undefined declared LABEL_VALUE rows are external-symbol placeholders only:
; scalar zero with section marker1, never finalized module image semantics.
; Other undefined values reject. No runtime Context or copied source arena.
publishKnown	.block
	moveq #1, d0
	bra.w publishSource
	.bend  ; publishKnown
	.priv
publishSource	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #PUBLISH_BYTES, sp
	movea.l sp, a3
	movea.l a0, a6
	movea.l a1, a4
	lea PublishLocal.Source(a3), a5
	move.l a2, interface.Frame.Destination(a3)
	tst.l d0
	bne.w knownSource
	move.l pkg.Context.Owner(a4), SourceView.Owner(a5)
	move.l pkg.Context.Values(a4), SourceView.Values(a5)
	move.l pkg.Context.Defined(a4), SourceView.Defined(a5)
	move.l pkg.Context.SectionIds(a4), SourceView.Sections(a5)
	move.l pkg.Context.Count(a4), SourceView.Count(a5)
	clr.l SourceView.Base(a5)
	clr.l SourceView.Scope(a5)
	bra.w sourceReady
knownSource
	moveq #0, d0
	move.w layout.State.Base(a4), d0
	cmp.l State.Base(a6), d0
	bne.w bad
	move.l d0, SourceView.Base(a5)
	moveq #0, d1
	move.w layout.State.Count(a4), d1
	cmp.l State.Count(a6), d1
	bne.w bad
	add.l d1, d0
	move.l d0, SourceView.Count(a5)
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a4), a0
	move.l a0, SourceView.Owner(a5)
	move.l layout.IMPORT_STATE+imports.KNOWN_VALUES_POINTER(a4), SourceView.Values(a5)
	move.l layout.IMPORT_STATE+imports.KNOWN_DEFINED_POINTER(a4), SourceView.Defined(a5)
	clr.l SourceView.Sections(a5)
	move.l a4, SourceView.Scope(a5)
sourceReady
	cmpi.l #1, State.Ready(a6)
	bne.w bad
	tst.l State.Consumed(a6)
	bne.w bad
	move.l #1, State.Consumed(a6)
	move.l SourceView.Owner(a5), interface.Frame.Owner(a3)
	; Reuse member bits for the exact descriptor closure, not all private fields.
	movea.l State.Bits+memory.Block.Pointer(a6), a0
	move.l State.Bits+memory.Block.Used(a6), d0
clearMembers
	tst.l d0
	beq.w valuesBegin
	clr.b (a0)+
	subq.l #1, d0
	bra.w clearMembers
valuesBegin
	moveq #0, d6
export
	cmp.l State.Exports+memory.Block.Used(a6), d6
	bhs.w valueNames
	movea.l State.Exports+memory.Block.Pointer(a6), a4
	adda.l d6, a4
	move.l interface.Export.Kind(a4), d1
	cmp.l SourceView.Count(a5), d1
	bhs.w bad
	move.l d1, d5
	sub.l SourceView.Base(a5), d5
	bcs.w bad
	movea.l SourceView.Defined(a5), a0
	move.l a0, d0
	beq.w bad
	tst.b 0(a0, d5.l)
	beq.w external
	movea.l SourceView.Owner(a5), a0
	move.l a0, d0
	beq.w bad
	jsr values.getKind
	bne.w bad
	move.l d2, interface.Export.Kind(a4)
	move.l d5, d0
	lsl.l #3, d0
	movea.l SourceView.Values(a5), a0
	move.l a0, d3
	beq.w bad
	move.l exprvm.Value.Low(a0, d0.l), interface.Export.Low(a4)
	move.l exprvm.Value.High(a0, d0.l), interface.Export.High(a4)
	clr.l interface.Export.Provenance(a4)
	tst.l d2
	bne.w compound
	tst.l SourceView.Scope(a5)
	bne.w dependenciesReady
	movea.l SourceView.Sections(a5), a0
	move.l a0, d0
	beq.w bad
	moveq #0, d0
	move.b 0(a0, d1.l), d0
	cmpi.l #8, d0
	bhi.w bad
	move.l d0, interface.Export.Provenance(a4)
	bra.w dependenciesReady
external
	movea.l SourceView.Scope(a5), a0
	move.l a0, d0
	beq.w bad
	adda.l #layout.LEXICAL_STATE, a0
	lea PublishLocal.Entry(a3), a1
	move.l d5, d0
	jsr lexical.readEntry
	bne.w bad
	move.w records.Entry.Flags(a1), d0
	andi.w #scopes.DECLARED+records.LABEL_VALUE, d0
	cmpi.w #scopes.DECLARED+records.LABEL_VALUE, d0
	bne.w bad
	clr.l interface.Export.Kind(a4)
	clr.l interface.Export.Low(a4)
	clr.l interface.Export.High(a4)
	move.l #1, interface.Export.Provenance(a4)
	bra.w dependenciesReady
compound
	tst.l interface.Export.High(a4)
	bne.w bad
dependenciesReady
	bsr.w dependencies
	bne.w bad
	addi.l #interface.EXPORT_BYTES, d6
	bra.w export
valueNames
	bsr.w sortExports
	move.l State.Exports+memory.Block.Pointer(a6), interface.Frame.Exports(a3)
	move.l State.ExportCount(a6), d7
	move.l d7, interface.Frame.ExportCount(a3)
	movea.l interface.Frame.Exports(a3), a4
exportName
	tst.l d7
	beq.w memberNames
	tst.l interface.Export.NameBytes(a4)
	beq.w nextName
	move.l interface.Export.Name(a4), d0
	move.l interface.Export.NameBytes(a4), d1
	bsr.w namePointer
	bne.w bad
	move.l d1, interface.Export.Name(a4)
nextName
	adda.w #interface.EXPORT_BYTES, a4
	subq.l #1, d7
	bra.w exportName
memberNames
	bsr.w pruneMembers
	move.l State.Members+memory.Block.Pointer(a6), interface.Frame.Members(a3)
	move.l State.MemberCount(a6), d7
	move.l d7, interface.Frame.MemberCount(a3)
	movea.l interface.Frame.Members(a3), a4
member
	tst.l d7
	beq.w build
	move.l interface.Member.Name(a4), d0
	move.l interface.Member.NameBytes(a4), d1
	bsr.w namePointer
	bne.w bad
	move.l d1, interface.Member.Name(a4)
	adda.w #interface.MEMBER_BYTES, a4
	subq.l #1, d7
	bra.w member
build
	movea.l a3, a0
	jsr interface.build
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #PUBLISH_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; publishSource
	.pub

; A0=State. Release partial preparation, consumed publication or successful
; temporary inputs. Destination belongs to the caller. D0/CCR=0; others kept.
release	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	lea State.Exports(a1), a0
	jsr memory.release
	lea State.Members(a1), a0
	jsr memory.release
	lea State.Names(a1), a0
	jsr memory.release
	lea State.Bits(a1), a0
	jsr memory.release
	lea State.Targets(a1), a0
	jsr memory.release
	movea.l a1, a0
	moveq #STATE_BYTES/4-1, d1
clear
	clr.l (a0)+
	dbra d1, clear
	moveq #OK, d0
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; release
	.priv

; A6=State,A5=SourceView,A4=export. Retain exactly the numeric schema
; and member closure. No lexical owner is live here. Other registers kept.
dependencies	.block
	movem.l d1-d7/a0-a4, -(sp)
	move.l interface.Export.Kind(a4), d4
	cmpi.l #values.TYPED_LIST, d4
	blo.w good
	movea.l SourceView.Owner(a5), a0
	move.l interface.Export.Low(a4), d1
	jsr values.view
	bne.w done
	cmp.l values.Record.Kind(a1), d4
	bne.w bad
	movea.l a1, a2
	move.l values.Record.Count(a2), d7
	cmpi.l #values.STRUCT_TYPE, d4
	bhs.w nominal
	cmpi.l #values.ADDRESS_RECORD, d4
	bne.w good
	lea values.RECORD_BASE_BYTES(a2), a2
	moveq #values.FIELD_BYTES, d5
	bra.w field
nominal
	move.l values.StructRecord.Nominal(a2), d1
	movea.l SourceView.Owner(a5), a0
	jsr values.getKind
	bne.w bad
	cmpi.l #values.STRUCT_TYPE, d2
	bne.w bad
	move.l State.ExportCount(a6), d0
	movea.l State.Exports+memory.Block.Pointer(a6), a0
findSchema
	tst.l d0
	beq.w appendSchema
	cmp.l interface.Export.Id(a0), d1
	beq.w schemaReady
	adda.w #interface.EXPORT_BYTES, a0
	subq.l #1, d0
	bra.w findSchema
appendSchema
	lea State.Exports(a6), a0
	move.l memory.Block.Used(a0), d0
	addi.l #interface.EXPORT_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	addi.l #interface.EXPORT_BYTES, memory.Block.Used(a0)
	move.l d1, interface.Export.Id(a1)
	clr.l interface.Export.Name(a1)
	clr.l interface.Export.NameBytes(a1)
	move.l d1, interface.Export.Kind(a1)
	clr.l interface.Export.Low(a1)
	clr.l interface.Export.High(a1)
	clr.l interface.Export.Provenance(a1)
	addq.l #1, State.ExportCount(a6)
schemaReady
	lea values.STRUCT_BASE_BYTES(a2), a2
	moveq #values.STRUCT_FIELD_BYTES, d5
	cmpi.l #values.STRUCT_TYPE, d4
	beq.w field
	moveq #values.FIELD_BYTES, d5
field
	tst.l d7
	beq.w good
	move.l (a2), d0
	sub.l State.Base(a6), d0
	bcs.w bad
	cmp.l State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	lsr.l #3, d0
	movea.l State.Bits+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	bset d1, (a0)
	adda.l d5, a2
	subq.l #1, d7
	bra.w field
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; dependencies

; A6=State. Sort canonical rows by ID after closure growth. Others kept.
sortExports	.block
	movem.l d0-d4/a0-a2, -(sp)
	move.l State.ExportCount(a6), d4
	beq.w done
pass
	moveq #0, d3
	movea.l State.Exports+memory.Block.Pointer(a6), a0
	move.l State.ExportCount(a6), d2
	subq.l #1, d2
pair
	tst.l d2
	beq.w passDone
	lea interface.EXPORT_BYTES(a0), a1
	move.l interface.Export.Id(a0), d0
	cmp.l interface.Export.Id(a1), d0
	bls.w ordered
	movea.l a0, a2
	moveq #interface.EXPORT_BYTES/4-1, d1
swap
	move.l (a2), d0
	move.l (a1), (a2)+
	move.l d0, (a1)+
	dbra d1, swap
	moveq #1, d3
ordered
	adda.w #interface.EXPORT_BYTES, a0
	subq.l #1, d2
	bra.w pair
passDone
	tst.l d3
	beq.w done
	subq.l #1, d4
	bne.w pass
done
	movem.l (sp)+, d0-d4/a0-a2
	rts
	.bend  ; sortExports

; A6=State. Discard member names outside the retained descriptor closure.
; Names are still offsets, so overlapping table compaction is safe. Others kept.
pruneMembers	.block
	movem.l d0-d4/a0-a2, -(sp)
	movea.l State.Members+memory.Block.Pointer(a6), a1
	movea.l a1, a2
	move.l State.MemberCount(a6), d3
	moveq #0, d4
next
	tst.l d3
	beq.w finish
	move.l interface.Member.Id(a1), d0
	sub.l State.Base(a6), d0
	move.l d0, d1
	lsr.l #3, d0
	movea.l State.Bits+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	btst d1, (a0)
	beq.w skip
	move.l interface.Member.Id(a1), interface.Member.Id(a2)
	move.l interface.Member.Name(a1), interface.Member.Name(a2)
	move.l interface.Member.NameBytes(a1), interface.Member.NameBytes(a2)
	adda.w #interface.MEMBER_BYTES, a2
	addq.l #1, d4
skip
	adda.w #interface.MEMBER_BYTES, a1
	subq.l #1, d3
	bra.w next
finish
	move.l d4, State.MemberCount(a6)
	mulu.l #interface.MEMBER_BYTES, d4
	move.l d4, State.Members+memory.Block.Used(a6)
	movem.l (sp)+, d0-d4/a0-a2
	rts
	.bend  ; pruneMembers

; A5=State,A6=scope,A4=Entry copy. D0=status,D1=owned name offset.
; Preserve other registers. The copied terminator is spare capacity, excluded
; from Used. No row pointer crosses Names growth or a lexical page lookup.
appendName	.block
	movem.l d2-d3/a0-a2, -(sp)
	moveq #0, d2
	move.w records.Entry.Length(a4), d2
	beq.w bad
	cmpi.l #interface.NAME_LIMIT, d2
	bhi.w bad
	lea State.Names(a5), a0
	move.l memory.Block.Used(a0), d3
	move.l d3, d0
	add.l d2, d0
	bcs.w bad
	addq.l #1, d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d3, a1
	lea layout.LEXICAL_STATE(a6), a0
	move.l records.Entry.Name(a4), d0
	move.l d2, d1
	jsr lexical.copyName
	bne.w bad
	add.l d2, State.Names+memory.Block.Used(a5)
	move.l d3, d1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d3/a0-a2
	tst.l d0
	rts
	.bend  ; appendName

; A5=State,A4=bare-key Entry,D1=owned name offset. No scope/name lookup.
; D0/CCR=status; other registers preserved.
bareName	.block
	movem.l d1-d2/a0, -(sp)
	movea.l State.Names+memory.Block.Pointer(a5), a0
	adda.l d1, a0
	moveq #0, d2
	move.w records.Entry.Length(a4), d2
next
	cmpi.b #'.', (a0)+
	beq.w bad
	subq.l #1, d2
	bne.w next
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	rts
	.bend  ; bareName

; A6=State,D0=name offset,D1=length. D0=status,D1=borrowed name pointer.
; Preserve other registers; used only after Names has stopped growing.
namePointer	.block
	movem.l d2, -(sp)
	move.l d0, d2
	add.l d1, d2
	bcs.w bad
	cmp.l State.Names+memory.Block.Used(a6), d2
	bhi.w bad
	move.l State.Names+memory.Block.Pointer(a6), d1
	beq.w bad
	add.l d0, d1
	bcs.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; namePointer
	.endsection
	.endmodule
