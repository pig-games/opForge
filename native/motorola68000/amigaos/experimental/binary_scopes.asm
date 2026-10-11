; Preparation-only lexical scopes. Packed records retain numeric identities only.
; @opforge-owner: experimental.amigaos.binary_scopes
	.module experimental.amigaos.binary_scopes
	.cpu 68020
	.include "telemetry_macros.i"
	.include "binding_telemetry.i"
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_imports as imports
	.use experimental.amigaos.binary_values as values
	.use opasm.amigaos.binary_expression as expr
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_section_prepare as sections
	.use experimental.amigaos.binary_structs as structs
	.use experimental.amigaos.binary_struct_values as struct_values
	.use experimental.amigaos.binary_package as pkg
	.use prvm.amigaos.packed_branch as branch_parser
	.use prvm.amigaos.packed_loop as loop_parser
	.use prvm.amigaos.abi as loop_abi
	.use experimental.amigaos.binary_iterator_bindings as iterators
	.use experimental.amigaos.binary_binding_plans as plans
	.use experimental.amigaos.binary_numeric_bindings as bindings
	.pub
LIMIT = layout.LIMIT
ARENA_BYTES = layout.ARENA_BYTES
ENTRY_BYTES = records.ENTRY_BYTES
AMBIGUOUS_BINDING_OWNER = plans.AMBIGUOUS_OWNER
ENTRIES = layout.ENTRIES
ENTRIES_POINTER = layout.ENTRIES_POINTER
BUCKETS = layout.BUCKETS
ARENA = layout.ARENA
ARENA_POINTER = layout.ARENA_POINTER
BUFFER = layout.BUFFER
MODULE_STATE = layout.MODULE_STATE
IMPORT_STATE = layout.IMPORT_STATE
DECLARED = 1
REFERENCED = records.REFERENCED
OPTIONAL_REFERENCED = records.OPTIONAL_REFERENCED; skipped conditional RHS still carries graph references
EXPLICIT = 4
FIELD_BUFFER_BYTES = layout.NAME_BYTES
SAVED_FIELD_STATE_BYTES = 3*2
SAVED_BASE_BYTES = 4
KIND_BLOCK = 1
KIND_NAMESPACE = 2
LEXICAL_BLOCK_BIT = 7
LEXICAL_BLOCK = 1<<LEXICAL_BLOCK_BIT; preparation-only: macro hygiene without a reachability unit
INDENT_FLAG_BIT = source.FLAG_INDENT-1
KEY_BLOCK = 1
KEY_ENDBLOCK = 2
KEY_END = 3
KEY_NAMESPACE = 4
KEY_ENDNAMESPACE = 5
KEY_MODULE = 6
KEY_ENDMODULE = 7
KEY_PUB = 8
KEY_PRIV = 9
KEY_USE = 10
KEY_SECTION = 11
KEY_ENDSECTION = 12
KEY_REGION = 13
KEY_PLACE = 14
KEY_SEGMENT = 15
KEY_ENDSEGMENT = 16
KEY_MACRO = 17
KEY_ENDMACRO = 18
KEY_OUTPUT = 19
KEY_STRUCT = structs.KEY_STRUCT
KEY_ENDSTRUCT = structs.KEY_ENDSTRUCT
KEY_DB = structs.KEY_DB
KEY_DW = structs.KEY_DW
KEY_IF = 24
KEY_ELSE = 25
KEY_ELSEIF = 26
KEY_ENDIF = 27
KEY_IFDEF = 28
KEY_IFNDEF = 29
SECTION_STATE = IMPORT_STATE+imports.SCRATCH_BYTES
STRUCT_STATE = SECTION_STATE+sections.SCRATCH_BYTES
BINDING_PLANS = STRUCT_STATE+structs.SCRATCH_BYTES
SCRATCH_BYTES = BINDING_PLANS+plans.STATE_BYTES
	.section code, kind=code

; A0=zero-initialized caller-owned SCRATCH_BYTES (or released prior session),
; D0=first source ID, D1=.end ID. D0/CCR=status; other registers preserved.
; Owned arena pointers are preparation-only; stored names/links are offsets/IDs.
begin	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	cmpi.l #65535, d0
	bhi.w bad
	move.w d0, layout.State.Base(a0)
	move.w d1, layout.State.EndDirective(a0)
	clr.w layout.State.Changed(a0)
	clr.w layout.State.FileContent(a0)
	clr.w layout.State.MaterializedLoops(a0)
	clr.w layout.State.LoopDepth(a0)
	clr.w layout.State.Deferred(a0)
	clr.l layout.State.Package(a0)
	move.l a0, -(sp)
	adda.l #BINDING_PLANS, a0
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	clr.l plans.State.Current(a0)
	movea.l (sp)+, a0
	clr.l ENTRIES+memory.Block.Pointer(a0)
	clr.l ENTRIES+memory.Block.Capacity(a0)
	clr.l ENTRIES+memory.Block.Used(a0)
	lea reserveIdentities, a1
	move.l a1, layout.State.ReserveRoutine(a0)
	lea earlierReference, a1
	move.l a1, layout.State.EarlierReference(a0)
	movea.l a0, a1
	clr.w layout.State.Count(a0)
	clr.w layout.State.Current(a0)
	clr.w layout.State.Ended(a0)
	clr.l layout.State.ArenaUsed(a0)
	clr.l ARENA+memory.Block.Pointer(a0)
	clr.l ARENA+memory.Block.Capacity(a0)
	clr.l ARENA+memory.Block.Used(a0)
	clr.w layout.State.BindingNamespace(a0)
	clr.w layout.State.PreviewBinding(a0)
	clr.w layout.State.FirstBound(a0)
	clr.w layout.State.FirstExplicit(a0)
	lea BUCKETS(a0), a0
	move.w #255, d1
clear
	clr.w (a0)+
	dbra d1, clear
	lea MODULE_STATE(a1), a0
	moveq #0, d0
	move.w layout.State.Base(a1), d0
	jsr modules.begin
	lea IMPORT_STATE(a1), a0
	jsr imports.begin
	movea.l a1, a0
	adda.l #SECTION_STATE, a0
	jsr sections.begin
	movea.l a1, a0
	adda.l #STRUCT_STATE, a0
	jsr structs.begin
	move.l #memberIdentity, structs.State.FieldBinder(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; begin

; A0=scope state,A1=validated package. Keep only the preparation policy owner.
; D0/CCR=0; other registers preserved. No pointers enter packed output.
configureLoops	.block
	move.l a1, layout.State.Package(a0)
	moveq #0, d0
	rts
	.bend  ; configureLoops

; A0=preparation scope state. Release all owned preparation tables.
; All registers preserved; CCR unspecified. Safe for empty/released storage.
release	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea ENTRIES(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea MODULE_STATE(a1), a0
	jsr modules.release
	lea IMPORT_STATE(a1), a0
	jsr imports.release
	movea.l a1, a0
	adda.l #STRUCT_STATE, a0
	jsr structs.release
	lea ARENA(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l a1, a0
	adda.l #BINDING_PLANS, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, a0-a1
	rts
	.bend  ; release

; A0=scope state,D0=minimum identity slots. Grow all owned per-ID tables.
; D0/CCR=status; other registers preserved. Existing logical entries survive failure.
reserveIdentities	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	move.l d0, d2
	cmpi.l #LIMIT, d0
	bhi.w bad
	moveq #0, d1
	move.w layout.State.Base(a1), d1
	add.l d0, d1
	cmpi.l #65536, d1
	bhi.w bad
	mulu.w #records.ENTRY_BYTES, d0
	lea ENTRIES(a1), a0
	jsr memory.reserve
	bne.w bad
	move.l d2, d0
	lea MODULE_STATE(a1), a0
	jsr modules.reserve
	bne.w bad
	move.l d2, d0
	lea IMPORT_STATE(a1), a0
	jsr imports.reserve
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; reserveIdentities

; A0=state. End one source without discarding shared definitions/imports.
; D0=nonzero requires explicit modules for nonempty files. D0/CCR=status;
; other registers preserved. Reject unfinished lexical/module scopes.
endFile	.block
	tst.l d0
	beq.w scopes
	tst.w layout.State.FileContent(a0)
	beq.w scopes
	tst.w MODULE_STATE+modules.State.Explicit(a0)
	beq.w bad
scopes
	move.l a1, -(sp)
	movea.l a0, a1
	adda.l #STRUCT_STATE, a1
	tst.w structs.State.Active(a1)
	movea.l (sp)+, a1
	bne.w bad
	tst.w layout.State.Current(a0)
	bne.w bad
	move.l a1, -(sp)
	movea.l a0, a1
	adda.l #SECTION_STATE, a1
	tst.w sections.State.Active(a1)
	movea.l (sp)+, a1
	bne.w bad
	clr.w layout.State.Ended(a0)
	clr.w layout.State.FileContent(a0)
	clr.w MODULE_STATE+modules.State.Explicit(a0)
	clr.w MODULE_STATE+modules.State.Outside(a0)
	clr.w MODULE_STATE+modules.State.Visibility(a0)
	clr.w MODULE_STATE+modules.State.FileDerived(a0)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; endFile

; A0=scope state,A1=basename,D0=name bytes. Open its file-derived module
; through the normal numeric module binder. D1=module index+1 on success;
; D0/CCR=status; other registers preserved.
beginFileDerived	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	tst.w layout.State.Current(a6)
	bne.w fileDerivedBad
	movea.l a5, a0
	movea.l a6, a1
	jsr bind
	bne.w fileDerivedBad
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	lea ENTRIES(a6), a1
	movea.l ARENA_POINTER(a6), a2
	movea.l a6, a3
	lea bind, a4
	lea MODULE_STATE(a6), a0
	jsr modules.open
	bne.w fileDerivedBad
	move.w modules.State.Active(a0), layout.State.Current(a6)
	move.w #1, modules.State.FileDerived(a0)
	moveq #0, d1
	move.w modules.State.Active(a0), d1
	moveq #0, d0
	bra.w fileDerivedDone
fileDerivedBad
	moveq #1, d0
fileDerivedDone
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; beginFileDerived

; A0=scope state. Close a file-derived module still active at physical EOF.
; D0/CCR=status; other registers preserved.
endFileDerived	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	tst.w MODULE_STATE+modules.State.FileDerived(a1)
	beq.w fileDerivedClosed
	moveq #0, d0
	move.w layout.State.Current(a1), d0
	lea MODULE_STATE(a1), a0
	jsr modules.close
	bne.w fileDerivedEnd
	clr.w layout.State.Current(a1)
	clr.w MODULE_STATE+modules.State.FileDerived(a1)
fileDerivedClosed
	moveq #0, d0
fileDerivedEnd
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; endFileDerived

; A0=state. Returns D0=next unused source ID, other registers preserved.
count	.block
	move.l d1, -(sp)
	moveq #0, d0
	move.w layout.State.Base(a0), d0
	moveq #0, d1
	move.w layout.State.Count(a0), d1
	add.l d1, d0
	move.l (sp)+, d1
	rts
	.bend  ; count

; A0=state. Reset source-name metadata for the next writer record; no clobbers.
startLine	.block
	clr.w layout.State.FirstBound(a0)
	clr.w layout.State.FirstExplicit(a0)
	rts
	.bend  ; startLine

; Writer callback: A0/D0=lexical bytes, A1=state; D0/CCR=status,
; D1=numeric ID, D2=preparation-only explicit-name marker. Preserves D3-D7/A2-A6.
; Package lookup precedes this callback; only source names arrive here.
bind	.block
	moveq #0, d2
	bra.w bindName
	.bend  ; bind

; Same lexical inputs as bind; return the original source ID, not Entry.Target.
; Deferred operand roles retain this identity until register/value selection.
bindOrigin	.block
	moveq #1, d2
	bra.w bindName
	.bend  ; bindOrigin
; A0/D0=normalized encoding bytes,A1=scope. Allocate a stable global identity
; in a separate non-value namespace; preserve lexical owner and line metadata.
; D0=status,D1=id,D2=zero; D3-D7/A2-A6 preserved, CCR reflects D0.
bindEncodingName	.block
	movem.l d3-d7/a2-a6, -(sp)
	movea.l a1, a6
	move.w layout.State.Current(a6), -(sp)
	move.w layout.State.BindingNamespace(a6), -(sp)
	move.w layout.State.FirstBound(a6), -(sp)
	move.w layout.State.FirstExplicit(a6), -(sp)
	clr.w layout.State.Current(a6)
	move.w #1, layout.State.BindingNamespace(a6)
	bsr.w bindOrigin
	move.w (sp)+, layout.State.FirstExplicit(a6)
	move.w (sp)+, layout.State.FirstBound(a6)
	move.w (sp)+, layout.State.BindingNamespace(a6)
	move.w (sp)+, layout.State.Current(a6)
	clr.l d2
	movem.l (sp)+, d3-d7/a2-a6
	tst.l d0
	rts
	.bend  ; bindEncodingName
; Preparation-only field key for a declared numeric identity.
; A0=scope state,D0=declaration ID. D0/CCR=status,D1=global bare member ID.
; Other registers preserved; lexical/line binding metadata survives lookup.
memberIdentity	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d7
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	moveq #0, d2
	move.w records.Entry.Leaf(a3), d2
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	sub.w d2, d0
	bls.w bad
	movea.l ARENA_POINTER(a6), a0
	adda.l records.Entry.Name(a3), a0
	adda.l d2, a0
	; Explicitly qualified declarations may have Leaf=0. Select their last
	; lexical component here, while spelling is still preparation-owned.
	movea.l a0, a2
	move.l d0, d3
component
	cmpi.b #'.', (a2)+
	bne.w nextByte
	movea.l a2, a0
	move.l d3, d0
	subq.l #1, d0
nextByte
	subq.l #1, d3
	bne.w component
	tst.l d0
	beq.w bad
	move.w layout.State.Current(a6), -(sp)
	move.w layout.State.FirstBound(a6), -(sp)
	move.w layout.State.FirstExplicit(a6), -(sp)
	clr.w layout.State.Current(a6)
	movea.l a6, a1
	bsr.w bindOrigin
	move.w (sp)+, layout.State.FirstExplicit(a6)
	move.w (sp)+, layout.State.FirstBound(a6)
	move.w (sp)+, layout.State.Current(a6)
	tst.l d0
	bne.w done
	mulu.w #records.ENTRY_BYTES, d7
	movea.l ENTRIES_POINTER(a6), a3
	move.w d1, records.Entry.MemberId(a3, d7.l)
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; memberIdentity
	.priv
bindName	.block
	movem.l d3-d7/a2-a6, -(sp)
	move.l d2, -(sp)
	movea.l a1, a6
	moveq #0, d5
	moveq #0, d3
	move.w layout.State.Current(a6), d3
	bsr.w compose
	bne.w bad
	tst.w layout.State.FirstBound(a6)
	bne.w find
	move.w #1, layout.State.FirstBound(a6)
	move.w d5, layout.State.FirstExplicit(a6)
find
	bsr.w lookup
	beq.w found
	moveq #0, d0
	move.w layout.State.Count(a6), d0
	addq.l #1, d0
	movea.l a6, a0
	bsr.w reserveIdentities
	bne.w bad
	moveq #0, d0
	move.l layout.State.ArenaUsed(a6), d0
	add.l d6, d0
	cmpi.l #ARENA_BYTES, d0
	bhi.w bad
	; Lookup bytes may originate in the arena being grown. Keep a stable copy.
	movem.l d0/a0-a1, -(sp)
	movea.l a2, a0
	lea BUFFER(a6), a1
	move.l d6, d1
stableName
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w stableName
	lea BUFFER(a6), a2
	move.l 0(sp), d0
	lea ARENA(a6), a0
	move.l #ARENA_BYTES, d1
	jsr memory.reserveBounded
	movem.l (sp)+, d0/a0-a1
	bne.w bad
	moveq #0, d1
	move.w layout.State.Count(a6), d1
	move.l d1, d2
	mulu.w #records.ENTRY_BYTES, d2
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d2, a3
	move.l layout.State.ArenaUsed(a6), records.Entry.Name(a3)
	move.w d6, records.Entry.Length(a3)
	move.w d3, records.Entry.Owner(a3)
	move.w d7, records.Entry.Leaf(a3)
	clr.w records.Entry.Flags(a3)
	tst.w layout.State.BindingNamespace(a6)
	beq.w ordinaryIdentity
	move.w #records.ENCODING_NAME, records.Entry.Flags(a3)
ordinaryIdentity
	clr.w records.Entry.ScopeKind(a3)
	clr.w records.Entry.Padding(a3)
	clr.w records.Entry.MemberBase(a3)
	clr.w records.Entry.TemplateModule(a3)
	clr.w records.Entry.TemplateFlags(a3)
	clr.l records.Entry.RegisterName(a3)
	move.w #$ffff, records.Entry.MemberId(a3)
	clr.w records.Entry.OriginalId(a3)
	move.w layout.State.Base(a6), d2
	add.w d1, d2
	move.w d2, records.Entry.Target(a3)
	lea BUCKETS(a6), a4
	add.w d4, d4
	move.w 0(a4, d4.w), records.Entry.Next(a3)
	addq.w #1, d1
	move.w d1, 0(a4, d4.w)
	addq.w #1, layout.State.Count(a6)
	moveq #0, d1
	move.w layout.State.Count(a6), d1
	mulu.w #records.ENTRY_BYTES, d1
	move.l d1, ENTRIES+memory.Block.Used(a6)
	movea.l ARENA_POINTER(a6), a1
	moveq #0, d1
	move.l layout.State.ArenaUsed(a6), d1
	adda.l d1, a1
	move.l d0, layout.State.ArenaUsed(a6)
	move.l d0, ARENA+memory.Block.Used(a6)
	movea.l a2, a0
	move.l d6, d0
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
found
	tst.w d5
	beq.w bound
	ori.w #EXPLICIT, records.Entry.Flags(a3)
bound
	moveq #0, d1
	tst.l (sp)
	beq.w canonicalIdentity
	move.l a3, d1
	sub.l ENTRIES_POINTER(a6), d1
	divu.w #records.ENTRY_BYTES, d1
	andi.l #$ffff, d1
	add.w layout.State.Base(a6), d1
	bra.w identityReady
canonicalIdentity
	move.w records.Entry.Target(a3), d1
identityReady
	move.l d5, d2
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	addq.w #4, sp
	movem.l (sp)+, d3-d7/a2-a6
	tst.l d0
	rts
	.bend  ; bindName
	.pub

; A0=scope,A1=configured values Owner. Copy finalized numeric plans.
; D0/CCR=status; others preserved. Names and preparation storage never escape.
exportBindings	.block
	movem.l d1/a0-a2, -(sp)
	movea.l a1, a2
	adda.l #BINDING_PLANS, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d0
	movea.l a2, a0
	jsr bindings.install
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend  ; exportBindings

; A0=completed scope,A1=source-indexed owner slots,D0=slot count.
; The finalized numeric plan owner maps occurrence proxies once, after all
; declaration owners are known. Unknown/mutable prefixes retain possible
; owners; differing possibilities reject only if the proxy becomes reachable.
; D0/CCR=status; other registers preserved. No borrowed pointer is retained.
indexBindingOwners	.block
	movem.l d1/a0-a4, -(sp)
	moveq #0, d1
	move.w layout.State.Count(a0), d1
	cmp.l d1, d0
	bne.w bad
	move.l d1, d0
	mulu.w #records.ENTRY_BYTES, d0
	cmp.l ENTRIES+memory.Block.Used(a0), d0
	bhi.w bad
	movea.l ENTRIES_POINTER(a0), a4
	tst.l d1
	beq.w entriesReady
	move.l a4, d0
	beq.w bad
entriesReady
	movea.l a1, a2
	lea IMPORT_STATE(a0), a1
	cmp.l imports.KNOWN_DEFINED+memory.Block.Used(a1), d1
	bhi.w bad
	movea.l imports.KNOWN_DEFINED_POINTER(a1), a3
	tst.l d1
	beq.w definedReady
	move.l a3, d0
	beq.w bad
definedReady
	adda.l #imports.KNOWN_OWNER, a1
	moveq #0, d0
	move.w layout.State.Base(a0), d0
	adda.l #BINDING_PLANS, a0
	jsr plans.indexOwners
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a4
	tst.l d0
	rts
	.bend  ; indexBindingOwners

; A0=bound scoped-loop header,A1=scope. Declare its collection once, even
; when no iterations are selected. D0/CCR=status,D1=label or $ffff.
materializedCollection	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a1, a6
	move.l #source.MAX_LINE, d0
	bsr.w normalizeLabel
	bne.w done
	moveq #0, d0
	move.b source.RecordHeader.Length(a0), d0
	cmpi.l #9, d0
	blo.w bad
	lea source.RECORD_HEADER_BYTES(a0), a0
	move.l #$ffff, d1
	cmpi.b #1, source.NameToken.Kind(a0)
	bhi.w good
	cmpi.b #5, source.DECLARATION_MARKER(a0)
	bne.w good
	bsr.w declare
	bne.w done
	moveq #0, d1
	move.w source.NameToken.Id(a0), d1
	andi.w #$ffff-records.LABEL_VALUE, records.Entry.Flags(a3)
	ori.w #records.COMPOUND_LABEL, records.Entry.Flags(a3)
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; materializedCollection

; A0/D0=full value spelling,A1=scope,D1=bound full origin ID.
; Capture earlier declared value prefixes longest first. Entire ordinary name
; remains bound; shared ExprVM selects actual records at execution.
; D0/CCR=status,D1=occurrence proxy ID when candidates exist, else original ID.
; Other registers preserved. Earlier ordinary references keep their own IDs.
captureBinding	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #FIELD_BUFFER_BYTES, sp
	movea.l a1, a6
	movea.l a0, a4
	move.l d0, d4
	move.l d1, d7
	cmpi.l #3, d4
	blo.w good
	cmpi.l #layout.NAME_BYTES-1, d4
	bhi.w bad
	movea.l a4, a2
	move.l d4, d5
hasDot
	cmpi.b #'.', (a2)+
	beq.w dotted
	subq.l #1, d5
	bne.w hasDot
	bra.w good
dotted
	move.l d7, d0
	sub.w layout.State.Base(a6), d0
	bcs.w good
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	btst #0, records.Entry.Flags+1(a3)
	bne.w good
	moveq #0, d6
	move.l d4, d5
component
	subq.l #1, d5
	beq.w good
	cmpi.b #'.', 0(a4, d5.l)
	bne.w component
	movea.l a4, a0
	move.l d5, d0
	bsr.w declaredName
	bne.w component
	tst.w records.Entry.ScopeKind(a3)
	bne.w component
	move.l a3, d0
	sub.l ENTRIES_POINTER(a6), d0
	divu.w #records.ENTRY_BYTES, d0
	andi.l #$ffff, d0
	move.l d0, -(sp)
	ori.w #REFERENCED, records.Entry.Flags(a3)
	move.l d0, d1
	movea.l a6, a0
	adda.l #MODULE_STATE, a0
	move.l d1, d0
	jsr modules.reference
	move.l (sp)+, d0
	move.l d0, -(sp)
	move.l d0, d1
	jsr modules.check
	move.l (sp)+, d1
	tst.l d0
	bne.w bad
	move.l d1, d0
	moveq #0, d1
	move.w layout.State.Base(a6), d1
	add.l d1, d0
	move.l d0, -(sp)
	tst.l d6
	bne.w collect
	move.l d7, d0
	bsr.w createBindingProxy
	bne.w failedBase
	move.l d1, d7
	movea.l a6, a0
	adda.l #BINDING_PLANS, a0
	move.l d7, d0
	jsr plans.start
	bne.w failedBase
	moveq #1, d6
collect
	move.w layout.State.Current(a6), -(sp)
	move.w layout.State.FirstBound(a6), -(sp)
	move.w layout.State.FirstExplicit(a6), -(sp)
	clr.w layout.State.Current(a6)
	lea 1(a4, d5.l), a2
	move.l d4, d2
	sub.l d5, d2
	subq.l #1, d2
	movea.l sp, a5
	adda.w #SAVED_FIELD_STATE_BYTES+SAVED_BASE_BYTES, a5
	moveq #0, d3
field
	movea.l a2, a3
	moveq #0, d1
scanField
	tst.l d2
	beq.w bindField
	cmpi.b #'.', (a3)
	beq.w bindField
	addq.l #1, a3
	addq.l #1, d1
	subq.l #1, d2
	bra.w scanField
bindField
	tst.l d1
	beq.w failedField
	move.l d2, -(sp)
	movea.l a2, a0
	move.l d1, d0
	movea.l a6, a1
	bsr.w bindOrigin
	move.l (sp)+, d2
	tst.l d0
	bne.w failedField
	move.w d1, (a5)+
	addq.l #1, d3
	tst.l d2
	beq.w append
	lea 1(a3), a2
	subq.l #1, d2
	bra.w field
append
	move.w (sp)+, layout.State.FirstExplicit(a6)
	move.w (sp)+, layout.State.FirstBound(a6)
	move.w (sp)+, layout.State.Current(a6)
	move.l (sp)+, d0
	move.l d3, d1
	movea.l sp, a1
	movea.l a6, a0
	adda.l #BINDING_PLANS, a0
	jsr plans.append
	bne.w bad
	bra.w component
failedField
	move.w (sp)+, layout.State.FirstExplicit(a6)
	move.w (sp)+, layout.State.FirstBound(a6)
	move.w (sp)+, layout.State.Current(a6)
failedBase
	addq.l #SAVED_BASE_BYTES, sp
bad
	moveq #1, d0
	bra.w done
good
	move.l d7, d1
	moveq #0, d0
done
	adda.w #FIELD_BUFFER_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; captureBinding
	.priv
; A6=scope,D0=original full ID. D0/CCR=status,D1=new unlinked proxy ID.
; Proxy names share stable arena bytes; only numeric origin/context differ.
createBindingProxy	.block
	movem.l d2-d5/a0-a3, -(sp)
	move.l d0, d5
	moveq #0, d4
	move.w layout.State.Count(a6), d4
	move.l d4, d0
	addq.l #1, d0
	movea.l a6, a0
	bsr.w reserveIdentities
	bne.w done
	move.l d5, d0
	sub.w layout.State.Base(a6), d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a2
	adda.l d0, a2
	move.l d4, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	movea.l a3, a1
	moveq #records.ENTRY_BYTES/4-1, d2
copy
	move.l (a2)+, (a1)+
	dbf d2, copy
	move.w layout.State.Current(a6), records.Entry.Owner(a3)
	move.w #records.BINDING_PROXY+EXPLICIT, records.Entry.Flags(a3)
	clr.w records.Entry.Next(a3)
	clr.w records.Entry.ScopeKind(a3)
	move.w d5, records.Entry.Target(a3)
	move.w d5, records.Entry.OriginalId(a3)
	move.w #$ffff, records.Entry.MemberId(a3)
	moveq #0, d1
	move.w layout.State.Base(a6), d1
	add.l d4, d1
	addq.w #1, layout.State.Count(a6)
	move.l d4, d0
	addq.l #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	move.l d0, ENTRIES+memory.Block.Used(a6)
	move.w #1, layout.State.Changed(a6)
	moveq #0, d0
done
	movem.l (sp)+, d2-d5/a0-a3
	tst.l d0
	rts
	.bend  ; createBindingProxy

; A6=scope. Prefer complete declared names, otherwise retain each proxy ID.
; D0/CCR=status; other registers preserved. Called after normal target binding.
finalizeBindingProxies	.block
	movem.l d1-d7/a0-a5, -(sp)
	moveq #0, d7
scan
	cmp.w layout.State.Count(a6), d7
	bhs.w good
	move.l d7, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a4
	adda.l d0, a4
	move.w records.Entry.Flags(a4), d0
	andi.w #records.BINDING_PROXY, d0
	beq.w next
	moveq #0, d0
	move.w records.Entry.OriginalId(a4), d0
	sub.w layout.State.Base(a6), d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	movea.l ARENA_POINTER(a6), a0
	adda.l records.Entry.Name(a3), a0
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	move.w layout.State.Current(a6), -(sp)
	move.w records.Entry.Owner(a4), layout.State.Current(a6)
	bsr.w declaredName
	move.w (sp)+, layout.State.Current(a6)
	tst.l d0
	bne.w fallback
	move.w records.Entry.Target(a3), records.Entry.Target(a4)
	move.l d7, d0
	moveq #0, d1
	move.w records.Entry.Target(a4), d1
	sub.w layout.State.Base(a6), d1
	lea MODULE_STATE(a6), a0
	jsr modules.check
	bne.w done
	bra.w next
fallback
	moveq #0, d0
	move.w layout.State.Base(a6), d0
	add.l d7, d0
	move.w d0, records.Entry.Target(a4)
next
	addq.w #1, d7
	bra.w scan
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; finalizeBindingProxies

; A0/D0=spelling,A6=scope. D0/CCR=0 declared,A3=entry,1 absent.
; Search current then enclosing owners without allocating provisional identities.
; Preserves D4-D7/A4-A6; clobbers D1-D3/A0-A2.
declaredName	.block
	movem.l d4-d7/a4-a5, -(sp)
	movea.l a0, a4
	movea.l d0, a5
	moveq #0, d3
	move.w layout.State.Current(a6), d3
owner
	movea.l a4, a0
	move.l a5, d0
	bsr.w compose
	bne.w absent
	bsr.w lookup
	bne.w relative
	bsr.w availableConfigurationName
	beq.w found
relative
	tst.l d5
	beq.w parent
	tst.l d3
	beq.w parent
	move.l d3, d0
	subq.l #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	moveq #0, d4
	move.w records.Entry.Length(a3), d4
	move.l a5, d6
	add.l d4, d6
	addq.l #1, d6
	cmpi.l #layout.NAME_BYTES-1, d6
	bhi.w parent
	movea.l ARENA_POINTER(a6), a0
	adda.l records.Entry.Name(a3), a0
	lea BUFFER(a6), a1
	move.l d4, d0
prefix
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w prefix
	move.b #'.', (a1)+
	movea.l a4, a0
	move.l a5, d0
suffix
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w suffix
	lea BUFFER(a6), a2
	bsr.w lookup
	bne.w parent
	bsr.w availableConfigurationName
	beq.w found
parent
	tst.l d3
	beq.w absent
	move.l d3, d0
	subq.l #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	moveq #0, d3
	move.w records.Entry.Owner(a3), d3
	bra.w owner
found
	moveq #0, d0
	bra.w done
absent
	moveq #1, d0
done
	movem.l (sp)+, d4-d7/a4-a5
	tst.l d0
	rts
	.bend  ; declaredName

; A3=entry,A6=scope. Declared symbols and earlier known configuration snapshots
; can provide a dotted prefix. D0/CCR=0 when available; other registers kept.
availableConfigurationName	.block
	btst #0, records.Entry.Flags+1(a3)
	bne.w good
	movem.l d1/a0, -(sp)
	move.l a3, d1
	sub.l ENTRIES_POINTER(a6), d1
	divu.w #records.ENTRY_BYTES, d1
	andi.l #$ffff, d1
	movea.l IMPORT_STATE+imports.KNOWN_DEFINED_POINTER(a6), a0
	tst.b 0(a0, d1.l)
	movem.l (sp)+, d1/a0
	beq.w bad
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; availableConfigurationName
	.pub

CANDIDATE_MATCHED = 0
CANDIDATE_ABSENT = 1
CANDIDATE_INVALID = 2

; A0=scope,D0=origin source ID,D1=package register ID,D2=qualifier.
; D0/CCR=matched or invalid; other registers preserved. Attach atomically;
; repeated identical candidates succeed, while conflicting candidates reject.
attachRegister	.block
	movem.l d1-d4/a1, -(sp)
	cmpi.l #$ffff, d0
	bhi.w bad
	sub.w layout.State.Base(a0), d0
	bcs.w bad
	cmp.w layout.State.Count(a0), d0
	bhs.w bad
	cmpi.l #$ffff, d1
	bhi.w bad
	cmp.w layout.State.Base(a0), d1
	bhs.w bad
	cmpi.l #$ff, d2
	bhi.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a0), a1
	adda.l d0, a1
	addq.w #1, d1
	swap d1
	lsl.w #8, d2
	or.l d2, d1
	move.l records.Entry.RegisterName(a1), d3
	beq.w attach
	cmp.l d1, d3
	bne.w bad
	bra.w good
attach
	tst.w layout.State.PreviewBinding(a0)
	bne.w good
	move.l d1, records.Entry.RegisterName(a1)
good
	moveq #CANDIDATE_MATCHED, d0
	bra.w done
bad
	moveq #CANDIDATE_INVALID, d0
done
	movem.l (sp)+, d1-d4/a1
	tst.l d0
	rts
	.bend  ; attachRegister

; A0=scope,D0=origin ID. D0/CCR=matched,absent or invalid;
; D1/D2=package register ID/qualifier on match. Other registers preserved.
; Package IDs are absent. Source candidates are read without following Target.
registerCandidate	.block
	movem.l d3/a1, -(sp)
	cmpi.l #$ffff, d0
	bhi.w bad
	sub.w layout.State.Base(a0), d0
	bcs.w absent
	cmp.w layout.State.Count(a0), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a0), a1
	adda.l d0, a1
	moveq #0, d1
	move.w records.Entry.RegisterName(a1), d1
	beq.w absent
	tst.b records.Entry.RegisterReserved(a1)
	bne.w bad
	subq.w #1, d1
	cmp.w layout.State.Base(a0), d1
	bhs.w bad
	moveq #0, d2
	move.b records.Entry.RegisterQualifier(a1), d2
	moveq #CANDIDATE_MATCHED, d0
	bra.w done
absent
	moveq #CANDIDATE_ABSENT, d0
	bra.w done
bad
	moveq #CANDIDATE_INVALID, d0
done
	movem.l (sp)+, d3/a1
	tst.l d0
	rts
	.bend  ; registerCandidate

; A0/D0=lexical name/bytes,A1=scope state. Probe only active bare values.
; D0/CCR=0 matched,1 absent; D1/D2=temporary ID/zero qualifier on match.
; Other registers preserved. Ordinary source-name binding is unchanged.
probeIterator	.block
	tst.w layout.State.LoopDepth(a1)
	beq.w absent
	movem.l a0-a3, -(sp)
	movea.l ENTRIES_POINTER(a1), a2
	movea.l ARENA_POINTER(a1), a3
	moveq #0, d1
	move.w layout.State.Base(a1), d1
	moveq #0, d2
	move.w layout.State.Current(a1), d2
	adda.l #layout.ITERATOR_STATE, a1
	jsr iterators.probe
	movem.l (sp)+, a0-a3
	tst.l d0
	rts
absent
	moveq #1, d0
	rts
	.bend  ; probeIterator
	.priv
; A0=scope state,D0=provisional variable ID. Allocate an unindexed identity
; sharing only immutable spelling bytes. D0/CCR=status,D1=new ID; others kept.
; This identity is never an import proxy, declaration or parent-fallback name.
createIterator	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d6
	moveq #0, d7
	move.w layout.State.Count(a6), d7
	moveq #0, d1
	move.w layout.State.Base(a6), d1
	add.l d7, d1
	cmpi.l #iterators.COUNTED, d1
	bhs.w bad
	move.l d7, d0
	addq.l #1, d0
	bsr.w reserveIdentities
	bne.w bad
	move.l d6, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a2
	adda.l d0, a2
	move.l d7, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	move.l records.Entry.Name(a2), records.Entry.Name(a3)
	move.w records.Entry.Length(a2), records.Entry.Length(a3)
	move.w records.Entry.Leaf(a2), records.Entry.Leaf(a3)
	move.w layout.State.Current(a6), records.Entry.Owner(a3)
	move.w #records.ITERATOR, records.Entry.Flags(a3)
	clr.w records.Entry.Next(a3)
	clr.w records.Entry.ScopeKind(a3)
	clr.w records.Entry.Padding(a3)
	clr.w records.Entry.MemberBase(a3)
	clr.w records.Entry.TemplateModule(a3)
	clr.w records.Entry.TemplateFlags(a3)
	move.l records.Entry.RegisterName(a2), records.Entry.RegisterName(a3)
	move.w records.Entry.MemberId(a2), records.Entry.MemberId(a3)
	clr.w records.Entry.OriginalId(a3)
	moveq #0, d1
	move.w layout.State.Base(a6), d1
	add.w d7, d1
	move.w d1, records.Entry.Target(a3)
	addq.w #1, layout.State.Count(a6)
	moveq #0, d0
	move.w layout.State.Count(a6), d0
	mulu.w #records.ENTRY_BYTES, d0
	move.l d0, ENTRIES+memory.Block.Used(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; createIterator
	.pub

; A0=writer copy,A1=scope state,D0=buffer capacity. Normalize a canonical label
; prefix through the ordinary shared path, without declaring its value.
; D0/CCR=status,D1=statement offset (4 or9); other registers preserved.
configurationStatement	.block
	movem.l d2-d3/a0-a2/a6, -(sp)
	movea.l a1, a6
	bsr.w normalizeLabel
	bne.w done
	move.l a0, -(sp)
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	movea.l a0, a1
	adda.l d0, a1
	addq.l #4, a0
	lea memberIdentity, a2
	jsr expr.bindFields
	movea.l (sp)+, a0
	bne.w done
	moveq #4, d1
	cmpi.b #8, (a0)
	blo.w good
	cmpi.b #1, 4(a0)
	bhi.w good
	cmpi.b #5, 8(a0)
	bne.w good
	moveq #9, d1
good
	moveq #0, d0
done
	movem.l (sp)+, d2-d3/a0-a2/a6
	tst.l d0
	rts
	.bend  ; configurationStatement

; A0=bound writer record,A1=scope,D0=capacity. Capture an available schema
; through shared struct lowering without declaring ordinary assembly symbols.
; D0/CCR=status; other registers preserved. Unknown geometry is unavailable.
configurationStruct	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	movea.l a0, a5
	move.l d0, d4
	bsr.w normalizeLabel
	bne.w done
	movea.l a5, a0
	movea.l a6, a1
	movea.l a6, a2
	adda.l #STRUCT_STATE, a2
	lea keyword, a3
	move.l d4, d0
	jsr structs.line
	bne.w done
	movea.l a5, a0
	movea.l a6, a1
	jsr imports.captureValue
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; configurationStruct

; A0=scope. Abandon unavailable configuration geometry, restoring its parent.
; D0/CCR=0; other registers preserved. Ordinary preparation validates it later.
abandonConfigurationStruct	.block
	move.l a1, -(sp)
	movea.l a0, a1
	adda.l #STRUCT_STATE, a1
	tst.w structs.State.Active(a1)
	beq.w done
	move.w structs.State.Parent(a1), layout.State.Current(a0)
	clr.w structs.State.Active(a1)
	clr.l structs.State.Fields+memory.Block.Used(a1)
done
	movea.l (sp)+, a1
	moveq #0, d0
	rts
	.bend  ; abandonConfigurationStruct

; A0=writer record,A1=scope. Reject preparation effects inside deferred
; branch envelopes. Mutable assignment/update records replay in source order;
; immutable scalar ownership is selected by the packed declaration inventory.
; Scope directive identities use the existing shared scope policy.
; D0/CCR=status; other registers preserved. Does not mutate source or scope.
deferredLine	.block
	movem.l d1-d3/a0-a2/a6, -(sp)
	movea.l a1, a6
	btst #LEXICAL_BLOCK_BIT, 1(a0)
	bne.w good
	moveq #0, d1
	move.b (a0), d1
	addq.w #1, d1
	cmpi.w #branch_parser.Record.Operand, d1
	blo.w good
	cmpi.b #loop_abi.PRVM_TOKEN_KIND_REGISTER, branch_parser.Record.Dot(a0)
	bhi.w directive
	move.l a0, -(sp)
	lea layout.ITERATOR_STATE(a6), a0
	jsr iterators.scoped
	movea.l (sp)+, a0
	beq.w good
	; Ordinary loops keep their existing declaration/update restrictions.
	; The extension here belongs to a top-level retained branch envelope.
	tst.w layout.State.LoopDepth(a6)
	bne.w declarationPolicy
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, branch_parser.Record.Qualifier(a0)
	beq.w good
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, branch_parser.Record.Qualifier(a0)
	beq.w good
	cmpi.b #source.TOKEN_UPDATE_FIRST, branch_parser.Record.Qualifier(a0)
	blo.w declarationPolicy
	cmpi.b #source.TOKEN_UPDATE_LAST, branch_parser.Record.Qualifier(a0)
	bls.w good
declarationPolicy
	btst #INDENT_FLAG_BIT, branch_parser.Record.Flags(a0)
	beq.w bad
	cmpi.b #loop_abi.PRVM_TOKEN_KIND_COLON, branch_parser.Record.Qualifier(a0)
	beq.w bad
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, branch_parser.Record.Qualifier(a0)
	beq.w bad
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, branch_parser.Record.Qualifier(a0)
	blo.w good
	cmpi.b #source.TOKEN_UPDATE_LAST, branch_parser.Record.Qualifier(a0)
	bls.w bad
	bra.w good
directive
	cmpi.b #loop_abi.PRVM_TOKEN_KIND_DOT, branch_parser.Record.Dot(a0)
	bne.w good
	moveq #0, d0
	move.w branch_parser.Record.Head(a0), d0
	bsr.w keyword
	tst.l d0
	beq.w good
	cmpi.w #KEY_IF, d0
	blo.w bad
	cmpi.w #KEY_IFNDEF, d0
	bhi.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2/a6
	tst.l d0
	rts
	.bend  ; deferredLine

; A0=bound configuration record,A1=scope state,D0=buffer capacity.
; Process only module boundaries,
; imports and preceding known assignments. The caller selects active top-level
; records; bodies, templates, structs and sections never enter this API.
; Uses the same module/import parsers and expression VM as normal preparation.
; D0/CCR=status; other registers preserved. Consumes directives in the copy.
configurationLine	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	movea.l a0, a5
	bsr.w configurationStatement
	bne.w bad
	move.l d1, d4
	moveq #0, d6
	move.b (a5), d6
	addq.w #1, d6
	cmpi.w #9, d6
	blo.w bad
	movea.l a5, a4
	adda.w d6, a4
	movea.l a5, a0
	adda.l d4, a0
	cmpi.b #1, (a0)
	bhi.w directive
	cmpi.b #34, 4(a0)
	beq.w scalarDeclaration
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a0)
	beq.w scalarDeclaration
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a0)
	beq.w scalarDeclaration
	cmpi.b #source.TOKEN_UPDATE_FIRST, 4(a0)
	blo.w bad
	cmpi.b #source.TOKEN_UPDATE_LAST, 4(a0)
	bhi.w bad
	bsr.w bindUpdate
	bne.w bad
	bra.w captureAssignment
scalarDeclaration
	bsr.w declare
	bne.w bad
captureAssignment
	movea.l a5, a0
	movea.l a6, a1
	jsr imports.captureValue
	bra.w done
directive
	cmpi.b #7, (a0)
	bne.w bad
	cmpi.b #1, 1(a0)
	bhi.w bad
	tst.b 4(a0)
	bne.w bad
	moveq #0, d0
	move.w 2(a0), d0
	bsr.w keyword
	cmpi.l #KEY_MODULE, d0
	beq.w module
	cmpi.l #KEY_ENDMODULE, d0
	beq.w endModule
	cmpi.l #KEY_USE, d0
	beq.w importing
	cmpi.l #KEY_END, d0
	beq.w end
	bra.w bad
module
	bsr.w openModule
	bne.w bad
	bra.w consumed
endModule
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	lea MODULE_STATE(a6), a0
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	jsr modules.close
	bne.w bad
	clr.w layout.State.Current(a6)
	bra.w consumed
importing
	movea.l a6, a1
	lea bind, a2
	movea.l a6, a3
	adda.l #SECTION_STATE, a3
	jsr imports.line
	bne.w bad
	bra.w consumed
end
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	tst.w layout.State.Current(a6)
	beq.w ended
	lea MODULE_STATE(a6), a0
	tst.w modules.State.FileDerived(a0)
	beq.w bad
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	jsr modules.close
	bne.w bad
	clr.w layout.State.Current(a6)
	clr.w modules.State.FileDerived(a0)
ended
	move.w #1, layout.State.Ended(a6)
consumed
	move.b #3, (a5)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; configurationLine

; A0=completed configuration scope,A1=fresh semantic scope. Transfer incoming
; typed parameters and mapped-section metadata by canonical name, remapping
; identities through the ordinary binder before dependency bodies run.
; D0/CCR=status; other registers preserved. No configuration IDs or pointers
; survive in destination preparation records. Destination Current must be zero.
seedConfiguration	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	tst.w layout.State.Current(a6)
	bne.w bad
	moveq #0, d7
	move.w IMPORT_STATE+imports.PARAM_COUNT(a5), d7
	lea IMPORT_STATE+imports.PARAMS(a5), a4
next
	tst.w d7
	beq.w good
	moveq #0, d0
	move.w pkg.Parameter.Id(a4), d0
	sub.w layout.State.Base(a5), d0
	bcs.w bad
	cmp.w layout.State.Count(a5), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a5), a3
	adda.l d0, a3
	moveq #0, d6
	move.w records.Entry.Owner(a3), d6
	beq.w bad
	cmp.w layout.State.Count(a5), d6
	bhi.w bad
	bsr.w bindConfigurationName
	bne.w bad
	move.l d1, d5
	move.l d6, d0
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a5), a3
	adda.l d0, a3
	bsr.w bindConfigurationName
	bne.w bad
	sub.w layout.State.Base(a6), d1
	bcs.w bad
	andi.l #$ffff, d1
	addq.l #1, d1
	movea.l a6, a0
	move.l d5, d0
	move.l pkg.Parameter.Low(a4), d2
	move.l pkg.Parameter.High(a4), d3
	moveq #0, d4
	move.w pkg.Parameter.Kind(a4), d4
	lea IMPORT_STATE+imports.PARAM_OWNER+values.Owner.Arena(a5), a1
	move.l memory.Block.Used(a1), d6
	movea.l memory.Block.Pointer(a1), a1
	lea remapConfigurationRecord, a2
	jsr imports.seedParameter
	bne.w bad
	adda.w #imports.PARAM_BYTES, a4
	subq.w #1, d7
	bra.w next
good
	bsr.w seedConfigurationMaps
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seedConfiguration
	.priv
; A0=destination Owner,D2=copied offset,A5=configuration scope,A6=semantic
; scope. Bind every nominal and member key before publishing the snapshot.
; D0/CCR=status; other registers preserved. Geometry and scalar cells are retained.
remapConfigurationRecord	.block
	movem.l d1-d7/a0-a4, -(sp)
	move.l d2, d1
	jsr values.view
	bne.w done
	move.l values.Record.Kind(a1), d4
	cmpi.l #values.STRUCT_TYPE, d4
	blo.w good
	cmpi.l #values.STRUCT_LITERAL, d4
	bhi.w bad
	movea.l a1, a2
	move.l values.StructRecord.Nominal(a2), d0
	bsr.w bindConfigurationId
	bne.w done
	move.l d1, values.StructRecord.Nominal(a2)
	move.l values.Record.Count(a2), d7
	lea values.STRUCT_BASE_BYTES(a2), a2
	moveq #values.STRUCT_FIELD_BYTES, d6
	cmpi.l #values.STRUCT_TYPE, d4
	beq.w field
	moveq #values.FIELD_BYTES, d6
field
	tst.l d7
	beq.w good
	move.l (a2), d0
	bsr.w bindConfigurationId
	bne.w done
	move.l d1, (a2)
	adda.l d6, a2
	subq.l #1, d7
	bra.w field
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; remapConfigurationRecord

; D0=configuration identity,A5=source,A6=destination. D0/CCR=status,
; D1=rebound identity; A0/A1/A3 scratch, other registers preserved.
bindConfigurationId	.block
	sub.w layout.State.Base(a5), d0
	bcs.w bad
	cmp.w layout.State.Count(a5), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a5), a3
	adda.l d0, a3
	bra.w bindConfigurationName
bad
	moveq #1, d0
	rts
	.bend  ; bindConfigurationId

; A5=configuration scope,A6=fresh semantic scope. Rebind bounded map fields
; before any dependency section is lowered. Owner/module are entry indices+1;
; logical/concrete are source IDs. No configuration identity survives.
; D0/CCR=status; other registers preserved.
seedConfigurationMaps	.block
	movem.l d1-d7/a0-a4, -(sp)
	moveq #0, d7
	move.w SECTION_STATE+sections.State.MapCount(a5), d7
	cmpi.w #2, d7
	bhi.w bad
	lea SECTION_STATE+sections.MAPS(a5), a4
	lea SECTION_STATE+sections.MAPS(a6), a2
	moveq #0, d4
nextMap
	cmp.w d7, d4
	bhs.w good
	moveq #0, d5
field
	moveq #0, d0
	move.w 0(a4, d5.w), d0
	cmpi.w #sections.Map.Logical, d5
	bhs.w sourceId
	subq.w #1, d0
	bra.w entry
sourceId
	sub.w layout.State.Base(a5), d0
entry
	bcs.w bad
	cmp.w layout.State.Count(a5), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a5), a3
	adda.l d0, a3
	cmpi.w #sections.Map.Logical, d5
	blo.w bindMapModule
	; A canonical dotted name bound globally loses its lexical leaf/owner.
	; Bind the original map leaf inside its importing owner instead.
	move.w sections.Map.Owner(a2), layout.State.Current(a6)
	movea.l ARENA_POINTER(a5), a0
	adda.l records.Entry.Name(a3), a0
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	moveq #0, d1
	move.w records.Entry.Leaf(a3), d1
	sub.l d1, d0
	adda.l d1, a0
	movea.l a6, a1
	bsr.w bind
	move.l d0, d3
	clr.w layout.State.Current(a6)
	move.l d3, d0
	bne.w bad
	bra.w store
bindMapModule
	bsr.w bindConfigurationName
	bne.w bad
	sub.w layout.State.Base(a6), d1
	bcs.w bad
	addq.w #1, d1
store
	move.w d1, 0(a2, d5.w)
	addq.w #2, d5
	cmpi.w #sections.MAP_BYTES, d5
	blo.w field
	move.w SECTION_STATE+sections.State.Seeded(a6), d0
	bset d4, d0
	move.w d0, SECTION_STATE+sections.State.Seeded(a6)
	addq.w #1, d4
	adda.w #sections.MAP_BYTES, a4
	adda.w #sections.MAP_BYTES, a2
	bra.w nextMap
good
	move.w d7, SECTION_STATE+sections.State.MapCount(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; seedConfigurationMaps

; A3=configuration entry,A5=configuration scope,A6=semantic scope.
; Bind its owned canonical spelling globally. D0/CCR=status,D1=new ID,D2=flags;
; A0/A1 scratch, other registers preserved.
bindConfigurationName	.block
	movea.l ARENA_POINTER(a5), a0
	adda.l records.Entry.Name(a3), a0
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	movea.l a6, a1
	bra.w bind
	.bend  ; bindConfigurationName
	.pub

; A0=writer record, A1=state, D0=record buffer capacity. Normalize labels,
; mark declarations/references and consume scope
; directives before expression preparation. Named blocks lower to entry labels;
; close directives lower to empty records. D0/CCR=status, other registers kept.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	clr.w layout.State.PendingRepeat(a6)
	movea.l a0, a5
	move.l d0, d4
	moveq #0, d5
	move.b 1(a5), d5
	andi.w #LEXICAL_BLOCK, d5
	andi.b #255-LEXICAL_BLOCK, 1(a5)
	tst.w layout.State.Ended(a6)
	bne.w empty
	bsr.w normalizeLabel
	bne.w bad
	bsr.w authorizeLine
	bne.w bad
	movea.l a5, a0
	movea.l a6, a1
	movea.l a6, a2
	adda.l #STRUCT_STATE, a2
	lea keyword, a3
	move.l d4, d0
	jsr structs.line
	bne.w bad
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	movea.l a5, a1
	adda.l d0, a1
	lea 4(a5), a0
	lea memberIdentity, a2
	jsr expr.bindFields
	bne.w bad
	moveq #0, d6
	move.b (a5), d6
	addq.w #1, d6
	movea.l a5, a4
	adda.w d6, a4
	lea 4(a5), a0
	moveq #-1, d7
	cmpi.w #9, d6
	blo.w ok
	cmpi.b #1, (a0)
	bhi.w statement
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, 4(a0)
	beq.w declaration
	cmpi.b #34, 4(a0)
	beq.w declaration
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a0)
	beq.w declaration
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a0)
	beq.w declaration
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, 4(a0)
	beq.w declaration
	cmpi.b #source.TOKEN_UPDATE_FIRST, 4(a0)
	blo.w prefixLabel
	cmpi.b #source.TOKEN_UPDATE_LAST, 4(a0)
	bls.w updateReference
prefixLabel
	cmpi.b #5, 4(a0)
	beq.w declaration
	bra.w statement
declaration
	bsr.w declare
	bne.w bad
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, 4(a0)
	beq.w ok
	moveq #0, d7
	move.w 1(a0), d7
	cmpi.b #34, 4(a0)
	beq.w assignment
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a0)
	beq.w assignment
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a0)
	beq.w assignment
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, 4(a0)
	beq.w assignment
	movem.l a0/d0, -(sp)
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	ori.w #records.LABEL_VALUE, records.Entry.Flags(a0)
	movem.l (sp)+, a0/d0
	addq.l #5, a0
	bra.w statement
updateReference
	bsr.w bindUpdate
	bne.w bad
	bra.w references
assignment
	addq.l #5, a0
	bra.w references
statement
	cmpa.l a4, a0
	beq.w ok
	cmpi.b #7, (a0)
	beq.w directive
	; The instruction name is package-owned, not a source-symbol reference.
	cmpi.b #1, (a0)
	bhi.w bad
	addq.l #4, a0
	bra.w references
directive
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w bad
	cmpi.b #7, (a0)
	bne.w bad
	cmpi.b #1, 1(a0)
	bhi.w bad
	tst.b 4(a0)
	bne.w bad
	moveq #0, d0
	move.w 2(a0), d0
	movea.l layout.State.Package(a6), a1
	move.l a1, d1
	beq.w ordinaryDirective
loopPolicy
	movea.l layout.State.Package(a6), a1
	movem.l d0/a0, -(sp)
	movea.l a5, a0
	move.l d6, d0
	move.l pkg.Header.LoopPlanBytes(a1), d1
	adda.l pkg.Header.LoopPlan(a1), a1
	jsr loop_parser.classify
	move.l d0, d3
	movem.l (sp)+, d0/a0
	cmpi.l #loop_abi.PRVM_LOOP_MALFORMED, d3
	beq.w bad
	cmpi.l #loop_abi.PRVM_LOOP_MATCHED, d3
	beq.w loopHeader
	cmpi.l #loop_abi.PRVM_LOOP_WHILE, d3
	beq.w loopHeader
	cmpi.l #loop_abi.PRVM_LOOP_BFOR, d3
	beq.w scopedLoopHeader
	cmpi.l #loop_abi.PRVM_LOOP_BWHILE, d3
	beq.w scopedLoopHeader
	cmpi.l #loop_abi.PRVM_LOOP_ENDFOR, d3
	beq.w closeFor
	cmpi.l #loop_abi.PRVM_LOOP_ENDWHILE, d3
	bne.w ordinaryDirective
	moveq #iterators.WHILE, d1
	bra.w closeLoop
closeFor
	moveq #iterators.FOR, d1
closeLoop
	moveq #0, d2
	move.w layout.State.LoopDepth(a6), d2
	beq.w bad
	subq.w #1, d2
	add.w d2, d2
	move.w layout.State.LoopParents(a6, d2.w), d2
	move.w d2, -(sp)
	movem.l d0/a0, -(sp)
	lea layout.ITERATOR_STATE(a6), a0
	jsr iterators.close
	movem.l (sp)+, d0/a0
	bne.w failedCloseLoop
	move.w (sp)+, layout.State.Current(a6)
	bra.w ordinaryDirective
failedCloseLoop
	addq.l #2, sp
	bra.w bad
ordinaryDirective
	bsr.w keyword
	cmpi.l #KEY_USE, d0
	beq.w importing
	cmpi.l #KEY_MODULE, d0
	beq.w module
	cmpi.l #KEY_ENDMODULE, d0
	beq.w endModule
	cmpi.l #KEY_PUB, d0
	beq.w public
	cmpi.l #KEY_PRIV, d0
	beq.w private
	cmpi.l #KEY_BLOCK, d0
	beq.w block
	cmpi.l #KEY_ENDBLOCK, d0
	beq.w endBlock
	cmpi.l #KEY_NAMESPACE, d0
	beq.w namespace
	cmpi.l #KEY_ENDNAMESPACE, d0
	beq.w endNamespace
	cmpi.l #KEY_END, d0
	beq.w end
	cmpi.l #KEY_SECTION, d0
	beq.w sectionControl
	cmpi.l #KEY_ENDSECTION, d0
	beq.w sectionControl
	cmpi.l #KEY_REGION, d0
	beq.w sectionControl
	cmpi.l #KEY_PLACE, d0
	beq.w sectionControl
	cmpi.l #KEY_OUTPUT, d0
	beq.w outputControl
	; Other directives retain their existing generic preparation/assembly route.
	addq.l #5, a0
	bra.w references
scopedLoopHeader
	tst.l d7
	bmi.w loopHeader
	movem.l d0-d2/a0-a1, -(sp)
	move.l d7, d0
	sub.w layout.State.Base(a6), d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a1
	adda.l d0, a1
	andi.w #$ffff-records.LABEL_VALUE, records.Entry.Flags(a1)
	ori.w #records.COMPOUND_LABEL, records.Entry.Flags(a1)
	movem.l (sp)+, d0-d2/a0-a1
loopHeader
	move.l d3, -(sp)
	moveq #0, d0
	move.w layout.State.LoopDepth(a6), d0
	cmpi.w #iterators.LIMIT, d0
	bhs.w failedLoopHeader
	add.w d0, d0
	move.w layout.State.Current(a6), layout.State.LoopParents(a6, d0.w)
	cmpi.l #loop_abi.PRVM_LOOP_BFOR, d3
	beq.w scopedIdentity
	cmpi.l #loop_abi.PRVM_LOOP_BWHILE, d3
	bne.w loopIdentity
scopedIdentity
	bsr.w anonymousRepeat
	bne.w failedLoopHeader
	move.w layout.State.Current(a6), layout.State.PendingRepeat(a6)
	moveq #0, d0
	move.w layout.State.LoopDepth(a6), d0
	add.w d0, d0
	move.w layout.State.LoopParents(a6, d0.w), layout.State.Current(a6)
loopIdentity
	tst.l d1
	bmi.w countedLoop
	move.l d1, d0
	movea.l a6, a0
	bsr.w createIterator
	bne.w failedLoopHeader
	; Iterator occupies the bare name immediately before the `in` token.
	movea.l a5, a0
	adda.l d2, a0
	suba.w #loop_abi.LOOP_EXPRESSION_OFFSET-loop_abi.LoopRecord.VariableKind, a0
	clr.b source.NameToken.Kind(a0)
	move.w d1, source.NameToken.Id(a0)
	clr.b source.NameToken.Qualifier(a0)
	move.l d1, d0
	bra.w openLoop
countedLoop
	move.l #iterators.COUNTED, d0
openLoop
	move.l (sp)+, d1
	cmpi.l #loop_abi.PRVM_LOOP_BFOR, d1
	beq.w openScopedFor
	cmpi.l #loop_abi.PRVM_LOOP_BWHILE, d1
	bne.w openKind
	moveq #iterators.SCOPED_WHILE, d1
	bra.w openKind
openScopedFor
	moveq #iterators.SCOPED_FOR, d1
openKind
	lea layout.ITERATOR_STATE(a6), a0
	jsr iterators.open
	bne.w bad
loopOperand
	movea.l a5, a0
	adda.l d2, a0
	bra.w references
failedLoopHeader
	addq.l #4, sp
	bra.w bad
sectionControl
	movea.l a5, a0
	movea.l a6, a1
	movea.l a6, a2
	adda.l #SECTION_STATE, a2
	subi.l #KEY_SECTION-1, d0
	jsr sections.line
	bne.w bad
	bra.w ok
outputControl
	movea.l a5, a0
	movea.l a6, a1
	movea.l a6, a2
	adda.l #SECTION_STATE, a2
	jsr sections.output
	bne.w bad
	bra.w ok
importing
	tst.l d7
	bpl.w bad
	movea.l a6, a1
	lea bind, a2
	movea.l a6, a3
	adda.l #SECTION_STATE, a3
	jsr imports.line
	bne.w bad
	bra.w empty
module
	bsr.w openModule
	bne.w bad
	bra.w retainedLabel
endModule
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	lea MODULE_STATE(a6), a0
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	jsr modules.close
	bne.w bad
	clr.w layout.State.Current(a6)
	bra.w retainedLabel
public
	moveq #1, d1
	bra.w visibility
private
	moveq #0, d1
visibility
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	lea MODULE_STATE(a6), a0
	move.w d1, modules.State.Visibility(a0)
retainedLabel
	tst.l d7
	bmi.w empty
	move.b #8, (a5)
	move.b #5, 8(a5)
	bra.w ok

block
	moveq #KIND_BLOCK, d2
	bsr.w openScope
	bne.w done
	tst.w d5
	beq.w addressBlock
	; Macro hygiene changes lexical binding only; its generated scope name
	; is not an address label to define again during counted replay.
	bra.w empty
addressBlock
	ori.b #source.FLAG_BLOCK_OPEN, 1(a5)
	bra.w done
namespace
	moveq #KIND_NAMESPACE, d2
opening
	bsr.w openScope
	bra.w done
endBlock
	tst.w d5
	bne.w lexicalClose
	ori.b #source.FLAG_BLOCK_CLOSE, 1(a5)
lexicalClose
	moveq #KIND_BLOCK, d2
	bra.w closing
endNamespace
	moveq #KIND_NAMESPACE, d2
closing
	bsr.w closeScope
	bne.w bad
	bra.w empty
end
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	tst.w layout.State.Current(a6)
	beq.w endSyntax
	lea MODULE_STATE(a6), a0
	tst.w modules.State.FileDerived(a0)
	beq.w bad
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	jsr modules.close
	bne.w bad
	clr.w layout.State.Current(a6)
	clr.w modules.State.FileDerived(a0)
endSyntax
	move.w #1, layout.State.Ended(a6)
	bra.w retainedLabel  ; EOF continues with the next explicit source
references
	cmpa.l a4, a0
	beq.w ok
	bhi.w bad
	moveq #0, d0
	move.b (a0), d0
	cmpi.b #1, d0
	bls.w reference
	cmpi.b #2, d0
	beq.w literal
	cmpi.b #source.TOKEN_WIDE_NUMBER, d0
	beq.w wideLiteral
	cmpi.b #3, d0
	beq.w string
	cmpi.b #7, d0
	beq.w callHead
	bra.w punctuation
literal
	moveq #source.NUMBER_BYTES, d0
	bra.w literalPayload
wideLiteral
	moveq #source.WIDE_NUMBER_BYTES, d0
literalPayload
	move.l a4, d1
	sub.l a0, d1
	cmp.l d0, d1
	blo.w bad
	adda.l d0, a0
	bra.w references
string
	move.l a4, d1
	sub.l a0, d1
	cmpi.l #2, d1
	blo.w bad
	moveq #0, d0
	move.b 1(a0), d0
	addq.l #2, a0
	subq.l #2, d1
	cmp.l d0, d1
	blo.w bad
	adda.l d0, a0
	bra.w references
; Dotted calls and postfix members carry keys, not address dependencies.
; Keep numeric identities opaque; argument names retain normal binding.
callHead
	move.l a4, d1
	sub.l a0, d1
	cmpi.l #5, d1
	blo.w punctuation
	cmpi.b #1, 1(a0)
	bhi.w punctuation
	addq.l #5, a0
	bra.w references
punctuation
	addq.l #1, a0
	bra.w references
reference
	cmpi.b #source.FIELD_KEY_QUALIFIER, 3(a0)
	beq.w packageName
	; A numeric fallback proxy already owns its occurrence and full-name
	; resolution. Import wrapping would discard that plan identity.
	moveq #0, d0
	move.w source.NameToken.Id(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w importReference
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	btst #records.ENCODING_NAME_HIGH_BIT, records.Entry.Flags(a3)
	bne.w packageName
	move.w records.Entry.Flags(a3), d0
	andi.w #records.BINDING_PROXY, d0
	bne.w sourceReference
importReference
	movea.l a6, a1
	jsr imports.reference
	bne.w bad
sourceReference
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w packageName
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	move.w layout.State.LoopDepth(a6), d0
	cmp.w layout.State.MaterializedLoops(a6), d0
	bne.w optionalReference
	; Retained branches validate availability only for selected numeric records.
	; A later ordinary use still upgrades this identity to a required reference.
	tst.w layout.State.Deferred(a6)
	bne.w optionalReference
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, 8(a5)
	beq.w optionalReference
	ori.w #REFERENCED, records.Entry.Flags(a3)
	bra.w referenceReady
optionalReference
	ori.w #OPTIONAL_REFERENCED, records.Entry.Flags(a3)
referenceReady
	clr.b 3(a0)
	btst #records.ITERATOR_HIGH_BIT, records.Entry.Flags(a3)
	bne.w packageName  ; temporary values create no module reachability edge
	move.l d1, d0
	move.l a0, -(sp)
	lea MODULE_STATE(a6), a0
	jsr modules.reference
	movea.l (sp)+, a0
packageName
	addq.l #4, a0
	bra.w references
empty
	move.b #3, (a5)
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	bne.w lineReturn
	tst.w layout.State.PendingRepeat(a6)
	beq.w lineReturn
	move.w layout.State.PendingRepeat(a6), layout.State.Current(a6)
lineReturn
	clr.w layout.State.PendingRepeat(a6)
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=owned name-token copy,A1=scope. Resolve an earlier lexical binding
; for a preparation snapshot, including shared dotted-member occurrence plans.
; D0/CCR=status; all other registers preserved. Missing earlier declarations
; retain the token identity and return success. New plans are installed before VM use.
earlierReference	.block
	movem.l d1-d3/a0-a3/a6, -(sp)
	suba.w #layout.NAME_BYTES, sp
	movea.l a1, a6
	movea.l a0, a2
	bsr.w bindEarlier
	bne.w done
	moveq #0, d1
	move.w source.NameToken.Id(a2), d1
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	bcs.w good
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	move.w records.Entry.Flags(a3), d0
	andi.w #records.BINDING_PROXY, d0
	bne.w publishBinding
	move.l d1, d3
	movea.l ARENA_POINTER(a6), a0
	adda.l records.Entry.Name(a3), a0
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	beq.w bad
	cmpi.l #layout.NAME_BYTES-1, d0
	bhi.w bad
	move.l d0, d2
	movea.l sp, a1
copySpelling
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bne.w copySpelling
	movea.l sp, a0
	movea.l a6, a1
	bsr.w captureBinding
	bne.w done
	cmp.l d3, d1
	beq.w good
	move.w d1, source.NameToken.Id(a2)
	clr.b source.NameToken.Qualifier(a2)
publishBinding
	; Ordinary binding may have created this proxy since the last owner export.
	; Publish its numeric plan before configuration/replay evaluation consumes it.
	movea.l a6, a0
	movea.l a6, a1
	adda.l #IMPORT_STATE+imports.KNOWN_OWNER, a1
	bsr.w exportBindings
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #layout.NAME_BYTES, sp
	movem.l (sp)+, d1-d3/a0-a3/a6
	tst.l d0
	rts
	.bend  ; earlierReference

; A0=update target token,A6=scope. Prefer an earlier declared lexical
; ancestor now, before preparation-time snapshots and later local declarations.
; An unknown target remains a reference and is rejected only if executed.
; D0/CCR=status; all other registers kept. No new declaration is manufactured.
bindUpdate	.block
	movem.l d1-d3/a0-a3, -(sp)
	moveq #0, d0
	move.w source.NameToken.Id(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w resolve
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a2
	adda.l d0, a2
	btst #2, records.Entry.Flags+1(a2)
	bne.w resolve
	moveq #0, d3
	move.w records.Entry.Owner(a2), d3
owner
	tst.w d3
	beq.w resolve
	move.l d3, d0
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	move.w records.Entry.ScopeKind(a3), d0
	andi.w #$ff, d0
	cmpi.w #KIND_BLOCK, d0
	beq.w local
	move.w records.Entry.Owner(a3), d3
	bra.w owner
local
	; Rust's first-pass update lookup excludes module/global values whenever
	; a block is active. Enclosing namespaces and scoped loops remain visible.
	ori.w #records.UPDATE_LOCAL, records.Entry.Flags(a2)
resolve
	bsr.w bindEarlier
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; bindUpdate

; Earlier value lookup shares the ancestor search, but only update targets
; carry the first-pass block restriction. Ordinary RHS names keep late binding.
bindEarlier	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l a0, a5
	moveq #0, d0
	move.w source.NameToken.Id(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w good
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a4
	adda.l d0, a4
	btst #0, records.Entry.Flags+1(a4)
	bne.w good
	btst #2, records.Entry.Flags+1(a4)
	bne.w good
	moveq #0, d3
	move.w records.Entry.Owner(a4), d3
parent
	tst.w d3
	beq.w good
	move.l d3, d0
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	moveq #0, d3
	move.w records.Entry.Owner(a3), d3
	btst #records.UPDATE_LOCAL_HIGH_BIT, records.Entry.Flags(a4)
	beq.w parentVisible
	bsr.w updateParentVisible
	bne.w good
parentVisible
	movea.l ARENA_POINTER(a6), a0
	adda.l records.Entry.Name(a4), a0
	moveq #0, d0
	move.w records.Entry.Leaf(a4), d0
	adda.l d0, a0
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	sub.w records.Entry.Leaf(a4), d0
	bsr.w compose
	bne.w bad
	bsr.w lookup
	bne.w parent
	btst #0, records.Entry.Flags+1(a3)
	beq.w parent
	move.w records.Entry.Target(a3), source.NameToken.Id(a5)
	clr.b source.NameToken.Qualifier(a5)
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; bindEarlier

; A4=target entry,D3=next ancestor scope,A6=state. D0/CCR=visible.
; Other registers preserved; scope metadata is preparation-only.
updateParentVisible	.block
	movem.l d1/a2, -(sp)
	tst.w d3
	beq.w hidden
	move.l d3, d0
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a2
	adda.l d0, a2
	move.w records.Entry.ScopeKind(a2), d1
	andi.w #$ff, d1
	cmpi.w #modules.KIND_MODULE, d1
	beq.w hidden
visible
	moveq #0, d0
	bra.w done
hidden
	moveq #1, d0
done
	movem.l (sp)+, d1/a2
	tst.l d0
	rts
	.bend  ; updateParentVisible

; A0=packed records, D0=bytes, A1=state. Complete binding then patch IDs in
; records and compact expressions. D0/CCR=status; other registers preserved.
; No source string survives this preparation boundary.
finish	.block
	movem.l d1-d7/a0-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	.BINDING_DIAGNOSTIC_CLEAR
	movea.l a1, a6
	.BINDING_ATTEMPT #binding_diagnostic.CURRENT, #-1, #-1
	tst.w layout.State.Current(a6)
	bne.w bad
	tst.w layout.State.LoopDepth(a6)
	bne.w bad
	movea.l a0, a5
	move.l d0, -(sp)
	movea.l a6, a0
	lea bind, a1
	.BINDING_ATTEMPT #binding_diagnostic.IMPORTS, #-1, #-1
	jsr imports.finish
	bne.w failSaved
	movea.l a6, a0
	adda.l #SECTION_STATE, a0
	.BINDING_ATTEMPT #binding_diagnostic.SECTIONS, #-1, #-1
	jsr sections.finish
	bne.w failSaved
	movea.l a0, a2
	movea.l a5, a0
	move.l (sp), d0
	movea.l a6, a1
	.BINDING_ATTEMPT #binding_diagnostic.OUTPUTS, #-1, #-1
	jsr sections.resolveOutputs
	bne.w failSaved
	moveq #0, d7
resolve
	cmp.w layout.State.Count(a6), d7
	bhs.w rewrite
	move.l d7, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a4
	adda.l d0, a4
	btst #records.ENCODING_NAME_HIGH_BIT, records.Entry.Flags(a4)
	bne.w next  ; text identities never canonicalize through value owners
	move.w records.Entry.Flags(a4), d0
	andi.w #records.BINDING_PROXY, d0
	bne.w next
	btst #records.ITERATOR_HIGH_BIT, records.Entry.Flags(a4)
	bne.w next  ; lexical temporaries never canonicalize through imports/parents
	move.w records.Entry.Flags(a4), d0
	andi.w #DECLARED+REFERENCED+OPTIONAL_REFERENCED, d0
	beq.w next
	tst.w MODULE_STATE+modules.State.Selection(a6)
	beq.w selectedReference
	move.l d7, d1
	add.l d1, d1
	movea.l MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	adda.l d1, a0
	btst #1, 1(a0)
	suba.l d1, a0
	beq.w selectedReference  ; a declaration may serve a selected module
	adda.l d1, a0
	btst #2, 1(a0)
	suba.l d1, a0
	bne.w selectedReference  ; mixed origins need normal validation
	movea.l MODULE_STATE+modules.ORIGINS_POINTER(a6), a0
	moveq #0, d0
	move.w 0(a0, d1.l), d0
	beq.w selectedReference
	subq.w #1, d0
	add.l d0, d0
	movea.l MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	adda.l d0, a0
	btst #4, 1(a0)
	suba.l d0, a0
	beq.w next
selectedReference
	btst #0, records.Entry.Flags+1(a4)
	bne.w access
	.BINDING_ATTEMPT #binding_diagnostic.EXPLICIT, d7, #-1
	btst #2, records.Entry.Flags+1(a4)
	bne.w unresolvedReference
	moveq #0, d3
	move.w records.Entry.Owner(a4), d3
parent
	.BINDING_ATTEMPT #binding_diagnostic.UNRESOLVED, d7, #-1
	tst.w d3
	beq.w unresolvedReference
	move.l d3, d0
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	moveq #0, d3
	move.w records.Entry.Owner(a3), d3
	btst #records.UPDATE_LOCAL_HIGH_BIT, records.Entry.Flags(a4)
	beq.w parentVisible
	bsr.w updateParentVisible
	bne.w unresolvedReference
parentVisible
	movea.l ARENA_POINTER(a6), a0
	moveq #0, d0
	move.l records.Entry.Name(a4), d0
	moveq #0, d1
	move.w records.Entry.Leaf(a4), d1
	add.l d1, d0
	adda.l d0, a0
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	sub.w records.Entry.Leaf(a4), d0
	movem.l d7/a4, -(sp)
	bsr.w compose
	bne.w lookupFailed
	bsr.w lookup
lookupFailed
	movem.l (sp)+, d7/a4
	tst.l d0
	bne.w parent
	btst #0, records.Entry.Flags+1(a3)
	beq.w parent
	move.w records.Entry.Target(a3), records.Entry.Target(a4)
	move.w #1, layout.State.Changed(a6)
access
	move.l d7, d0
	moveq #0, d1
	move.w records.Entry.Target(a4), d1
	sub.w layout.State.Base(a6), d1
	lea MODULE_STATE(a6), a0
	.BINDING_ATTEMPT #binding_diagnostic.VISIBILITY, d7, d1
	jsr modules.check
	bne.w failSaved
	bra.w next
unresolvedReference
	; Optional skipped RHS names do not require a scalar definition. A later
	; ordinary reference to the same entry retains the usual hard error.
	btst #1, records.Entry.Flags+1(a4)
	bne.w failSaved
next
	addq.w #1, d7
	bra.w resolve
rewrite
	bsr.w finalizeBindingProxies
	bne.w failSaved
	move.l (sp)+, d0
	tst.w layout.State.Changed(a6)
	beq.w unchanged
	movea.l a5, a0
	movea.l ENTRIES_POINTER(a6), a1
	moveq #0, d1
	move.w layout.State.Base(a6), d1
	moveq #0, d2
	move.w layout.State.Count(a6), d2
	.BINDING_ATTEMPT #binding_diagnostic.REMAP, #-1, #-1
	jsr records.remap
	bne.w done
unchanged
	movea.l a6, a0
	adda.l #BINDING_PLANS, a0
	movea.l ENTRIES_POINTER(a6), a1
	moveq #0, d0
	move.w layout.State.Base(a6), d0
	moveq #0, d1
	move.w layout.State.Count(a6), d1
	jsr plans.finalize
	bne.w done
	movea.l a6, a0
	adda.l #IMPORT_STATE+imports.KNOWN_OWNER, a0
	lea structNominal, a1
	jsr struct_values.remapNominals
	bne.w done
	movea.l a6, a0
	adda.l #IMPORT_STATE+imports.PARAM_OWNER, a0
	jsr struct_values.remapNominals
	bra.w done
failSaved
	addq.l #4, sp
bad
	moveq #1, d0
done
	.BINDING_DIAGNOSTIC_COMMIT d0, a6, BindingView
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finish
	.priv

; D0=preparation nominal ID,A6=scope. D0/CCR=status,D1=canonical ID;
; other registers preserved. Bare field keys have a separate immutable identity.
structNominal	.block
	movem.l d2/a0, -(sp)
	move.l d0, d1
	sub.w layout.State.Base(a6), d0
	bcs.w good
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a0
	moveq #0, d1
	move.w records.Entry.Target(a0, d0.l), d1
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2/a0
	tst.l d0
	rts
	.bend  ; structNominal

; Classify the whole statement before declaring an optional label. Module
; boundary directives and .end may carry labels without becoming outside content.
; A0=normalized record,A6=state. D0/CCR=status; other registers preserved.
authorizeLine	.block
	movem.l d1-d2/a0-a1, -(sp)
	moveq #0, d1
	move.b (a0), d1
	addq.w #1, d1
	cmpi.w #4, d1
	beq.w ok
	move.w #1, layout.State.FileContent(a6)
	movea.l a0, a1
	adda.w d1, a1
	addq.l #4, a0
	cmpi.w #9, d1
	blo.w content
	cmpi.b #1, (a0)
	bhi.w directive
	cmpi.b #5, 4(a0)
	bne.w content
	addq.l #5, a0
directive
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w content
	cmpi.b #7, (a0)
	bne.w content
	tst.b 4(a0)
	bne.w content
	moveq #0, d0
	move.w 2(a0), d0
	bsr.w keyword
	cmpi.l #KEY_MODULE, d0
	beq.w ok
	cmpi.l #KEY_ENDMODULE, d0
	beq.w ok
	cmpi.l #KEY_END, d0
	beq.w ok
content
	lea MODULE_STATE(a6), a0
	jsr modules.content
	bra.w done
ok
	moveq #0, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; authorizeLine

; A0=directive,A4=end,A6=scope state. Validate the single dotted module name,
; then delegate identity, prefix construction and ownership to modules.
; D0/CCR=status; other registers preserved.
openModule	.block
	movem.l d1-d3/a0-a4, -(sp)
	tst.w layout.State.Current(a6)
	bne.w bad
	addq.l #5, a0
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	lea ENTRIES(a6), a1
	movea.l ARENA_POINTER(a6), a2
	movea.l a6, a3
	lea bind, a4
	lea MODULE_STATE(a6), a0
	jsr modules.open
	bne.w bad
	move.w modules.State.Active(a0), layout.State.Current(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a4
	tst.l d0
	rts
	.bend  ; openModule

; Canonical column-one Identifier/Register prefixes are labels regardless of
; instruction spelling. A0=record,D0=capacity,A6=state. D0/CCR=status;
; D1-D3/A1-A2 scratch. Normalize in place only after proving one spare byte.
; Assignment syntax keeps its existing indentation-independent behavior.
normalizeLabel	.block
	moveq #0, d1
	move.b (a0), d1
	addq.w #1, d1
	cmpi.w #4, d1
	blo.w bad
	cmp.l d0, d1
	bhi.w bad
	cmpi.w #4, d1
	beq.w ok
	cmpi.b #1, 4(a0)
	bhi.w ok
	cmpi.w #8, d1
	blo.w bad
	cmpi.w #9, d1
	blo.w indentation
	cmpi.b #34, 8(a0)
	beq.w ok
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 8(a0)
	beq.w ok
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 8(a0)
	beq.w ok
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, 8(a0)
	beq.w ok
	cmpi.b #source.TOKEN_UPDATE_FIRST, 8(a0)
	blo.w prefixLabel
	cmpi.b #source.TOKEN_UPDATE_LAST, 8(a0)
	bls.w ok
prefixLabel
	cmpi.b #5, 8(a0)
	bne.w indentation
	tst.b 1(a0)
	bne.w bad
	bra.w ok
indentation
	tst.b 1(a0)
	bne.w ok  ; operands, including dotted calls, belong to preparation/selection
	moveq #0, d2
	move.w 5(a0), d2
	cmp.w layout.State.Base(a6), d2
	blo.w bad  ; package-reserved label names remain outside the native subset
	cmpi.w #256, d1
	bhs.w bad
	cmp.l d0, d1
	bhs.w bad
	movea.l a0, a1
	adda.l d1, a1
	movea.l a1, a2
	addq.l #1, a2
	move.l d1, d2
	subq.l #8, d2
	beq.w colon
shift
	move.b -(a1), -(a2)
	subq.l #1, d2
	bne.w shift
colon
	move.b #5, 8(a0)
	addq.b #1, (a0)
	bra.w ok
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; normalizeLabel

; A6=scope state. Create a preparation-only anonymous lexical identity.
; The spelling cannot occur in TKVM source; only its numeric identity survives.
; D0/CCR=status; other registers preserved. No reachability marker is emitted.
anonymousRepeat	.block
	movem.l d1-d7/a0-a5, -(sp)
	subq.l #6, sp
	movea.l sp, a0
	move.b #'#', (a0)+
	moveq #0, d1
	move.w layout.State.Count(a6), d1
	moveq #3, d2
hexDigit
	rol.w #4, d1
	move.w d1, d3
	andi.w #15, d3
	addi.b #'0', d3
	cmpi.b #'9', d3
	bls.w digitReady
	addq.b #7, d3
digitReady
	move.b d3, (a0)+
	dbra d2, hexDigit
	movea.l sp, a0
	moveq #5, d0
	movea.l a6, a1
	bsr.w bind
	bne.w done
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	move.w #KIND_NAMESPACE, records.Entry.ScopeKind(a3)
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	addq.w #1, d0
	move.w d0, layout.State.Current(a6)
	moveq #0, d0
done
	addq.l #6, sp
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; anonymousRepeat

; Shared scope-open boundary. A0=directive,A4=end,A5=record,A6=state,
; D7=optional parent-scope label ID (-1 absent), D2=kind. D0/CCR=status;
; D1/D3/A0/A3 scratch. Namespace identity is independent of DECLARED: an
; operand opener creates no value, and can reopen a path or share a symbol name.
openScope	.block
	addq.l #5, a0
	move.l d7, d3
	cmpa.l a4, a0
	beq.w labelScope
	cmpi.w #KIND_NAMESPACE, d2
	bne.w bad
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	tst.b 3(a0)
	bne.w bad  ; dotted namespace declarations remain outside this increment
	moveq #0, d3
	move.w 1(a0), d3
	bra.w label
labelScope
	tst.l d3
	bmi.w bad  ; anonymous scopes are unsupported
label
	tst.l d7
	bmi.w noLabel
	tst.w layout.State.FirstExplicit(a6)
	bne.w bad
	lea 4(a5), a0
	cmpi.b #5, 4(a0)
	beq.w labelReady
	bsr.w declare
	bne.w bad
labelReady
	move.b #8, (a5)
	move.b #5, 8(a5)
	bra.w enter
noLabel
	move.b #3, (a5)
enter
	move.l d3, d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	mulu.w #records.ENTRY_BYTES, d1
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d1, a3
	; Only opening a path owns its parent metadata. A later qualified value
	; declaration must not change an existing namespace's lexical parent.
	move.w layout.State.Current(a6), records.Entry.Owner(a3)
	lea MODULE_STATE(a6), a0
	move.w modules.State.Visibility(a0), d1
	lsl.w #8, d1
	or.w d2, d1
	move.w d1, records.Entry.ScopeKind(a3)
	addq.w #1, d0
	move.w d0, layout.State.Current(a6)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; openScope

; Same line/state inputs as openScope, D2=expected kind. D0/CCR=status;
; A0/A3 scratch. Kind lives in the otherwise unused entry word; no extra table.
closeScope	.block
	tst.l d7
	bpl.w bad
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	beq.w bad
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	move.w records.Entry.ScopeKind(a3), d0
	andi.w #$ff, d0
	cmp.w d0, d2
	bne.w bad
	move.w records.Entry.ScopeKind(a3), d0
	lsr.w #8, d0
	move.l a0, -(sp)
	lea MODULE_STATE(a6), a0
	move.w d0, modules.State.Visibility(a0)
	movea.l (sp)+, a0
	move.w records.Entry.Owner(a3), layout.State.Current(a6)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; closeScope

; A0=name token, A6=state. D0/status, D1/A3 scratch; other registers kept.
declare	.block
	cmpi.b #1, 3(a0)
	bhi.w bad
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	btst #0, records.Entry.Flags+1(a3)
	beq.w firstDeclaration
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a0)
	beq.w ignored
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, 4(a0)
	beq.w conditionalCandidate
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a0)
	bne.w bad
	; Retained branches validate replacement ownership only when selected.
	; Keep readonly metadata intact; runtime mutable execution rejects it.
	tst.w layout.State.Deferred(a6)
	bne.w deferredReplacement
	btst #records.MUTABLE_BIT-8, records.Entry.Flags(a3)
	beq.w bad
deferredReplacement
	clr.b 3(a0)
	moveq #0, d0
	rts
conditionalCandidate
	; Deferred branches may contain alternative definitions. Keep their
	; shared lexical ID; the runtime inventory rejects duplicate active RHSs.
	move.l a0, -(sp)
	lea layout.ITERATOR_STATE(a6), a0
	jsr iterators.scoped
	movea.l (sp)+, a0
	beq.w conditionalOwnership
	tst.w layout.State.LoopDepth(a6)
	bne.w bad
conditionalOwnership
	btst #records.MUTABLE_BIT-8, records.Entry.Flags(a3)
	bne.w bad
	tst.w layout.State.Deferred(a6)
	bne.w candidate
	move.w records.Entry.Flags(a3), d0
	andi.w #records.CONDITIONAL_VALUE, d0
	beq.w bad
candidate
	ori.w #records.CONDITIONAL_VALUE, records.Entry.Flags(a3)
	clr.b 3(a0)
	moveq #0, d0
	rts
ignored
	move.b #source.TOKEN_IGNORED_DECLARATION, 4(a0)
	clr.b 3(a0)
	moveq #0, d0
	rts
firstDeclaration
	clr.b 3(a0)
	ori.w #DECLARED, records.Entry.Flags(a3)
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, 4(a0)
	bne.w declarationKind
	tst.w layout.State.Deferred(a6)
	beq.w declarationKind
	ori.w #records.CONDITIONAL_VALUE, records.Entry.Flags(a3)
declarationKind
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a0)
	beq.w mutableOwnership
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a0)
	bne.w ownershipReady
mutableOwnership
	ori.w #records.MUTABLE, records.Entry.Flags(a3)
ownershipReady
	; Only declarations collectible by scoped replay need global field keys.
	; Ordinary files retain their existing identity count and binding cost.
	move.l a0, -(sp)
	lea layout.ITERATOR_STATE(a6), a0
	jsr iterators.scoped
	movea.l (sp)+, a0
	bne.w declarationOwnership
	movem.l d1-d2/a0-a3, -(sp)
	moveq #0, d0
	move.w 1(a0), d0
	movea.l a6, a0
	bsr.w memberIdentity
	movem.l (sp)+, d1-d2/a0-a3
	tst.l d0
	bne.w bad
declarationOwnership
	move.l d1, d0
	move.l a0, -(sp)
	lea MODULE_STATE(a6), a0
	jsr modules.claim
	movea.l (sp)+, a0
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; declare

; A0/D0=name, D3=scope index+1. Returns folded lookup input A2/D6, leaf
; offset D7 and D5=explicitly qualified. Clobbers D0-D2/A0-A1/A3; keeps D3/D4.
compose	.block
	movea.l a0, a2
	move.l d0, d6
	moveq #0, d7
	moveq #0, d5
	tst.l d0
	beq.w bad
	cmpi.l #layout.NAME_BYTES-1, d0
	bhi.w bad
	tst.w layout.State.BindingNamespace(a6)
	bne.w ready  ; encoding spelling has no lexical/member qualification
scan
	cmpi.b #'.', (a0)+
	beq.w qualified
	subq.l #1, d0
	bne.w scan
	tst.w d3
	beq.w ready
	move.l d3, d0
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	moveq #0, d7
	move.w records.Entry.Length(a3), d7
	addq.w #1, d7
	move.l d6, d0
	add.l d7, d0
	cmpi.l #layout.NAME_BYTES-1, d0
	bhi.w bad
	movea.l ARENA_POINTER(a6), a0
	moveq #0, d1
	move.l records.Entry.Name(a3), d1
	adda.l d1, a0
	lea BUFFER(a6), a1
	move.l d7, d1
	subq.w #1, d1
prefix
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.w prefix
	move.b #'.', (a1)+
	movea.l a2, a0
	move.l d6, d1
leaf
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.w leaf
	lea BUFFER(a6), a2
	add.l d7, d6
	bra.w ready
qualified
	moveq #1, d5
ready
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; compose

; A2/D6=lookup bytes. D0/CCR=found status, A3=entry; D4=bucket (even on miss).
; Clobbers D0-D2/A0-A1/A3; preserves other registers.
lookup	.block
	movea.l a2, a0
	move.l d6, d0
	moveq #0, d4
hash
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.l d4, d2
	lsl.l #5, d4
	add.l d2, d4
	add.l d1, d4
	subq.l #1, d0
	bne.w hash
	andi.l #255, d4
	move.l d4, d0
	add.w d0, d0
	lea BUCKETS(a6), a0
	moveq #0, d2
	move.w 0(a0, d0.w), d2
chain
	tst.w d2
	beq.w missing
	subq.w #1, d2
	mulu.w #records.ENTRY_BYTES, d2
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d2, a3
	moveq #0, d0
	btst #records.ENCODING_NAME_HIGH_BIT, records.Entry.Flags(a3)
	beq.w namespaceReady
	moveq #1, d0
namespaceReady
	cmp.w layout.State.BindingNamespace(a6), d0
	bne.w next
	cmp.w records.Entry.Length(a3), d6
	bne.w next
	movea.l ARENA_POINTER(a6), a1
	moveq #0, d0
	move.l records.Entry.Name(a3), d0
	adda.l d0, a1
	movea.l a2, a0
	move.l d6, d0
compare
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.w d1, d2
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d1, d2
	bne.w next
	subq.l #1, d0
	bne.w compare
	moveq #0, d0
	rts
next
	moveq #0, d2
	move.w records.Entry.Next(a3), d2
	bra.w chain
missing
	moveq #1, d0
	rts
	.bend  ; lookup

; D1=ASCII byte, fold uppercase letters only. Other registers preserved.
fold	.block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; fold

; D0=numeric directive ID. Returns D0=KEY_* code, zero for other directives.
; A6=state; preserves other registers. Package .end is supplied by caller.
keyword	.block
	movem.l d1-d4/a0-a3, -(sp)
	cmp.w layout.State.EndDirective(a6), d0
	beq.w end
	sub.w layout.State.Base(a6), d0
	bcs.w none
	cmp.w layout.State.Count(a6), d0
	bhs.w none
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	movea.l ARENA_POINTER(a6), a0
	moveq #0, d0
	move.l records.Entry.Name(a3), d0
	adda.l d0, a0
	moveq #0, d4
	move.w records.Entry.Length(a3), d4
	movea.l a0, a1
	move.l d4, d0
leafScan
	cmpi.b #'.', (a1)+
	bne.w leafNext
	movea.l a1, a0
	move.l d0, d4
	subq.w #1, d4
leafNext
	subq.w #1, d0
	bne.w leafScan
	bsr.w matchKeyword
	bra.w done
end
	moveq #KEY_END, d0
	bra.w done
none
	moveq #0, d0
done
	movem.l (sp)+, d1-d4/a0-a3
	rts
	.bend  ; keyword

; A0=directive leaf,D4=bytes. Match the shared directive table without binding.
; D0=KEY_* or zero; D1-D3/A1-A3 scratch. CCR unspecified.
matchKeyword	.block
	lea Words, a2
word
	moveq #0, d3
	move.b (a2)+, d3
	beq.w none
	moveq #0, d2
	move.b (a2)+, d2
	cmp.w d4, d2
	bne.w skip
	movea.l a0, a1
	move.l d2, d0
character
	moveq #0, d1
	move.b (a1)+, d1
	bsr.w fold
	cmp.b (a2)+, d1
	bne.w mismatch
	subq.w #1, d0
	bne.w character
	move.l d3, d0
	bra.w done
mismatch
	subq.w #1, d0
	adda.w d0, a2
	bra.w next
skip
	adda.w d2, a2
next
	bra.w word
none
	moveq #0, d0
done
	rts
	.bend  ; matchKeyword

	.pub
; A0=owned directive leaf,D0=bytes. Classify using the same table as numeric
; preparation, without allocating an identity. D0/CCR=KEY_* or zero; other
; registers preserved. Input must be a bounded TKVM identifier spelling.
classifySpelling	.block
	movem.l d1-d4/a0-a3, -(sp)
	move.l d0, d4
	bsr.w matchKeyword
	movem.l (sp)+, d1-d4/a0-a3
	tst.l d0
	rts
	.bend  ; classifySpelling

; A0=scope state,D0=numeric directive ID. D0=KEY_* or zero; other
; registers preserved. Classification reads only the bound numeric name.
classifyDirective	.block
	move.l a6, -(sp)
	movea.l a0, a6
	bsr.w keyword
	movea.l (sp)+, a6
	tst.l d0
	rts
	.bend  ; classifyDirective
; A0=scope state,D0=definition-time source ID,D1=original qualifier.
; D0/CCR=status,D1=call-scope ID. Qualified names retain their identity;
; unqualified source names bind from their lexical leaf in the active scope.
; Other registers preserved. Package and encoding IDs are returned unchanged.
rebindLocal	.block
	movem.l d2-d5/a0-a3/a6, -(sp)
	move.l a1, -(sp)
	movea.l a0, a6
	tst.w d1
	bne.w unchanged
	move.l d0, d5
	sub.w layout.State.Base(a6), d5
	bcs.w unchanged
	cmp.w layout.State.Count(a6), d5
	bhs.w badRebind
	mulu.w #records.ENTRY_BYTES, d5
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d5, a3
	; Encoding identities are global non-value names. Macro hygiene must
	; preserve their facet and ID instead of creating a call-local symbol.
	btst #records.ENCODING_NAME_HIGH_BIT, records.Entry.Flags(a3)
	bne.w unchanged
	move.l records.Entry.RegisterName(a3), d4
	moveq #0, d2
	move.w records.Entry.Leaf(a3), d2
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	sub.w d2, d0
	beq.w badRebind
	movea.l ARENA_POINTER(a6), a0
	moveq #0, d3
	move.l records.Entry.Name(a3), d3
	add.l d2, d3
	adda.l d3, a0
	movea.l a6, a1
	tst.l d4
	bne.w candidateBinding
	bsr.w bind
	bra.w rebindDone
candidateBinding
	; Invocation-local iterators win before ordinary local-name rebinding.
	move.l d0, d5
	bsr.w probeIterator
	beq.w attachCandidate
	move.l d5, d0
	bsr.w bindOrigin
	bne.w badRebind
attachCandidate
	move.l d1, -(sp)
	move.l d1, d0
	move.l d4, d1
	swap d1
	andi.l #$ffff, d1
	subq.w #1, d1
	move.l d4, d2
	lsr.w #8, d2
	andi.l #$ff, d2
	movea.l a6, a0
	bsr.w attachRegister
	move.l (sp)+, d1
	tst.l d0
	bne.w badRebind
	bra.w rebindDone
unchanged
	move.l d0, d1
	moveq #0, d0
	bra.w rebindDone
badRebind
	moveq #1, d0
rebindDone
	movea.l (sp)+, a1
	movem.l (sp)+, d2-d5/a0-a3/a6
	tst.l d0
	rts
	.bend  ; rebindLocal

; A0=scope state,D0=bound name ID. D0/CCR=status,D1=folded final-component
; bucket (0..255). This is a candidate key, never a visibility decision.
; Entry.Leaf denotes binding context, so explicit qualification is scanned here.
templateKey	.block
	movem.l d2-d4/a0-a2, -(sp)
	sub.w layout.State.Base(a0), d0
	bcs.w bad
	cmp.w layout.State.Count(a0), d0
	bhs.w bad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a0), a1
	adda.l d0, a1
	moveq #0, d2
	move.w records.Entry.Length(a1), d2
	beq.w bad
	movea.l ARENA_POINTER(a0), a2
	adda.l records.Entry.Name(a1), a2
	moveq #0, d3
scan
	moveq #0, d1
	move.b (a2)+, d1
	cmpi.b #'.', d1
	beq.w component
	bsr.w fold
	move.l d3, d4
	lsl.l #5, d3
	add.l d4, d3
	eor.l d1, d3
	bra.w next
component
	moveq #0, d3
next
	subq.w #1, d2
	bne.w scan
	move.l d3, d1
	andi.l #255, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d4/a0-a2
	tst.l d0
	rts
	.bend  ; templateKey

; Read-only selected-name hint. The import resolver remains authoritative.
; A0=scope state,D0=call ID. D0/CCR=status,D1=selected definition ID.
templateAliasTarget	.block
	jsr imports.templateAliasTarget
	rts
	.bend  ; templateAliasTarget

; A0=scope state,D0=call ID,D1=definition ID. A read-only candidate check
; prevents ordinary dot directives from allocating import proxies. Selected
; per-item aliases may have a different leaf than their definition.
; D0/CCR=zero on a matching leaf or selected alias; other registers preserved.
templateCandidate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.w d0, d4
	move.w d1, d5
	bsr.w templateLeafEqual
	beq.w candidateFound
	moveq #0, d0
	move.w d4, d0
	moveq #0, d1
	move.w d5, d1
	movea.l a6, a0
	jsr imports.templateAliasCandidate
	beq.w candidateFound
	bra.w candidateMissing
candidateFound
	moveq #0, d0
	bra.w candidateDone
candidateMissing
	moveq #1, d0
candidateDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateCandidate

; A0=scope state,D0/D1=numeric name IDs. Compare folded leaf bytes.
; D0/CCR=zero on equal leaves; other registers preserved.
templateLeafEqual	.block
	movem.l d1-d5/a0-a3/a6, -(sp)
	movea.l a0, a6
	sub.w layout.State.Base(a6), d0
	bcs.w leafMissing
	cmp.w layout.State.Count(a6), d0
	bhs.w leafMissing
	sub.w layout.State.Base(a6), d1
	bcs.w leafMissing
	cmp.w layout.State.Count(a6), d1
	bhs.w leafMissing
	mulu.w #records.ENTRY_BYTES, d0
	mulu.w #records.ENTRY_BYTES, d1
	movea.l ENTRIES_POINTER(a6), a2
	adda.l d0, a2
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d1, a3
	moveq #0, d2
	move.w records.Entry.Length(a2), d2
	moveq #0, d3
	move.w records.Entry.Length(a3), d3
	movea.l ARENA_POINTER(a6), a0
	moveq #0, d0
	move.l records.Entry.Name(a2), d0
	adda.l d0, a0
	movea.l ARENA_POINTER(a6), a1
	moveq #0, d0
	move.l records.Entry.Name(a3), d0
	adda.l d0, a1
	; Entry.Leaf is the first binding context, not necessarily the final
	; component of an explicitly qualified import alias.
	moveq #0, d4
	moveq #0, d5
scanCallLeaf
	cmp.l d2, d4
	bhs.w callLeafReady
	cmpi.b #'.', 0(a0, d4.w)
	bne.w nextCallLeaf
	move.l d4, d5
	addq.l #1, d5
nextCallLeaf
	addq.l #1, d4
	bra.w scanCallLeaf
callLeafReady
	adda.l d5, a0
	sub.l d5, d2
	moveq #0, d4
	moveq #0, d5
scanDefinitionLeaf
	cmp.l d3, d4
	bhs.w definitionLeafReady
	cmpi.b #'.', 0(a1, d4.w)
	bne.w nextDefinitionLeaf
	move.l d4, d5
	addq.l #1, d5
nextDefinitionLeaf
	addq.l #1, d4
	bra.w scanDefinitionLeaf
definitionLeafReady
	adda.l d5, a1
	sub.l d5, d3
	cmp.l d3, d2
	bne.w leafMissing
	tst.l d2
	beq.w leafMissing
leafCompare
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.w d1, d4
	moveq #0, d1
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d1, d4
	bne.w leafMissing
	subq.w #1, d2
	bne.w leafCompare
	moveq #0, d0
	bra.w leafDone
leafMissing
	moveq #1, d0
leafDone
	movem.l (sp)+, d1-d5/a0-a3/a6
	tst.l d0
	rts
	.bend  ; templateLeafEqual

; A0=scope state,D0=call name ID,D1=definition name ID.
; D0/CCR=zero when visible, D1=ancestor distance (nearest is zero).
; Only an unqualified call may search lexical ancestors; exact IDs also match.
; Other registers preserved.
templateDistance	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	cmp.w d1, d0
	beq.w exact
	moveq #0, d6
	move.w d0, d6
	sub.w layout.State.Base(a6), d6
	bcs.w missing
	cmp.w layout.State.Count(a6), d6
	bhs.w missing
	moveq #0, d7
	move.w d1, d7
	sub.w layout.State.Base(a6), d7
	bcs.w missing
	cmp.w layout.State.Count(a6), d7
	bhs.w missing
	mulu.w #records.ENTRY_BYTES, d6
	mulu.w #records.ENTRY_BYTES, d7
	movea.l ENTRIES_POINTER(a6), a4
	adda.l d6, a4
	movea.l ENTRIES_POINTER(a6), a5
	adda.l d7, a5
	tst.w records.Entry.Leaf(a4)
	beq.w missing  ; an explicitly qualified call needs an exact ID
	moveq #0, d2
	move.w records.Entry.Length(a4), d2
	sub.w records.Entry.Leaf(a4), d2
	moveq #0, d3
	move.w records.Entry.Length(a5), d3
	sub.w records.Entry.Leaf(a5), d3
	cmp.w d3, d2
	bne.w missing
	movea.l ARENA_POINTER(a6), a2
	moveq #0, d0
	move.l records.Entry.Name(a4), d0
	moveq #0, d1
	move.w records.Entry.Leaf(a4), d1
	add.l d1, d0
	adda.l d0, a2
	movea.l ARENA_POINTER(a6), a3
	moveq #0, d0
	move.l records.Entry.Name(a5), d0
	moveq #0, d1
	move.w records.Entry.Leaf(a5), d1
	add.l d1, d0
	adda.l d0, a3
compareTemplateLeaf
	moveq #0, d1
	move.b (a2)+, d1
	bsr.w fold
	move.w d1, d6
	moveq #0, d1
	move.b (a3)+, d1
	bsr.w fold
	cmp.b d1, d6
	bne.w missing
	subq.w #1, d2
	bne.w compareTemplateLeaf
	moveq #0, d4
	move.w records.Entry.Owner(a4), d4
	moveq #0, d5
	move.w records.Entry.Owner(a5), d5
	moveq #0, d1
templateAncestor
	cmp.w d5, d4
	beq.w foundTemplate
	tst.w d4
	beq.w missing
	moveq #0, d0
	move.w d4, d0
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	move.w records.Entry.Owner(a0), d4
	addq.w #1, d1
	bra.w templateAncestor
exact
	move.w d1, d0
	moveq #0, d1
	sub.w layout.State.Base(a6), d0
	bcs.w missing
	cmp.w layout.State.Count(a6), d0
	bhs.w missing
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	move.w records.Entry.TemplateModule(a0), d0
	beq.w foundTemplate  ; global template
	cmp.w MODULE_STATE+modules.State.Active(a6), d0
	bne.w missing
foundTemplate
	moveq #0, d0
	bra.w templateDone
missing
	moveq #1, d0
	moveq #-1, d1
templateDone
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateDistance

; A0=template name token,A1=scope state. Declare the independent template
; facet of a lexical identity; the body remains in binary_templates.
; D0/CCR=status; other registers preserved.
declareTemplate	.block
	movem.l d1/a0-a1/a3/a6, -(sp)
	movea.l a1, a6
	cmpi.b #1, 3(a0)
	bhi.w templateBad
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w templateBad
	cmp.w layout.State.Count(a6), d0
	bhs.w templateBad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	btst #0, records.Entry.TemplateFlags+1(a3)
	bne.w templateBad
	ori.w #records.TEMPLATE_DECLARED, records.Entry.TemplateFlags(a3)
	movea.l a3, a1
	lea MODULE_STATE(a6), a0
	jsr modules.claimTemplate
	moveq #0, d0
	bra.w templateDeclared
templateBad
	moveq #1, d0
templateDeclared
	movem.l (sp)+, d1/a0-a1/a3/a6
	tst.l d0
	rts
	.bend  ; declareTemplate

; A0=four-byte call-name token,A1=scope state. Resolve an imported template
; without changing the caller's record. D0/CCR=status,D1=definition ID.
; Other registers preserved.
resolveTemplate	.block
	movem.l d2/a0-a2, -(sp)
	subq.l #4, sp
	move.l (a0), (sp)
	movea.l sp, a0
	lea bind, a2
	jsr imports.resolveTemplate
	addq.l #4, sp
	movem.l (sp)+, d2/a0-a2
	tst.l d0
	rts
	.bend  ; resolveTemplate
	.pub
; A0=numeric writer record,A1=scope. D0=existing shared scope KEY_* role;
; other registers preserved. Read-only: reuse the normal directive resolver.
statementRole	.block
	movem.l d1-d3/a0-a2/a6, -(sp)
	movea.l a1, a6
	moveq #0, d1
	move.b (a0), d1
	addq.w #1, d1
	cmpi.w #9, d1
	blo.w absent
	lea 4(a0), a2
	cmpi.b #1, (a2)
	bhi.w head
	cmpi.b #7, 4(a2)
	beq.w namedHead
	cmpi.b #5, 4(a2)
	bne.w absent
	cmpi.w #14, d1
	blo.w absent
	addq.w #5, a2
	bra.w head
namedHead
	cmpi.w #13, d1
	blo.w absent
	addq.w #4, a2
head
	cmpi.b #7, (a2)
	bne.w absent
	cmpi.b #1, 1(a2)
	bhi.w absent
	moveq #0, d0
	move.w 2(a2), d0
	bsr.w keyword
	bra.w done
absent
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0-a2/a6
	rts
	.bend  ; statementRole
	.priv
	.priv
Words
	.byte KEY_END, 3, "end"
	.byte KEY_BLOCK, 5, "block"
	.byte KEY_ENDBLOCK, 8, "endblock"
	.byte KEY_ENDBLOCK, 4, "bend"
	.byte KEY_NAMESPACE, 9, "namespace"
	.byte KEY_ENDNAMESPACE, 12, "endnamespace"
	.byte KEY_ENDNAMESPACE, 4, "endn"
	.byte KEY_MODULE, 6, "module"
	.byte KEY_ENDMODULE, 9, "endmodule"
	.byte KEY_PUB, 3, "pub"
	.byte KEY_PRIV, 4, "priv"
	.byte KEY_USE, 3, "use"
	.byte KEY_SECTION, 7, "section"
	.byte KEY_ENDSECTION, 10, "endsection"
	.byte KEY_REGION, 6, "region"
	.byte KEY_PLACE, 5, "place"
	.byte KEY_OUTPUT, 6, "output"
	.byte KEY_SEGMENT, 7, "segment"
	.byte KEY_ENDSEGMENT, 10, "endsegment"
	.byte KEY_ENDSEGMENT, 4, "ends"
	.byte KEY_MACRO, 5, "macro"
	.byte KEY_ENDMACRO, 8, "endmacro"
	.byte KEY_ENDMACRO, 4, "endm"
	.byte KEY_STRUCT, 6, "struct"
	.byte KEY_ENDSTRUCT, 9, "endstruct"
	.byte KEY_DB, 2, "db"
	.byte KEY_DW, 2, "dw"
	.byte KEY_IF, 2, "if"
	.byte KEY_ELSE, 4, "else"
	.byte KEY_ELSEIF, 6, "elseif"
	.byte KEY_ENDIF, 5, "endif"
	.byte KEY_IFDEF, 5, "ifdef"
	.byte KEY_IFNDEF, 6, "ifndef"
	.byte 0
	.align 2  ; the next module shares this instruction section
	.endsection

.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.section data, kind=data
	.priv
BindingView	.word layout.State.Count, layout.State.Base, layout.State.Current
	.word layout.ENTRIES_POINTER, layout.ARENA_POINTER, layout.State.ArenaUsed
	.word records.ENTRY_BYTES, records.Entry.Name, records.Entry.Length, records.Entry.Flags
	.word records.Entry.Owner
	.endsection
.endif
.endif
.endif
	.endmodule
