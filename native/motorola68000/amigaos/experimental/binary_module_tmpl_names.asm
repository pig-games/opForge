; Referenced template names, with owned spelling and separate member identities.
; @opforge-owner: experimental.amigaos.binary_module_tmpl_names
	.module experimental.amigaos.binary_module_tmpl_names
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_lexical_store as lexical
	.use experimental.amigaos.binary_binding_records as records
	.pub
VALUE = 0
MEMBER = 1
OK = 0
INVALID = 1
Row	.struct
Id	.long ?
Namespace	.long ?
Name	.long ?  ; offset into the owned Names block
Bytes	.long ?
Leaf	.long ?
RegisterFacets	.long ?  ; Entry.RegisterName/Qualifier/Reserved transport
ValueEntity	.long ?  ; provider-local value ID, $ffffffff when absent
TemplateEntity	.long ?  ; provider-local template origin, $ffffffff when absent
Facets	.long ?  ; VALUE_FACET/TEMPLATE_FACET/TEMPLATE_PUBLIC
ValueDependency	.long ?  ; zero=self, otherwise provider dependency ordinal+1
TemplateDependency	.long ?
	.endstruct
ROW_BYTES = Row.TemplateDependency+4
VALUE_FACET = 1
TEMPLATE_FACET = 2
TEMPLATE_PUBLIC = 4
State	.struct
Scope	.long ?  ; borrowed only while capturing
Base	.long ?
Rows	.res memory.Block.Used+4
Names	.res memory.Block.Used+4
Keys	.res memory.Block.Used+4  ; capture-only source ID/namespace pairs
	.endstruct
STATE_BYTES = State.Keys+memory.Block.Used+4
InstallState	.struct
Scope	.long ?
Base	.long ?
Rows	.long ?
RowBytes	.long ?
Names	.long ?
NameBytes	.long ?
Map	.res memory.Block.Used+4
	.endstruct
INSTALL_BYTES = InstallState.Map+memory.Block.Used+4
MapRow	.struct
Id	.long ?
Namespace	.long ?
Origin	.long ?
Canonical	.long ?
	.endstruct
MAP_BYTES = MapRow.Canonical+4
	.priv
Key	.struct
Id	.long ?
Namespace	.long ?
	.endstruct
KEY_BYTES = Key.Namespace+4
CaptureLocal	.struct
Entry	.res records.ENTRY_BYTES
Name	.res records.NAME_BYTES
Origin	.long ?
	.endstruct
CAPTURE_LOCAL_BYTES = CaptureLocal.Origin+4
	.section code, kind=code
	.pub
; A0=checked stored Row. Validate its provider entity/facet relation.
; D0/CCR=status; other registers preserved. No scope or package pointers needed.
validateEntities	.block
	cmpi.l #65535, Row.ValueDependency(a0)
	bhi.w bad
	cmpi.l #65535, Row.TemplateDependency(a0)
	bhi.w bad
	cmpi.l #VALUE_FACET+TEMPLATE_FACET+TEMPLATE_PUBLIC, Row.Facets(a0)
	bhi.w bad
	tst.l Row.Namespace(a0)
	beq.w value
	tst.l Row.Facets(a0)
	bne.w bad
value
	btst #0, Row.Facets+3(a0)
	beq.w noValue
	cmpi.l #65535, Row.ValueEntity(a0)
	bhi.w bad
	bra.w template
noValue
	cmpi.l #-1, Row.ValueEntity(a0)
	bne.w bad
	tst.l Row.ValueDependency(a0)
	bne.w bad
template
	btst #1, Row.Facets+3(a0)
	beq.w noTemplate
	cmpi.l #65535, Row.TemplateEntity(a0)
	bhi.w bad
	bra.w good
noTemplate
	cmpi.l #-1, Row.TemplateEntity(a0)
	bne.w bad
	tst.l Row.TemplateDependency(a0)
	bne.w bad
	btst #2, Row.Facets+3(a0)
	bne.w bad
good
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; validateEntities

; A0=zero State,A1=live source scope. D0/CCR=status; others preserved.
; Capture visits only IDs requested by the shared closed-template remapper.
begin	.block
	movem.l d1/a0, -(sp)
	move.l a1, d0
	beq.w bad
	lea State.Rows(a0), a0
	moveq #(STATE_BYTES-State.Rows)/4-1, d1
empty
	tst.l (a0)+
	bne.w bad
	dbra d1, empty
	movea.l 4(sp), a0
	move.l a1, State.Scope(a0)
	moveq #0, d0
	move.w layout.State.Base(a1), d0
	move.l d0, State.Base(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; begin

; Remap callback A0=State,D0=source ID,D2=VALUE/MEMBER. D0/CCR=status,
; D1=local ID; others preserved. Package IDs retain identity. Source IDs are
; deduplicated per namespace; local IDs are Base plus the shared row ordinal.
; No pointer into source entries or names survives this call.
captureResolve	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #CAPTURE_LOCAL_BYTES, sp
	movea.l sp, a4
	movea.l a0, a6
	move.l d0, d7
	move.l d2, d6
	cmpi.l #MEMBER, d6
	bhi.w bad
	cmpi.l #65535, d7
	bhi.w bad
	movea.l State.Scope(a6), a5
	cmpa.w #0, a5
	beq.w bad
	cmp.l State.Base(a6), d7
	blo.w packageId
	move.l State.Rows+memory.Block.Used(a6), d5
	move.l d5, d0
	moveq #ROW_BYTES, d1
	divu.l d1, d0
	move.l d0, d4
	mulu.l #ROW_BYTES, d0
	cmp.l d5, d0
	bne.w bad
	move.l d4, d0
	mulu.l #KEY_BYTES, d0
	cmp.l State.Keys+memory.Block.Used(a6), d0
	bne.w bad
	movea.l State.Keys+memory.Block.Pointer(a6), a3
	moveq #0, d3
find
	cmp.l d4, d3
	bhs.w append
	cmp.l Key.Id(a3), d7
	bne.w next
	cmp.l Key.Namespace(a3), d6
	beq.w found
next
	adda.w #KEY_BYTES, a3
	addq.l #1, d3
	bra.w find
found
	move.l d3, d1
	add.l State.Base(a6), d1
	bra.w good
packageId
	move.l d7, d1
	bra.w good
append
	move.l d4, d0
	add.l State.Base(a6), d0
	cmpi.l #65535, d0
	bhi.w bad
	move.l d7, d0
	sub.l State.Base(a6), d0
	moveq #0, d1
	move.w layout.State.Count(a5), d1
	cmp.l d1, d0
	bhs.w bad
	move.l d1, d3
	lea layout.LEXICAL_STATE(a5), a0
	lea CaptureLocal.Entry(a4), a1
readOrigin
	move.l d0, CaptureLocal.Origin(a4)
	jsr lexical.readEntry
	bne.w bad
	; Import proxies reuse Leaf for selection, not a spelling boundary. Their
	; ScopeKind link retains the actual lexical origin; never follow value Target.
	btst #records.IMPORT_PROXY_BIT, CaptureLocal.Entry+records.Entry.Flags+1(a4)
	beq.w originReady
	moveq #0, d0
	move.w CaptureLocal.Entry+records.Entry.ScopeKind(a4), d0
	beq.w bad
	subq.l #1, d0
	moveq #0, d1
	move.w layout.State.Count(a5), d1
	cmp.l d1, d0
	bhs.w bad
	subq.l #1, d3
	beq.w bad
	bra.w readOrigin
originReady
	; Encoding identities have their own non-value namespace. This capsule
	; transports only value origins and member keys, so reject rather than rebind.
	btst #records.ENCODING_NAME_HIGH_BIT, CaptureLocal.Entry+records.Entry.Flags(a4)
	bne.w bad
	moveq #0, d3
	move.w CaptureLocal.Entry+records.Entry.Length(a4), d3
	beq.w bad
	cmpi.l #records.NAME_BYTES-1, d3
	bhi.w bad
	moveq #0, d0
	move.w CaptureLocal.Entry+records.Entry.Leaf(a4), d0
	cmp.l d3, d0
	bhs.w bad
	tst.b CaptureLocal.Entry+records.Entry.RegisterReserved(a4)
	bne.w bad
	moveq #0, d0
	move.w CaptureLocal.Entry+records.Entry.RegisterName(a4), d0
	cmp.l State.Base(a6), d0
	bhi.w bad
	move.l CaptureLocal.Entry+records.Entry.Name(a4), d0
	move.l d3, d1
	lea CaptureLocal.Name(a4), a1
	jsr lexical.copyName
	bne.w bad
	move.l State.Names+memory.Block.Used(a6), d2
	move.l d2, d0
	add.l d3, d0
	bcs.w bad
	lea State.Names(a6), a0
	jsr memory.reserveExact
	bne.w bad
	move.l d5, d0
	addi.l #ROW_BYTES, d0
	bcs.w bad
	lea State.Rows(a6), a0
	jsr memory.reserveExact
	bne.w bad
	move.l State.Keys+memory.Block.Used(a6), d0
	addq.l #KEY_BYTES, d0
	bcs.w bad
	lea State.Keys(a6), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l State.Names+memory.Block.Pointer(a6), a1
	adda.l d2, a1
	lea CaptureLocal.Name(a4), a0
	move.l d3, d0
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
	movea.l State.Rows+memory.Block.Pointer(a6), a3
	adda.l d5, a3
	move.l d4, d1
	add.l State.Base(a6), d1
	move.l d1, Row.Id(a3)
	move.l d6, Row.Namespace(a3)
	move.l d2, Row.Name(a3)
	move.l d3, Row.Bytes(a3)
	moveq #0, d0
	move.w CaptureLocal.Entry+records.Entry.Leaf(a4), d0
	move.l d0, Row.Leaf(a3)
	move.l CaptureLocal.Entry+records.Entry.RegisterName(a4), Row.RegisterFacets(a3)
	move.l #-1, Row.ValueEntity(a3)
	move.l #-1, Row.TemplateEntity(a3)
	clr.l Row.Facets(a3)
	clr.l Row.ValueDependency(a3)
	clr.l Row.TemplateDependency(a3)
	tst.l d6
	bne.w entitiesReady
	btst #0, CaptureLocal.Entry+records.Entry.Flags+1(a4)
	beq.w templateEntity
	moveq #0, d0
	move.w CaptureLocal.Entry+records.Entry.Target(a4), d0
	move.l d0, Row.ValueEntity(a3)
	ori.l #VALUE_FACET, Row.Facets(a3)
templateEntity
	move.w CaptureLocal.Entry+records.Entry.TemplateFlags(a4), d0
	andi.w #records.TEMPLATE_DECLARED+records.STATEMENT_DECLARED, d0
	beq.w entitiesReady
	move.l CaptureLocal.Origin(a4), d0
	add.l State.Base(a6), d0
	move.l d0, Row.TemplateEntity(a3)
	ori.l #TEMPLATE_FACET, Row.Facets(a3)
	move.w CaptureLocal.Entry+records.Entry.TemplateFlags(a4), d0
	andi.w #records.TEMPLATE_PUBLIC+records.STATEMENT_PUBLIC, d0
	beq.w entitiesReady
	ori.l #TEMPLATE_PUBLIC, Row.Facets(a3)
entitiesReady
	movea.l State.Keys+memory.Block.Pointer(a6), a3
	adda.l State.Keys+memory.Block.Used(a6), a3
	move.l d7, Key.Id(a3)
	move.l d6, Key.Namespace(a3)
	add.l d3, State.Names+memory.Block.Used(a6)
	addi.l #ROW_BYTES, State.Rows+memory.Block.Used(a6)
	addq.l #KEY_BYTES, State.Keys+memory.Block.Used(a6)
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
	moveq #0, d1
done
	adda.w #CAPTURE_LOCAL_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; captureResolve

; A0=State. Release owned capture tables and retire the source borrow.
; All registers preserved; CCR unspecified. Safe after partial capture.
release	.block
	movem.l d0/a0-a1, -(sp)
	movea.l a0, a1
	lea State.Rows(a1), a0
	jsr memory.release
	lea State.Names(a1), a0
	jsr memory.release
	lea State.Keys(a1), a0
	jsr memory.release
	movea.l a1, a0
	moveq #STATE_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	movem.l (sp)+, d0/a0-a1
	rts
	.bend  ; release

; A0=InstallState with empty Map and borrowed offset-only rows/names.
; D0/CCR=status; others preserved. Validate the whole table before appending
; destination identities. Failure may leave unused undeclared names/aliases;
; no public value or template facet is claimed. Release Map on every outcome.
install	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	bsr.w validateInstall
	bne.w bad
	move.l InstallState.RowBytes(a6), d7
	moveq #ROW_BYTES, d1
	divu.l d1, d7
	move.l d7, d0
	mulu.l #MAP_BYTES, d0
	lea InstallState.Map(a6), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l InstallState.Rows(a6), a4
	movea.l InstallState.Map+memory.Block.Pointer(a6), a5
next
	tst.l d7
	beq.w good
	movea.l InstallState.Names(a6), a1
	adda.l Row.Name(a4), a1
	move.l Row.Bytes(a4), d0
	movea.l InstallState.Scope(a6), a0
	tst.l Row.Namespace(a4)
	bne.w member
	move.l Row.Leaf(a4), d1
	move.l Row.RegisterFacets(a4), d2
	jsr scopes.bindTemplateOrigin
	bne.w bad
	bra.w mapped
member
	; Member keys are global bare spellings, independent of value origins.
	; Explicit lexical qualification is reduced to its final component.
	movea.l a1, a2
	move.l d0, d3
scanLeaf
	cmpi.b #'.', (a2)+
	bne.w nextByte
	movea.l a2, a1
	move.l d3, d0
	subq.l #1, d0
nextByte
	subq.l #1, d3
	bne.w scanLeaf
	tst.l d0
	beq.w bad
	bsr.w bindMember
	bne.w bad
	move.l d1, d2
mapped
	move.l Row.Id(a4), MapRow.Id(a5)
	move.l Row.Namespace(a4), MapRow.Namespace(a5)
	move.l d1, MapRow.Origin(a5)
	move.l d2, MapRow.Canonical(a5)
	addi.l #MAP_BYTES, InstallState.Map+memory.Block.Used(a6)
	adda.w #MAP_BYTES, a5
	adda.w #ROW_BYTES, a4
	subq.l #1, d7
	bra.w next
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; install

; Same callback ABI as captureResolve. VALUE returns the source metadata
; origin used by hygienic rebinding; MEMBER returns the bare canonical key.
installResolve	.block
	cmpi.l #MEMBER, d2
	bhi.w invalidNamespace
	cmp.l InstallState.Base(a0), d0
	blo.w packageId
	movem.l d2-d3/a0-a1, -(sp)
	move.l d2, d3
	bsr.w mapEntry
	bne.w bad
	cmp.l MapRow.Namespace(a1), d3
	bne.w bad
	move.l MapRow.Origin(a1), d1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d3/a0-a1
	tst.l d0
	rts
packageId
	move.l d0, d1
	moveq #OK, d0
	rts
invalidNamespace
	moveq #0, d1
	moveq #INVALID, d0
	rts
	.bend  ; installResolve

; A0=InstallState,D0=VALUE local ID. D0/CCR=status,D1=canonical value ID;
; others preserved. Definition names use this, rather than the body origin.
canonical	.block
	cmp.l InstallState.Base(a0), d0
	blo.w packageId
	movem.l a0-a1, -(sp)
	bsr.w mapEntry
	bne.w bad
	tst.l MapRow.Namespace(a1)
	bne.w bad
	move.l MapRow.Canonical(a1), d1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
	moveq #0, d1
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
packageId
	move.l d0, d1
	moveq #OK, d0
	rts
	.bend  ; canonical

; A0=InstallState. Release mapping and retire all borrowed capsule pointers.
; All registers preserved; CCR unspecified. Safe after partial installation.
releaseInstall	.block
	movem.l d0/a0-a1, -(sp)
	movea.l a0, a1
	lea InstallState.Map(a1), a0
	jsr memory.release
	movea.l a1, a0
	moveq #INSTALL_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	movem.l (sp)+, d0/a0-a1
	rts
	.bend  ; releaseInstall
	.priv
; A0=InstallState,D0=local ID. D0/CCR=status,A1=map row on success.
; D1-D3 preserved. Public adapters handle package IDs before row lookup.
mapEntry	.block
	movem.l d1-d3, -(sp)
	cmpi.l #65535, d0
	bhi.w bad
	move.l d0, d1
	sub.l InstallState.Base(a0), d1
	bcs.w bad
	mulu.l #MAP_BYTES, d1
	move.l InstallState.Map+memory.Block.Used(a0), d2
	sub.l d1, d2
	bcs.w bad
	cmpi.l #MAP_BYTES, d2
	blo.w bad
	movea.l InstallState.Map+memory.Block.Pointer(a0), a1
	adda.l d1, a1
	cmp.l MapRow.Id(a1), d0
	bne.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d3
	tst.l d0
	rts
	.bend  ; mapEntry

; A6=InstallState. D0/CCR=status; others scratch. Full preflight precedes
; scope mutation, including the offset spans and register-candidate metadata.
validateInstall	.block
	movea.l InstallState.Scope(a6), a0
	cmpa.w #0, a0
	beq.w bad
	tst.w layout.State.Current(a0)
	bne.w bad
	moveq #0, d0
	move.w layout.State.Base(a0), d0
	cmp.l InstallState.Base(a6), d0
	bne.w bad
	move.l InstallState.Map+memory.Block.Pointer(a6), d0
	or.l InstallState.Map+memory.Block.Capacity(a6), d0
	or.l InstallState.Map+memory.Block.Used(a6), d0
	bne.w bad
	move.l InstallState.RowBytes(a6), d7
	cmpi.l #memory.LIMIT, d7
	bhi.w bad
	move.l d7, d0
	moveq #ROW_BYTES, d1
	divu.l d1, d0
	move.l d0, d6
	mulu.l #ROW_BYTES, d0
	cmp.l d7, d0
	bne.w bad
	move.l d6, d0
	add.l InstallState.Base(a6), d0
	cmpi.l #65536, d0
	bhi.w bad
	move.l InstallState.Rows(a6), d0
	tst.l d7
	beq.w rowsReady
	tst.l d0
	beq.w bad
	btst #0, d0
	bne.w bad
rowsReady
	add.l d7, d0
	bcs.w bad
	move.l InstallState.Names(a6), d0
	move.l InstallState.NameBytes(a6), d1
	cmpi.l #memory.LIMIT, d1
	bhi.w bad
	tst.l d1
	beq.w namesReady
	tst.l d0
	beq.w bad
namesReady
	add.l d1, d0
	bcs.w bad
	movea.l InstallState.Rows(a6), a3
	move.l InstallState.Base(a6), d5
next
	tst.l d6
	beq.w good
	cmp.l Row.Id(a3), d5
	bne.w bad
	cmpi.l #MEMBER, Row.Namespace(a3)
	bhi.w bad
	move.l Row.Bytes(a3), d1
	beq.w bad
	cmpi.l #records.NAME_BYTES-1, d1
	bhi.w bad
	cmp.l Row.Leaf(a3), d1
	bls.w bad
	move.l Row.Name(a3), d0
	add.l d1, d0
	bcs.w bad
	cmp.l InstallState.NameBytes(a6), d0
	bhi.w bad
	movea.l InstallState.Names(a6), a1
	adda.l Row.Name(a3), a1
nameByte
	tst.b (a1)+
	beq.w bad
	subq.l #1, d1
	bne.w nameByte
	move.l Row.RegisterFacets(a3), d0
	tst.b d0
	bne.w bad
	swap d0
	andi.l #$ffff, d0
	cmp.l InstallState.Base(a6), d0
	bhi.w bad
	tst.w d0
	bne.w nextRow
	tst.l Row.RegisterFacets(a3)
	bne.w bad
nextRow
	movea.l a3, a0
	bsr.w validateEntities
	bne.w bad
	addq.l #1, d5
	adda.w #ROW_BYTES, a3
	subq.l #1, d6
	bra.w next
good
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; validateInstall

; A0=scope,A1=bare name,D0=bytes. D0/CCR=status,D1=canonical key.
; D2-D3/A0-A1 preserved; line/namespace metadata survives ordinary binding.
bindMember	.block
	movem.l d2-d3/a0-a1/a6, -(sp)
	movea.l a0, a6
	move.w layout.State.Current(a6), -(sp)
	move.w layout.State.BindingNamespace(a6), -(sp)
	move.w layout.State.FirstBound(a6), -(sp)
	move.w layout.State.FirstExplicit(a6), -(sp)
	clr.w layout.State.Current(a6)
	clr.w layout.State.BindingNamespace(a6)
	movea.l a1, a0
	movea.l a6, a1
	jsr scopes.bindOrigin
	move.w (sp)+, layout.State.FirstExplicit(a6)
	move.w (sp)+, layout.State.FirstBound(a6)
	move.w (sp)+, layout.State.BindingNamespace(a6)
	move.w (sp)+, layout.State.Current(a6)
	movem.l (sp)+, d2-d3/a0-a1/a6
	tst.l d0
	rts
	.bend  ; bindMember
	.endsection
	.endmodule
