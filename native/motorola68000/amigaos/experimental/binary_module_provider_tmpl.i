; Demanded template capsules and provider-bound spelling origins. The numeric
; binding table owns identities; only one capsule window survives a read.
	.priv
; A6=State,A5/D7=canonical call spelling,D4=module prefix bytes,D3=provider,
; D6=consumer call identity. D0/CCR=status; other registers preserved.
templateName	.block
	movem.l d1-d7/a0-a5, -(sp)
	move.l d3, State.TemplateProvider(a6)
	clr.l State.TemplateFound(a6)
	tst.l State.Reader+reader.State.TemplateBytes(a6)
	beq.w absent
	moveq #0, d5
binding
	cmp.l State.Reader+reader.State.Templates+templates.Header.BindingCount(a6), d5
	bhs.w selected
	move.l d5, d2
	mulu.l #names.ROW_BYTES, d2
	add.l State.Reader+reader.State.Templates+templates.Header.Bindings(a6), d2
	moveq #names.ROW_BYTES, d3
	lea State.Reader(a6), a0
	jsr reader.templateRead
	bne.w bad
	btst #2, names.Row.Facets+3(a1)
	beq.w next
	tst.l names.Row.TemplateDependency(a1)
	bne.w next
	move.l names.Row.TemplateEntity(a1), State.TemplateSource(a6)
	move.l names.Row.Bytes(a1), d3
	cmp.l d7, d3
	beq.w fullName
	sub.l names.Row.Leaf(a1), d3
	move.l d7, d0
	sub.l d4, d0
	subq.l #1, d0
	cmp.l d0, d3
	bne.w next
	move.l names.Row.Name(a1), d2
	add.l names.Row.Leaf(a1), d2
	add.l State.Reader+reader.State.Templates+templates.Header.Names(a6), d2
	jsr reader.templateRead
	bne.w bad
	movea.l a5, a0
	adda.l d4, a0
	addq.l #1, a0
	move.l d3, d0
	bsr.w equal
	bne.w next
	bra.w matched
fullName
	move.l names.Row.Name(a1), d2
	add.l State.Reader+reader.State.Templates+templates.Header.Names(a6), d2
	jsr reader.templateRead
	bne.w bad
	movea.l a5, a0
	move.l d7, d0
	bsr.w equal
	bne.w next
matched
	move.l State.TemplateSource(a6), d0
	addq.l #1, d0
	tst.l State.TemplateFound(a6)
	beq.w remember
	cmp.l State.TemplateFound(a6), d0
	bne.w bad
remember
	move.l d0, State.TemplateFound(a6)
next
	addq.l #1, d5
	bra.w binding
selected
	move.l State.TemplateFound(a6), d2
	beq.w absent
	subq.l #1, d2
	move.l State.TemplateProvider(a6), d1
	move.l d6, d3
	bsr.w ensureTemplate
	bne.w bad
	bsr.w drainTemplates
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
	.bend  ; templateName

; A6=State,D1=provider,D2=template entity,D3=canonical spelling or zero.
; D0/CCR=status,D1=consumer canonical call identity; others preserved.
ensureTemplate	.block
	movem.l d2-d7/a0-a2, -(sp)
	move.l d1, d4
	move.l d2, d5
	move.l d3, d6
	moveq #TEMPLATE, d3
	movea.l a6, a0
	bsr.w lookup
	tst.l d0
	beq.w done
	tst.l d6
	bne.w allocate
	movea.l State.Scope(a6), a0
	move.l State.FallbackModule(a6), d0
	jsr scope.allocateEntity
	bne.w bad
	move.l d1, d6
allocate
	move.l State.Bindings+memory.Block.Used(a6), d7
	move.l d7, d0
	addi.l #BINDING_BYTES, d0
	lea State.Bindings(a6), a0
	move.l #65536, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l State.Bindings+memory.Block.Pointer(a6), a0
	adda.l d7, a0
	move.l d4, Binding.Provider(a0)
	move.l d5, Binding.Entity(a0)
	move.l d6, Binding.Consumer(a0)
	move.l #TEMPLATE, Binding.Kind(a0)
	clr.l Binding.Wrapper(a0)
	clr.l Binding.Ready(a0)
	addi.l #BINDING_BYTES, State.Bindings+memory.Block.Used(a6)
	move.l d6, d1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d7/a0-a2
	tst.l d0
	rts
	.bend  ; ensureTemplate

; A6=State. Seed queued helper overloads only after the previous capsule's
; remap/seed completes, preserving the template owner's sequential contract.
drainTemplates	.block
	movem.l d1-d7/a0-a5, -(sp)
	moveq #0, d7
binding
	cmp.l State.Bindings+memory.Block.Used(a6), d7
	bhs.w good
	movea.l State.Bindings+memory.Block.Pointer(a6), a4
	adda.l d7, a4
	cmpi.l #TEMPLATE, Binding.Kind(a4)
	bne.w next
	tst.l Binding.Ready(a4)
	bne.w next
	move.l Binding.Provider(a4), State.TemplateProvider(a6)
	move.l Binding.Entity(a4), d6
	move.l Binding.Consumer(a4), d5
	move.l State.FallbackModule(a6), State.TemplateModule(a6)
	movea.l State.Modules+memory.Block.Pointer(a6), a0
	move.l State.Modules+memory.Block.Used(a6), d0
module
	tst.l d0
	beq.w moduleReady
	move.l State.TemplateProvider(a6), d1
	cmp.l Module.Stored(a0), d1
	beq.w moduleFound
	adda.w #MODULE_BYTES, a0
	subi.l #MODULE_BYTES, d0
	bra.w module
moduleFound
	move.l Module.Local(a0), State.TemplateModule(a6)
moduleReady
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	move.l State.TemplateProvider(a6), d1
	jsr reader.select
	bne.w bad
	moveq #0, d4
row
	cmp.l State.Reader+reader.State.Templates+templates.Header.Count(a6), d4
	bhs.w seeded
	move.l d4, d1
	jsr reader.templateRow
	bne.w bad
	cmp.l templates.Row.Entity(a1), d6
	bne.w nextRow
	bsr.w templateRow
	bne.w bad
nextRow
	addq.l #1, d4
	; Closure callbacks can select another provider; restore this directory.
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	move.l State.TemplateProvider(a6), d1
	jsr reader.select
	bne.w bad
	bra.w row
seeded
	movea.l State.Bindings+memory.Block.Pointer(a6), a4
	adda.l d7, a4
	move.l #1, Binding.Ready(a4)
next
	addi.l #BINDING_BYTES, d7
	bra.w binding
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; drainTemplates

; A1=leased selected Row,D5=canonical consumer name,D6=provider entity.
; Build one checked capsule carrier, never the provider's complete interface.
; D0/CCR=status; other registers preserved.
templateRow	.block
	movem.l d1-d7/a0-a5, -(sp)
	suba.w #templates.ROW_BYTES, sp
	movea.l sp, a0
	moveq #templates.ROW_BYTES/4-1, d0
saveRow
	move.l (a1)+, (a0)+
	dbra d0, saveRow
	move.l templates.Row.Bytes(sp), d2
	add.l templates.Row.StatementBytes(sp), d2
	bcs.w bad
	addi.l #templates.HEADER_BYTES+templates.ROW_BYTES, d2
	bcs.w bad
	cmpi.l #reader.WINDOW_BYTES, d2
	bhi.w bad
	lea State.TemplateImage(a6), a0
	move.l d2, d0
	jsr memory.reserveExact
	bne.w bad
	movea.l State.TemplateImage+memory.Block.Pointer(a6), a4
	movea.l a4, a0
	move.l d2, d0
clear
	clr.b (a0)+
	subq.l #1, d0
	bne.w clear
	move.l #templates.MAGIC, templates.Header.Magic(a4)
	move.l d2, templates.Header.Bytes(a4)
	move.l State.Reader+reader.State.Templates+templates.Header.Base(a6), templates.Header.Base(a4)
	move.l #templates.HEADER_BYTES, templates.Header.Bindings(a4)
	move.l #templates.HEADER_BYTES, templates.Header.Names(a4)
	move.l #templates.HEADER_BYTES, templates.Header.Rows(a4)
	move.l #1, templates.Header.Count(a4)
	move.l #templates.HEADER_BYTES+templates.ROW_BYTES, templates.Header.Data(a4)
	subi.l #templates.HEADER_BYTES+templates.ROW_BYTES, d2
	move.l d2, templates.Header.DataBytes(a4)
	lea templates.HEADER_BYTES(a4), a5
	movea.l sp, a1
	movea.l a5, a0
	moveq #templates.ROW_BYTES/4-1, d0
copyRow
	move.l (a1)+, (a0)+
	dbra d0, copyRow
	clr.l templates.Row.Offset(a5)
	tst.l templates.Row.StatementBytes(a5)
	beq.w noStatement
	move.l templates.Row.Bytes(a5), templates.Row.Statement(a5)
noStatement
	move.l templates.Row.Offset(sp), d2
	add.l State.Reader+reader.State.Templates+templates.Header.Data(a6), d2
	move.l templates.Row.Bytes(sp), d3
	lea State.Reader(a6), a0
	jsr reader.templateRead
	bne.w bad
	lea templates.HEADER_BYTES+templates.ROW_BYTES(a4), a0
	move.l d3, d0
body
	move.b (a1)+, (a0)+
	subq.l #1, d0
	bne.w body
	tst.l templates.Row.StatementBytes(sp)
	beq.w demand
	move.l templates.Row.Statement(sp), d2
	add.l State.Reader+reader.State.Templates+templates.Header.Data(a6), d2
	move.l templates.Row.StatementBytes(sp), d3
	lea State.Reader(a6), a0
	jsr reader.templateRead
	bne.w bad
	lea templates.HEADER_BYTES+templates.ROW_BYTES(a4), a0
	adda.l templates.Row.Bytes(sp), a0
	move.l d3, d0
statement
	move.b (a1)+, (a0)+
	subq.l #1, d0
	bne.w statement
demand
	move.l State.Frontend(a6), State.TemplateFrame+templates.DemandFrame.Frontend(a6)
	move.l State.TemplateModule(a6), State.TemplateFrame+templates.DemandFrame.Module(a6)
	move.l a4, State.TemplateFrame+templates.DemandFrame.Image(a6)
	move.l templates.Header.Bytes(a4), State.TemplateFrame+templates.DemandFrame.Bytes(a6)
	move.l a5, State.TemplateFrame+templates.DemandFrame.Row(a6)
	move.l #templateResolve, State.TemplateFrame+templates.DemandFrame.Resolve(a6)
	move.l a6, State.TemplateFrame+templates.DemandFrame.Context(a6)
	; D5 was preserved by all bounded reads and still names this overload head.
	move.l d5, State.TemplateFrame+templates.DemandFrame.Name(a6)
	lea State.TemplateFrame(a6), a0
	jsr templates.demand
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #templates.ROW_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; templateRow

; Resolve a stored dependency ordinal in the current template declaration
; context. A6=State,D1=zero self or directory ordinal+1 -> D1=stored module.
templateDependency	.block
	tst.l d1
	beq.w self
	subq.l #1, d1
	lea State.Reader(a6), a0
	jsr reader.dependencyName
	bne.w done
	move.l d1, d2
	movea.l State.Registry(a6), a0
	jsr registry.findObject
done
	tst.l d0
	rts
self
	move.l State.TemplateProvider(a6), d1
	moveq #OK, d0
	rts
	.bend  ; templateDependency

; Closed-template resolver. A0=State,D0=stored binding ID,D2=namespace.
; Return a consumer metadata origin (or bare member key); declaration values
; and helper templates are resolved under the provider dependency directory.
templateResolve	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #names.ROW_BYTES+12, sp
	movea.l a0, a6
	move.l d0, d7
	move.l d2, d6
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	move.l State.TemplateProvider(a6), d1
	jsr reader.select
	bne.w bad
	move.l State.Reader+reader.State.Templates+templates.Header.Base(a6), d0
	cmp.l d0, d7
	blo.w package
	move.l State.TemplateProvider(a6), d1
	move.l d7, d2
	move.l d6, d3
	addi.l #ORIGIN_VALUE, d3
	movea.l a6, a0
	bsr.w lookup
	tst.l d0
	beq.w done
	move.l d7, d2
	sub.l State.Reader+reader.State.Templates+templates.Header.Base(a6), d2
	cmp.l State.Reader+reader.State.Templates+templates.Header.BindingCount(a6), d2
	bhs.w bad
	mulu.l #names.ROW_BYTES, d2
	add.l State.Reader+reader.State.Templates+templates.Header.Bindings(a6), d2
	moveq #names.ROW_BYTES, d3
	lea State.Reader(a6), a0
	jsr reader.templateRead
	bne.w bad
	movea.l sp, a0
	moveq #names.ROW_BYTES/4-1, d0
row
	move.l (a1)+, (a0)+
	dbra d0, row
	cmp.l names.Row.Namespace(sp), d6
	bne.w bad
	move.l names.Row.Name(sp), d2
	add.l State.Reader+reader.State.Templates+templates.Header.Names(a6), d2
	move.l names.Row.Bytes(sp), d3
	lea State.Reader(a6), a0
	jsr reader.templateRead
	bne.w bad
	tst.l d6
	bne.w member
	movea.l State.Scope(a6), a0
	move.l names.Row.Bytes(sp), d0
	move.l names.Row.Leaf(sp), d1
	move.l names.Row.RegisterFacets(sp), d2
	jsr scopes.bindTemplateOrigin
	bne.w bad
	move.l d1, names.ROW_BYTES(sp)
	move.l d2, names.ROW_BYTES+4(sp)
	btst #0, names.Row.Facets+3(sp)
	beq.w template
	move.l names.Row.ValueDependency(sp), d1
	bsr.w templateDependency
	bne.w bad
	move.l names.Row.ValueEntity(sp), d2
	move.l names.ROW_BYTES+4(sp), d3
	movea.l a6, a0
	bsr.w acquire
	tst.l d0
	bne.w done
	move.l d1, names.ROW_BYTES+8(sp)
	; Preserve the spelling origin's independent template identity while its
	; value target follows canonical imported aliases and nominal forwarding.
	suba.w #records.ENTRY_BYTES, sp
	move.l names.ROW_BYTES+records.ENTRY_BYTES(sp), d0
	movea.l State.Scope(a6), a2
	sub.w layout.State.Base(a2), d0
	andi.l #$ffff, d0
	move.l d0, d5
	lea layout.LEXICAL_STATE(a2), a0
	movea.l sp, a1
	jsr lexical.readEntry
	bne.w originBad
	move.l names.ROW_BYTES+8+records.ENTRY_BYTES(sp), d0
	move.w d0, records.Entry.Target(sp)
	move.l d5, d0
	jsr lexical.writeEntry
originBad
	adda.w #records.ENTRY_BYTES, sp
	tst.l d0
	bne.w bad
template
	btst #1, names.Row.Facets+3(sp)
	beq.w remember
	; Value hydration may have selected a dependency; restore this provider
	; before interpreting its template dependency ordinal.
	lea State.Reader(a6), a0
	movea.l State.Registry(a6), a1
	move.l State.TemplateProvider(a6), d1
	jsr reader.select
	bne.w bad
	move.l names.Row.TemplateDependency(sp), d1
	bsr.w templateDependency
	bne.w bad
	move.l names.Row.TemplateEntity(sp), d2
	move.l names.ROW_BYTES+4(sp), d3
	bsr.w ensureTemplate
	bne.w bad
	bra.w remember
member
	move.l names.Row.Bytes(sp), d0
	bsr.w bindMember
	tst.l d0
	bne.w bad
	move.l d1, names.ROW_BYTES(sp)
	move.l d1, names.ROW_BYTES+4(sp)
remember
	move.l State.Bindings+memory.Block.Used(a6), d5
	move.l d5, d0
	addi.l #BINDING_BYTES, d0
	lea State.Bindings(a6), a0
	move.l #65536, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l State.Bindings+memory.Block.Pointer(a6), a0
	adda.l d5, a0
	move.l State.TemplateProvider(a6), Binding.Provider(a0)
	move.l d7, Binding.Entity(a0)
	move.l names.ROW_BYTES(sp), Binding.Consumer(a0)
	move.l d6, d0
	addi.l #ORIGIN_VALUE, d0
	move.l d0, Binding.Kind(a0)
	move.l names.ROW_BYTES+4(sp), Binding.Wrapper(a0)
	move.l #1, Binding.Ready(a0)
	addi.l #BINDING_BYTES, State.Bindings+memory.Block.Used(a6)
	move.l names.ROW_BYTES(sp), d1
	moveq #OK, d0
	bra.w done
package
	move.l d7, d1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #names.ROW_BYTES+12, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateResolve
