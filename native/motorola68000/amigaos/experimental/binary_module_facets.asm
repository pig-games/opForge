; Transient dependency projection of independent public preparation facets.
	.module experimental.amigaos.binary_module_facets
	.cpu 68020
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_lexical_store as lexical
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_templates as templates
	.use experimental.amigaos.binary_statements as statements
	.use experimental.amigaos.binary_statement_session as session
	.use experimental.amigaos.binary_memory as memory
	.pub
Frame	.struct
SourceFrontend	.long ?
DestinationFrontend	.long ?
SourceModule	.long ?  ; lexical module index plus one
DestinationModule	.long ?
	.endstruct
FRAME_BYTES = Frame.DestinationModule+4
OK = 0
INVALID = 1
	.priv
Local	.struct
SourceScope	.long ?
SourceTemplates	.long ?
SourceStatements	.long ?
DestinationScratch	.long ?
Kind	.long ?
NameId	.long ?
Current	.word ?
Namespace	.word ?
FirstBound	.word ?
FirstExplicit	.word ?
SourceEntry	.res records.ENTRY_BYTES
DestinationEntry	.res records.ENTRY_BYTES
Name	.res records.NAME_BYTES
Seed	.res statements.SEED_FRAME_BYTES
	.endstruct
LOCAL_BYTES = Local.Seed+statements.SEED_FRAME_BYTES
	.section code, kind=code
	.pub
; A0=Frame, retained and private frontend owners must be distinct and live.
; D0/CCR=status; other registers preserved. Imports PUBLIC macro/segment and
; statement facets independently of value exports, preserving same-name values.
; Source arrays/plans stay immutable during this call. Only unavailable bodies
; and copied closed statement metadata enter the destination; no source pointer
; is retained. Failure may publish partial facets: discard the private session.
; Restores destination binding context on success and failure.
import	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.l #LOCAL_BYTES, sp
	movea.l sp, a4
	movea.l a0, a5
	move.l Frame.SourceModule(a5), d0
	beq.w invalidFrame
	cmpi.l #65535, d0
	bhi.w invalidFrame
	move.l Frame.DestinationModule(a5), d0
	beq.w invalidFrame
	cmpi.l #65535, d0
	bhi.w invalidFrame
	movea.l Frame.SourceFrontend(a5), a0
	movea.l frontend.Frame.Scratch(a0), a0
	movea.l Frame.DestinationFrontend(a5), a1
	movea.l frontend.Frame.Scratch(a1), a1
	cmpa.l a0, a1
	beq.w invalidFrame
	move.l a1, Local.DestinationScratch(a4)
	movea.l a1, a6
	adda.l #frontend.SCOPE_STATE, a6
	movea.l a0, a2
	adda.l #frontend.TEMPLATE_STATE, a2
	move.l a2, Local.SourceTemplates(a4)
	movea.l a0, a2
	adda.l #frontend.STATEMENT_STATE+session.State.Definitions, a2
	move.l a2, Local.SourceStatements(a4)
	adda.l #frontend.SCOPE_STATE, a0
	move.l a0, Local.SourceScope(a4)
	move.w layout.State.Current(a6), Local.Current(a4)
	move.w layout.State.BindingNamespace(a6), Local.Namespace(a4)
	move.w layout.State.FirstBound(a6), Local.FirstBound(a4)
	move.w layout.State.FirstExplicit(a6), Local.FirstExplicit(a4)
	clr.w layout.State.Current(a6)
	clr.w layout.State.BindingNamespace(a6)
	movea.l Local.SourceTemplates(a4), a2
	moveq #0, d6
	move.w templates.State.Count(a2), d6
	move.l d6, d0
	mulu.l #templates.DEF_BYTES, d0
	cmp.l templates.DEFS+memory.Block.Used(a2), d0
	bhi.w bad
	movea.l templates.DEFS+memory.Block.Pointer(a2), a3
nextTemplate
	tst.l d6
	beq.w statementBegin
	moveq #0, d0
	move.w templates.Def.Kind(a3), d0
	cmpi.l #templates.KIND_MACRO, d0
	bhi.w advanceTemplate
	move.l d0, Local.Kind(a4)
	moveq #0, d0
	move.w templates.Def.Name(a3), d0
	bsr.w sourceEntry
	bne.w bad
	move.l Frame.SourceModule(a5), d0
	cmp.w Local.SourceEntry+records.Entry.TemplateModule(a4), d0
	bne.w advanceTemplate
	move.w Local.SourceEntry+records.Entry.TemplateFlags(a4), d0
	andi.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, d0
	cmpi.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, d0
	bne.w advanceTemplate
	bsr.w bindName
	bne.w bad
	bsr.w seedBody
	bne.w bad
advanceTemplate
	adda.l #templates.DEF_BYTES, a3
	subq.l #1, d6
	bra.w nextTemplate
statementBegin
	movea.l Local.SourceStatements(a4), a2
	moveq #0, d6
	move.w statements.State.Count(a2), d6
	move.l d6, d0
	mulu.l #statements.ROW_BYTES, d0
	cmp.l statements.ROWS+memory.Block.Used(a2), d0
	bhi.w bad
	movea.l statements.ROWS+memory.Block.Pointer(a2), a3
nextStatement
	tst.l d6
	beq.w good
	move.l Frame.SourceModule(a5), d0
	cmp.w statements.Row.Module(a3), d0
	bne.w advanceStatement
	cmpi.w #1, statements.Row.Visibility(a3)
	bne.w advanceStatement
	moveq #0, d0
	move.w statements.Row.Name(a3), d0
	bsr.w sourceEntry
	bne.w bad
	move.l Frame.SourceModule(a5), d0
	cmp.w Local.SourceEntry+records.Entry.StatementModule(a4), d0
	bne.w bad
	move.w Local.SourceEntry+records.Entry.TemplateFlags(a4), d0
	andi.w #records.STATEMENT_DECLARED+records.STATEMENT_PUBLIC, d0
	cmpi.w #records.STATEMENT_DECLARED+records.STATEMENT_PUBLIC, d0
	bne.w bad
	move.l #templates.KIND_EXTERNAL, Local.Kind(a4)
	bsr.w bindName
	bne.w bad
	bsr.w seedBody
	bne.w bad
	move.l d1, Local.Seed+statements.SeedFrame.Template(a4)
	movea.l Local.DestinationScratch(a4), a0
	adda.l #frontend.STATEMENT_STATE+session.State.Definitions, a0
	move.l a0, Local.Seed+statements.SeedFrame.State(a4)
	move.l a3, Local.Seed+statements.SeedFrame.Row(a4)
	move.l Local.NameId(a4), Local.Seed+statements.SeedFrame.Name(a4)
	move.l Frame.DestinationModule(a5), Local.Seed+statements.SeedFrame.Scope(a4)
	move.l Frame.DestinationModule(a5), Local.Seed+statements.SeedFrame.Module(a4)
	movea.l Local.SourceStatements(a4), a2
	move.l statements.Row.PlanOffset(a3), d0
	move.l statements.Row.PlanBytes(a3), d1
	add.l d1, d0
	bcs.w bad
	cmp.l statements.PLANS+memory.Block.Used(a2), d0
	bhi.w bad
	movea.l statements.PLANS+memory.Block.Pointer(a2), a0
	adda.l statements.Row.PlanOffset(a3), a0
	move.l a0, Local.Seed+statements.SeedFrame.Plan(a4)
	move.l d1, Local.Seed+statements.SeedFrame.PlanBytes(a4)
	move.l statements.Row.KeywordOffset(a3), d0
	move.l statements.Row.KeywordBytes(a3), d1
	add.l d1, d0
	bcs.w bad
	cmp.l statements.KEYWORDS+memory.Block.Used(a2), d0
	bhi.w bad
	movea.l statements.KEYWORDS+memory.Block.Pointer(a2), a0
	adda.l statements.Row.KeywordOffset(a3), a0
	move.l a0, Local.Seed+statements.SeedFrame.Keyword(a4)
	move.l d1, Local.Seed+statements.SeedFrame.KeywordBytes(a4)
	lea Local.Seed(a4), a0
	jsr statements.seed
	bne.w bad
advanceStatement
	adda.l #statements.ROW_BYTES, a3
	subq.l #1, d6
	bra.w nextStatement
good
	moveq #OK, d0
	bra.w restore
bad
	moveq #INVALID, d0
restore
	move.w Local.Current(a4), layout.State.Current(a6)
	move.w Local.Namespace(a4), layout.State.BindingNamespace(a6)
	move.w Local.FirstBound(a4), layout.State.FirstBound(a6)
	move.w Local.FirstExplicit(a4), layout.State.FirstExplicit(a6)
	bra.w done
invalidFrame
	moveq #INVALID, d0
done
	adda.l #LOCAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; import
	.priv
; D0=source canonical ID. Copy the checked lexical entry into owned scratch.
sourceEntry	.block
	movea.l Local.SourceScope(a4), a0
	sub.w layout.State.Base(a0), d0
	bcs.w bad
	andi.l #65535, d0
	lea layout.LEXICAL_STATE(a0), a0
	lea Local.SourceEntry(a4), a1
	jsr lexical.readEntry
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; sourceEntry
; Bind a copied complete canonical name at root, preserving existing values.
bindName	.block
	movea.l Local.SourceScope(a4), a0
	adda.l #layout.LEXICAL_STATE, a0
	lea Local.Name(a4), a1
	move.l Local.SourceEntry+records.Entry.Name(a4), d0
	moveq #0, d1
	move.w Local.SourceEntry+records.Entry.Length(a4), d1
	jsr lexical.copyName
	bne.w done
	move.l d1, d0
	lea Local.Name(a4), a0
	movea.l a6, a1
	jsr scopes.bindOrigin
	bne.w done
	move.l d1, Local.NameId(a4)
done
	tst.l d0
	rts
	.bend  ; bindName
; Publish only the selected facet, then create an unavailable body carrier.
; D1=one-based carrier on success. Existing value flags/payload stay untouched.
seedBody	.block
	move.l Local.NameId(a4), d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	andi.l #65535, d0
	lea layout.LEXICAL_STATE(a6), a0
	lea Local.DestinationEntry(a4), a1
	jsr lexical.readEntry
	bne.w bad
	move.l Frame.DestinationModule(a5), d1
	cmpi.l #templates.KIND_EXTERNAL, Local.Kind(a4)
	beq.w statement
	move.w records.Entry.TemplateFlags(a1), d0
	andi.w #records.TEMPLATE_DECLARED, d0
	beq.w templateOwner
	move.w records.Entry.TemplateFlags(a1), d0
	andi.w #records.TEMPLATE_PUBLIC, d0
	beq.w bad
	cmp.w records.Entry.TemplateModule(a1), d1
	bne.w bad
templateOwner
	move.w d1, records.Entry.TemplateModule(a1)
	ori.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, records.Entry.TemplateFlags(a1)
	bra.w publish
statement
	btst #records.STATEMENT_DECLARED_BIT, records.Entry.TemplateFlags+1(a1)
	beq.w statementOwner
	btst #records.STATEMENT_PUBLIC_BIT, records.Entry.TemplateFlags+1(a1)
	beq.w bad
	cmp.w records.Entry.StatementModule(a1), d1
	bne.w bad
statementOwner
	move.w d1, records.Entry.StatementModule(a1)
	ori.w #records.STATEMENT_DECLARED+records.STATEMENT_PUBLIC, records.Entry.TemplateFlags(a1)
publish
	move.l Local.NameId(a4), d0
	sub.w layout.State.Base(a6), d0
	andi.l #65535, d0
	jsr lexical.writeEntry
	bne.w bad
	movea.l Local.DestinationScratch(a4), a0
	adda.l #frontend.TEMPLATE_STATE, a0
	movea.l a6, a1
	move.l Local.NameId(a4), d0
	move.l Local.Kind(a4), d1
	jsr templates.seedUnavailable
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; seedBody
	.endsection
	.endmodule
