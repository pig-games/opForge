	.pub
; A0=InstallFrame at a source boundary. D0/CCR=status; others preserved.
; Bind owned name origins and copy closed bodies into the ordinary template
; and spelling owners. No capsule pointer survives. Failure requires discarding
; the destination preparation session: definitions cannot be rolled back.
install	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l Active
	bne.w busy
	move.l #1, Active
	move.l a0, Caller
	movea.l a0, a6
	tst.l InstallFrame.Module(a6)
	beq.w bad
	cmpi.l #65535, InstallFrame.Module(a6)
	bhi.w bad
	movea.l InstallFrame.Image(a6), a0
	move.l InstallFrame.Bytes(a6), d0
	bsr.w validate
	bne.w bad
	movea.l InstallFrame.Image(a6), a5
	movea.l InstallFrame.Frontend(a6), a0
	movea.l frontend.Frame.Scratch(a0), a4
	move.l a4, d0
	beq.w bad
	move.l a4, d0
	addi.l #frontend.TEMPLATE_STATE, d0
	move.l d0, Seed+templates.SeedFrame.State
	adda.l #frontend.SCOPE_STATE, a4
	move.l a4, Seed+templates.SeedFrame.Scope
	move.l a4, Imported+names.InstallState.Scope
	move.l Header.Base(a5), Imported+names.InstallState.Base
	move.l a5, d0
	add.l Header.Bindings(a5), d0
	move.l d0, Imported+names.InstallState.Rows
	move.l Header.BindingCount(a5), d0
	mulu.l #names.ROW_BYTES, d0
	move.l d0, Imported+names.InstallState.RowBytes
	move.l a5, d0
	add.l Header.Names(a5), d0
	move.l d0, Imported+names.InstallState.Names
	move.l Header.NameBytes(a5), Imported+names.InstallState.NameBytes
	lea Imported, a0
	jsr names.install
	bne.w bad
	move.l Header.Count(a5), d7
	movea.l a5, a3
	adda.l Header.Rows(a5), a3
row
	tst.l d7
	beq.w good
	lea Temporary, a0
	movea.l a5, a1
	adda.l Header.Data(a5), a1
	adda.l Row.Offset(a3), a1
	move.l Row.Bytes(a3), d0
	bsr.w append
	bne.w bad
	move.l Temporary+memory.Block.Pointer, Remap+templates.RemapFrame.Image
	move.l Temporary+memory.Block.Used, Remap+templates.RemapFrame.Bytes
	move.l #names.installResolve, Remap+templates.RemapFrame.Resolve
	move.l #Imported, Remap+templates.RemapFrame.Context
	lea Remap, a0
	jsr templates.remapClosed
	bne.w bad
	lea Imported, a0
	move.l Row.Name(a3), d0
	jsr names.canonical
	bne.w bad
	move.l d1, d5
	movea.l a4, a0
	move.l d5, d0
	move.l InstallFrame.Module(a6), d1
	movea.l Temporary+memory.Block.Pointer, a1
	adda.l templates.ClosedHeader.Definition(a1), a1
	moveq #0, d2
	move.w templates.Def.Kind(a1), d2
	bsr.w declareFacet
	bne.w bad
	move.l Temporary+memory.Block.Pointer, Seed+templates.SeedFrame.Image
	move.l Temporary+memory.Block.Used, Seed+templates.SeedFrame.Bytes
	move.l d5, Seed+templates.SeedFrame.Name
	lea Seed, a0
	jsr templates.seedClosed
	bne.w bad
	tst.l Row.StatementBytes(a3)
	beq.w seeded
	move.l d1, StatementSeed+statementImage.InstallFrame.Template
	movea.l InstallFrame.Frontend(a6), a0
	movea.l frontend.Frame.Scratch(a0), a0
	adda.l #frontend.STATEMENT_STATE+statementSession.State.Definitions, a0
	move.l a0, StatementSeed+statementImage.InstallFrame.State
	move.l a5, d0
	add.l Header.Data(a5), d0
	add.l Row.Statement(a3), d0
	move.l d0, StatementSeed+statementImage.InstallFrame.Image
	move.l Row.StatementBytes(a3), StatementSeed+statementImage.InstallFrame.Bytes
	move.l d5, StatementSeed+statementImage.InstallFrame.Name
	move.l InstallFrame.Module(a6), StatementSeed+statementImage.InstallFrame.Module
	lea StatementSeed, a0
	jsr statementImage.install
	bne.w bad
seeded
	lea Temporary, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	adda.w #ROW_BYTES, a3
	subq.l #1, d7
	bra.w row
good
	movea.l a4, a0
	jsr scopes.count
	movea.l InstallFrame.Frontend(a6), a0
	move.l d0, frontend.Frame.NameCount(a0)
	moveq #OK, d7
	bra.w cleanup
bad
	moveq #INVALID, d7
cleanup
	bsr.w release
	move.l d7, d0
	bra.w done
busy
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; install
	.priv
; A0=scope,D0=canonical name,D1=module,D2=template kind. Preserve value facets.
; Public template ownership must be absent or already belong to this module.
; D0/CCR=status; others preserved. No source spelling is looked up here.
declareFacet	.block
	movem.l d1-d4/a0-a2, -(sp)
	move.l d1, d3
	move.l d2, d4
	sub.w scope.State.Base(a0), d0
	bcs.w bad
	andi.l #65535, d0
	cmp.w scope.State.Count(a0), d0
	bhs.w bad
	move.l d0, d2
	lea scope.LEXICAL_STATE(a0), a0
	lea Entry, a1
	jsr lexical.readEntry
	bne.w bad
	cmpi.l #templates.KIND_EXTERNAL, d4
	beq.w statement
	move.w records.Entry.TemplateFlags(a1), d0
	andi.w #records.TEMPLATE_DECLARED, d0
	beq.w fresh
	move.w records.Entry.TemplateFlags(a1), d0
	andi.w #records.TEMPLATE_PUBLIC, d0
	beq.w bad
	cmp.w records.Entry.TemplateModule(a1), d3
	bne.w bad
fresh
	ori.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, records.Entry.TemplateFlags(a1)
	move.w d3, records.Entry.TemplateModule(a1)
	bra.w publish
statement
	btst #records.STATEMENT_DECLARED_BIT, records.Entry.TemplateFlags+1(a1)
	beq.w freshStatement
	btst #records.STATEMENT_PUBLIC_BIT, records.Entry.TemplateFlags+1(a1)
	beq.w bad
	cmp.w records.Entry.StatementModule(a1), d3
	bne.w bad
freshStatement
	ori.w #records.STATEMENT_DECLARED+records.STATEMENT_PUBLIC, records.Entry.TemplateFlags(a1)
	move.w d3, records.Entry.StatementModule(a1)
publish
	move.l d2, d0
	jsr lexical.writeEntry
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d4/a0-a2
	tst.l d0
	rts
	.bend  ; declareFacet
	.pub
CopyLocal	.struct
Capsule	.res memory.Block.Used+4
Capture	.res CAPTURE_BYTES
Install	.res INSTALL_BYTES
	.endstruct
COPY_LOCAL_BYTES = CopyLocal.Install+INSTALL_BYTES
; A0=CopyFrame. D0/CCR=status; others preserved. Project one retained module's
; public definitions into a private consumer through the same closed boundary
; used when the original producer retires. Temporary capsule always releases.
copyModule	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #COPY_LOCAL_BYTES, sp
	movea.l sp, a5
	movea.l a0, a6
	clr.l CopyLocal.Capsule+memory.Block.Pointer(a5)
	clr.l CopyLocal.Capsule+memory.Block.Capacity(a5)
	clr.l CopyLocal.Capsule+memory.Block.Used(a5)
	move.l CopyFrame.SourceFrontend(a6), CopyLocal.Capture+CaptureFrame.Frontend(a5)
	move.l CopyFrame.SourceModule(a6), CopyLocal.Capture+CaptureFrame.Module(a5)
	lea CopyLocal.Capsule(a5), a0
	move.l a0, CopyLocal.Capture+CaptureFrame.Destination(a5)
	lea CopyLocal.Capture(a5), a0
	bsr.w capture
	bne.w failed
	move.l CopyFrame.DestinationFrontend(a6), CopyLocal.Install+InstallFrame.Frontend(a5)
	move.l CopyFrame.DestinationModule(a6), CopyLocal.Install+InstallFrame.Module(a5)
	move.l CopyLocal.Capsule+memory.Block.Pointer(a5), CopyLocal.Install+InstallFrame.Image(a5)
	move.l CopyLocal.Capsule+memory.Block.Used(a5), CopyLocal.Install+InstallFrame.Bytes(a5)
	lea CopyLocal.Install(a5), a0
	bsr.w install
failed
	move.l d0, d7
	lea CopyLocal.Capsule(a5), a0
	jsr memory.release
	move.l d7, d0
	adda.w #COPY_LOCAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; copyModule
	.priv
