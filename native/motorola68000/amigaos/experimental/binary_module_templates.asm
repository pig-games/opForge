; Closed public template transfer between sequential preparation owners.
; @opforge-owner: experimental.amigaos.binary_module_templates
	.module experimental.amigaos.binary_module_templates
	.cpu 68020
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_templates as templates
	.use experimental.amigaos.binary_module_tmpl_names as names
	.use experimental.amigaos.binary_scope_layout as scope
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_lexical_store as lexical
	.use experimental.amigaos.binary_statements as statements
	.use experimental.amigaos.binary_module_stmt_image as statementImage
	.use experimental.amigaos.binary_statement_session as statementSession
	.use experimental.amigaos.binary_memory as memory
	.pub
Header	.struct
Magic	.long ?
Bytes	.long ?
Base	.long ?
BindingCount	.long ?
Bindings	.long ?
Names	.long ?
NameBytes	.long ?
Count	.long ?
Rows	.long ?
Data	.long ?
DataBytes	.long ?
	.endstruct
HEADER_BYTES = Header.DataBytes+4
MAGIC = $4f46544d; OFTM: owned current-build preparation data
Row	.struct
Offset	.long ?  ; Data-relative closed template capsule
Bytes	.long ?
Name	.long ?  ; capsule-local name ID, not a runtime handle
Statement	.long ?  ; optional Data-relative closed statement metadata
StatementBytes	.long ?
	.endstruct
ROW_BYTES = Row.StatementBytes+4
CaptureFrame	.struct
Frontend	.long ?
Module	.long ?
Destination	.long ?  ; empty owned block; transfers only on success
	.endstruct
CAPTURE_BYTES = CaptureFrame.Destination+4
InstallFrame	.struct
Frontend	.long ?
Module	.long ?
Image	.long ?  ; immutable borrowed module capsule
Bytes	.long ?
	.endstruct
INSTALL_BYTES = InstallFrame.Bytes+4
CopyFrame	.struct
SourceFrontend	.long ?
SourceModule	.long ?
DestinationFrontend	.long ?
DestinationModule	.long ?
	.endstruct
COPY_BYTES = CopyFrame.DestinationModule+4
OK = 0
INVALID = 1
	.priv
	.section bss, kind=bss
Active	.res long, 1
Caller	.res long, 1
SourceScope	.res long, 1
SourceTemplates	.res long, 1
SourceStatements	.res long, 1
Entry	.res byte, records.ENTRY_BYTES
NameState	.res byte, names.STATE_BYTES
Imported	.res byte, names.INSTALL_BYTES
Table	.res byte, memory.Block.Used+4
Data	.res byte, memory.Block.Used+4
Temporary	.res byte, memory.Block.Used+4
Image	.res byte, memory.Block.Used+4
Export	.res byte, templates.EXPORT_FRAME_BYTES
Remap	.res byte, templates.REMAP_FRAME_BYTES
Seed	.res byte, templates.SEED_FRAME_BYTES
Statement	.res byte, memory.Block.Used+4
StatementRequest	.res byte, statementImage.CAPTURE_FRAME_BYTES
StatementSeed	.res byte, statementImage.INSTALL_FRAME_BYTES
	.endsection
BORROW_BYTES = templates.EXPORT_FRAME_BYTES+templates.REMAP_FRAME_BYTES+templates.SEED_FRAME_BYTES+(memory.Block.Used+4)+statementImage.CAPTURE_FRAME_BYTES+statementImage.INSTALL_FRAME_BYTES
	.section code, kind=code
	.pub
; A0=CaptureFrame. D0/CCR=status; other registers preserved. Capture public
; macros, segments and statements. Packed bodies, defaults, spelling plans,
; closed signatures and referenced name facets are copied. No frontend pointer
; enters the result; ordinary parsers and expansion still own all behavior.
capture	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l Active
	bne.w busy
	move.l #1, Active
	move.l a0, Caller
	movea.l a0, a6
	tst.l CaptureFrame.Module(a6)
	beq.w bad
	cmpi.l #65535, CaptureFrame.Module(a6)
	bhi.w bad
	movea.l CaptureFrame.Destination(a6), a0
	move.l a0, d0
	beq.w bad
	move.l memory.Block.Pointer(a0), d0
	or.l memory.Block.Capacity(a0), d0
	or.l memory.Block.Used(a0), d0
	bne.w bad
	movea.l CaptureFrame.Frontend(a6), a0
	movea.l frontend.Frame.Scratch(a0), a0
	move.l a0, d0
	beq.w bad
	move.l a0, d1
	addi.l #frontend.TEMPLATE_STATE, d1
	move.l d1, SourceTemplates
	adda.l #frontend.SCOPE_STATE, a0
	move.l a0, SourceScope
	movea.l a0, a1
	lea NameState, a0
	jsr names.begin
	bne.w bad
definitions
	movea.l SourceTemplates, a5
	moveq #0, d7
	move.w templates.State.Count(a5), d7
	move.l d7, d0
	mulu.l #templates.DEF_BYTES, d0
	cmp.l templates.DEFS+memory.Block.Used(a5), d0
	bhi.w bad
	moveq #0, d6
definition
	cmp.l d7, d6
	bhs.w statementDefinitions
	move.l d6, d0
	mulu.l #templates.DEF_BYTES, d0
	movea.l templates.DEFS+memory.Block.Pointer(a5), a4
	adda.l d0, a4
	cmpi.w #templates.KIND_MACRO, templates.Def.Kind(a4)
	bhi.w nextDefinition
	moveq #0, d0
	move.w templates.Def.Name(a4), d0
	movea.l SourceScope, a0
	sub.w scope.State.Base(a0), d0
	bcs.w bad
	andi.l #65535, d0
	lea scope.LEXICAL_STATE(a0), a0
	lea Entry, a1
	jsr lexical.readEntry
	bne.w bad
	move.l CaptureFrame.Module(a6), d0
	cmp.w records.Entry.TemplateModule(a1), d0
	bne.w nextDefinition
	move.w records.Entry.TemplateFlags(a1), d0
	andi.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, d0
	cmpi.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, d0
	bne.w nextDefinition
	move.l d6, d0
	addq.l #1, d0
	moveq #0, d1
	bsr.w captureOne
	bne.w bad
nextDefinition
	addq.l #1, d6
	bra.w definition
statementDefinitions
	movea.l CaptureFrame.Frontend(a6), a0
	movea.l frontend.Frame.Scratch(a0), a5
	adda.l #frontend.STATEMENT_STATE+statementSession.State.Definitions, a5
	move.l a5, SourceStatements
	moveq #0, d7
	move.w statements.State.Count(a5), d7
	move.l d7, d0
	mulu.l #statements.ROW_BYTES, d0
	cmp.l statements.ROWS+memory.Block.Used(a5), d0
	bhi.w bad
	movea.l statements.ROWS+memory.Block.Pointer(a5), a4
statementLoop
	tst.l d7
	beq.w serialize
	move.l CaptureFrame.Module(a6), d0
	cmp.w statements.Row.Module(a4), d0
	bne.w nextStatement
	cmpi.w #1, statements.Row.Visibility(a4)
	bne.w nextStatement
	move.l a5, StatementRequest+statementImage.CaptureFrame.State
	move.l a4, StatementRequest+statementImage.CaptureFrame.Row
	move.l #Statement, StatementRequest+statementImage.CaptureFrame.Destination
	lea StatementRequest, a0
	jsr statementImage.capture
	bne.w bad
	moveq #0, d0
	move.w statements.Row.Template(a4), d0
	moveq #1, d1
	bsr.w captureOne
	bne.w bad
nextStatement
	adda.w #statements.ROW_BYTES, a4
	subq.l #1, d7
	bra.w statementLoop
serialize
	bsr.w build
	bne.w bad
	movea.l Caller, a6
	movea.l CaptureFrame.Destination(a6), a0
	lea Image, a1
	move.l memory.Block.Pointer(a1), memory.Block.Pointer(a0)
	move.l memory.Block.Capacity(a1), memory.Block.Capacity(a0)
	move.l memory.Block.Used(a1), memory.Block.Used(a0)
	clr.l memory.Block.Pointer(a1)
	clr.l memory.Block.Capacity(a1)
	clr.l memory.Block.Used(a1)
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
	.bend  ; capture
	.priv
; D0=one-based source body handle,D1=external flag. Statement holds optional
; closed signature bytes. Append one remapped body and its metadata atomically
; within this unpublished capture owner. D0/CCR=status; others preserved.
captureOne	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l SourceTemplates, Export+templates.ExportFrame.State
	move.l d0, Export+templates.ExportFrame.Handle
	move.l d1, Export+templates.ExportFrame.External
	move.l #Temporary, Export+templates.ExportFrame.Destination
	lea Export, a0
	jsr templates.exportClosed
	bne.w bad
	move.l Temporary+memory.Block.Pointer, Remap+templates.RemapFrame.Image
	move.l Temporary+memory.Block.Used, Remap+templates.RemapFrame.Bytes
	move.l #names.captureResolve, Remap+templates.RemapFrame.Resolve
	move.l #NameState, Remap+templates.RemapFrame.Context
	lea Remap, a0
	jsr templates.remapClosed
	bne.w bad
	lea Table, a0
	move.l memory.Block.Used(a0), d0
	addi.l #ROW_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l Data+memory.Block.Used, Row.Offset(a1)
	move.l Temporary+memory.Block.Used, Row.Bytes(a1)
	clr.l Row.Statement(a1)
	move.l Statement+memory.Block.Used, Row.StatementBytes(a1)
	beq.w metadataReady
	move.l Data+memory.Block.Used, d0
	add.l Temporary+memory.Block.Used, d0
	move.l d0, Row.Statement(a1)
metadataReady
	movea.l Temporary+memory.Block.Pointer, a2
	adda.l templates.ClosedHeader.Definition(a2), a2
	moveq #0, d0
	move.w templates.Def.Name(a2), d0
	move.l d0, Row.Name(a1)
	addi.l #ROW_BYTES, memory.Block.Used(a0)
	lea Data, a0
	movea.l Temporary+memory.Block.Pointer, a1
	move.l Temporary+memory.Block.Used, d0
	bsr.w append
	bne.w bad
	lea Temporary, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Data, a0
	movea.l Statement+memory.Block.Pointer, a1
	move.l Statement+memory.Block.Used, d0
	bsr.w append
	bne.w bad
	lea Statement, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; captureOne

; A0=block,A1=borrowed bytes,D0=length. No input may alias a growing owner.
; D0/CCR=status; other registers preserved. Used changes after copying only.
append	.block
	movem.l d1-d3/a0-a3, -(sp)
	movea.l a0, a3
	movea.l a1, a2
	move.l memory.Block.Used(a3), d2
	move.l d0, d3
	add.l d2, d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a3), a1
	adda.l d2, a1
	movea.l a2, a0
	move.l d3, d0
	bsr.w copy
	add.l d3, memory.Block.Used(a3)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; append

; Caller validated disjoint owner buffers; copies zero bytes without touching
; null empty pools. Other registers preserved; CCR unspecified.
copy	.block
	movem.l d0/a0-a1, -(sp)
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	movem.l (sp)+, d0/a0-a1
	rts
	.bend  ; copy
	.include "binary_module_template_image.i"
	.include "binary_module_template_seed.i"

release	.block
	lea NameState, a0
	jsr names.release
	lea Imported, a0
	jsr names.releaseInstall
	lea Table, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Data, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Temporary, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Image, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Statement, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Export, a0
	moveq #BORROW_BYTES/4-1, d0
clearBorrows
	clr.l (a0)+
	dbra d0, clearBorrows
	clr.l Caller
	clr.l SourceScope
	clr.l SourceTemplates
	clr.l SourceStatements
	clr.l Active
	rts
	.bend  ; release
	.endsection
	.endmodule
