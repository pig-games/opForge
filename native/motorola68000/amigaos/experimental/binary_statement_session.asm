; Session orchestration for retained statement facets and definition storage.
; Semantic selection belongs to shared PRVM; templates own substitution/cursors.
; @opforge-owner: experimental.amigaos.binary_statement_session
	.module experimental.amigaos.binary_statement_session
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_statements as statements
	.use experimental.amigaos.binary_statement_heads as heads
	.use experimental.amigaos.binary_statement_facets as facets
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_policy as policy
	.use tkvm.amigaos.runtime as tokenizer
	.pub
BLOCK_BYTES = memory.Block.Used+4
State	.struct
Definitions	.res statements.STATE_BYTES
Buffers	.res BLOCK_BYTES
	.endstruct
STATE_BYTES = State.Buffers+BLOCK_BYTES
OWNERSHIP_FAILURE = 36
Frame	.struct
State	.long ?
Package	.long ?
HeadNames	.long ?
Templates	.long ?
Scope	.long ?
MacroPlans	.long ?
Tokens	.long ?
Count	.long ?
Lexemes	.long ?
LexemeBytes	.long ?
Record	.long ?
RecordBytes	.long ?
Origin	.long ?
Module	.long ?  ; explicit module owner, independent of lexical scope
Visibility	.long ?  ; source-owner visibility at this declaration
Active	.long ?
DeclarationAllowed	.long ?  ; source owner guards deferred/retained definitions
Intern	.long ?
KeywordIntern	.long ?  ; opaque module-wide keyword binding
Context	.long ?
	.endstruct
FRAME_BYTES = Frame.Context+4
Snapshot	.struct
Views	.long ?
Count	.long ?
Arena	.long ?
ArenaBytes	.long ?
	.endstruct
SNAPSHOT_BYTES = Snapshot.ArenaBytes+4
	.priv
TOKEN_LIMIT = facets.TOKEN_LIMIT
VIEWS_BYTES = TOKEN_LIMIT*abi.TOKEN_BYTES
IDS_BYTES = TOKEN_LIMIT*abi.IDENTITY_BYTES
FACET_PREFIX = TOKEN_LIMIT*(abi.TOKEN_BYTES+abi.IDENTITY_BYTES)
PLAN_PREFIX = abi.PLAN_HEADER_BYTES+TOKEN_LIMIT*abi.ATOM_ROW_BYTES
Local	.struct
Views	.long ?
Ids	.long ?
Names	.long ?
Arena	.long ?
ArenaCapacity	.long ?
FacetWork	.long ?
FacetCapacity	.long ?
Output	.long ?
CompileCapacity	.long ?
CompileWork	.long ?
Budget	.long ?
Head	.res heads.FRAME_BYTES
Facets	.res facets.FRAME_BYTES
Definition	.res statements.FRAME_BYTES
	.endstruct
LOCAL_BYTES = Local.Definition+statements.FRAME_BYTES
	.section code, kind=code
	.pub
; A0=zero-initialized or completed State. D0/CCR=zero; others preserved.
; Dynamic buffers are acquired only when a statement definition/body needs them.
begin	.block
	bsr.w finish
	rts
	.bend  ; begin
; A0=State. Release all session-owned storage; template/macro owners are external.
; D0/CCR=zero; other registers preserved. Safe for zero-initialized ownership.
finish	.block
	move.l a0, -(sp)
	jsr statements.finish
	movea.l (sp), a0
	lea State.Buffers(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l (sp)+, a0
	moveq #abi.OK, d0
	rts
	.bend  ; finish
; A0=State. A physical file cannot leave an open statement definition.
; D0/CCR=status; other registers preserved.
endFile	.block
	jmp statements.endFile
	.bend  ; endFile
; A0=Frame,A1=Snapshot. Prepare canonical facets from the current owned TKVM
; line. The returned spans borrow this session's reusable buffer until its next
; line/prepare/finish; definitions retain their own offset-based copies.
; D0/CCR=status; other registers preserved. Publish the snapshot only on success.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a1, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l Frame.State(a5), a4
	movea.l sp, a6
	bsr.w buffers
	bne.w done
	lea Local.Head(a6), a0
	bsr.w headRequest
	movea.l Local.Ids(a6), a1
	jsr heads.identities
	bne.w done
	bsr.w facetRequest
	lea Local.Facets(a6), a0
	jsr facets.prepare
	bne.w done
	movea.l LOCAL_BYTES(sp), a0
	move.l Local.Views(a6), Snapshot.Views(a0)
	move.l Frame.Count(a5), Snapshot.Count(a0)
	move.l Local.Arena(a6), Snapshot.Arena(a0)
	move.l Local.Facets+facets.Frame.ArenaUsed(a6), Snapshot.ArenaBytes(a0)
done
	adda.w #LOCAL_BYTES+4, sp
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; prepare
; A0=Frame containing an original owned lexical line plus its raw writer record.
; D0/CCR=status,D1=statements.ACTION_*; other registers preserved. No original
; source is read. This entry handles definitions/body capture; invocation is a
; separate shared-VM dispatch, so ACTION_REGULAR never claims an expansion.
line	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l Frame.State(a5), a4
	movea.l sp, a6
	lea Local.Head(a6), a0
	bsr.w headRequest
	jsr heads.classify
	bne.w failed
	cmpi.l #abi.HEAD_DEFINITION, d1
	bne.w selected
	tst.l Frame.Active(a5)
	beq.w selected
	tst.l Frame.DeclarationAllowed(a5)
	bne.w selected
	moveq #OWNERSHIP_FAILURE, d0
	bra.w failed
selected
	tst.l d1
	bne.w needed
	tst.w statements.State.Open(a4)
	bne.w needed
	tst.w statements.State.Skipping(a4)
	bne.w needed
	moveq #statements.ACTION_REGULAR, d1
	moveq #abi.OK, d0
	bra.w done
needed
	bsr.w buffers
	bne.w failed
	lea Local.Head(a6), a0
	movea.l Local.Ids(a6), a1
	jsr heads.identities
	bne.w failed
	bsr.w facetRequest
	lea Local.Facets(a6), a0
	jsr facets.prepare
	bne.w failed
	bsr.w definitionRequest
	lea Local.Definition(a6), a0
	jsr statements.line
	bra.w done
failed
	moveq #statements.ACTION_REGULAR, d1
done
	adda.w #LOCAL_BYTES, sp
	tst.l d0
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; line
	.priv
; A0=head Frame,A5=caller Frame. Initialize borrowed lexical/capsule fields.
; Clobbers D0; no status contract. All interpretation remains in the shared head.
headRequest	.block
	move.l Frame.Package(a5), heads.Frame.Package(a0)
	move.l Frame.HeadNames(a5), heads.Frame.Names(a0)
	move.l Frame.Tokens(a5), heads.Frame.Tokens(a0)
	move.l Frame.Count(a5), heads.Frame.Count(a0)
	move.l Frame.Lexemes(a5), heads.Frame.Lexemes(a0)
	move.l Frame.LexemeBytes(a5), heads.Frame.LexemeBytes(a0)
	rts
	.bend  ; headRequest
; A4=State,A5=Frame,A6=locals. D0/CCR=status; others preserved.
; Capacity derives from owned lexical extent and existing captured-token bound.
; A lossy UTF8 byte needs at most three bytes; punctuation width comes from
; the validated package renderer, including captures with no retained spelling.
; Compiler workspace has one row per token plus its normalized payload arena.
; Pointers are borrowed session fields only; definitions retain offsets/IDs.
buffers	.block
	movem.l d1-d4/a0-a2, -(sp)
	cmpi.l #TOKEN_LIMIT, Frame.Count(a5)
	bhi.w invalid
	move.l Frame.LexemeBytes(a5), d1
	cmpi.l #memory.LIMIT/12, d1
	bhi.w overflow
	move.l #100000, Local.Budget(a6)
	lea Local.Budget(a6), a2
	movea.l Frame.Package(a5), a0
	move.l package.Header.StatementCompileBytes(a0), d0
	adda.l package.Header.StatementCompile(a0), a0
	jsr policy.compile
	bne.w done
	moveq #0, d4
render
	tst.l d2
	beq.w renderBound
	moveq #0, d0
	move.b abi.RenderText.TextBytes(a1), d0
	cmp.l d4, d0
	bls.w nextRender
	move.l d0, d4
nextRender
	adda.w #abi.RENDER_HEADER_BYTES, a1
	adda.l d0, a1
	subq.l #1, d2
	bra.w render
renderBound
	move.l Frame.LexemeBytes(a5), d1
	move.l d1, d0
	add.l d0, d1
	add.l d0, d1
	mulu.w #TOKEN_LIMIT, d4
	add.l d4, d1
	addq.l #3, d1
	andi.l #$fffffffc, d1
	move.l d1, Local.ArenaCapacity(a6)
	move.l d1, d2
	add.l #FACET_PREFIX, d2
	move.l d2, Local.FacetCapacity(a6)
	move.l d1, d3
	add.l #PLAN_PREFIX, d3
	move.l d3, Local.CompileCapacity(a6)
	move.l d1, d0
	add.l d2, d0
	add.l d3, d0
	add.l d3, d0
	add.l #VIEWS_BYTES+IDS_BYTES*2, d0
	lea State.Buffers(a4), a0
	jsr memory.reserveExact
	bne.w overflow
	movea.l memory.Block.Pointer(a0), a1
	move.l a1, Local.Views(a6)
	adda.l #VIEWS_BYTES, a1
	move.l a1, Local.Ids(a6)
	adda.l #IDS_BYTES, a1
	move.l a1, Local.Names(a6)
	adda.l #IDS_BYTES, a1
	move.l a1, Local.Arena(a6)
	adda.l d1, a1
	move.l a1, Local.FacetWork(a6)
	adda.l d2, a1
	move.l a1, Local.Output(a6)
	adda.l d3, a1
	move.l a1, Local.CompileWork(a6)
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w done
overflow
	moveq #statements.STORAGE_FAILURE, d0
done
	movem.l (sp)+, d1-d4/a0-a2
	tst.l d0
	rts
	.bend  ; buffers
; A5=Frame,A6=locals. Populate the bounded neutral facet adapter packet.
; Clobbers D0/A0; CCR unspecified. All IDs come from package head lookup only.
facetRequest	.block
	lea Local.Facets(a6), a0
	move.l Frame.Package(a5), facets.Frame.Package(a0)
	move.l Frame.Tokens(a5), facets.Frame.Tokens(a0)
	move.l Frame.Count(a5), d0
	mulu.w #tokenizer.TOKEN_RECORD_SIZE, d0
	move.l d0, facets.Frame.TokenBytes(a0)
	move.l Frame.Count(a5), facets.Frame.Count(a0)
	move.l Frame.Lexemes(a5), facets.Frame.Lexemes(a0)
	move.l Frame.LexemeBytes(a5), facets.Frame.LexemeBytes(a0)
	move.l Local.Ids(a6), facets.Frame.Ids(a0)
	move.l #IDS_BYTES, facets.Frame.IdsBytes(a0)
	move.l Local.Views(a6), facets.Frame.Views(a0)
	move.l #VIEWS_BYTES, facets.Frame.ViewCapacity(a0)
	move.l Local.Names(a6), facets.Frame.Names(a0)
	move.l #IDS_BYTES, facets.Frame.NameCapacity(a0)
	move.l Local.Arena(a6), facets.Frame.Arena(a0)
	move.l Local.ArenaCapacity(a6), facets.Frame.ArenaCapacity(a0)
	move.l Local.FacetWork(a6), facets.Frame.Workspace(a0)
	move.l Local.FacetCapacity(a6), facets.Frame.WorkspaceBytes(a0)
	lea Local.Budget(a6), a0
	move.l a0, Local.Facets+facets.Frame.Budget(a6)
	rts
	.bend  ; facetRequest
; A5=Frame,A6=locals. Route immutable facets, caller binding and shared output
; capacity to the storage owner. Clobbers A0; CCR unspecified.
definitionRequest	.block
	lea Local.Definition(a6), a0
	move.l Frame.State(a5), statements.Frame.State(a0)
	move.l Frame.Templates(a5), statements.Frame.Templates(a0)
	move.l Frame.Scope(a5), statements.Frame.Scope(a0)
	move.l Frame.MacroPlans(a5), statements.Frame.MacroPlans(a0)
	move.l Frame.Package(a5), statements.Frame.Package(a0)
	move.l Local.Views(a6), statements.Frame.Views(a0)
	move.l Frame.Count(a5), statements.Frame.Count(a0)
	move.l Local.Arena(a6), statements.Frame.Arena(a0)
	move.l Local.Facets+facets.Frame.ArenaUsed(a6), statements.Frame.ArenaBytes(a0)
	move.l Local.Names(a6), statements.Frame.Ids(a0)
	move.l Frame.Record(a5), statements.Frame.RawRecord(a0)
	move.l Frame.RecordBytes(a5), statements.Frame.RawBytes(a0)
	move.l Frame.Origin(a5), statements.Frame.Origin(a0)
	move.l Frame.Module(a5), statements.Frame.Module(a0)
	move.l Frame.Visibility(a5), statements.Frame.Visibility(a0)
	move.l Frame.Active(a5), statements.Frame.Active(a0)
	move.l Frame.Intern(a5), statements.Frame.Intern(a0)
	move.l Frame.KeywordIntern(a5), statements.Frame.KeywordIntern(a0)
	move.l Frame.Context(a5), statements.Frame.Context(a0)
	move.l Local.Output(a6), statements.Frame.CompileOutput(a0)
	move.l Local.CompileCapacity(a6), statements.Frame.OutputCapacity(a0)
	move.l Local.CompileWork(a6), statements.Frame.CompileWork(a0)
	move.l Local.CompileCapacity(a6), statements.Frame.WorkCapacity(a0)
	move.l Local.Budget(a6), statements.Frame.StepBudget(a0)
	rts
	.bend  ; definitionRequest
	.endsection
	.endmodule
