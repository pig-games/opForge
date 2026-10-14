; Preparation-only ownership of numeric statement plans and template bodies.
; Shared PRVM owns all definition/signature grammar. Source binding and exposed
; import spellings belong to the caller and the existing scope/template owners.
; @opforge-owner: experimental.amigaos.binary_statements
	.module experimental.amigaos.binary_statements
	.cpu 68020
	.include "lazy_work.i"
	.include "lazy_template_hooks.i"
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_source as writer
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_templates as templates
	.use experimental.amigaos.binary_macro_plans as macroPlans
	.use prvm.amigaos.abi as macroAbi
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_head as head
	.use prvm.amigaos.statement_compile as compiler
	.pub
ACTION_REGULAR = 0
ACTION_CONSUMED = 1
TOKEN_LIMIT = 64; current original-token acquisition/facet contract
STORAGE_FAILURE = 32
FORMAL_LIMIT = 33; current native template storage supports nine captures
TEMPLATE_FAILURE = 34
INTERN_FAILURE = 35
State	.struct
Count	.word ?  ; closed rows only, in definition order
Open	.word ?  ; pending row index plus one
Skipping	.word ?
Reserved	.word ?
	.endstruct
BLOCK_BYTES = memory.Block.Used+4
ROWS = State.Reserved+2
PLANS = ROWS+BLOCK_BYTES
KEYWORDS = PLANS+BLOCK_BYTES
STATE_BYTES = KEYWORDS+BLOCK_BYTES
Row	.struct
Name	.word ?
Scope	.word ?
Template	.word ?
CaptureCount	.word ?
Module	.word ?  ; source owner's modules.State.Active identity, zero global
Visibility	.word ?  ; per-overload source visibility: zero private, one public
KeywordOffset	.long ?
KeywordBytes	.long ?
PlanOffset	.long ?
PlanBytes	.long ?
Origin	.long ?
Line	.word ?
Reserved	.word ?
	.endstruct
ROW_BYTES = Row.Reserved+2
Frame	.struct
State	.long ?
Templates	.long ?
Scope	.long ?
MacroPlans	.long ?
Package	.long ?  ; validated current capsule
Views	.long ?  ; canonical abi.TokenView rows
Count	.long ?
Arena	.long ?
ArenaBytes	.long ?
Ids	.long ?  ; one u16 identity per canonical facet
RawRecord	.long ?
RawBytes	.long ?  ; complete writer record, including its four-byte header
Origin	.long ?
Module	.long ?  ; source owner's module identity, never a lexical block
Visibility	.long ?  ; zero private, one public for this overload
Active	.long ?
Intern	.long ?  ; caller source-name interning callback, never a grammar parser
KeywordIntern	.long ?  ; source owner's opaque full-module statement-key binder
Context	.long ?
CompileOutput	.long ?
OutputCapacity	.long ?
CompileWork	.long ?
WorkCapacity	.long ?
StepBudget	.long ?
	.endstruct
FRAME_BYTES = Frame.StepBudget+4
	.priv
Local	.struct
Role	.long ?
Keyword	.long ?
Signature	.long ?
Captures	.long ?
PlanBytes	.long ?
PlanOffset	.long ?
KeywordOffset	.long ?
KeywordBytes	.long ?
KeywordNew	.long ?
RowOffset	.long ?
MacroBefore	.long ?
MacroHandle	.long ?
Name	.word ?
Scope	.word ?
NameToken	.res writer.NAME_TOKEN_BYTES
Head	.res abi.HEAD_FRAME_BYTES
Compile	.res abi.COMPILE_FRAME_BYTES
Definition	.res templates.ExternalDefinition.HeaderPlan+4
Generated	.res macroPlans.GENERATED_FRAME_BYTES
Formals	.res (templates.PARAM_LIMIT+1)*macroPlans.ROW_BYTES
	.endstruct
LOCAL_BYTES = Local.Formals+(templates.PARAM_LIMIT+1)*macroPlans.ROW_BYTES
	.section code, kind=code
	.pub

; A0=zero-initialized State or prior session. Release only statement-owned pools.
; D0/CCR=zero; other registers preserved. Template/macro arenas have separate owners.
begin	.block
	bsr.w finish
	clr.w State.Count(a0)
	clr.w State.Open(a0)
	clr.w State.Skipping(a0)
	clr.w State.Reserved(a0)
	moveq #abi.OK, d0
	rts
	.bend  ; begin

; A0=State. Release rows/plans/keyword spelling, clearing all live handles.
; D0/CCR=zero; other registers preserved. Safe on zero-initialized state.
finish	.block
	move.l a0, -(sp)
	lea ROWS(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	adda.w #BLOCK_BYTES, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	adda.w #BLOCK_BYTES, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l (sp)+, a0
	clr.w State.Count(a0)
	clr.w State.Open(a0)
	clr.w State.Skipping(a0)
	moveq #abi.OK, d0
	rts
	.bend  ; finish

; A0=State. Definitions cannot cross a physical source boundary.
; D0/CCR=OK or INVALID_TOKEN; other registers preserved. Closed rows remain usable.
endFile	.block
	moveq #abi.OK, d0
	tst.w State.Open(a0)
	bne.w open
	tst.w State.Skipping(a0)
	beq.w done
open
	moveq #abi.INVALID_TOKEN, d0
done
	tst.l d0
	rts
	.bend  ; endFile

; A0=State,A1=validated package,D0=already VM-selected numeric head identity.
; D0/CCR=boolean; other registers preserved. An empty idle store only needs
; facets for the package's definition/end identities. No packed head parsing.
needsFacets	.block
	movem.l d1/a1, -(sp)
	tst.w State.Count(a0)
	bne.w needed
	tst.w State.Open(a0)
	bne.w needed
	tst.w State.Skipping(a0)
	bne.w needed
	move.l package.Header.StatementHead(a1), d1
	adda.l d1, a1
	cmp.w abi.HeadProgram.Definition(a1), d0
	beq.w needed
	cmp.w abi.HeadProgram.EndMarker(a1), d0
	beq.w needed
	moveq #0, d0
	bra.w done
needed
	moveq #1, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; needsFacets

; A0=Frame. D0/CCR=PRVM status,D1=A8 role,D2=keyword/D3=signature indices.
; Other registers preserved. Classification changes no statement/template storage.
classify	.block
	movem.l d4/a0-a2/a5, -(sp)
	suba.w #abi.HEAD_FRAME_BYTES, sp
	movea.l a0, a5
	movea.l sp, a2
	moveq #abi.HEAD_CLASSIFY, d4
	bsr.w headRequest
	movea.l a2, a0
	moveq #abi.HEAD_FRAME_BYTES, d0
	jsr head.run
	adda.w #abi.HEAD_FRAME_BYTES, sp
	movem.l (sp)+, d4/a0-a2/a5
	tst.l d0
	rts
	.bend  ; classify

; A0=Frame. D0/CCR=status,D1=ACTION_*; other registers preserved.
; A8 selects roles, A6 compiles definitions, external templates own all bodies.
; Intern/KeywordIntern callbacks: A0/name,D0/bytes,A1/Context -> D0/status,
; D1/u16 source ID; lexical formals and opaque module keys stay independent;
; clobbers only D0/D1/A0/A1. Binding/namespace/package-name policy belongs to it.
; Nine captures is an explicit pending storage bound, reported as FORMAL_LIMIT.
; Output/work buffers belong to caller and must satisfy the shared A6 span ABI.
line	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l Frame.State(a5), a6
	movea.l sp, a4
	cmpi.l #TOKEN_LIMIT, Frame.Count(a5)
	bhi.w invalidArgument
	cmpi.l #65535, Frame.Module(a5)
	bhi.w invalidArgument
	cmpi.l #1, Frame.Visibility(a5)
	bhi.w invalidArgument
	bsr.w classify
	bne.w failed
	move.l d1, Local.Role(a4)
	move.l d2, Local.Keyword(a4)
	move.l d3, Local.Signature(a4)
	tst.w State.Skipping(a6)
	beq.w body
	cmpi.l #abi.HEAD_END, d1
	bne.w consumed
	clr.w State.Skipping(a6)
	bra.w consumed
body
	tst.w State.Open(a6)
	beq.w outside
	cmpi.l #abi.HEAD_DEFINITION, d1
	beq.w invalidToken
	cmpi.l #abi.HEAD_END, d1
	beq.w close
	bsr.w record
	bne.w failed
	movea.l Frame.RawRecord(a5), a0
	movea.l Frame.Scope(a5), a1
	movea.l Frame.Templates(a5), a2
	jsr templates.role
	cmpi.l #templates.ROLE_MACRO, d0
	bhs.w invalidToken
	tst.l Frame.Active(a5)
	beq.w consumed
	movea.l Frame.Templates(a5), a0
	movea.l Frame.RawRecord(a5), a1
	jsr templates.externalCapture
	bne.w templateFailure
	bra.w consumed
close
	moveq #0, d0
	move.w State.Open(a6), d0
	subq.w #1, d0
	mulu.w #ROW_BYTES, d0
	movea.l ROWS+memory.Block.Pointer(a6), a1
	adda.l d0, a1
	moveq #0, d1
	move.w Row.Template(a1), d1
	movea.l Frame.Templates(a5), a0
	jsr templates.externalClose
	bne.w templateFailure
	addq.w #1, State.Count(a6)
	clr.w State.Open(a6)
	bra.w consumed
outside
	cmpi.l #abi.HEAD_DEFINITION, d1
	bne.w regular
	movea.l Frame.Templates(a5), a0
	tst.w templates.State.Open(a0)
	bne.w invalidToken
	tst.w templates.State.Skipping(a0)
	bne.w invalidToken
	lea Local.Head(a4), a2
	moveq #abi.HEAD_COMPLETE, d4
	bsr.w headRequest
	movea.l a2, a0
	moveq #abi.HEAD_FRAME_BYTES, d0
	jsr head.run
	bne.w failed
	move.l d2, Local.Keyword(a4)
	move.l d3, Local.Signature(a4)
	tst.l Frame.Active(a5)
	bne.w define
	move.w #1, State.Skipping(a6)
	bra.w consumed
define
	bsr.w definition
	bne.w failed
consumed
	moveq #ACTION_CONSUMED, d1
	moveq #abi.OK, d0
	bra.w done
regular
	moveq #ACTION_REGULAR, d1
	moveq #abi.OK, d0
	bra.w done
invalidArgument
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w failed
invalidToken
	moveq #abi.INVALID_TOKEN, d0
	bra.w failed
templateFailure
	moveq #TEMPLATE_FAILURE, d0
failed
	moveq #ACTION_REGULAR, d1
done
	adda.w #LOCAL_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=State. D0=closed row count/CCR; other registers preserved.
count	.block
	moveq #0, d0
	move.w State.Count(a0), d0
	rts
	.bend  ; count

; A0=State,D1=one-based closed row handle. D0/CCR=status; on success A1=row,
; A2=plan,D2=plan bytes,A3=keyword,D3=keyword bytes. D1/other registers preserved.
; Borrowed pointers expire on the next storage mutation/begin/finish; stored
; rows contain only offsets, numeric IDs, body handles and opaque origins.
view	.block
	movem.l d1/d4, -(sp)
	tst.l d1
	beq.w invalid
	moveq #0, d4
	move.w State.Count(a0), d4
	cmp.l d4, d1
	bhi.w invalid
	subq.l #1, d1
	mulu.w #ROW_BYTES, d1
	movea.l ROWS+memory.Block.Pointer(a0), a1
	adda.l d1, a1
	movea.l PLANS+memory.Block.Pointer(a0), a2
	adda.l Row.PlanOffset(a1), a2
	move.l Row.PlanBytes(a1), d2
	movea.l KEYWORDS+memory.Block.Pointer(a0), a3
	adda.l Row.KeywordOffset(a1), a3
	move.l Row.KeywordBytes(a1), d3
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	moveq #0, d2
	moveq #0, d3
	suba.l a1, a1
	suba.l a2, a2
	suba.l a3, a3
done
	movem.l (sp)+, d1/d4
	tst.l d0
	rts
	.bend  ; view

; A0=State. D0/CCR=status,D1=first closed handle or zero; others preserved.
; Declaration order is module-flat; scope/import exposure and replacement order
; belong to the source owner. Open rows are never returned.
first	.block
	moveq #0, d1
	tst.w State.Count(a0)
	beq.w done
	moveq #1, d1
done
	moveq #abi.OK, d0
	rts
	.bend  ; first

; A0=State,D1=current closed handle (zero remains exhausted).
; D0/CCR=status,D1=next handle or zero; other registers preserved.
next	.block
	move.l d2, -(sp)
	tst.l d1
	beq.w exhausted
	moveq #0, d2
	move.w State.Count(a0), d2
	cmp.l d2, d1
	bhs.w exhausted
	addq.l #1, d1
	bra.w done
exhausted
	moveq #0, d1
done
	move.l (sp)+, d2
	moveq #abi.OK, d0
	rts
	.bend  ; next
	.priv
; A5=Frame,A2=HeadFrame,D4=shared A8 mode. Clobbers D0/D1/A0/A1 only.
headRequest	.block
	movea.l Frame.Package(a5), a0
	move.l package.Header.StatementHeadBytes(a0), abi.HeadFrame.ProgramBytes(a2)
	move.l package.Header.StatementHead(a0), d0
	adda.l d0, a0
	move.l a0, abi.HeadFrame.ProgramPtr(a2)
	move.l Frame.Views(a5), abi.HeadFrame.TokenPtr(a2)
	move.l Frame.Count(a5), abi.HeadFrame.TokenCount(a2)
	move.l Frame.Ids(a5), abi.HeadFrame.NamesPtr(a2)
	move.l Frame.Count(a5), abi.HeadFrame.NamesCount(a2)
	move.l d4, abi.HeadFrame.Mode(a2)
	move.l Frame.StepBudget(a5), abi.HeadFrame.StepBudget(a2)
	rts
	.bend  ; headRequest

; A5=Frame. D0/status/CCR; clobbers A0. No token/source grammar is interpreted.
record	.block
	move.l Frame.RawRecord(a5), d0
	beq.w invalid
	movea.l d0, a0
	move.l Frame.RawBytes(a5), d0
	cmpi.l #writer.RECORD_HEADER_BYTES, d0
	blo.w invalid
	cmpi.l #256, d0
	bhi.w invalid
	moveq #0, d0
	move.b writer.RecordHeader.Length(a0), d0
	addq.l #1, d0
	cmp.l Frame.RawBytes(a5), d0
	bne.w invalid
	moveq #abi.OK, d0
	rts
invalid
	moveq #abi.INVALID_TOKEN, d0
	rts
	.bend  ; record

; A5=Frame,A6=State,A4=locals. Compile and stage one immutable definition.
; D0/status/CCR; clobbers D1-D7/A0-A3. No pool usage/row publication until
; both the spelling owner and external template accept the definition.
definition	.block
	bsr.w record
	bne.w done
	movea.l Frame.Scope(a5), a0
	move.w layout.State.Current(a0), Local.Scope(a4)
	lea Local.Compile(a4), a2
	movea.l Frame.Package(a5), a0
	move.l package.Header.StatementCompileBytes(a0), abi.CompileFrame.ProgramBytes(a2)
	move.l package.Header.StatementCompile(a0), d0
	adda.l d0, a0
	move.l a0, abi.CompileFrame.ProgramPtr(a2)
	move.l Local.Signature(a4), d0
	move.l Frame.Count(a5), d1
	sub.l d0, d1
	bcs.w invalid
	move.l d1, abi.CompileFrame.TokenCount(a2)
	lsl.l #5, d0
	movea.l Frame.Views(a5), a0
	adda.l d0, a0
	move.l a0, abi.CompileFrame.TokenPtr(a2)
	move.l Frame.Arena(a5), abi.CompileFrame.ArenaPtr(a2)
	move.l Frame.ArenaBytes(a5), abi.CompileFrame.ArenaBytes(a2)
	move.l Frame.CompileOutput(a5), abi.CompileFrame.OutputPtr(a2)
	move.l Frame.OutputCapacity(a5), abi.CompileFrame.OutputCapacity(a2)
	move.l Frame.CompileWork(a5), abi.CompileFrame.WorkspacePtr(a2)
	move.l Frame.WorkCapacity(a5), abi.CompileFrame.WorkspaceCapacity(a2)
	move.l Frame.StepBudget(a5), abi.CompileFrame.StepBudget(a2)
	movea.l a2, a0
	moveq #abi.COMPILE_FRAME_BYTES, d0
	jsr compiler.run
	bne.w done
	move.l d1, Local.PlanBytes(a4)
	bsr.w formals
	bne.w done
	bsr.w keyword
	bne.w done
	clr.b Local.NameToken(a4)
	move.w Local.Name(a4), Local.NameToken+writer.NameToken.Id(a4)
	clr.b Local.NameToken+writer.NameToken.Qualifier(a4)
	lea Local.NameToken(a4), a0
	movea.l Frame.Scope(a5), a1
	move.l Frame.Module(a5), d0
	jsr scopes.validateStatement
	bne.w internFailure
	bsr.w reserve
	bne.w done
	movea.l Frame.Templates(a5), a0
	jsr templates.externalReserve
	bne.w templateFailure
	movea.l Frame.MacroPlans(a5), a0
	move.l memory.Block.Used(a0), Local.MacroBefore(a4)
	lea Local.Generated(a4), a0
	move.l Frame.MacroPlans(a5), macroPlans.GeneratedFrame.Arena(a0)
	lea Local.Formals(a4), a1
	move.l a1, macroPlans.GeneratedFrame.PackedEvents(a0)
	move.l a1, macroPlans.GeneratedFrame.SpellingEvents(a0)
	move.l Local.Captures(a4), d0
	addq.l #1, d0
	move.l d0, Local.Generated+macroPlans.GeneratedFrame.PackedCount(a4)
	move.l d0, Local.Generated+macroPlans.GeneratedFrame.SpellingCount(a4)
	move.l Frame.CompileOutput(a5), Local.Generated+macroPlans.GeneratedFrame.Source(a4)
	move.l Local.PlanBytes(a4), Local.Generated+macroPlans.GeneratedFrame.SourceBytes(a4)
	clr.l Local.Generated+macroPlans.GeneratedFrame.PackedBytes(a4)
	clr.l Local.Generated+macroPlans.GeneratedFrame.RecipeEvents(a4)
	clr.l Local.Generated+macroPlans.GeneratedFrame.RecipeCount(a4)
	lea Local.Generated(a4), a0
	jsr macroPlans.createGenerated
	bne.w storageFailure
	move.l d1, Local.MacroHandle(a4)
	move.l d1, Local.Definition+templates.ExternalDefinition.HeaderPlan(a4)
	move.w Local.Name(a4), Local.Definition+templates.ExternalDefinition.Name(a4)
	move.w Local.Captures+2(a4), Local.Definition+templates.ExternalDefinition.ParamCount(a4)
	move.l Frame.Origin(a5), Local.Definition+templates.ExternalDefinition.Origin(a4)
	lea Local.NameToken(a4), a0
	movea.l Frame.Scope(a5), a1
	move.l Frame.Module(a5), d0
	jsr scopes.declareStatement
	bne.w declarationFailure
	movea.l Frame.Templates(a5), a0
	movea.l Frame.Scope(a5), a1
	lea Local.Definition(a4), a2
	jsr templates.externalBegin
	beq.w publish
	movea.l Frame.MacroPlans(a5), a0
	move.l Local.MacroBefore(a4), memory.Block.Used(a0)
	moveq #TEMPLATE_FAILURE, d0
	bra.w done
publish
	movea.l ROWS+memory.Block.Pointer(a6), a0
	adda.l Local.RowOffset(a4), a0
	move.w Local.Name(a4), Row.Name(a0)
	move.w Local.Scope(a4), Row.Scope(a0)
	move.w d1, Row.Template(a0)
	move.w Local.Captures+2(a4), Row.CaptureCount(a0)
	move.w Frame.Module+2(a5), Row.Module(a0)
	move.w Frame.Visibility+2(a5), Row.Visibility(a0)
	move.l Local.KeywordOffset(a4), Row.KeywordOffset(a0)
	move.l Local.KeywordBytes(a4), Row.KeywordBytes(a0)
	move.l Local.PlanOffset(a4), Row.PlanOffset(a0)
	move.l Local.PlanBytes(a4), Row.PlanBytes(a0)
	move.l Frame.Origin(a5), Row.Origin(a0)
	movea.l Frame.RawRecord(a5), a1
	move.w writer.RecordHeader.Line(a1), Row.Line(a0)
	.WORK_TEMPLATE_LINE Frame.Templates(a5), Row.Template(a0), #1, Row.Line(a0)
	clr.w Row.Reserved(a0)
	move.l Local.RowOffset(a4), d0
	addi.l #ROW_BYTES, d0
	move.l d0, ROWS+memory.Block.Used(a6)
	move.l Local.PlanOffset(a4), d0
	add.l Local.PlanBytes(a4), d0
	addq.l #1, d0
	andi.l #$fffffffe, d0
	move.l d0, PLANS+memory.Block.Used(a6)
	tst.l Local.KeywordNew(a4)
	beq.w opened
	move.l Local.KeywordOffset(a4), d0
	add.l Local.KeywordBytes(a4), d0
	move.l d0, KEYWORDS+memory.Block.Used(a6)
opened
	move.w State.Count(a6), d0
	addq.w #1, d0
	move.w d0, State.Open(a6)
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w done
declarationFailure
	movea.l Frame.MacroPlans(a5), a0
	move.l Local.MacroBefore(a4), memory.Block.Used(a0)
internFailure
	moveq #INTERN_FAILURE, d0
	bra.w done
templateFailure
	moveq #TEMPLATE_FAILURE, d0
	bra.w done
storageFailure
	moveq #STORAGE_FAILURE, d0
done
	rts
	.bend  ; definition

; Adapt trusted A6 capture payloads into the spelling owner's ordinary formal
; rows. A5=Frame,A4=locals. D0/status/CCR; clobbers D1-D7/A0-A3.
; Normal rows use zero packed spans; no source tokenization/macro grammar runs.
formals	.block
	lea Local.Formals(a4), a0
	moveq #(templates.PARAM_LIMIT+1)*macroPlans.ROW_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	clr.l Local.Captures(a4)
	movea.l Frame.CompileOutput(a5), a3
	move.l abi.PlanHeader.RowCount(a3), d7
	move.l d7, d0
	mulu.w #abi.ATOM_ROW_BYTES, d0
	addi.l #abi.PLAN_HEADER_BYTES, d0
	lea Local.Formals(a4), a0
	move.w #macroAbi.PRVM_RESULT_MACRO_LINE, macroPlans.Row.Kind(a0)
	move.l d0, macroPlans.Row.SpellingStart(a0)
	move.l Local.PlanBytes(a4), macroPlans.Row.SpellingEnd(a0)
	move.l #abi.NONE, macroPlans.Row.Aux2(a0)
	adda.w #abi.PLAN_HEADER_BYTES, a3
next
	tst.l d7
	beq.w complete
	cmpi.b #abi.ATOM_CAPTURE, abi.AtomRow.Kind(a3)
	bne.w advance
	move.l Local.Captures(a4), d6
	cmpi.l #templates.PARAM_LIMIT, d6
	bhs.w tooMany
	move.l Frame.CompileOutput(a5), d0
	movea.l d0, a0
	adda.l abi.AtomRow.PayloadOffset(a3), a0
	move.l abi.AtomRow.PayloadBytes(a3), d0
	move.l Frame.Intern(a5), d1
	bsr.w intern
	bne.w done
	move.l d6, d0
	add.w d0, d0
	lea Local.Definition+templates.ExternalDefinition.Parameter(a4), a1
	move.w d1, 0(a1, d0.w)
	move.l d6, d0
	addq.l #1, d0
	mulu.w #macroPlans.ROW_BYTES, d0
	lea Local.Formals(a4), a0
	adda.l d0, a0
	move.w #macroAbi.PRVM_RESULT_MACRO_FORMAL, macroPlans.Row.Kind(a0)
	move.l abi.AtomRow.PayloadOffset(a3), macroPlans.Row.SpellingStart(a0)
	move.l abi.AtomRow.PayloadOffset(a3), d0
	add.l abi.AtomRow.PayloadBytes(a3), d0
	move.l d0, macroPlans.Row.SpellingEnd(a0)
	move.l #abi.NONE, macroPlans.Row.Aux0(a0)
	move.l #abi.NONE, macroPlans.Row.Aux1(a0)
	addq.l #1, Local.Captures(a4)
advance
	adda.w #abi.ATOM_ROW_BYTES, a3
	subq.l #1, d7
	bra.w next
complete
	move.l Local.Captures(a4), Local.Formals+macroPlans.Row.Aux1(a4)
	moveq #abi.OK, d0
	bra.w done
tooMany
	moveq #FORMAL_LIMIT, d0
done
	rts
	.bend  ; formals

; A0/name,D0/bytes,D1=source-owner callback,A5=Frame.
; D0/status/CCR,D1=u16 source identity.
; Clobbers A0/A1; other registers preserved by the caller-provided callback.
intern	.block
	tst.l d1
	beq.w failed
	move.l a2, -(sp)
	movea.l d1, a2
	movea.l Frame.Context(a5), a1
	jsr (a2)
	movea.l (sp)+, a2
	tst.l d0
	bne.w failed
	cmpi.l #$ffff, d1
	bhi.w failed
	moveq #abi.OK, d0
	rts
failed
	moveq #INTERN_FAILURE, d0
	rts
	.bend  ; intern

; A5=Frame,A6=State,A4=locals. Select keyword facet/name and reuse its prior
; spelling offset for same-name overloads. D0/status/CCR; clobbers D1-D3/A0-A2.
keyword	.block
	move.l Local.Keyword(a4), d0
	lsl.l #5, d0
	movea.l Frame.Views(a5), a2
	adda.l d0, a2
	move.l Frame.ArenaBytes(a5), d0
	sub.l abi.TokenView.LiteralOffset(a2), d0
	bcs.w invalid
	move.l abi.TokenView.LiteralBytes(a2), d1
	beq.w invalid
	cmp.l d0, d1
	bhi.w invalid
	move.l d1, Local.KeywordBytes(a4)
	movea.l Frame.Arena(a5), a0
	adda.l abi.TokenView.LiteralOffset(a2), a0
	move.l d1, d0
	move.l Frame.KeywordIntern(a5), d1
	bsr.w intern
	bne.w done
	move.w d1, Local.Name(a4)
	move.l KEYWORDS+memory.Block.Used(a6), Local.KeywordOffset(a4)
	move.l #1, Local.KeywordNew(a4)
	moveq #0, d3
scan
	cmp.w State.Count(a6), d3
	bhs.w good
	move.l d3, d0
	mulu.w #ROW_BYTES, d0
	movea.l ROWS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	cmp.w Row.Name(a0), d1
	beq.w reused
	addq.w #1, d3
	bra.w scan
reused
	move.l Row.KeywordOffset(a0), Local.KeywordOffset(a4)
	move.l Row.KeywordBytes(a0), Local.KeywordBytes(a4)
	clr.l Local.KeywordNew(a4)
good
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_TOKEN, d0
done
	rts
	.bend  ; keyword

; Reserve all statement pools before the spelling/template owners publish.
; A5=Frame,A6=State,A4=locals. D0/status/CCR; clobbers D1-D3/A0-A2.
; Copy staged immutable bytes without changing pool usage until open succeeds.
reserve	.block
	cmpi.w #$ffff, State.Count(a6)
	beq.w failed
	moveq #0, d0
	move.w State.Count(a6), d0
	mulu.w #ROW_BYTES, d0
	move.l d0, Local.RowOffset(a4)
	addi.l #ROW_BYTES, d0
	lea ROWS(a6), a0
	jsr memory.reserve
	bne.w failed
	move.l PLANS+memory.Block.Used(a6), d1
	move.l d1, Local.PlanOffset(a4)
	add.l Local.PlanBytes(a4), d1
	bcs.w failed
	addq.l #1, d1
	bcs.w failed
	andi.l #$fffffffe, d1
	move.l d1, d0
	lea PLANS(a6), a0
	jsr memory.reserve
	bne.w failed
	tst.l Local.KeywordNew(a4)
	beq.w copyPlan
	move.l Local.KeywordOffset(a4), d0
	add.l Local.KeywordBytes(a4), d0
	bcs.w failed
	lea KEYWORDS(a6), a0
	jsr memory.reserve
	bne.w failed
	move.l Local.Keyword(a4), d0
	lsl.l #5, d0
	movea.l Frame.Views(a5), a2
	adda.l d0, a2
	movea.l Frame.Arena(a5), a0
	adda.l abi.TokenView.LiteralOffset(a2), a0
	movea.l KEYWORDS+memory.Block.Pointer(a6), a1
	adda.l Local.KeywordOffset(a4), a1
	move.l Local.KeywordBytes(a4), d0
	bsr.w copy
copyPlan
	movea.l Frame.CompileOutput(a5), a0
	movea.l PLANS+memory.Block.Pointer(a6), a1
	adda.l Local.PlanOffset(a4), a1
	move.l Local.PlanBytes(a4), d0
	bsr.w copy
	moveq #abi.OK, d0
	rts
failed
	moveq #STORAGE_FAILURE, d0
	rts
	.bend  ; reserve

; A0/A1=source/destination,D0=checked bytes. Clobbers A0/A1/D0/CCR.
copy	.block
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	rts
	.bend  ; copy
	.endsection
	.endmodule
