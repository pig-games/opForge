; Source-session orchestration of shared statement selection and expansion.
; Grammar lives in PRVM/TKVM; imports own exposure and templates own cursors.
; @opforge-owner: experimental.amigaos.binary_statement_execution
	.module experimental.amigaos.binary_statement_execution
	.cpu 68020
	.include "memory_telemetry.i"
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_source as writer
	.use experimental.amigaos.binary_statements as definitions
	.use experimental.amigaos.binary_statement_session as session
	.use experimental.amigaos.binary_statement_call as calls
	.use experimental.amigaos.binary_statement_tail as tail
	.use experimental.amigaos.binary_statement_arguments as arguments
	.use experimental.amigaos.binary_templates as templates
	.use experimental.amigaos.binary_tokenizer_capsule as capsule_tokenizer
	.use prvm.amigaos.statement_abi as abi
	.pub
BLOCK_BYTES = memory.Block.Used+4
State	.struct
Calls	.res calls.STATE_BYTES
Tail	.res tail.STATE_BYTES
Work	.res BLOCK_BYTES
	.endstruct
STATE_BYTES = State.Work+BLOCK_BYTES
Frame	.struct
State	.long ?
Source	.long ?  ; configured session.Frame, whose current writer record is live
Writer	.long ?  ; exclusive live writer.Frame, restored by argument preparation
InheritedLabelPresent	.long ?  ; numeric parent label, absent from lexical facets
InheritedLabel	.long ?
	.endstruct
FRAME_BYTES = Frame.InheritedLabel+4
ACTION_REGULAR = 0
ACTION_EXPANDED = 2; common frontend expansion queue action
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.section bss, kind=bss
FailureStep	.res long, 1
	.endsection
.endif
.endif
.endif
	.priv
STEP_FACETS = 1
STEP_ROUTE = 2
STEP_TAIL = 3
STEP_STORAGE = 4
STEP_SELECT = 5
STEP_TOKENIZER = 6
STEP_ARGUMENTS = 7
STEP_CALL = 8
STEP_QUEUE = 9
TOKEN_LIMIT = 64
SELECTION_BYTES = abi.SELECTION_BYTES+templates.PARAM_LIMIT*abi.CAPTURE_BYTES
FALLBACK_OFFSET = SELECTION_BYTES
FALLBACK_BYTES = 2*TOKEN_LIMIT*abi.TOKEN_BYTES
MATCH_OFFSET = FALLBACK_OFFSET+FALLBACK_BYTES
; Selector staging uses112 bytes/input. Matcher needs one nesting-stack word
; or Capture12 row per signature atom; current compilation consumes<=64 tokens.
MATCH_BYTES = TOKEN_LIMIT*(2*abi.TOKEN_BYTES+4*abi.CAPTURE_BYTES)+TOKEN_LIMIT*(4+abi.CAPTURE_BYTES)
TOKENS_OFFSET = MATCH_OFFSET+MATCH_BYTES
TOKENS_BYTES = TOKEN_LIMIT*(writer.Token.Length+4)
LEXEMES_OFFSET = TOKENS_OFFSET+TOKENS_BYTES
LEXEMES_BYTES = 5*templates.TEXT_LIMIT+TOKEN_LIMIT*12
ARGUMENTS_OFFSET = LEXEMES_OFFSET+LEXEMES_BYTES
ARGUMENTS_BYTES = templates.PARAM_LIMIT*templates.EXTERNAL_ARGUMENT_BYTES
PACKED_OFFSET = ARGUMENTS_OFFSET+ARGUMENTS_BYTES
FULL_OFFSET = PACKED_OFFSET+templates.ARG_LIMIT
FULL_BYTES = 252
WORK_BYTES = FULL_OFFSET+FULL_BYTES
STEP_BUDGET = 100000
Local	.struct
Source	.res session.SNAPSHOT_BYTES
Arguments	.res session.SNAPSHOT_BYTES
Prepare	.res calls.PREPARE_FRAME_BYTES
Tail	.res tail.FRAME_BYTES
Select	.res calls.SELECT_FRAME_BYTES
Pack	.res arguments.FRAME_BYTES
Call	.res templates.ExternalCall.FullBytes+2
Views	.long ?
ViewCount	.long ?
Label	.long ?
	.endstruct
LOCAL_BYTES = (Local.Label+4+3)&$fffffffc
	.section code, kind=code
	.pub
; A0=zeroed State or prior completed session. D0/CCR=zero; others preserved.
begin	.block
	bsr.w finish
	rts
	.bend  ; begin
; A0=State. Release invocation scratch; definitions/templates remain external.
; D0/CCR=zero; other registers preserved.
finish	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	jsr calls.finish
	lea State.Tail(a1), a0
	jsr tail.finish
	lea State.Work(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	moveq #0, d0
	movem.l (sp)+, a0-a1
	rts
	.bend  ; finish
; A0=Frame. D0/status,D1=ACTION_*. Other registers preserved. A successful
; expansion queues the selected private carrier; the frontend drains the common
; template cursor. No original file access or CPU-specific name parsing occurs.
run	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l Frame.State(a5), a4
	movea.l Frame.Source(a5), a3
	movea.l sp, a6
	tst.l session.Frame.Active(a3)
	beq.w ordinary
	movea.l session.Frame.State(a3), a0
	tst.w definitions.State.Count(a0)
	beq.w ordinary
	tst.w definitions.State.Open(a0)
	bne.w ordinary
	tst.w definitions.State.Skipping(a0)
	bne.w ordinary
	movea.l session.Frame.Templates(a3), a0
	jsr templates.hasOpen
	bne.w ordinary
	tst.w templates.State.Skipping(a0)
	bne.w ordinary
	movea.l a3, a0
	lea Local.Source(a6), a1
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_FACETS
	jsr session.prepare
	bne.w failed
	bsr.w prepareRequest
	lea Local.Prepare(a6), a0
	moveq #calls.PREPARE_FRAME_BYTES, d0
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_ROUTE
	jsr calls.prepare
	bne.w failed
	tst.l d1
	beq.w ordinary
	bsr.w tailRequest
	lea Local.Tail(a6), a0
	lea State.Tail(a4), a1
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_TAIL
	jsr tail.prepare
	bne.w failed
	lea State.Work(a4), a0
	move.l #WORK_BYTES, d0
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_STORAGE
	jsr memory.reserveExact
	bne.w storageFailure
	move.l #WORK_BYTES, memory.Block.Used(a0)
	bsr.w selectRequest
	lea Local.Select(a6), a0
	moveq #calls.SELECT_FRAME_BYTES, d0
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_SELECT
	jsr calls.select
	bne.w failed
	tst.l d1
	beq.w ordinary
	movea.l State.Work+memory.Block.Pointer(a4), a2
	move.l Local.Arguments+session.Snapshot.Views(a6), Local.Views(a6)
	move.l Local.Arguments+session.Snapshot.Count(a6), Local.ViewCount(a6)
	tst.l abi.Selection.FallbackUsed(a2)
	beq.w selectedViews
	lea FALLBACK_OFFSET(a2), a0
	move.l a0, Local.Views(a6)
	move.l d3, Local.ViewCount(a6)
selectedViews
	bsr.w packRequest
	movea.l session.Frame.Package(a3), a0
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_TOKENIZER
	jsr capsule_tokenizer.activate
	bne.w failed
	move.l a1, Local.Pack+arguments.Frame.Program(a6)
	move.l d1, Local.Pack+arguments.Frame.ProgramBytes(a6)
	lea Local.Pack(a6), a0
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_ARGUMENTS
	jsr arguments.run
	bne.w failed
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_CALL
	bsr.w callRequest
	bne.w failed
	movea.l session.Frame.Templates(a3), a0
	lea Local.Call(a6), a1
	.ASSEMBLY_FAILURE_STAGE FailureStep, #STEP_QUEUE
	jsr templates.externalInvoke
	bne.w storageFailure
	moveq #ACTION_EXPANDED, d1
	moveq #abi.OK, d0
	bra.w done
ordinary
	moveq #abi.OK, d0
failed
	moveq #ACTION_REGULAR, d1
	bra.w done
storageFailure
	moveq #definitions.TEMPLATE_FAILURE, d0
	bra.w failed
done
	adda.w #LOCAL_BYTES, sp
	tst.l d0
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; run
	.priv
	.include "binary_statement_exec_req.i"
	.endsection
	.endmodule
