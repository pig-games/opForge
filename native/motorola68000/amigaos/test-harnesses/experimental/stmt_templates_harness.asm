; Direct external storage/cursor controls; source routing is separate.
.module main
.cpu 68020
.use experimental.amigaos.binary_templates as templates
.use experimental.amigaos.binary_scopes as scopes
.use experimental.amigaos.binary_macro_plans as plans
.use experimental.amigaos.binary_source as writer
.use prvm.amigaos.abi as abi
.priv
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
; Test-only failure phases; success remains exactly zero.
PHASE_SCOPE = 41
PHASE_TEMPLATE = 42
PHASE_PLAN = 43
PHASE_KEY = 44
PHASE_FORMAL = 45
PHASE_LABEL = 46
PHASE_GENERATED = 47
PHASE_OPEN = 50
PHASE_CAPTURE = 60
PHASE_CLOSE = 70
PHASE_LIMIT = 80
PHASE_VISIBLE = 90
PHASE_SUMMARY = 100
PHASE_INVOKE = 110
PHASE_NEXT = 120
PHASE_ORIGIN = 121
PHASE_NONEMPTY = 122
PHASE_PREFIX = 130
PHASE_RECORD = 140
PHASE_EXHAUST = 150
	.pub
	.section entry, kind=code
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	jsr cases
	move.l d0, ReturnCode
	movea.l DosBase, a6
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.endsection
	.priv
	.section code, kind=code
cases .block
	move.l #PHASE_SCOPE,Phase
	lea ScopeState,a0
	moveq #100,d0
	moveq #0,d1
	jsr scopes.begin
	bne.w bad
	move.l #PHASE_TEMPLATE,Phase
	lea TemplateState,a0
	jsr templates.begin
	move.l #PHASE_PLAN,Phase
	lea PlanState,a0
	jsr plans.begin
	move.l #PlanState,TemplateState+templates.State.Plans
	move.l #PHASE_KEY,Phase
	lea Keyword,a0
	moveq #1,d0
	lea ScopeState,a1
	jsr scopes.bindOrigin
	bne.w bad
	cmpi.w #100,d1
	bne.w bad
	move.l #PHASE_FORMAL,Phase
	lea Formal,a0
	moveq #1,d0
	lea ScopeState,a1
	jsr scopes.bindOrigin
	bne.w bad
	cmpi.w #101,d1
	bne.w bad
	move.l #PHASE_LABEL,Phase
	lea LabelName,a0
	moveq #5,d0
	lea ScopeState,a1
	jsr scopes.bindOrigin
	bne.w bad
	cmpi.w #102,d1
	bne.w bad
	move.l #PHASE_GENERATED,Phase
	lea Generated,a0
	jsr plans.createGenerated
	bne.w bad
	move.l d1,PlanHandle
	moveq #1,d6
	; Numeric, all-blank, empty, textual and nine-formal empty overloads.
nextDefinition
	move.w #1,Definition+templates.ExternalDefinition.ParamCount
	cmpi.w #2,d6
	beq.w zeroFormals
	cmpi.w #3,d6
	beq.w zeroFormals
	cmpi.w #5,d6
	bne.w open
	move.w #9,Definition+templates.ExternalDefinition.ParamCount
	bra.w open
zeroFormals
	clr.w Definition+templates.ExternalDefinition.ParamCount
open
	clr.l Definition+templates.ExternalDefinition.HeaderPlan
	cmpi.w #1,Definition+templates.ExternalDefinition.ParamCount
	bne.w headerReady
	move.l PlanHandle,Definition+templates.ExternalDefinition.HeaderPlan
headerReady
	move.l #PHASE_OPEN,Phase
	add.w d6,Phase+2
	lea TemplateState,a0
	lea ScopeState,a1
	lea Definition,a2
	jsr templates.externalBegin
	bne.w bad
	cmp.w d6,d1
	bne.w bad
	cmpi.w #1,d6
	beq.w numericBody
	cmpi.w #2,d6
	beq.w blankBody
	cmpi.w #4,d6
	beq.w textBody
	bra.w close
numericBody
	move.l #PHASE_CAPTURE,Phase
	lea Blank,a1
	jsr templates.externalCapture
	bne.w bad
	lea NumericRecord,a1
	jsr templates.externalCapture
	bne.w bad
	bra.w close
blankBody
	move.l #PHASE_CAPTURE,Phase
	lea Blank,a1
	jsr templates.externalCapture
	bne.w bad
	jsr templates.externalCapture
	bne.w bad
	bra.w close
textBody
	move.l #PHASE_CAPTURE,Phase
	lea TextRecord,a1
	jsr templates.externalCapture
	bne.w bad
close
	move.l #PHASE_CLOSE,Phase
	add.w d6,Phase+2
	move.w d6,d1
	jsr templates.externalClose
	bne.w bad
	addq.w #1,d6
	cmpi.w #6,d6
	blo.w nextDefinition
	move.l #PHASE_LIMIT,Phase
	; The tenth formal must preserve five private carrier handles.
	move.w #10,Definition+templates.ExternalDefinition.ParamCount
	lea TemplateState,a0
	lea ScopeState,a1
	lea Definition,a2
	jsr templates.externalBegin
	move.l d0,Summary+8
	tst.l d0
	beq.w bad
	moveq #0,d0
	move.w TemplateState+templates.State.ExternalCount,d0
	move.l d0,Summary+4
	; Carriers must not claim the macro namespace or create index entries.
	move.l #PHASE_VISIBLE,Phase
	tst.w TemplateState+templates.State.Count
	bne.w bad
	lea NameToken,a0
	lea ScopeState,a1
	jsr scopes.declareTemplate
	bne.w bad
	move.l #PHASE_SUMMARY,Phase
	lea Summary,a0
	moveq #12,d0
	bsr.w writeSpan
	bne.w bad
	moveq #1,d6
invoke
	move.w d6,Call+templates.ExternalCall.Definition
	cmpi.w #6,d6
	bne.w selectedBody
	move.w #4,Call+templates.ExternalCall.Definition
selectedBody
	clr.w Call+templates.ExternalCall.ArgCount
	move.l #Number,Arguments+templates.ExternalArgument.Packed
	move.w #writer.NUMBER_BYTES,Arguments+templates.ExternalArgument.PackedBytes
	move.l #Spelling,Arguments+templates.ExternalArgument.Spelling
	move.w #4,Arguments+templates.ExternalArgument.SpellingBytes
	cmpi.w #6,d6
	bne.w nonemptyArgument
	clr.l Arguments+templates.ExternalArgument.Packed
	clr.w Arguments+templates.ExternalArgument.PackedBytes
	clr.l Arguments+templates.ExternalArgument.Spelling
	clr.w Arguments+templates.ExternalArgument.SpellingBytes
	bra.w argumentReady
nonemptyArgument
	cmpi.w #4,d6
	bne.w argumentReady
	move.l #StringArgument,Arguments+templates.ExternalArgument.Packed
	move.w #6,Arguments+templates.ExternalArgument.PackedBytes
argumentReady
	cmpi.w #6,d6
	beq.w oneArgument
	cmpi.w #1,d6
	beq.w oneArgument
	cmpi.w #4,d6
	beq.w oneArgument
	cmpi.w #5,d6
	bne.w invokeReady
	move.w #9,Call+templates.ExternalCall.ArgCount
	bra.w invokeReady
oneArgument
	move.w #1,Call+templates.ExternalCall.ArgCount
invokeReady
	move.l #PHASE_INVOKE,Phase
	add.w d6,Phase+2
	lea TemplateState,a0
	lea Call,a1
	jsr templates.externalInvoke
	bne.w bad
	move.l #PHASE_NEXT,Phase
	lea RecordOutput,a1
	lea ScopeState,a2
	jsr templates.next
	bne.w bad
	move.l #PHASE_ORIGIN,Phase
	cmpi.l #$12345678,TemplateState+templates.State.Origin
	bne.w bad
	move.l #PHASE_NONEMPTY,Phase
	tst.l d1
	beq.w bad
	move.l d1,RecordBytes
	move.l #PHASE_PREFIX,Phase
	lea RecordBytes,a0
	moveq #4,d0
	bsr.w writeSpan
	bne.w bad
	move.l #PHASE_RECORD,Phase
	lea RecordOutput,a0
	move.l RecordBytes,d0
	bsr.w writeSpan
	bne.w bad
	move.l #PHASE_EXHAUST,Phase
	add.w d6,Phase+2
	lea TemplateState,a0
	lea RecordOutput,a1
	lea ScopeState,a2
	jsr templates.next
	bne.w bad
	tst.l d1
	bne.w bad
	addq.w #1,d6
	cmpi.w #7,d6
	blo.w invoke
	lea TemplateState,a0
	jsr templates.finish
	lea PlanState,a0
	jsr plans.finish
	lea ScopeState,a0
	jsr scopes.finish
	moveq #0,d0
	rts
bad
	move.l Phase,d0
	rts
	.bend ; cases

; A0/D0=output span. D0=status/CCR; others preserved. Exact partial-write loop.
writeSpan	.block
	movem.l d1-d5/a0-a6, -(sp)
	movea.l a0, a3
	move.l d0, d5
next
	tst.l d5
	beq.w valid
	movea.l DosBase, a6
	move.l OutputHandle, d1
	move.l a3, d2
	move.l d5, d3
	jsr DOS_WRITE(a6)
	tst.l d0
	ble.w fail
	cmp.l d5, d0
	bhi.w fail
	adda.l d0, a3
	sub.l d0, d5
	bra.w next
valid
	moveq #0, d0
	bra.w done
fail
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1-d5/a0-a6
	rts
	.bend  ; writeSpan
	.endsection
.section data,kind=data
DosName .byte "dos.library",0
OutputName .byte "Work:statement-templates-results.bin",0
Keyword .byte "T"
Formal .byte "x"
LabelName .byte "label"
.align 4
NameToken .byte abi.PRVM_TOKEN_KIND_IDENTIFIER,0,100,0
Definition
	.word 100,1
	.for i in 0..templates.PARAM_LIMIT
	.word 101
	.endfor
	.long $12345678,0
Call
	.word 1,100,77,1
	.byte abi.PRVM_TOKEN_KIND_IDENTIFIER,0,102,0
	.word 1
	.long Arguments,0
	.word 0
Arguments
	.for i in 0..templates.PARAM_LIMIT
	.long Number
	.word writer.NUMBER_BYTES
	.long Spelling
	.word 4
	.endfor
Number .byte abi.PRVM_TOKEN_KIND_NUMBER,0,0,0,184
StringArgument .byte abi.PRVM_TOKEN_KIND_STRING,4,"0B8H"
Spelling .byte "0B8H"
Blank .byte writer.RECORD_HEADER_BYTES-1,writer.FLAG_INDENT,0,12
NumericRecord .byte 8,writer.FLAG_INDENT,0,13,abi.PRVM_TOKEN_KIND_DOT,abi.PRVM_TOKEN_KIND_IDENTIFIER,0,101,0
TextRecord .byte 10,writer.FLAG_INDENT,0,14,abi.PRVM_TOKEN_KIND_STRING,5,".x:@1"
.align 4
Generated
	.long PlanState,FormalRows,2,FormalRows,2,Formal,1,0,0,0
FormalRows
	.word abi.PRVM_RESULT_MACRO_LINE,0
	.long 0,0,0,1,0,1,-1
	.word abi.PRVM_RESULT_MACRO_FORMAL,0
	.long 0,0,0,1,-1,-1,0
Summary .long 6,0,0
.endsection
.section bss,kind=bss
.align 4
DosBase .res long,1
OutputHandle .res long,1
ReturnCode .res long,1
RecordBytes .res long,1
PlanHandle .res long,1
Phase .res long,1
RecordOutput .res byte,256
TemplateState .res byte,templates.SCRATCH_BYTES
ScopeState .res byte,scopes.SCRATCH_BYTES
PlanState .res byte,plans.STATE_BYTES
.endsection
.output "statement-templates.hunk",format=hunk,sections=entry,code,data,bss
.endmodule
