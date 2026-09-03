; Bounded observation-only VM and service execution profile.
;
; @opforge-owner: debug.amigaos.runtime_profile
; @opforge-slice: documentation/plans/slices/native-porting-slice-runtime-execution-v1.toml
; @opforge-role: implementation

	.module debug.amigaos.runtime_profile
	.cpu 68020

	.pub

OPFORGE_RUNTIME_MAGIC                    = $4f465645; "OFVE"
OPFORGE_RUNTIME_SCHEMA_VERSION           = 1
OPFORGE_RUNTIME_RECORD_BYTES             = 192

OPFORGE_RUNTIME_FLAG_ACTIVE              = 1
OPFORGE_RUNTIME_FLAG_COMPLETE            = 2
OPFORGE_RUNTIME_FLAG_INCOMPLETE          = 4

OPFORGE_RUNTIME_VM_TKVM                  = 1
OPFORGE_RUNTIME_VM_PRVM                  = 2
OPFORGE_RUNTIME_VM_EXVM                  = 3
OPFORGE_RUNTIME_VM_EXPRVM                = 4
OPFORGE_RUNTIME_VM_COUNT                 = 4

OPFORGE_RUNTIME_PROGRAM_TOKENIZER        = 1
OPFORGE_RUNTIME_PROGRAM_PARSER           = 2
OPFORGE_RUNTIME_PROGRAM_EXPRESSION_FRONTEND = 3
OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR = 4
OPFORGE_RUNTIME_PROGRAM_COUNT            = 4
OPFORGE_RUNTIME_VM_STACK_CAPACITY        = 4

OPFORGE_RUNTIME_SERVICE_EXPRESSION       = 1
OPFORGE_RUNTIME_SERVICE_SELECTION        = 2
OPFORGE_RUNTIME_SERVICE_ENCODING         = 3
OPFORGE_RUNTIME_SERVICE_OPERAND          = 4
OPFORGE_RUNTIME_SERVICE_STATE            = 5
OPFORGE_RUNTIME_SERVICE_BRANCH           = 6
OPFORGE_RUNTIME_SERVICE_FIXUP            = 7
OPFORGE_RUNTIME_SERVICE_VALUE            = 8
OPFORGE_RUNTIME_SERVICE_COUNT            = 8
OPFORGE_RUNTIME_SERVICE_STACK_CAPACITY   = 4

OPFORGE_RUNTIME_CANDIDATE_SELECTION      = 1
OPFORGE_RUNTIME_CANDIDATE_ENCODING       = 2

OPFORGE_RUNTIME_OVERFLOW_INVOCATIONS     = 1
OPFORGE_RUNTIME_OVERFLOW_OPCODES         = 2
OPFORGE_RUNTIME_OVERFLOW_SERVICES        = 4
OPFORGE_RUNTIME_OVERFLOW_CANDIDATES      = 8
OPFORGE_RUNTIME_OVERFLOW_UNKNOWN_ID      = 16
OPFORGE_RUNTIME_OVERFLOW_PHASE           = 32

OPFORGE_RUNTIME_MAGIC_OFFSET             = 0
OPFORGE_RUNTIME_SCHEMA_OFFSET            = 4
OPFORGE_RUNTIME_FLAGS_OFFSET             = 6
OPFORGE_RUNTIME_RUN_ID_OFFSET            = 8
OPFORGE_RUNTIME_PHASE_OFFSET             = 12
OPFORGE_RUNTIME_PASS_OFFSET              = 14
OPFORGE_RUNTIME_CURRENT_VM_OFFSET        = 16
OPFORGE_RUNTIME_CURRENT_PROGRAM_OFFSET   = 18
OPFORGE_RUNTIME_CURRENT_SERVICE_OFFSET   = 20
OPFORGE_RUNTIME_VM_INVOCATIONS_OFFSET    = 24
OPFORGE_RUNTIME_VM_OPCODES_OFFSET        = 40
OPFORGE_RUNTIME_PROGRAM_INVOCATIONS_OFFSET = 56
OPFORGE_RUNTIME_PROGRAM_OPCODES_OFFSET   = 72
OPFORGE_RUNTIME_SERVICE_INVOCATIONS_OFFSET = 88
OPFORGE_RUNTIME_SELECTION_CANDIDATES_OFFSET = 120
OPFORGE_RUNTIME_ENCODING_CANDIDATES_OFFSET = 124
OPFORGE_RUNTIME_OVERFLOW_OFFSET          = 128
OPFORGE_RUNTIME_EXIT_STATUS_OFFSET       = 132
OPFORGE_RUNTIME_OPCODE_PHASE_OFFSET      = 136
OPFORGE_RUNTIME_SERVICE_PHASE_OFFSET     = 152

OPFORGE_RUNTIME_PHASE_PASS_ONE           = 5
OPFORGE_RUNTIME_PHASE_LAYOUT             = 6
OPFORGE_RUNTIME_PHASE_FINAL              = 7

	.section code, kind=code

; Return the authoritative fixed-size runtime record.
; Inputs: none. Output: A0 = 192-byte record. Other registers and CCR preserved.
opforgeRuntimeProfileGetRecordV1	.block
	lea OpforgeRuntimeRecord, a0
	rts
	.bend  ; opforgeRuntimeProfileGetRecordV1

; Start one correlated runtime profile and bind the existing OFPR context slots.
; Inputs: D0.L = run ID; A0 = OFPR VM/service field; A1 = OFPR program field.
; Outputs/clobbers: none; all registers and CCR preserved; stack delta zero.
opforgeRuntimeProfileBeginRunV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	move.l d0, d7
	move.l a0, OpforgeRuntimeProgressVmServicePtr
	move.l a1, OpforgeRuntimeProgressProgramPtr
	lea OpforgeRuntimeRecord, a5
	movea.l a5, a0
	moveq #0, d0
	moveq #(OPFORGE_RUNTIME_RECORD_BYTES / 4) - 1, d1
clearLoop
	move.l d0, (a0)+
	dbf d1, clearLoop
	move.l #OPFORGE_RUNTIME_MAGIC, OPFORGE_RUNTIME_MAGIC_OFFSET(a5)
	move.w #OPFORGE_RUNTIME_SCHEMA_VERSION, OPFORGE_RUNTIME_SCHEMA_OFFSET(a5)
	move.w #OPFORGE_RUNTIME_FLAG_ACTIVE, OPFORGE_RUNTIME_FLAGS_OFFSET(a5)
	move.l d7, OPFORGE_RUNTIME_RUN_ID_OFFSET(a5)
	clr.w OpforgeRuntimeVmDepth.l
	clr.w OpforgeRuntimeServiceDepth.l
	bsr.w publishContext
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileBeginRunV1

; Retain the current progress phase/pass for coarse attribution.
; Inputs: D0.W = phase; D1.W = pass. Outputs/clobbers: none; CCR preserved.
opforgeRuntimeProfileSetContextV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgeRuntimeRecord, a5
	bsr.w profileIsActive
	beq.s return
	move.w d0, OPFORGE_RUNTIME_PHASE_OFFSET(a5)
	move.w d1, OPFORGE_RUNTIME_PASS_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileSetContextV1

; Count and enter one VM/program pair.
; Inputs: D0.W = VM ID; D1.W = program ID. Outputs/clobbers: none; CCR preserved.
opforgeRuntimeProfileEnterVmV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgeRuntimeRecord, a5
	bsr.w profileIsActive
	beq.w return
	moveq #0, d4
	move.w d0, d4
	moveq #0, d5
	move.w d1, d5
	tst.w d4
	beq.s unknown
	cmpi.w #OPFORGE_RUNTIME_VM_COUNT, d4
	bhi.s unknown
	tst.w d5
	beq.s unknown
	cmpi.w #OPFORGE_RUNTIME_PROGRAM_COUNT, d5
	bhi.s unknown
	moveq #0, d6
	move.w OpforgeRuntimeVmDepth.l, d6
	cmpi.w #OPFORGE_RUNTIME_VM_STACK_CAPACITY, d6
	bhs.s unknown
	lsl.w #2, d6
	lea OpforgeRuntimeVmStack, a0
	move.w OPFORGE_RUNTIME_CURRENT_VM_OFFSET(a5), d2
	move.w d2, 0(a0,d6.w)
	move.w OPFORGE_RUNTIME_CURRENT_PROGRAM_OFFSET(a5), d2
	move.w d2, 2(a0,d6.w)
	addq.w #1, OpforgeRuntimeVmDepth.l
	move.w d4, OPFORGE_RUNTIME_CURRENT_VM_OFFSET(a5)
	move.w d5, OPFORGE_RUNTIME_CURRENT_PROGRAM_OFFSET(a5)
	subq.w #1, d4
	lsl.w #2, d4
	lea OPFORGE_RUNTIME_VM_INVOCATIONS_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_RUNTIME_OVERFLOW_INVOCATIONS, d0
	bsr.w increment
	subq.w #1, d5
	lsl.w #2, d5
	lea OPFORGE_RUNTIME_PROGRAM_INVOCATIONS_OFFSET(a5), a0
	adda.w d5, a0
	moveq #OPFORGE_RUNTIME_OVERFLOW_INVOCATIONS, d0
	bsr.w increment
	bsr.w publishContext
	bra.s return
unknown
	ori.l #OPFORGE_RUNTIME_OVERFLOW_UNKNOWN_ID, OPFORGE_RUNTIME_OVERFLOW_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileEnterVmV1

; Leave the active VM while retaining any enclosing service context.
; Inputs: none. Outputs/clobbers: none; all registers and CCR preserved.
opforgeRuntimeProfileLeaveVmV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgeRuntimeRecord, a5
	moveq #0, d4
	move.w OpforgeRuntimeVmDepth.l, d4
	beq.s clear
	subq.w #1, d4
	move.w d4, OpforgeRuntimeVmDepth.l
	lsl.w #2, d4
	lea OpforgeRuntimeVmStack, a0
	move.w 0(a0,d4.w), d5
	move.w d5, OPFORGE_RUNTIME_CURRENT_VM_OFFSET(a5)
	move.w 2(a0,d4.w), d5
	move.w d5, OPFORGE_RUNTIME_CURRENT_PROGRAM_OFFSET(a5)
	bra.s publish
clear
	clr.w OPFORGE_RUNTIME_CURRENT_VM_OFFSET(a5)
	clr.w OPFORGE_RUNTIME_CURRENT_PROGRAM_OFFSET(a5)
publish
	bsr.w publishContext
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileLeaveVmV1

; Count one executed opcode for a stable VM/program pair and current phase.
; Inputs: D0.W = VM ID; D1.W = program ID. Outputs/clobbers: none; CCR preserved.
opforgeRuntimeProfileRecordOpcodeV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgeRuntimeRecord, a5
	bsr.w profileIsActive
	beq.w return
	moveq #0, d4
	move.w d0, d4
	moveq #0, d5
	move.w d1, d5
	tst.w d4
	beq.s unknown
	cmpi.w #OPFORGE_RUNTIME_VM_COUNT, d4
	bhi.s unknown
	tst.w d5
	beq.s unknown
	cmpi.w #OPFORGE_RUNTIME_PROGRAM_COUNT, d5
	bhi.s unknown
	subq.w #1, d4
	lsl.w #2, d4
	lea OPFORGE_RUNTIME_VM_OPCODES_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_RUNTIME_OVERFLOW_OPCODES, d0
	bsr.w increment
	subq.w #1, d5
	lsl.w #2, d5
	lea OPFORGE_RUNTIME_PROGRAM_OPCODES_OFFSET(a5), a0
	adda.w d5, a0
	moveq #OPFORGE_RUNTIME_OVERFLOW_OPCODES, d0
	bsr.w increment
	bsr.w phaseIndex
	lea OPFORGE_RUNTIME_OPCODE_PHASE_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_RUNTIME_OVERFLOW_PHASE, d0
	bsr.w increment
	bra.s return
unknown
	ori.l #OPFORGE_RUNTIME_OVERFLOW_UNKNOWN_ID, OPFORGE_RUNTIME_OVERFLOW_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileRecordOpcodeV1

; Count and enter one CPU-neutral service category.
; Inputs: D0.W = service ID. Outputs/clobbers: none; CCR preserved.
opforgeRuntimeProfileEnterServiceV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgeRuntimeRecord, a5
	bsr.w profileIsActive
	beq.w return
	moveq #0, d4
	move.w d0, d4
	tst.w d4
	beq.s unknown
	cmpi.w #OPFORGE_RUNTIME_SERVICE_COUNT, d4
	bhi.s unknown
	moveq #0, d5
	move.w OpforgeRuntimeServiceDepth.l, d5
	cmpi.w #OPFORGE_RUNTIME_SERVICE_STACK_CAPACITY, d5
	bhs.s unknown
	add.w d5, d5
	lea OpforgeRuntimeServiceStack, a0
	move.w OPFORGE_RUNTIME_CURRENT_SERVICE_OFFSET(a5), d6
	move.w d6, 0(a0,d5.w)
	addq.w #1, OpforgeRuntimeServiceDepth.l
	move.w d4, OPFORGE_RUNTIME_CURRENT_SERVICE_OFFSET(a5)
	subq.w #1, d4
	lsl.w #2, d4
	lea OPFORGE_RUNTIME_SERVICE_INVOCATIONS_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_RUNTIME_OVERFLOW_SERVICES, d0
	bsr.w increment
	bsr.w phaseIndex
	lea OPFORGE_RUNTIME_SERVICE_PHASE_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_RUNTIME_OVERFLOW_PHASE, d0
	bsr.w increment
	bsr.w publishContext
	bra.s return
unknown
	ori.l #OPFORGE_RUNTIME_OVERFLOW_UNKNOWN_ID, OPFORGE_RUNTIME_OVERFLOW_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileEnterServiceV1

; Leave the active service context.
; Inputs: none. Outputs/clobbers: none; all registers and CCR preserved.
opforgeRuntimeProfileLeaveServiceV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgeRuntimeRecord, a5
	moveq #0, d4
	move.w OpforgeRuntimeServiceDepth.l, d4
	beq.s clear
	subq.w #1, d4
	move.w d4, OpforgeRuntimeServiceDepth.l
	add.w d4, d4
	lea OpforgeRuntimeServiceStack, a0
	move.w 0(a0,d4.w), d5
	move.w d5, OPFORGE_RUNTIME_CURRENT_SERVICE_OFFSET(a5)
	bra.s publish
clear
	clr.w OPFORGE_RUNTIME_CURRENT_SERVICE_OFFSET(a5)
publish
	bsr.w publishContext
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileLeaveServiceV1

; Count one selector or encoder candidate attempt.
; Inputs: D0.W = OPFORGE_RUNTIME_CANDIDATE_*. Outputs/clobbers: none; CCR preserved.
opforgeRuntimeProfileRecordCandidateV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgeRuntimeRecord, a5
	bsr.w profileIsActive
	beq.s return
	cmpi.w #OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	beq.s selection
	cmpi.w #OPFORGE_RUNTIME_CANDIDATE_ENCODING, d0
	beq.s encoding
	ori.l #OPFORGE_RUNTIME_OVERFLOW_UNKNOWN_ID, OPFORGE_RUNTIME_OVERFLOW_OFFSET(a5)
	bra.s return
selection
	lea OPFORGE_RUNTIME_SELECTION_CANDIDATES_OFFSET(a5), a0
	bra.s count
encoding
	lea OPFORGE_RUNTIME_ENCODING_CANDIDATES_OFFSET(a5), a0
count
	moveq #OPFORGE_RUNTIME_OVERFLOW_CANDIDATES, d0
	bsr.w increment
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileRecordCandidateV1

; Seal the correlated record and clear bound OFPR current-context fields.
; Inputs: D0.L = guest/CLI status. Outputs/clobbers: none; CCR preserved.
opforgeRuntimeProfileFinishV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgeRuntimeRecord, a5
	move.l d0, OPFORGE_RUNTIME_EXIT_STATUS_OFFSET(a5)
	andi.w #$fffe, OPFORGE_RUNTIME_FLAGS_OFFSET(a5)
	tst.l d0
	bne.s incomplete
	ori.w #OPFORGE_RUNTIME_FLAG_COMPLETE, OPFORGE_RUNTIME_FLAGS_OFFSET(a5)
	bra.s terminal
incomplete
	ori.w #OPFORGE_RUNTIME_FLAG_INCOMPLETE, OPFORGE_RUNTIME_FLAGS_OFFSET(a5)
terminal
	clr.w OpforgeRuntimeVmDepth.l
	clr.w OpforgeRuntimeServiceDepth.l
	clr.w OPFORGE_RUNTIME_CURRENT_VM_OFFSET(a5)
	clr.w OPFORGE_RUNTIME_CURRENT_PROGRAM_OFFSET(a5)
	clr.w OPFORGE_RUNTIME_CURRENT_SERVICE_OFFSET(a5)
	bsr.w publishContext
	clr.l OpforgeRuntimeProgressVmServicePtr
	clr.l OpforgeRuntimeProgressProgramPtr
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeRuntimeProfileFinishV1

	.priv

; Return Z clear when the record is active. A5 points at the record.
profileIsActive	.block
	move.w OPFORGE_RUNTIME_FLAGS_OFFSET(a5), d3
	andi.w #OPFORGE_RUNTIME_FLAG_ACTIVE, d3
	rts
	.bend  ; profileIsActive

; Saturating increment. A0 = field; D0.L = overflow bit; A5 = record.
increment	.block
	cmpi.l #-1, (a0)
	beq.s overflow
	addq.l #1, (a0)
	rts
overflow
	or.l d0, OPFORGE_RUNTIME_OVERFLOW_OFFSET(a5)
	rts
	.bend  ; increment

; Return D4.W as a byte offset for pass-one/layout/final/other phase buckets.
phaseIndex	.block
	moveq #0, d4
	move.w OPFORGE_RUNTIME_PHASE_OFFSET(a5), d3
	cmpi.w #OPFORGE_RUNTIME_PHASE_PASS_ONE, d3
	beq.s ready
	moveq #1, d4
	cmpi.w #OPFORGE_RUNTIME_PHASE_LAYOUT, d3
	beq.s ready
	moveq #2, d4
	cmpi.w #OPFORGE_RUNTIME_PHASE_FINAL, d3
	beq.s ready
	moveq #3, d4
ready
	lsl.w #2, d4
	rts
	.bend  ; phaseIndex

; Publish the current VM/service/program IDs into the bound OFPR fields.
publishContext	.block
	moveq #0, d0
	move.w OPFORGE_RUNTIME_CURRENT_VM_OFFSET(a5), d0
	swap d0
	move.w OPFORGE_RUNTIME_CURRENT_SERVICE_OFFSET(a5), d0
	movea.l OpforgeRuntimeProgressVmServicePtr, a0
	move.l a0, d1
	beq.s program
	move.l d0, (a0)
program
	moveq #0, d0
	move.w OPFORGE_RUNTIME_CURRENT_PROGRAM_OFFSET(a5), d0
	movea.l OpforgeRuntimeProgressProgramPtr, a0
	move.l a0, d1
	beq.s return
	move.l d0, (a0)
return
	rts
	.bend  ; publishContext

	.endsection

	.section bss, kind=bss
	.align 4
OpforgeRuntimeRecord
	.res byte, OPFORGE_RUNTIME_RECORD_BYTES
OpforgeRuntimeProgressVmServicePtr
	.res long, 1
OpforgeRuntimeProgressProgramPtr
	.res long, 1
OpforgeRuntimeVmDepth
	.res word, 1
OpforgeRuntimeVmStack
	.res word, OPFORGE_RUNTIME_VM_STACK_CAPACITY * 2
OpforgeRuntimeServiceDepth
	.res word, 1
OpforgeRuntimeServiceStack
	.res word, OPFORGE_RUNTIME_SERVICE_STACK_CAPACITY
	.endsection

	.endmodule
