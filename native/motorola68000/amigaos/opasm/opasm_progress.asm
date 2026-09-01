; Bounded observation-only progress for native assembly sessions.
;
; @opforge-owner: opasm.amigaos.progress
; @opforge-slice: documentation/plans/slices/native-porting-slice-bounded-progress-v1.toml
; @opforge-role: implementation

	.module opasm.amigaos.progress
	.cpu 68020

	.use opforge.debug.contracts as contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"

	.pub

OPASM_PROGRESS_MAGIC                = $4f465052; "OFPR"
OPASM_PROGRESS_SCHEMA_VERSION       = 1
OPASM_PROGRESS_RECORD_BYTES         = 128

OPASM_PROGRESS_FLAG_ACTIVE          = 1
OPASM_PROGRESS_FLAG_COMPLETE        = 2
OPASM_PROGRESS_FLAG_INCOMPLETE      = 4
OPASM_PROGRESS_FLAG_ABORT_REQUESTED = 8
OPASM_PROGRESS_FLAG_HEARTBEAT       = 16

OPASM_PROGRESS_OVERFLOW_VISITS      = 1
OPASM_PROGRESS_OVERFLOW_PHASE_TICKS = 2

OPASM_PROGRESS_PHASE_IDLE           = 0
OPASM_PROGRESS_PHASE_STARTUP        = 1
OPASM_PROGRESS_PHASE_PACKAGE        = 2
OPASM_PROGRESS_PHASE_FRONTEND       = 3
OPASM_PROGRESS_PHASE_STATEMENT_BUILD = 4
OPASM_PROGRESS_PHASE_PASS_ONE       = 5
OPASM_PROGRESS_PHASE_LAYOUT         = 6
OPASM_PROGRESS_PHASE_FINAL_EMISSION = 7
OPASM_PROGRESS_PHASE_ARTIFACTS      = 8
OPASM_PROGRESS_PHASE_COUNT          = 8

OPASM_PROGRESS_MAGIC_OFFSET         = 0
OPASM_PROGRESS_SCHEMA_OFFSET        = 4
OPASM_PROGRESS_FLAGS_OFFSET         = 6
OPASM_PROGRESS_RUN_ID_OFFSET        = 8
OPASM_PROGRESS_PHASE_OFFSET         = 12
OPASM_PROGRESS_PASS_OFFSET          = 14
OPASM_PROGRESS_LAYOUT_ROUND_OFFSET  = 16
OPASM_PROGRESS_CURRENT_STMT_OFFSET  = 20
OPASM_PROGRESS_LAST_STMT_OFFSET     = 24
OPASM_PROGRESS_TOTAL_STMTS_OFFSET   = 28
OPASM_PROGRESS_VISITS_OFFSET        = 32
OPASM_PROGRESS_SOURCE_ID_OFFSET     = 36
OPASM_PROGRESS_MODULE_ID_OFFSET     = 40
OPASM_PROGRESS_VM_SERVICE_ID_OFFSET = 44
OPASM_PROGRESS_PROGRAM_ID_OFFSET    = 48
OPASM_PROGRESS_FLOW_REDIRECTS_OFFSET = 52
OPASM_PROGRESS_BACK_REDIRECTS_OFFSET = 56
OPASM_PROGRESS_LAST_TICK_OFFSET     = 60
OPASM_PROGRESS_RUN_START_TICK_OFFSET = 64
OPASM_PROGRESS_TOTAL_TICKS_OFFSET   = 68
OPASM_PROGRESS_PHASE_START_TICK_OFFSET = 72
OPASM_PROGRESS_PHASE_TICKS_OFFSET  = 76
OPASM_PROGRESS_HEARTBEAT_QUANTUM_OFFSET = 108
OPASM_PROGRESS_NEXT_HEARTBEAT_OFFSET = 112
OPASM_PROGRESS_ABORT_VISITS_OFFSET = 116
OPASM_PROGRESS_OVERFLOW_OFFSET      = 120
OPASM_PROGRESS_EXIT_STATUS_OFFSET   = 124
OPASM_PROGRESS_TICK_CALLBACK_OFFSET = 128
OPASM_PROGRESS_FALLBACK_TICK_OFFSET = 132

	.section code, kind=code

; Return the authoritative record pointer for a debugger/export harness.
; Inputs: none. Output: A0 = 128-byte record. D0-D7/A1-A6 and CCR preserved.
opasmProgressGetRecordV1	.block
	lea OpasmProgressRecord, a0
	rts
	.bend  ; opasmProgressGetRecordV1

; Start one fresh bounded progress record.
; Inputs: A0 = tick callback returning a monotonically wrapping D0.L value.
; Outputs/clobbers: none. CCR and D0-D7/A0-A6 are preserved; stack delta zero.
opasmProgressBeginRunV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpasmProgressRecord, a5
	move.l a0, OPASM_PROGRESS_TICK_CALLBACK_OFFSET(a5)
	clr.l OPASM_PROGRESS_FALLBACK_TICK_OFFSET(a5)
	movea.l a5, a0
	moveq #0, d0
	moveq #(OPASM_PROGRESS_RECORD_BYTES / 4) - 1, d1
clearLoop
	move.l d0, (a0)+
	dbf d1, clearLoop
	move.l #OPASM_PROGRESS_MAGIC, OPASM_PROGRESS_MAGIC_OFFSET(a5)
	move.w #OPASM_PROGRESS_SCHEMA_VERSION, OPASM_PROGRESS_SCHEMA_OFFSET(a5)
	move.w #OPASM_PROGRESS_FLAG_ACTIVE, OPASM_PROGRESS_FLAGS_OFFSET(a5)
	move.l #-1, OPASM_PROGRESS_CURRENT_STMT_OFFSET(a5)
	move.l #-1, OPASM_PROGRESS_LAST_STMT_OFFSET(a5)
	bsr.w sampleTick
	move.l d0, OPASM_PROGRESS_RUN_ID_OFFSET(a5)
	move.l d0, OPASM_PROGRESS_LAST_TICK_OFFSET(a5)
	move.l d0, OPASM_PROGRESS_RUN_START_TICK_OFFSET(a5)
	move.l d0, OPASM_PROGRESS_PHASE_START_TICK_OFFSET(a5)
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opasmProgressBeginRunV1

; Enter one coarse phase/pass/layout boundary and charge the previous phase.
; Inputs: D0.W = phase, D1.W = pass, D2.W = layout round (zero otherwise).
; Outputs/clobbers: none. CCR and D0-D7/A0-A6 are preserved; stack delta zero.
opasmProgressSetPhaseV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpasmProgressRecord, a5
	move.w OPASM_PROGRESS_FLAGS_OFFSET(a5), d7
	andi.w #OPASM_PROGRESS_FLAG_ACTIVE, d7
	beq.s return
	move.w d0, d4
	move.w d1, d5
	move.w d2, d6
	bsr.w sampleAndChargePhase
	move.w d4, OPASM_PROGRESS_PHASE_OFFSET(a5)
	move.w d5, OPASM_PROGRESS_PASS_OFFSET(a5)
	move.w d6, OPASM_PROGRESS_LAYOUT_ROUND_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opasmProgressSetPhaseV1

; Record one statement-loop visit without sampling the clock.
; Inputs: D0.L = current statement, D1.L = total statements.
; Outputs/clobbers: none. CCR and D0-D7/A0-A6 are preserved; stack delta zero.
opasmProgressStatementBeginV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpasmProgressRecord, a5
	move.w OPASM_PROGRESS_FLAGS_OFFSET(a5), d7
	andi.w #OPASM_PROGRESS_FLAG_ACTIVE, d7
	beq.w return
	move.l d0, OPASM_PROGRESS_CURRENT_STMT_OFFSET(a5)
	move.l d1, OPASM_PROGRESS_TOTAL_STMTS_OFFSET(a5)
	move.l OPASM_PROGRESS_VISITS_OFFSET(a5), d2
	cmpi.l #-1, d2
	beq.s visitOverflow
	addq.l #1, d2
	move.l d2, OPASM_PROGRESS_VISITS_OFFSET(a5)
	bra.s heartbeat
visitOverflow
	ori.l #OPASM_PROGRESS_OVERFLOW_VISITS, OPASM_PROGRESS_OVERFLOW_OFFSET(a5)
heartbeat
	move.w OPASM_PROGRESS_FLAGS_OFFSET(a5), d7
	andi.w #OPASM_PROGRESS_FLAG_HEARTBEAT, d7
	beq.s return
	move.l OPASM_PROGRESS_NEXT_HEARTBEAT_OFFSET(a5), d3
	cmp.l d3, d2
	blo.s return
	bsr.w sampleTick
	move.l d0, OPASM_PROGRESS_LAST_TICK_OFFSET(a5)
	moveq #0, d1
	moveq #1, d2
	moveq #0, d3
	move.w OPASM_PROGRESS_PHASE_OFFSET(a5), d3
	swap d3
	move.w OPASM_PROGRESS_PASS_OFFSET(a5), d3
	move.l OPASM_PROGRESS_CURRENT_STMT_OFFSET(a5), d4
	move.l OPASM_PROGRESS_TOTAL_STMTS_OFFSET(a5), d5
	move.l OPASM_PROGRESS_VISITS_OFFSET(a5), d6
	.DEBUG_EVENT_U32X4 contracts.EVENT_ASSEMBLY_PROGRESS
	move.l OPASM_PROGRESS_NEXT_HEARTBEAT_OFFSET(a5), d2
	add.l OPASM_PROGRESS_HEARTBEAT_QUANTUM_OFFSET(a5), d2
	bcc.s storeNext
	move.l #-1, d2
	ori.l #OPASM_PROGRESS_OVERFLOW_VISITS, OPASM_PROGRESS_OVERFLOW_OFFSET(a5)
storeNext
	move.l d2, OPASM_PROGRESS_NEXT_HEARTBEAT_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opasmProgressStatementBeginV1

; Mark one statement visit completed.
; Inputs: D0.L = completed statement. Outputs/clobbers: none; CCR preserved.
opasmProgressStatementCompleteV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpasmProgressRecord, a5
	move.w OPASM_PROGRESS_FLAGS_OFFSET(a5), d1
	andi.w #OPASM_PROGRESS_FLAG_ACTIVE, d1
	beq.s return
	move.l d0, OPASM_PROGRESS_LAST_STMT_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opasmProgressStatementCompleteV1

; Enable a bounded heartbeat quantum. Zero disables it.
; Inputs: D0.L = visit quantum. Outputs/clobbers: none; CCR preserved.
opasmProgressSetHeartbeatV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpasmProgressRecord, a5
	move.l d0, OPASM_PROGRESS_HEARTBEAT_QUANTUM_OFFSET(a5)
	move.l d0, OPASM_PROGRESS_NEXT_HEARTBEAT_OFFSET(a5)
	beq.s disable
	ori.w #OPASM_PROGRESS_FLAG_HEARTBEAT, OPASM_PROGRESS_FLAGS_OFFSET(a5)
	bra.s return
disable
	andi.w #$ffef, OPASM_PROGRESS_FLAGS_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opasmProgressSetHeartbeatV1

; Configure the optional graceful diagnostic visit limit. Zero disables it.
; Inputs: D0.L = visit limit. Outputs/clobbers: none; CCR preserved.
opasmProgressSetAbortVisitsV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpasmProgressRecord, a5
	move.l d0, OPASM_PROGRESS_ABORT_VISITS_OFFSET(a5)
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opasmProgressSetAbortVisitsV1

; Query the optional diagnostic stop after a statement visit was recorded.
; Inputs: none. Outputs: D0 = zero continue, one abort. Other registers saved.
; CCR reflects D0; stack delta zero.
opasmProgressAbortRequestedV1	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea OpasmProgressRecord, a5
	move.l OPASM_PROGRESS_ABORT_VISITS_OFFSET(a5), d1
	beq.s continue
	cmp.l OPASM_PROGRESS_VISITS_OFFSET(a5), d1
	bhi.s continue
	ori.w #OPASM_PROGRESS_FLAG_ABORT_REQUESTED, OPASM_PROGRESS_FLAGS_OFFSET(a5)
	moveq #1, d0
	bra.s return
continue
	moveq #0, d0
return
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; opasmProgressAbortRequestedV1

; Seal the record once at the controlled CLI boundary.
; Inputs: D0.L = guest/CLI status. Outputs/clobbers: none; CCR preserved.
opasmProgressFinishV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpasmProgressRecord, a5
	move.w OPASM_PROGRESS_FLAGS_OFFSET(a5), d7
	andi.w #OPASM_PROGRESS_FLAG_ACTIVE, d7
	beq.s return
	move.l d0, d6
	bsr.w sampleAndChargePhase
	move.l d6, OPASM_PROGRESS_EXIT_STATUS_OFFSET(a5)
	andi.w #$fffe, OPASM_PROGRESS_FLAGS_OFFSET(a5)
	tst.l d6
	bne.s incomplete
	ori.w #OPASM_PROGRESS_FLAG_COMPLETE, OPASM_PROGRESS_FLAGS_OFFSET(a5)
	bra.s return
incomplete
	ori.w #OPASM_PROGRESS_FLAG_INCOMPLETE, OPASM_PROGRESS_FLAGS_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opasmProgressFinishV1

	.priv

; Sample the registered callback. A missing callback advances a deterministic
; harness-only fallback tick; production always supplies DateStamp.
sampleTick	.block
	movea.l OPASM_PROGRESS_TICK_CALLBACK_OFFSET(a5), a0
	move.l a0, d0
	beq.s fallback
	jsr (a0)
	rts
fallback
	move.l OPASM_PROGRESS_FALLBACK_TICK_OFFSET(a5), d0
	addq.l #1, OPASM_PROGRESS_FALLBACK_TICK_OFFSET(a5)
	rts
	.bend  ; sampleTick

sampleAndChargePhase	.block
	bsr.w sampleTick
	move.l d0, OPASM_PROGRESS_LAST_TICK_OFFSET(a5)
	move.l d0, d1
	sub.l OPASM_PROGRESS_RUN_START_TICK_OFFSET(a5), d1
	move.l d1, OPASM_PROGRESS_TOTAL_TICKS_OFFSET(a5)
	moveq #0, d2
	move.w OPASM_PROGRESS_PHASE_OFFSET(a5), d2
	beq.s startNext
	cmpi.w #OPASM_PROGRESS_PHASE_COUNT, d2
	bhi.s startNext
	move.l d0, d1
	sub.l OPASM_PROGRESS_PHASE_START_TICK_OFFSET(a5), d1
	subq.w #1, d2
	lsl.w #2, d2
	lea OPASM_PROGRESS_PHASE_TICKS_OFFSET(a5), a0
	move.l 0(a0, d2.w), d3
	add.l d1, d3
	bcc.s storePhaseTicks
	move.l #-1, d3
	ori.l #OPASM_PROGRESS_OVERFLOW_PHASE_TICKS, OPASM_PROGRESS_OVERFLOW_OFFSET(a5)
storePhaseTicks
	move.l d3, 0(a0, d2.w)
startNext
	move.l d0, OPASM_PROGRESS_PHASE_START_TICK_OFFSET(a5)
	rts
	.bend  ; sampleAndChargePhase

	.endsection

	.pub

	.section bss, kind=bss
	.align 4

OpasmProgressRecord
	.res byte, OPASM_PROGRESS_RECORD_BYTES
OpasmProgressTickCallback
	.res long, 1
OpasmProgressFallbackTick
	.res long, 1

	.endsection
	.endmodule
