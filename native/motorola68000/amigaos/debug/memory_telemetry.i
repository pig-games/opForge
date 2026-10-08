; Reusable memory accounting. Both gates are required; release emits nothing.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.use debug.amigaos.memory_profile as memory_profile
.endif
.endif
MEMORY_ALLOC	.macro amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	jsr memory_profile.allocate
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Local bounded work counters for owners that report through gated progress.
; No register or CCR effect, and no emitted bytes in ordinary builds.
MEMORY_COUNTER_CLEAR	.macro counter
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	clr.l .counter
	move.w (sp)+, ccr
.endif
.endif
.endmacro

MEMORY_COUNTER_INC	.macro counter
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	addq.l #1, .counter
	move.w (sp)+, ccr
.endif
.endif
.endmacro

MEMORY_COUNTER_ADD	.macro counter, amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	add.l d0, .counter
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Bounded work and phase-clock accounting uses the same optional record.
MEMORY_WORK	.macro index, amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .amount, d1
	move.l .index, d0
	jsr memory_profile.work
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endmacro

MEMORY_CLOCK	.macro dosbase, index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	move.l .index, d0
	movea.l .dosbase, a0
	jsr memory_profile.clock
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
MEMORY_FREE	.macro amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	jsr memory_profile.release
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Mark a bounded reserve failure (64) or Exec allocation failure (128).
; The call is compiled out of ordinary builds and preserves the failed status.
MEMORY_FAILURE	.macro kind, request, capacity, used
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d3, -(sp)
	move.l .request, d1
	move.l .capacity, d2
	move.l .used, d3
	move.l .kind, d0
	jsr memory_profile.failure
	movem.l (sp)+, d0-d3
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Opt-in bounded source/preparation progress. The observer preserves the entire
; caller frame and writes a short structured line to the guest's captured stdout.
MEMORY_PROGRESS	.macro dosbase, phase, source, line, records
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	move.l .phase, d0
	move.l .source, d1
	move.l .line, d2
	move.l .records, d3
	movea.l .dosbase, a0
	jsr memory_profile.progress
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Snapshot a bounded assembly sweep, not each record. Word-valued section state
; is zero-extended. Capture storage and this type require the same three gates.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
AssemblyPosition	.struct
Pass	.long ?
Sweep	.long ?
Mode	.long ?
Section	.long ?
Count	.long ?
.endstruct
PREPARATION_PREFIX_BYTES = 32
PreparationPosition	.struct
Input	.long ?
Prepared	.long ?
Record	.res PREPARATION_PREFIX_BYTES
Status	.long ?
.endstruct
PREPARATION_POSITION_BYTES = PreparationPosition.Status+4
.endif
.endif
.endif
ASSEMBLY_POSITION_CLEAR	.macro capture
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l a0, -(sp)
	lea .capture, a0
.for 5
	clr.l (a0)+
.endfor
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Bare base labels retain Hunk relocations. Field offsets are displacements;
; do not form absolute label+field operands in diagnostic loads or stores.
ASSEMBLY_POSITION	.macro capture, passValue, sweepValue, state, modeOffset, sectionOffset, countOffset
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0/a0-a1, -(sp)
	lea .capture, a0
	move.l .passValue, AssemblyPosition.Pass(a0)
	move.l .sweepValue, AssemblyPosition.Sweep(a0)
	lea .state, a1
	moveq #0, d0
	move.w .modeOffset(a1), d0
	move.l d0, AssemblyPosition.Mode(a0)
	move.w .sectionOffset(a1), d0
	move.l d0, AssemblyPosition.Section(a0)
	move.w .countOffset(a1), d0
	move.l d0, AssemblyPosition.Count(a0)
	movem.l (sp)+, d0/a0-a1
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; One last attempted numeric selector row and projection, without event growth.
; Owners gate their 12-byte storage identically. All-ones means not attempted.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
SelectionPosition	.struct
Priority	.long ?
Recipe	.long ?
Projection	.long ?
.endstruct
.endif
.endif
.endif
SELECTION_POSITION_CLEAR	.macro capture
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	moveq #-1, d0
	lea .capture, a0
	move.l d0, SelectionPosition.Priority(a0)
	move.l d0, SelectionPosition.Recipe(a0)
	move.l d0, SelectionPosition.Projection(a0)
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Priority is a word memory operand, recipe a byte memory operand. Read both
; before loading the relocated capture base, including for A0-based operands.
SELECTION_POSITION_CANDIDATE	.macro capture, priorityValue, recipeValue
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d1/a0, -(sp)
	moveq #0, d0
	moveq #0, d1
	move.w .priorityValue, d0
	move.b .recipeValue, d1
	lea .capture, a0
	move.l d0, SelectionPosition.Priority(a0)
	move.l d1, SelectionPosition.Recipe(a0)
	moveq #-1, d0
	move.l d0, SelectionPosition.Projection(a0)
	movem.l (sp)+, d0-d1/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Kind is a byte memory operand; only the projection field is replaced.
SELECTION_POSITION_PROJECTION	.macro capture, kind
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	moveq #0, d0
	move.b .kind, d0
	lea .capture, a0
	move.l d0, SelectionPosition.Projection(a0)
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Report three long fields from one relocated base. Same passive progress ABI.
MEMORY_PROGRESS_BLOCK	.macro dosbase, phase, base, firstOffset, secondOffset, thirdOffset
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	lea .base, a0
	move.l .firstOffset(a0), d1
	move.l .secondOffset(a0), d2
	move.l .thirdOffset(a0), d3
	move.l .phase, d0
	movea.l .dosbase, a0
	jsr memory_profile.progress
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Source/line scalars plus a block's long byte-count field, read from its base.
MEMORY_PROGRESS_RECORDS	.macro dosbase, phase, sourceValue, lineValue, base, usedOffset
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	lea .base, a0
	move.l .usedOffset(a0), d3
	move.l .sourceValue, d1
	move.l .lineValue, d2
	move.l .phase, d0
	movea.l .dosbase, a0
	jsr memory_profile.progress
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Read a bounded prepared-record prefix at an assembly failure. The frame
; fields are supplied by its owner; validate the active extent, then copy at
; most 24 bytes to a zeroed stack prefix. No following record is observed.
; Preserves registers/CCR and stack depth; release emits no code or storage.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
AssemblyRecordPrefix	.struct
First	.long ?
Second	.long ?
Third	.long ?
Fourth	.long ?
Fifth	.long ?
Sixth	.long ?
	.endstruct
ASSEMBLY_PREFIX_BYTES = AssemblyRecordPrefix.Sixth+4
ASSEMBLY_PREFIX_LONGS = ASSEMBLY_PREFIX_BYTES/4
.endif
.endif
.endif
MEMORY_ASSEMBLY_RECORD	.macro dosbase, phase, frame, recordsOffset, offsetOffset, bytesOffset
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d3/a0-a1, -(sp)
	suba.w #ASSEMBLY_PREFIX_BYTES, sp
	movea.l sp, a1
	moveq #ASSEMBLY_PREFIX_LONGS-1, d0
clearPrefix
	clr.l (a1)+
	dbra d0, clearPrefix
	lea .frame, a1
	move.l .offsetOffset(a1), d0
	move.l .bytesOffset(a1), d1
	cmp.l d1, d0
	bhs.w done
	sub.l d0, d1
	movea.l .recordsOffset(a1), a0
	move.l a0, d2
	beq.w done
	adda.l d0, a0
	moveq #0, d3
	move.b (a0), d3
	addq.l #1, d3
	cmp.l d1, d3
	bhi.w done
	cmpi.l #ASSEMBLY_PREFIX_BYTES, d3
	bls.w bounded
	moveq #ASSEMBLY_PREFIX_BYTES, d3
bounded
	movea.l sp, a1
copyPrefix
	move.b (a0)+, (a1)+
	subq.l #1, d3
	bne.w copyPrefix
	movea.l sp, a0
	.MEMORY_PROGRESS_BLOCK .dosbase, .phase, (a0), AssemblyRecordPrefix.First, AssemblyRecordPrefix.Second, AssemblyRecordPrefix.Third
	.MEMORY_PROGRESS_BLOCK .dosbase, .phase+1, (a0), AssemblyRecordPrefix.Fourth, AssemblyRecordPrefix.Fifth, AssemblyRecordPrefix.Sixth
done
	adda.w #ASSEMBLY_PREFIX_BYTES, sp
	movem.l (sp)+, d0-d3/a0-a1
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Identify the last assembly operation when optional progress diagnostics report
; a failure. This preserves registers and CCR and emits nothing in release builds.
ASSEMBLY_FAILURE_STAGE	.macro slot, stage
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l .stage, .slot
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
; Retain the input boundary, independently of any failed preparation output.
; A0=writer record, A1=distinct output. Snapshot uses PreparationPosition's
; two pointers and bounded prefix; its status belongs to the caller's boundary.
; Copy only caller-bounded bytes; zero the unused prefix. All state survives.
MEMORY_PREPARATION_INPUT	.macro snapshot, capacity
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d1/a0-a2, -(sp)
	lea .snapshot, a2
	move.l a0, (a2)+
	move.l a1, (a2)+
	movea.l a2, a1
	moveq #PREPARATION_PREFIX_BYTES/4-1, d0
clear
	clr.l (a1)+
	dbra d0, clear
	move.l .capacity, d0
	beq.w done
	moveq #0, d1
	move.b (a0), d1
	addq.l #1, d1
	cmp.l d1, d0
	bls.w bounded
	move.l d1, d0
bounded
	cmpi.l #PREPARATION_PREFIX_BYTES, d0
	bls.w ready
	moveq #PREPARATION_PREFIX_BYTES, d0
ready
	subq.w #1, d0
copy
	move.b (a0)+, (a2)+
	dbra d0, copy
done
	movem.l (sp)+, d0-d1/a0-a2
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Last preparation boundary and published counters. Keep argument spellings
; within the current macro limit; record capture is a separate bounded call.
; Both helpers use existing progress storage and preserve all state.
MEMORY_PREPARATION_FAILURE	.macro dosbase, assembling, stage, used, count, phase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	tst.l .assembling
	bne.w done
	.MEMORY_PROGRESS .dosbase, .phase, .stage, .used, .count
done
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Return status and opaque output/input observations for the same failure.
MEMORY_PREPARATION_RECORD	.macro dosbase, assembling, stage, output, capacity, snapshot, phase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	tst.l .assembling
	bne.w done
	.MEMORY_PROGRESS .dosbase, .phase+7, .snapshot+PreparationPosition.Status, #0, #0
	cmpi.l #6*4, .capacity
	blo.w done
	movea.l .output, a0
	move.l a0, d0
	beq.w done
	; Only bytes within byte0+1 belong to the active record. The decoder
	; discards the rest of this fixed 24-byte observation.
	.MEMORY_PROGRESS_BLOCK .dosbase, .phase+1, (a0), 0*4, 1*4, 2*4
	.MEMORY_PROGRESS_BLOCK .dosbase, .phase+2, (a0), 3*4, 4*4, 5*4
	cmpi.l #8, .stage
	bne.w done  ; input snapshot belongs only to the expression boundary
	; The pre-call snapshot also identifies the two distinct buffers. Its
	; record framing bounds which prefix bytes the decoder may interpret.
	.MEMORY_PROGRESS_BLOCK .dosbase, .phase+3, .snapshot, PreparationPosition.Input, PreparationPosition.Prepared, PreparationPosition.Record
	.MEMORY_PROGRESS_BLOCK .dosbase, .phase+4, .snapshot, PreparationPosition.Record+1*4, PreparationPosition.Record+2*4, PreparationPosition.Record+3*4
	.MEMORY_PROGRESS_BLOCK .dosbase, .phase+5, .snapshot, PreparationPosition.Record+4*4, PreparationPosition.Record+5*4, PreparationPosition.Record+6*4
	.MEMORY_PROGRESS .dosbase, .phase+6, .snapshot+PreparationPosition.Record+7*4, #0, #0
done
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
MEMORY_PHASE	.macro value
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .value, d0
	jsr memory_profile.phase
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
MEMORY_SAVE	.macro dosbase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr memory_profile.save
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

MEMORY_LAYOUT	.macro runtime_bytes, record_bytes, source_bytes
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d2, -(sp)
	move.l .runtime_bytes, d0
	move.l .record_bytes, d1
	move.l .source_bytes, d2
	jsr memory_profile.layout
	movem.l (sp)+, d0-d2
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Exclusive preparation stage transition, D0/CCR and all other registers preserved.
MEMORY_STAGE	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.stage
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Bounded packed-source scopes. Ordinary and phase-only builds emit no calls.
MEMORY_DETAIL_BEGIN	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_BINDING_DETAIL_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.detailBegin
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
MEMORY_DETAIL_END	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_BINDING_DETAIL_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.detailEnd
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
MEMORY_BIND_SAMPLE_BEGIN	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_BINDING_DETAIL_TELEMETRY
	jsr memory_profile.bindSampleBegin
.endif
.endif
.endif
.endmacro
MEMORY_BIND_SAMPLE_END	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_BINDING_DETAIL_TELEMETRY
	jsr memory_profile.bindSampleEnd
.endif
.endif
.endif
.endmacro

; Aggregate template work without clock reads at candidate boundaries.
MEMORY_TEMPLATE_WORK	.macro index, amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_TEMPLATE_WORK_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .amount, d1
	move.l .index, d0
	jsr memory_profile.templateWork
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Physical line collection only: begin/end receive the cumulative source bytes.
; Clock reads occur once per collection attempt, never once per input byte.
MEMORY_INPUT_BEGIN	.macro bytes
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_INPUT_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .bytes, d0
	jsr memory_profile.inputBegin
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
MEMORY_INPUT_END	.macro bytes
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_INPUT_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .bytes, d0
	jsr memory_profile.inputEnd
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
MEMORY_INPUT_READ	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_INPUT_TELEMETRY
	jsr memory_profile.inputRead
.endif
.endif
.endif
.endmacro

TOKEN_BEGIN	.macro amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_TOKEN_DETAIL_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	jsr memory_profile.tokenBegin
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

TOKEN_OPCODE	.macro opcode
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_TOKEN_DETAIL_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .opcode, d0
	jsr memory_profile.tokenOpcode
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

TOKEN_SCOPE_BEGIN	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_TOKEN_DETAIL_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.tokenScopeBegin
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

TOKEN_SCOPE_END	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_TOKEN_DETAIL_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.tokenScopeEnd
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

TOKEN_WORK	.macro index, amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_TOKEN_DETAIL_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .amount, d1
	move.l .index, d0
	jsr memory_profile.tokenWork
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

; Close a scope if active, for shared success/failure return boundaries.
TOKEN_SCOPE_CLOSE	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_TOKEN_DETAIL_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.tokenScopeClose
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
