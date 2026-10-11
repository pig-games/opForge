; Bounded sizing and final emission over binary source, with no text interface.
; Mapped Hunk layout first measures concrete prefixes in a disposable pass.
; @opforge-owner: experimental.amigaos.binary_assembly

	.module experimental.amigaos.binary_assembly
	.cpu 68020
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_assembly_session as session
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_parameters as parameters
	.use experimental.amigaos.binary_state as state
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as exprvm
	.use prvm.amigaos.abi as dataabi
	.use prvm.amigaos.macro_runtime as datavm
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_dependencies as dependencies
	.use experimental.amigaos.binary_scalar_observations as scalarObservations
	.use experimental.amigaos.binary_mutable as mutable
	.use experimental.amigaos.binary_constants as constants
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_hunk_mapping as mapping
	.use experimental.amigaos.binary_hunk_placement as placement
	.use experimental.amigaos.binary_hunk_references as hunkrefs
	.use experimental.amigaos.binary_repetition as repetition
	.use experimental.amigaos.binary_branches as branches
	.use experimental.amigaos.binary_guard_values as guards
	.use experimental.amigaos.binary_repeat_inventory as repeatInventory
	.use experimental.amigaos.binary_text_execution as textExecution
	.use experimental.amigaos.binary_text as text
	.use experimental.amigaos.binary_text_layout as textLayout
	.use experimental.amigaos.binary_string_provider as stringProvider
	.use experimental.amigaos.binary_memory as memory
	.include "memory_telemetry.i"
	.pub

Frame	.struct
Records	.long ?
RecordBytes	.long ?
Context	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
Line	.word ?
Failure	.word ?
Allocate	.long ?
RecordOffset	.long ?
Sections	.long ?
AddReloc	.long ?
Emitted	.long ?
MemberKeys	.long ?  ; bounded numeric field IDs, indexed by context symbol
Warn	.long ?  ; optional byte-truncation diagnostic callback
Warnings	.long ?  ; final-pass warning events, without per-event allocation
AsciiName	.word ?  ; prepared global source-origin encoding IDs
PetsciiName	.word ?
ModuleStarts	.long ?  ; ordered-relative numeric module boundaries
ModuleCount	.long ?
	.endstruct

OutputReloc	.struct
Source	.long ?
Target	.long ?
Offset	.long ?
	.endstruct
OUTPUT_RELOC_BYTES = OutputReloc.Offset+4
FRAME_BYTES = Frame.ModuleCount+4
FAILURE_NONE = 0
FAILURE_MUTABLE_LAYOUT = 1
FAILURE_WHILE_LAYOUT = 2
MAX_LAYOUT_ROUNDS = 8

	.section bss, kind=bss
	.priv
TextExecution
	.res byte, textExecution.FRAME_BYTES
ModuleCursor	.res long, 1
TextState
	.res byte, textLayout.STATE_BYTES
Session
	.res byte, session.STATE_BYTES
LayoutRounds
	.res word, 1
SizingReplay
	.res word, 1
SizingRounds
	.res word, 1
BranchChanged
	.res word, 1
	.align 4
DataPc	.res long, 1
PendingFlatGap	.res long, 1
DataBytes
	.res byte, 4
SectionState
	.res byte, sections.SCRATCH_BYTES
BranchState
	.res byte, branches.STATE_BYTES
RepeatState
	.res byte, repetition.STATE_BYTES
InventoryState
	.res byte, repeatInventory.STATE_BYTES
ScalarObservations
	.res byte, scalarObservations.STATE_BYTES
HunkInstructionRefs
	.res word, 1
HunkInstructionRefCount
	.res word, 1
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
TraversalPasses
	.res long, 1
TraversalRecords
	.res long, 1
SizingPasses
	.res long, 1
	.priv
.endif
.endif
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.pub
FailureStage
	.res long, 1
FailureName
	.res long, 1
Position
	.res byte, AssemblyPosition.Count+4
	.priv
.endif
.endif
.endif
	.endsection
	.section code, kind=code
	.pub

; Frozen-input lifecycle. Interior views in inventory/branches/repetition remain
; pinned to the same Frame.Records and RecordBytes until release. Advance uses
; that complete frozen frontier; relocation or partial publication is rejected.
; Context/count/cell buffers/member keys are likewise pinned until release.
; A0=Frame. Initialize semantic owners once and start sizing, without execution.
beginSizing	.block
	move.l d2, -(sp)
	moveq #session.BEGIN, d2
	bsr.w execute
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; beginSizing
; A0=Frame,D0=published bytes,D1=record budget (zero drains the frontier).
; D0/CCR=status,D1=ADVANCED/FRONTIER/TERMINATED. Suspend only after a complete
; record/control. An unfinished branch or repetition at frontier is not EOF.
advance	.block
	move.l d2, -(sp)
	moveq #session.ADVANCE, d2
	bsr.w execute
	move.l (sp)+, d2
	moveq #0, d1
	move.w Session+session.State.Result, d1
	tst.l d0
	rts
	.bend  ; advance
; A0=Frame. Seal the sizing frontier and run existing bounded convergence;
; allocate output only after stable geometry/branch observations.
finishSizing	.block
	move.l d2, -(sp)
	moveq #session.FINISH, d2
	bsr.w execute
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; finishSizing
; A0=Frame. Strict final emission after successful finishSizing.
emit	.block
	move.l d2, -(sp)
	moveq #session.EMIT, d2
	bsr.w execute
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; emit
; A0=Frame. Idempotent release of session semantic owners; caller buffers remain.
release	.block
	move.l d2, -(sp)
	moveq #session.RELEASE, d2
	bsr.w execute
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; release
; Complete frozen-input driver using the same public session lifecycle.
assemble	.block
	movem.l d1/a0, -(sp)
	bsr.w beginSizing
	bne.w return
	move.l Frame.RecordBytes(a0), d0
	moveq #0, d1
	bsr.w advance
	bne.w done
	bsr.w finishSizing
	bne.w done
	bsr.w emit
done
	move.l d0, -(sp)
	bsr.w release
	tst.l (sp)
	beq.w releaseResult
	move.l (sp), d0
releaseResult
	addq.l #4, sp
return
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; assemble

	.priv
; A0=Frame. Context values/defined arrays cover Count entries. The caller owns
; all buffers. Returns D0=0 only after stable sizing and checked final emission;
; Used=output bytes. Unavailable scoped guards enable bounded retained replay.
; Allocate callback: A0=Frame, D0=stable sizing output bytes; D0/CCR=status,
; preserves other registers, supplies Frame.Output/Capacity before final emission.
; Optional Emitted(A0=Frame,D1=address,D2=buffer offset,D3=bytes) runs only
; for nonempty initialized final-pass emissions, before advancing state. It returns D0/CCR
; status and preserves other registers. No text/CPU grammar enters the callback.
; Optional Warn(A0=Frame,D1=low-u32 value) reports final-pass byte truncation;
; D0/CCR=status, other registers preserved. Warnings counts all such events.
; Other registers preserved; CCR reflects D0. No text/dictionary pointer enters
; this module. General variable-size convergence and discontiguous origins remain
; unsupported.
execute	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	cmpi.w #session.BEGIN, d2
	beq.w startSession
	cmpi.w #session.RELEASE, d2
	beq.w releaseSession
	move.w d2, Session+session.State.Operation
	cmpa.l Session+session.State.Frame, a0
	bne.w invalidApi
	move.l Frame.Records(a5), d3
	cmp.l Session+session.State.Base, d3
	bne.w invalidApi
	move.l Frame.RecordBytes(a5), d3
	cmp.l Session+session.State.Bytes, d3
	bne.w invalidApi
	movea.l Frame.Context(a5), a6
	cmpa.l Session+session.State.Context, a6
	bne.w invalidApi
	move.l pkg.Context.Count(a6), d3
	cmp.l Session+session.State.Count, d3
	bne.w invalidApi
	move.l pkg.Context.Values(a6), d3
	cmp.l Session+session.State.Values, d3
	bne.w invalidApi
	move.l pkg.Context.Defined(a6), d3
	cmp.l Session+session.State.Defined, d3
	bne.w invalidApi
	move.l pkg.Context.SectionIds(a6), d3
	cmp.l Session+session.State.SectionIds, d3
	bne.w invalidApi
	move.l pkg.Context.Owner(a6), d3
	cmp.l Session+session.State.Owner, d3
	bne.w invalidApi
	move.l Frame.MemberKeys(a5), d3
	cmp.l Session+session.State.MemberKeys, d3
	bne.w invalidApi
	move.l Frame.ModuleStarts(a5), d3
	cmp.l Session+session.State.ModuleStarts, d3
	bne.w invalidApi
	move.l Frame.ModuleCount(a5), d3
	cmp.l Session+session.State.ModuleCount, d3
	bne.w invalidApi
	move.w Frame.AsciiName(a5), d3
	cmp.w Session+session.State.AsciiName, d3
	bne.w invalidApi
	move.w Frame.PetsciiName(a5), d3
	cmp.w Session+session.State.PetsciiName, d3
	bne.w invalidApi
	move.l d0, -(sp)
	bsr.w refreshViews
	tst.l d0
	movem.l (sp)+, d0
	bne.w invalidApi
	cmpi.w #session.EMIT, d2
	beq.w startEmission
	cmpi.w #session.SIZING, Session+session.State.Phase
	bne.w invalidApi
	moveq #0, d7
	move.w Session+session.State.Pass, d7
	moveq #0, d5
	move.w Session+session.State.Sweep, d5
	moveq #0, d4
	move.w Session+session.State.Selection, d4
	movea.l Frame.Records(a5), a4
	adda.l Session+session.State.Cursor, a4
	cmpi.w #session.FINISH, d2
	beq.w sealSizing
	cmpi.w #session.ADVANCE, d2
	bne.w invalidApi
	cmp.l Session+session.State.Bytes, d0
	bne.w invalidApi
	cmp.l Session+session.State.Cursor, d0
	blo.w invalidApi
	move.l d0, Session+session.State.Published
	move.l d1, Session+session.State.Budget
	clr.w Session+session.State.Unlimited
	tst.l d1
	bne.w boundedAdvance
	move.w #1, Session+session.State.Unlimited
boundedAdvance
	movea.l Frame.Records(a5), a3
	adda.l d0, a3
	bra.w line
sealSizing
	tst.w Session+session.State.Ended
	bne.w sizingSealed
	move.l Session+session.State.Bytes, d0
	cmp.l Session+session.State.Published, d0
	bne.w invalidApi
	cmp.l Session+session.State.Cursor, d0
	bne.w invalidApi
sizingSealed
	movea.l Frame.Records(a5), a3
	adda.l Frame.RecordBytes(a5), a3
	bra.w passDone
startEmission
	cmpi.w #session.SIZED, Session+session.State.Phase
	bne.w invalidApi
	moveq #2, d7
	bra.w pass
releaseSession
	tst.w Session+session.State.Phase
	beq.w released
	cmpa.l Session+session.State.Frame, a0
	bne.w invalidApi
	move.w #session.RELEASE, Session+session.State.Operation
	; Release uses the captured semantic owner, independent of caller registers
	; and of a Frame that may have been modified after a rejected advance.
	movea.l Session+session.State.Context, a6
	moveq #0, d0
	bra.w cleanup
released
	moveq #0, d0
	bra.w apiDone
startSession
	tst.w Session+session.State.Phase
	bne.w invalidApi
	move.l Frame.RecordBytes(a5), d3
	move.l Frame.Records(a5), d4
	tst.l d3
	beq.w validFrozenBase
	tst.l d4
	beq.w invalidApi
validFrozenBase
	add.l d3, d4
	bcs.w invalidApi
	movea.l Frame.Context(a5), a6
	move.l a6, d3
	beq.w invalidApi
	move.l a6, Session+session.State.Context
	move.l pkg.Context.Count(a6), Session+session.State.Count
	move.l pkg.Context.Values(a6), Session+session.State.Values
	move.l pkg.Context.Defined(a6), Session+session.State.Defined
	move.l pkg.Context.SectionIds(a6), Session+session.State.SectionIds
	move.l pkg.Context.Owner(a6), Session+session.State.Owner
	move.l Frame.MemberKeys(a5), Session+session.State.MemberKeys
	move.l Frame.ModuleStarts(a5), Session+session.State.ModuleStarts
	move.l Frame.ModuleCount(a5), Session+session.State.ModuleCount
	move.w Frame.AsciiName(a5), Session+session.State.AsciiName
	move.w Frame.PetsciiName(a5), Session+session.State.PetsciiName
	move.w #session.BEGIN, Session+session.State.Operation
	move.w #session.SIZING, Session+session.State.Phase
	move.l a0, Session+session.State.Frame
	move.l Frame.Records(a5), Session+session.State.Base
	move.l Frame.RecordBytes(a5), Session+session.State.Bytes
	clr.l Session+session.State.Cursor
	clr.l Session+session.State.Published
	clr.w Session+session.State.Ended
	move.l a0, Session+session.State.Frame
	.ASSEMBLY_FAILURE_STAGE FailureStage, #0
	.ASSEMBLY_POSITION_CLEAR Position
	.MEMORY_COUNTER_CLEAR TraversalPasses
	.MEMORY_COUNTER_CLEAR TraversalRecords
	.MEMORY_COUNTER_CLEAR SizingPasses
	.MEMORY_COUNTER_CLEAR branches.DeferredEvaluations
	.MEMORY_COUNTER_CLEAR repetition.IterableScopes
	.MEMORY_COUNTER_CLEAR repetition.IterableBindings
	.MEMORY_COUNTER_CLEAR scalarObservations.ActivePasses
	.MEMORY_COUNTER_CLEAR scalarObservations.ChangedRows
	.MEMORY_COUNTER_CLEAR scalarObservations.RestoredRows
	clr.l Frame.Used(a5)
	clr.l Frame.Warnings(a5)
	clr.w LayoutRounds
	clr.w SizingReplay
	clr.w SizingRounds
	clr.w Frame.Failure(a5)
	lea BranchState, a0
	jsr branches.release
	lea RepeatState, a0
	jsr repetition.release
	lea ScalarObservations, a0
	jsr scalarObservations.release
	jsr dependencies.release
	lea InventoryState, a0
	jsr repeatInventory.release
	move.l #-1, Frame.RecordOffset(a5)
	movea.l Frame.Context(a5), a6
	movea.l pkg.Context.Owner(a6), a0
	move.l a0, d0
	beq.w scalarOwner
	jsr values.reset
	bne.w fail
scalarOwner
	move.l pkg.Context.Count(a6), d0
	beq.w fail
	cmpi.l #65536, d0
	bhi.w fail
	movea.l pkg.Context.Values(a6), a0
	movea.l pkg.Context.Defined(a6), a1
	movea.l pkg.Context.SectionIds(a6), a2
clearSymbols
	clr.l (a0)+
	clr.l (a0)+
	clr.b (a1)+
	clr.b (a2)+
	subq.l #1, d0
	bne.w clearSymbols
	movea.l a6, a0
	jsr parameters.load
	bne.w fail
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	movea.l a6, a2
	movea.l a0, a1
	movea.l Frame.MemberKeys(a5), a3
	lea InventoryState, a0
	jsr repeatInventory.prepare
	bne.w fail
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	movea.l InventoryState+repeatInventory.State.Declarations+memory.Block.Pointer, a1
	move.l InventoryState+repeatInventory.State.Globals, d1
	jsr dependencies.resolve
	tst.l d0
	bne.w fail
	lea RepeatState, a0
	lea InventoryState, a1
	movea.l a6, a2
	movea.l Frame.Records(a5), a3
	move.l Frame.RecordBytes(a5), d0
	jsr repetition.prepare
	bne.w fail
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	lea SectionState, a1
	movea.l pkg.Context.Package(a6), a2
	move.l pkg.Header.MaxAddress(a2), d1
	jsr sections.scan
	bne.w fail
	lea SectionState+placement.BASES, a0
	move.l a0, pkg.Context.SectionBases(a6)
	lea SectionState, a1
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	movea.l pkg.Context.Package(a6), a2
	move.l pkg.Header.MaxAddress(a2), d1
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a1)
	bne.w placementReady
	jsr placement.scan
	bne.w fail
placementReady
	move.l #SectionState, Frame.Sections(a5)
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	lea SectionState, a1
	move.w sections.State.Mode(a1), d2
	jsr mutable.validateLayout
	beq.w loopLayout
	move.l d1, Frame.RecordOffset(a5)
	cmpi.l #mutable.UNSUPPORTED_LAYOUT, d0
	bne.w fail
	move.w #FAILURE_MUTABLE_LAYOUT, Frame.Failure(a5)
	bra.w fail
loopLayout
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	movea.l pkg.Context.Package(a6), a2
	moveq #0, d1
	move.w SectionState+sections.State.Mode, d1
	jsr repetition.validateLayout
	tst.l d0
	beq.w layoutReady
	move.l d1, Frame.RecordOffset(a5)
	cmpi.l #repetition.UNSUPPORTED_LAYOUT, d0
	bne.w fail
	move.w #FAILURE_WHILE_LAYOUT, Frame.Failure(a5)
	bra.w fail
layoutReady
	clr.w pkg.Context.Relocatable(a6)
	lea SectionState, a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	bne.w outputReady
	move.w #1, pkg.Context.Relocatable(a6)
	jsr mapping.validate
	bne.w fail
outputReady
	lea RepeatState+repetition.State.Guard, a0
	movea.l Frame.Records(a5), a1
	move.l Frame.RecordBytes(a5), d0
	movea.l a6, a2
	lea InventoryState, a3
	jsr guards.prepare
	bne.w fail

	moveq #1, d7
pass
	lea ScalarObservations, a0
	jsr scalarObservations.beginPass
	lea RepeatState, a0
	move.l d7, d0
	tst.w SizingReplay
	beq.w repetitionPass
	moveq #2, d0  ; retain iteration cells; compare the same invocation paths
repetitionPass
	jsr repetition.beginPass
	bne.w fail
	moveq #0, d0
	cmpi.w #2, d7
	beq.w strictBranches
	bra.w branchPass
strictBranches
	moveq #1, d0
branchPass
	movem.l a0-a2, -(sp)
	lea BranchState, a0
	lea RepeatState, a1
	movea.l Frame.Records(a5), a2
	jsr branches.beginPass
	movem.l (sp)+, a0-a2
	move.w d7, pkg.Context.Pass(a6)
	clr.l pkg.Context.Pc(a6)
	clr.l Frame.Used(a5)
	clr.l PendingFlatGap
	lea SectionState, a0
	movea.l a6, a1
	jsr sections.beginPass
	moveq #0, d5  ; ordinary single sweep
	lea SectionState, a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	beq.w sweep
	cmpi.w #2, sections.State.Mode(a0)
	beq.w oneMap
	cmpi.w #4, sections.State.Mode(a0)
	bne.w sweep
	moveq #3, d5  ; two maps: four section sweeps, then outside controls
	bra.w sweep
oneMap
	moveq #1, d5  ; explicit map: concrete sweep, then remaining records
	bra.w sweep
sweep
	moveq #0, d4  ; section selection state for mapped sweeps
sweepRecords
	; A complete replay rebuilds definitions; suspended advances retain them.
	lea TextState, a0
	jsr text.release
	moveq #0, d1
	moveq #0, d2
	move.w Frame.AsciiName(a5), d1
	move.w Frame.PetsciiName(a5), d2
	jsr text.init
	bne.w fail
	move.l #TextState, pkg.Context.TextState(a6)
	jsr stringProvider.configure
	clr.l ModuleCursor
	movem.l a2, -(sp)
	movea.l pkg.Context.Package(a6), a2
	jsr state.reset
	movem.l (sp)+, a2
	bne.w fail
	.MEMORY_COUNTER_INC TraversalPasses
	.ASSEMBLY_POSITION Position, d7, d5, SectionState, sections.State.Mode, sections.State.HunkCurrent, sections.State.OrderCount
	lea RepeatState, a0
	movea.l Frame.Records(a5), a1
	move.l d5, d0
	jsr repetition.begin
	bne.w fail
	lea BranchState, a0
	jsr branches.begin
	movea.l Frame.Records(a5), a4
	move.l a4, d0
	add.l Frame.RecordBytes(a5), d0
	bcs.w fail
	movea.l d0, a3
line
	cmpi.w #session.BEGIN, Session+session.State.Operation
	beq.w suspendAdvanced
	cmpi.w #session.ADVANCE, Session+session.State.Operation
	bne.w executeRecord
	tst.w Session+session.State.Ended
	bne.w suspendTerminated
	cmpa.l a3, a4
	beq.w suspendFrontier
	tst.w Session+session.State.Unlimited
	bne.w executeRecord
	tst.l Session+session.State.Budget
	beq.w suspendAdvanced
	subq.l #1, Session+session.State.Budget
executeRecord
	.ASSEMBLY_FAILURE_STAGE FailureStage, #1
	move.l a4, d0
	sub.l Frame.Records(a5), d0
	move.l d0, Frame.RecordOffset(a5)
	bsr.w moduleEntries
	bne.w fail
	cmpa.l a3, a4
	beq.w passDone
	bhi.w fail
	moveq #0, d6
	move.b (a4), d6
	addq.w #1, d6
	cmpi.w #4, d6
	blo.w fail
	move.l a3, d0
	sub.l a4, d0
	cmp.l d0, d6
	bhi.w fail
	move.w 2(a4), Frame.Line(a5)
	moveq #0, d0
	move.b 1(a4), d0
	cmpi.b #source.FLAG_ALLOWED, d0
	bhi.w fail
	btst #3, d0
	bne.w omitted
	movea.l a4, a0
	movea.l a6, a2
	movea.l a3, a1
	lea RepeatState, a3
	jsr repetition.observe
	movea.l a1, a3
	bne.w fail
	btst #4, 1(a4)
	bne.w controlsReady
	movea.l a4, a0
	movea.l a3, a1
	movea.l a6, a2
	lea BranchState, a3
	jsr branches.step
	movea.l a1, a3
	cmpi.l #2, d0
	beq.w fail
	cmpi.l #branches.RESULT_SELECTED, d0
	beq.w selectedBranch
	tst.l d0
	bne.w omitted
	; Loop traversal precedes output selection, so skipped/repeated bodies also
	; govern state directives in every mapped sweep.
	movea.l a4, a0
	movea.l a3, a1
	movea.l a6, a2
	lea RepeatState, a3
	.ASSEMBLY_FAILURE_STAGE FailureStage, #4
	jsr repetition.step
	movea.l a1, a3
	cmpi.l #2, d0
	beq.w fail
	tst.l d0
	beq.w controlsReady
	movea.l a0, a4
	bra.w line
selectedBranch
	movea.l a4, a0
	movea.l a3, a1
	lea RepeatState, a3
	jsr repetition.activateBranch
	movea.l a1, a3
	bne.w fail
	bra.w omitted
controlsReady
	; Declaration execution follows shared control selection, before independent
	; section sweeps filter emission. Each sweep owns a fresh immutable claim set.
	movea.l a4, a0
	movea.l a3, a1
	movea.l a6, a2
	lea RepeatState, a3
	jsr repetition.claim
	movea.l a1, a3
	bne.w fail
	movem.l a0-a2, -(sp)
	lea ScalarObservations, a0
	movea.l a4, a1
	movea.l a3, a2
	jsr scalarObservations.mark
	movem.l (sp)+, a0-a2
	bne.w fail
	; Text controls execute for active source records before section filtering.
	movem.l a0-a2, -(sp)
	lea TextExecution, a0
	move.l #TextState, textExecution.Frame.State(a0)
	move.l a6, textExecution.Frame.Context(a0)
	move.l a4, textExecution.Frame.Record(a0)
	move.l d6, textExecution.Frame.Bytes(a0)
	move.l #emitBytes, textExecution.Frame.Emit(a0)
	jsr textExecution.prepare
	movem.l (sp)+, a0-a2
	bne.w fail
	tst.w d5
	beq.w stateReady
	; Replay state in source order even when section sweeps filter byte emission.
	; Omitted conditional/reachability records never change package state.
	movem.l a0-a2, -(sp)
	movea.l a4, a0
	movea.l a4, a1
	adda.w d6, a1
	movea.l pkg.Context.Package(a6), a2
	bsr.w stateRecord
	movem.l (sp)+, a0-a2
	tst.l d0
	bne.w fail
stateReady
	cmpi.w #3, d5
	bhs.w pairedSelect
	; An explicit map needs concrete bytes and labels before the imported
	; logical section. Filter the same packed records into two ordered sweeps.
	tst.w d5
	beq.w selected
	btst #4, 1(a4)
	beq.w selectStatement
	cmpi.w #5, d6
	blo.w fail
	cmpi.b #6, 4(a4)
	beq.w concreteOpen
	cmpi.b #3, 4(a4)
	bne.w selectOther
	tst.w d4
	beq.w selectOther
	moveq #0, d4
	bra.w selectConcrete
concreteOpen
	tst.w d4
	bne.w fail
	moveq #1, d4
selectConcrete
	cmpi.w #1, d5
	beq.w selected
	bra.w omitted
selectOther
	cmpi.w #1, d5
	beq.w omitted
	tst.w d4
	bne.w omitted
	bra.w selected
selectStatement
	cmpi.w #1, d5
	bne.w selectRemaining
	tst.w d4
	beq.w omitted
	bra.w selected
selectRemaining
	tst.w d4
	bne.w omitted
	bra.w selected
pairedSelect
	btst #4, 1(a4)
	beq.w pairedStatement
	cmpi.w #5, d6
	blo.w fail
	moveq #0, d0
	move.b 4(a4), d0
	cmpi.w #3, d0
	beq.w pairedClose
	cmpi.w #1, d0
	beq.w pairedOpen
	cmpi.w #2, d0
	beq.w pairedOpen
	cmpi.w #6, d0
	beq.w pairedOpen
	cmpi.w #7, d0
	beq.w pairedOpen
	cmpi.w #10, d0
	beq.w pairedOpen
	cmpi.w #11, d0
	beq.w pairedOpen
	cmpi.w #7, d5
	beq.w selected  ; region and place controls run after all section bodies
	bra.w omitted
pairedOpen
	tst.w d4
	bne.w fail
	moveq #2, d4
	cmpi.w #3, d5
	bne.w pairedFirstLogical
	cmpi.w #6, d0
	bne.w pairedOpenDone
	bra.w pairedChosen
pairedFirstLogical
	cmpi.w #4, d5
	bne.w pairedSecondConcrete
	cmpi.w #1, d0
	bne.w pairedOpenDone
	bra.w pairedChosen
pairedSecondConcrete
	cmpi.w #5, d5
	bne.w pairedSecondLogical
	cmpi.w #11, d0
	bne.w pairedOpenDone
	bra.w pairedChosen
pairedSecondLogical
	cmpi.w #6, d5
	bne.w pairedOpenDone
	cmpi.w #10, d0
	bne.w pairedOpenDone
pairedChosen
	moveq #1, d4
pairedOpenDone
	cmpi.w #1, d4
	beq.w selected
	bra.w omitted
pairedClose
	tst.w d4
	beq.w fail
	move.w d4, d0
	moveq #0, d4
	cmpi.w #1, d0
	beq.w selected
	bra.w omitted
pairedStatement
	cmpi.w #1, d4
	beq.w selected
	cmpi.w #7, d5
	bne.w omitted
	tst.w d4
	bne.w omitted
	bra.w selected
selected
	.MEMORY_COUNTER_INC TraversalRecords
	.ASSEMBLY_FAILURE_STAGE FailureStage, #3
	move.w 2(a4), Frame.Line(a5)
	moveq #0, d0
	move.b 1(a4), d0
	cmpi.b #source.FLAG_ALLOWED, d0
	bhi.w fail
	btst #4, d0
	bne.w layoutControl
	btst #3, d0
	bne.w omitted
ordinaryStatement
	tst.l TextExecution+textExecution.Frame.Action
	beq.w ordinaryNonText
	; Publish a grammar-validated label prefix through the shared statement owner.
	move.l TextExecution+textExecution.Frame.Results+dataabi.PRVM_TEXT_PREFIX_BYTES, d0
	beq.w textLabelReady
	lea source.RECORD_HEADER_BYTES(a4), a0
	movea.l a0, a1
	adda.l d0, a1
	movea.l a6, a2
	moveq #0, d0
	bsr.w statement
	bne.w fail
textLabelReady
	cmpi.l #textExecution.TEXT, TextExecution+textExecution.Frame.Action
	blo.w omitted
	lea SectionState, a0
	jsr sections.checkInitialized
	bne.w fail
	lea TextExecution, a0
	jsr textExecution.emit
	bne.w fail
	bra.w omitted
ordinaryNonText
	moveq #0, d0
	move.b 1(a4), d0
	andi.w #source.FLAG_INDENT, d0
	lea 4(a4), a0
	movea.l a4, a1
	adda.l d6, a1
	movea.l a6, a2
	.ASSEMBLY_FAILURE_STAGE FailureStage, #5
	bsr.w statement
	cmpi.l #2, d0
	beq.w statementEnded
	tst.l d0
	bne.w fail
	bra.w omitted
statementEnded
	cmpi.w #session.ADVANCE, Session+session.State.Operation
	bne.w passDone
	move.w #1, Session+session.State.Ended
	adda.l d6, a4
	bra.w suspendTerminated
layoutControl
	.ASSEMBLY_FAILURE_STAGE FailureStage, #6
	lea SectionState, a0
	movea.l a6, a1
	movea.l a4, a2
	jsr sections.control
	bne.w fail
	.ASSEMBLY_POSITION Position, d7, d5, SectionState, sections.State.Mode, sections.State.HunkCurrent, sections.State.OrderCount
omitted
	adda.l d6, a4
	bra.w line
passDone
	; The last module (or a component replay without a module schedule)
	; must close every definition scope before this traversal completes.
	tst.l TextState+textLayout.State.Scopes
	bne.w fail
	move.l ModuleCursor, d0
	cmp.l Frame.ModuleCount(a5), d0
	bne.w fail
	.ASSEMBLY_FAILURE_STAGE FailureStage, #7
	lea RepeatState, a0
	moveq #0, d0
	move.w BranchState+branches.State.Deferred, d0
	jsr repetition.end
	bne.w fail
	lea BranchState, a0
	jsr branches.end
	bne.w fail
	; Continue the same assembly pass at the current PC/output offset.
	cmpi.w #3, d5
	bhs.w pairedPassDone
	cmpi.w #1, d5
	bne.w sweepDone
	tst.w d4
	bne.w fail
	moveq #2, d5
	bra.w sweep
pairedPassDone
	tst.w d4
	bne.w fail
	cmpi.w #7, d5
	beq.w sweepDone
	addq.w #1, d5
	bra.w sweep
sweepDone
	lea RepeatState, a0
	jsr repetition.endPass
	bne.w fail
	lea BranchState, a0
	jsr branches.endPass
	move.w d0, BranchChanged
	lea RepeatState, a0
	jsr repetition.activityChanged
	bne.w fail
	or.w d1, BranchChanged
	lea ScalarObservations, a0
	jsr scalarObservations.endPass
	bne.w fail
	or.w d1, BranchChanged
	.ASSEMBLY_FAILURE_STAGE FailureStage, #9
	tst.w d4
	bne.w fail
	lea SectionState, a0
	movea.l a6, a1
	jsr sections.finishPass
	bne.w fail
	lea SectionState, a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	bne.w finishedLayout
	move.l d1, Frame.Used(a5)
	moveq #0, d2  ; provisional geometry changed, requiring a fresh pass one
	tst.w sections.State.MapCount(a0)
	beq.w placedBounds
	tst.w sections.State.MapReady(a0)
	beq.w mappedFreeze
	jsr mapping.check
	beq.w placedBounds
	cmpi.w #1, d7
	bne.w fail
mappedFreeze
	jsr mapping.freeze
	bne.w fail
	moveq #1, d2
placedBounds
	tst.w sections.State.PlaceCount(a0)
	beq.w layoutCompared
	jsr placement.freeze
	bne.w fail
	or.l d1, d2
layoutCompared
	tst.l d2
	beq.w stableLayout
	cmpi.w #1, d7
	bne.w fail
	addq.w #1, LayoutRounds
	cmpi.w #MAX_LAYOUT_ROUNDS, LayoutRounds
	bhs.w fail
	jsr dependencies.resetLayout
	bne.w fail
	bra.w pass
stableLayout
	tst.w sections.State.MapCount(a0)
	beq.w finishedLayout
	cmpi.w #2, d7
	bne.w finishedLayout
	jsr mapping.merge
	bne.w fail
finishedLayout
	cmpi.w #1, d7
	bne.w emittedPass
	tst.w BranchChanged
	beq.w allocateOutput
	; Deferred controls or changed owner/value observations require bounded
	; sizing before allocation. Only numeric scalar history survives the compound
	; rewind; final emission remains strict and reproduces the completed controls.
	move.w #1, SizingReplay
	addq.w #1, SizingRounds
	cmpi.w #MAX_LAYOUT_ROUNDS, SizingRounds
	bhs.w fail
	.MEMORY_COUNTER_INC SizingPasses
	lea ScalarObservations, a0
	movea.l a6, a1
	jsr scalarObservations.prepare
	bne.w fail
	movea.l a6, a1
	jsr dependencies.resetLayout
	bne.w fail
	lea ScalarObservations, a0
	jsr scalarObservations.restore
	bra.w pass
emittedPass
	tst.w BranchChanged
	bne.w fail
	bra.w nextPass
allocateOutput
	.ASSEMBLY_FAILURE_STAGE FailureStage, #18
	movea.l Frame.Allocate(a5), a1
	movea.l a5, a0
	move.l Frame.Used(a5), d0
	jsr (a1)
	tst.l d0
	bne.w fail
nextPass
	cmpi.w #session.FINISH, Session+session.State.Operation
	bne.w emittedComplete
	move.w #session.SIZED, Session+session.State.Phase
	moveq #0, d0
	bra.w apiDone
emittedComplete
	moveq #0, d0
	move.w #session.EMITTED, Session+session.State.Phase
	bra.w apiDone
fail
	clr.l Frame.Used(a5)
	moveq #1, d0
cleanup
	; Provider borrows TextState only while this assembly session owns it.
	move.l d0, -(sp)
	lea TextState, a0
	jsr text.release
	jsr stringProvider.release
	suba.l a0, a0
	jsr stringProvider.configure
	clr.l pkg.Context.TextState(a6)
	move.l (sp)+, d0
	clr.w Session+session.State.Phase
	lea BranchState, a0
	jsr branches.release
	lea RepeatState, a0
	jsr repetition.release
	lea ScalarObservations, a0
	jsr scalarObservations.release
	jsr dependencies.release
	lea InventoryState, a0
	jsr repeatInventory.release
apiDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
invalidApi
	moveq #1, d0
	bra.w apiDone
suspendFrontier
	move.w #session.FRONTIER, Session+session.State.Result
	bra.w suspend
suspendTerminated
	move.w #session.TERMINATED, Session+session.State.Result
	bra.w suspend
suspendAdvanced
	move.w #session.ADVANCED, Session+session.State.Result
suspend
	move.l a4, d0
	sub.l Frame.Records(a5), d0
	move.l d0, Session+session.State.Cursor
	move.w d7, Session+session.State.Pass
	move.w d5, Session+session.State.Sweep
	move.w d4, Session+session.State.Selection
	moveq #0, d0
	bra.w apiDone
	.bend  ; execute

; A5=Frame. Consume validated ordered-relative module entries at this frontier.
; Empty modules can share a boundary. Entries inside a record reject on the next
; complete frontier; source/file origins do not affect encoding selection.
; D0/CCR=status, all other registers preserved.
moduleEntries	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l Frame.ModuleCount(a5), d2
	cmpi.l #512, d2
	bhi.w bad
	move.l ModuleCursor, d1
	cmp.l d2, d1
	bhi.w bad
	beq.w good
	move.l Frame.ModuleStarts(a5), d3
	beq.w bad
	btst #0, d3
	bne.w bad
	movea.l d3, a1
	move.l d2, d0
	lsl.l #2, d0
	add.l d3, d0
	bcs.w bad
loop
	move.l d1, d3
	lsl.l #2, d3
	move.l 0(a1, d3.l), d3
	cmp.l Frame.RecordBytes(a5), d3
	bhi.w bad
	cmp.l Frame.RecordOffset(a5), d3
	blo.w bad
	bhi.w good
	; Validate closure before resetting selection, including adjacent empty
	; modules. Reset must never erase evidence of an unclosed definition.
	tst.l TextState+textLayout.State.Scopes
	bne.w bad
	lea TextState, a0
	jsr text.resetProfile
	addq.l #1, d1
	move.l d1, ModuleCursor
	cmp.l d2, d1
	blo.w loop
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; moduleEntries

; A5=frozen Frame,A6=Context. Refresh owner-borrowed views at a complete
; record boundary, after frozen identity checks. No source publication occurs.
; D0/CCR=status; other registers preserved. DFS calls are never suspended.
refreshViews	.block
	movem.l d1-d3/a0-a3, -(sp)
	lea InventoryState, a0
	movea.l Frame.Records(a5), a1
	movea.l a6, a2
	movea.l Frame.MemberKeys(a5), a3
	move.l Frame.RecordBytes(a5), d0
	jsr repeatInventory.refresh
	bne.w done
	lea RepeatState, a0
	move.l Frame.RecordBytes(a5), d0
	jsr repetition.refresh
	bne.w done
	lea BranchState, a0
	jsr branches.refresh
	jsr dependencies.discardViews
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; refreshViews

	.priv

; A0=valid prepared record,A1=end,A2=package. D0/CCR=status; others preserved.
; Inspect only the packed statement prefix; expressions and encodings stay in VMs.
stateRecord	.block
	movem.l d1/a0-a1, -(sp)
	move.b 1(a0), d0
	andi.w #source.FLAG_LAYOUT+source.FLAG_OMIT, d0
	bne.w ok
	addq.l #4, a0
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w ok
	cmpi.b #1, (a0)
	bhi.w directive
	cmpi.b #5, 4(a0)
	bne.w ok
	addq.l #5, a0
directive
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w ok
	cmpi.b #7, (a0)+
	bne.w ok
	cmpi.b #1, (a0)
	bhi.w ok
	tst.b 3(a0)
	bne.w ok
	moveq #0, d0
	move.w 1(a0), d0
	addq.l #4, a0
	cmp.w pkg.Header.CpuDirective(a2), d0
	beq.w resetCpu
	jsr state.apply
	cmpi.l #2, d0
	beq.w ok
	bra.w done
resetCpu
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	tst.b 3(a0)
	bne.w bad
	move.w 1(a0), d0
	cmp.w pkg.Header.CpuName(a2), d0
	bne.w bad
	jsr state.reset
	bra.w done
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; stateRecord

; A0/A1=bounded token range, A2=Context, D0=indent flag. D0 status, 2=.end.
statement	.block
	.ASSEMBLY_FAILURE_STAGE FailureStage, #10
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d7
	movea.l pkg.Context.Package(a2), a3
	cmpa.l a1, a0
	beq.w ok
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w dispatch
	cmpi.b #1, (a0)
	bhi.w dispatch
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, 4(a0)
	beq.w structDeclaration
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, 4(a0)
	beq.w constant
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a0)
	beq.w mutableDeclaration
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a0)
	beq.w mutableDeclaration
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, 4(a0)
	beq.w ok
	cmpi.b #source.TOKEN_UPDATE_FIRST, 4(a0)
	blo.w labelPrefix
	cmpi.b #source.TOKEN_UPDATE_LAST, 4(a0)
	bls.w compoundUpdate
labelPrefix
	cmpi.b #5, 4(a0)
	bne.w dispatch
	lea SectionState, a4
	tst.w sections.State.Mode(a4)
	beq.w labelSectionReady
	tst.w sections.State.Active(a4)
	beq.w bad
labelSectionReady
	.ASSEMBLY_FAILURE_STAGE FailureStage, #11
	tst.b 3(a0)
	bne.w bad
	moveq #0, d0
	move.w 1(a0), d0
	cmp.w pkg.Header.NameCount(a3), d0
	blo.w bad  ; package spellings are reserved in this bounded experiment
	cmp.l pkg.Context.Count(a2), d0
	bhs.w bad
	movea.l pkg.Context.Defined(a2), a4
	movea.l pkg.Context.Values(a2), a5
	move.l d0, d1
	lsl.l #3, d1
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w existingLabel
	tst.w SizingReplay
	bne.w storeLabel  ; retained labels are refreshed during strict sizing
	tst.b 0(a4, d0.l)
	bne.w bad
storeLabel
	move.b #1, 0(a4, d0.l)
	move.l pkg.Context.Pc(a2), exprvm.Value.Low(a5, d1.l)
	clr.l exprvm.Value.High(a5, d1.l)
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w labelReady
	movea.l pkg.Context.SectionIds(a2), a5
	moveq #0, d1
	move.w sections.State.HunkCurrent(a4), d1
	addq.b #1, d1
	move.b d1, 0(a5, d0.l)
	bra.w labelReady
existingLabel
	tst.l exprvm.Value.High(a5, d1.l)
	bne.w bad
	move.l pkg.Context.Pc(a2), d2
	cmp.l exprvm.Value.Low(a5, d1.l), d2
	bne.w bad  ; fail closed if this subset needs another layout iteration
labelReady
	addq.l #5, a0
	moveq #1, d7  ; an explicit label permits an instruction on the same line
dispatch
	cmpa.l a1, a0
	beq.w ok
	cmpi.b #7, (a0)
	beq.w directive
	tst.l d7
	beq.w bad
	bsr.w name
	bne.w bad
	; Name returns the numeric identity and qualifier without source reconstruction.
	clr.w HunkInstructionRefs
	clr.w HunkInstructionRefCount
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w instructionReady
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w instructionReady
	move.l d0, d5
	movea.l a0, a5
	move.w d1, -(sp)  ; retain the mnemonic qualifier across reference counting
	jsr hunkrefs.tokens
	move.w d1, HunkInstructionRefCount
	move.w (sp)+, d1
	cmpi.l #hunkrefs.STATUS_BAD, d0
	beq.w bad
	move.w d0, HunkInstructionRefs
	movea.l a5, a0
	move.l d5, d0
instructionReady
	.ASSEMBLY_FAILURE_STAGE FailureStage, #12
	.ASSEMBLY_FAILURE_STAGE FailureName, d0
	jsr encoding.encode
	tst.l d0
	bne.w bad
	movea.l a1, a5
	move.l d1, d5
	.ASSEMBLY_FAILURE_STAGE FailureStage, #14
	bsr.w markInstructionRelocs
	bne.w bad
	movea.l a1, a0
	move.l d1, d0
	.ASSEMBLY_FAILURE_STAGE FailureStage, #17
	bsr.w emitBytes
	bra.w done
compoundUpdate
	jsr mutable.update
	bra.w done
mutableDeclaration
	jsr mutable.execute
	bra.w done
structDeclaration
	moveq #0, d0
	move.b -3(a0), d0
	jsr constants.execute
	bra.w done
constant
	.ASSEMBLY_FAILURE_STAGE FailureStage, #15
	movem.l a0-a3, -(sp)
	movea.l a0, a1
	movea.l 4(sp), a3
	lea RepeatState+repetition.State.Guard, a0
	jsr guards.declaration
	movem.l (sp)+, a0-a3
	bne.w bad
	moveq #0, d0
	move.b -3(a0), d0
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w constantInitial
	tst.w SizingReplay
	beq.w constantInitial
	jsr constants.replay
	bra.w done
constantInitial
	jsr constants.execute
	bra.w done
directive
	.ASSEMBLY_FAILURE_STAGE FailureStage, #16
	addq.l #1, a0
	bsr.w name
	bne.w bad
	.ASSEMBLY_FAILURE_STAGE FailureName, d0
	tst.l d1
	bne.w bad
	cmp.w pkg.Header.CpuDirective(a3), d0
	beq.w cpu
	cmp.w pkg.Header.OrgDirective(a3), d0
	beq.w origin
	cmp.w pkg.Header.EndDirective(a3), d0
	beq.w end
	cmp.w pkg.Header.AlignDirective(a3), d0
	beq.w align
	cmp.w pkg.Header.DsDirective(a3), d0
	beq.w reserveByteCount
	cmp.w pkg.Header.ResDirective(a3), d0
	beq.w reserve
	cmp.w pkg.Header.EmitDirective(a3), d0
	beq.w packedData
	moveq #1, d6
	cmp.w pkg.Header.ByteDirective(a3), d0
	beq.w data
	moveq #2, d6
	cmp.w pkg.Header.WordDirective(a3), d0
	beq.w data
	moveq #4, d6
	cmp.w pkg.Header.LongDirective(a3), d0
	beq.w data
	movem.l a2, -(sp)
	movea.l a3, a2
	jsr state.apply
	movem.l (sp)+, a2
	tst.l d0
	beq.w ok
	bra.w bad
packedData
	bsr.w emitData
	bra.w done
cpu
	bsr.w name
	bne.w bad
	tst.l d1
	bne.w bad
	cmp.w pkg.Header.CpuName(a3), d0
	bne.w bad  ; the capsule declares this experiment's single pipeline
	cmpa.l a1, a0
	bne.w bad
	movem.l a2, -(sp)
	movea.l a3, a2
	jsr state.reset
	movem.l (sp)+, a2
	bra.w done
origin
	lea SectionState, a4
	tst.w sections.State.Mode(a4)
	bne.w bad
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l pkg.Context.High(a2)
	bne.w bad
	tst.l d1
	bmi.w bad
	cmpa.l a1, a0
	bne.w bad
	cmp.l pkg.Header.MaxAddress(a3), d1
	bhi.w bad
	movea.l Session+session.State.Frame, a4
	tst.l Frame.Used(a4)
	beq.w setOrigin
	cmp.l pkg.Context.Pc(a2), d1
	bne.w bad
setOrigin
	move.l d1, pkg.Context.Pc(a2)
	move.l d1, RepeatState+repetition.State.ConditionPc
	bra.w ok
end
	cmpa.l a1, a0
	bne.w bad
	moveq #2, d0
	bra.w done
align
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l pkg.Context.High(a2)
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	tst.l d1
	ble.w bad
	move.l d1, d6
	subq.l #1, d6
	move.l d6, d0
	and.l d1, d0
	bne.w bad
	move.l pkg.Context.Pc(a2), d3
	move.l d3, d0
	add.l d6, d0
	bcs.w bad
	not.l d6
	and.l d0, d6
	sub.l d3, d6
	move.l d6, d0
	lea SectionState, a4
	cmpi.w #3, sections.State.ActiveKind(a4)
	bne.w alignBytes
	movea.l a4, a0
	movea.l a2, a1
	jsr sections.reserve
	bra.w done
alignBytes
	bsr.w emitGap
	bra.w done
reserveByteCount
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l pkg.Context.High(a2)
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	tst.l pkg.Context.Kind(a2)
	bne.w bad
	move.l d1, d0
	bsr.w reserveDataBytes
	bra.w done
reserve
	bsr.w name
	bne.w bad
	tst.l d1
	bne.w bad
	moveq #1, d6
	cmp.w pkg.Header.ByteDirective(a3), d0
	beq.w reserveCount
	moveq #2, d6
	cmp.w pkg.Header.WordDirective(a3), d0
	beq.w reserveCount
	moveq #4, d6
	cmp.w pkg.Header.LongDirective(a3), d0
	bne.w bad
reserveCount
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #4, (a0)+
	bne.w bad
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l pkg.Context.High(a2)
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	tst.l d1
	bmi.w bad
	move.l d1, d0
	mulu.w d6, d0
	; MULU.W would truncate a large count; reject instead of reserving less.
	move.l d1, d3
	swap d3
	tst.w d3
	bne.w bad
	lea SectionState, a0
	movea.l a2, a1
	jsr sections.reserve
	bra.w done
data
	move.l pkg.Context.Pc(a2), DataPc
dataValueStart
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #3, (a0)
	bne.w dataExpression
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d1
	move.b 1(a0), d1
	move.l d1, d5
	addq.l #2, d5
	cmp.l d5, d0
	blo.w bad
	movea.l a0, a5
	adda.l d5, a5  ; source continuation survives encoding buffer ownership
	movem.l a1-a3, -(sp)
	lea 2(a0), a1
	lea TextState, a0
	jsr stringProvider.encode
	movea.l a1, a4
	movem.l (sp)+, a1-a3
	bne.w bad
	tst.l d1
	beq.w bad
	cmpi.l #1, d1
	beq.w dataStringScalar
	move.l d1, d0
	movea.l a4, a0
	bsr.w emitBytes
	bne.w bad
	movea.l a5, a0
	bra.w dataNext
dataStringScalar
	moveq #0, d1
	move.b (a4), d1
	movea.l a5, a0
	bra.w dataEmitValue
dataExpression
	movea.l a0, a5
	movea.l a2, a6
	move.l pkg.Context.Pc(a2), -(sp)
	move.l DataPc, pkg.Context.Pc(a2)
	jsr expr.evaluate
	movea.l a6, a2
	move.l (sp)+, pkg.Context.Pc(a2)
	tst.l d0
	bne.w bad
	tst.l d2
	beq.w dataValue
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w bad
	; Fixed-width unresolved data reserves layout in pass one. Its provisional
	; value and incomplete section provenance cannot prove range or relocation.
	moveq #0, d1
	bra.w dataEmitValue
dataValue
	; Shared data directives narrow to low u32, then to their output width.
	; Only byte truncation warns; .emit has its separate strict unit checks.
	cmpi.w #1, d6
	bne.w dataRangeOk
	cmpi.l #255, d1
	bls.w dataRangeOk
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w dataRangeOk
	bsr.w warnByte
	bne.w bad
dataRangeOk
	bsr.w markDataReloc
	bne.w bad
dataEmitValue
	movea.l a0, a5
	lea DataBytes, a4
	move.l d6, d5
	tst.w pkg.Header.LittleEndian(a3)
	beq.w bigEndian
littleLoop
	move.b d1, (a4)+
	lsr.l #8, d1
	subq.w #1, d5
	bne.w littleLoop
	bra.w dataReady
bigEndian
	move.l d6, d4
	subq.l #1, d4
	lsl.l #3, d4
bigLoop
	move.l d1, d3
	lsr.l d4, d3
	move.b d3, (a4)+
	subi.l #8, d4
	subq.w #1, d5
	bne.w bigLoop
dataReady
	lea DataBytes, a0
	move.l d6, d0
	bsr.w emitBytes
	tst.l d0
	bne.w bad
	movea.l a5, a0
dataNext
	cmpa.l a1, a0
	beq.w ok
	cmpi.b #4, (a0)+
	bne.w bad
	bra.w dataValueStart
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; statement

; D1=low-u32 byte value. Report one final-pass warning through its owner.
; D0/CCR=status; other registers preserved. No allocation or source parsing.
warnByte	.block
	movem.l a0-a1, -(sp)
	movea.l Session+session.State.Frame, a0
	addq.l #1, Frame.Warnings(a0)
	bcs.w bad
	movea.l Frame.Warn(a0), a1
	move.l a1, d0
	beq.w done
	jsr (a1)
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; warnByte

; Translate package-VM absolute-long output fixups to section-offset Hunk
; records. Instruction bytes and their fixup offsets came from the selected
; package program; no opcode or operand spelling is interpreted here.
; A2=Context,D5=instruction bytes,A5=output. Preserves caller registers.
markInstructionRelocs	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w good
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w good
	tst.w sections.State.HunkSelected(a4)
	beq.w good
	jsr encoding.outputFixupCount
	move.l d0, d6
	cmpi.w #hunkrefs.STATUS_SECTION, HunkInstructionRefs
	bne.w countReady
	jsr encoding.outputPositionProof
	tst.l d0
	beq.w absoluteOnly
	; Mixed positional and absolute fixups need per-reference accounting.
	; This bounded Hunk slice accepts one PC cancellation only when it is the
	; instruction's sole section reference and emits no Hunk relocation.
	cmpi.l #1, d0
	bne.w bad
	tst.l d6
	bne.w bad
	cmpi.w #1, HunkInstructionRefCount
	bne.w bad
	; Projection binds an opaque target directly to its canonical section.
	; The package VM proved cancellation; it never interprets this identity.
	move.w d1, d2
	moveq #0, d3
	move.w sections.State.HunkCurrent(a4), d3
	addq.w #1, d3
	cmp.w d3, d2
	bne.w bad
	bra.w countReady
absoluteOnly
	tst.l d6
	beq.w bad  ; fail closed without a package-proven instruction fixup
countReady
	moveq #0, d7
nextFixup
	cmp.l d6, d7
	bhs.w good
	move.l d7, d0
	jsr encoding.outputFixup
	tst.l d0
	bne.w bad
	cmpi.l #4, d2
	bne.w bad
	move.l d1, d2
	addq.l #4, d2
	bcs.w bad
	cmp.l d5, d2
	bhi.w bad
	move.l d1, -(sp)  ; package field offset
	move.l d4, -(sp)  ; encoded absolute address
	move.l d3, d1
	jsr hunkrefs.sectionBase
	bne.w fixupBad
	move.l (sp)+, d4
	; Instruction fixups permit signed addends represented in the long field.
	sub.l d1, d4
	move.l d3, -(sp)  ; canonical target section
	move.l d4, d1
	move.l d7, d0
	jsr encoding.patchOutputFixup
	bne.w targetBad
	move.l (sp)+, d4
	subq.l #1, d4
	move.l (sp)+, d2
	add.l pkg.Context.Pc(a2), d2
	bcs.w bad
	moveq #0, d1
	move.w sections.State.HunkCurrent(a4), d1
	addq.l #1, d1
	jsr hunkrefs.sectionBase
	bne.w bad
	sub.l d1, d2
	bcs.w bad
	move.l d4, d1
	moveq #0, d0
	move.w sections.State.HunkCurrent(a4), d0
	movea.l Session+session.State.Frame, a0
	movea.l Frame.AddReloc(a0), a1
	move.l a1, d3
	beq.w bad
	jsr (a1)
	tst.l d0
	bne.w bad
	addq.l #1, d7
	bra.w nextFixup
	; Keep the bounded fixup stack balanced on every validation failure.
targetBad
fixupBad
	addq.l #4, sp
	addq.l #4, sp
	bra.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; markInstructionRelocs

; Classify shared DATA expressions after the caller evaluates their scalar value.
; Same-section address differences are absolute; one surviving section base
; requires a long and records its section relocation only in pass two.
; A5=expression start,A1=bounded end,A2=Context,D6=unit bytes. D0/CCR=status.
; D1 returns the normalized scalar; other registers are preserved.
; ExprVM evaluation and unresolved handling stay caller-owned.
markDataReloc	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w good
	; Unselected sections evaluate data normally but have no Hunk payload.
	tst.w sections.State.HunkSelected(a4)
	beq.w good
	movea.l a5, a0
	jsr hunkrefs.affineTarget
	cmpi.l #hunkrefs.STATUS_CLEAR, d0
	beq.w good
	cmpi.l #hunkrefs.STATUS_SECTION, d0
	bne.w bad
	cmpi.w #4, d6
	bne.w bad
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w good
	jsr hunkrefs.baseSection
	bne.w bad
	move.l d1, d7
	jsr hunkrefs.sectionBase
	bne.w bad
	move.l (sp), d2  ; caller's evaluated scalar
	sub.l d1, d2
	bcs.w bad
	move.l d2, (sp)  ; returned value is now section relative
	moveq #0, d1
	move.w sections.State.HunkCurrent(a4), d1
	addq.l #1, d1
	jsr hunkrefs.sectionBase
	bne.w bad
	move.l pkg.Context.Pc(a2), d2
	sub.l d1, d2
	bcs.w bad
	move.l d7, d1
	subq.l #1, d1
	moveq #0, d0
	move.w sections.State.HunkCurrent(a4), d0
	movea.l Session+session.State.Frame, a0
	movea.l Frame.AddReloc(a0), a1
	move.l a1, d3
	beq.w bad
	jsr (a1)
	tst.l d0
	bne.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; markDataReloc

; Consume one bounded identifier token. D0=u16 ID, D1=u8 qualifier, A0 advances.
; CCR signals failure; on failure D0=-1. Other registers preserved.
name	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w fail
	cmpi.b #1, (a0)
	bhi.w fail
	moveq #0, d0
	move.w 1(a0), d0
	moveq #0, d1
	move.b 3(a0), d1
	addq.l #4, a0
	cmp.l d0, d0  ; explicit successful CCR, independent of the numeric ID
	rts
fail
	moveq #-1, d0
	rts
	.bend  ; name

	.include "binary_data_reserve.asm"

; D0=padding count,A2=Context. Flat address gaps are uninitialized; placed
; section payloads retain initialized padding, as in Rust's section replay.
; Bin/Hunk storage stays zero-filled. D0/CCR=status; other registers preserved.
; An already-aligned statement is a no-op.
emitGap	.block
	tst.l d0
	beq.w done
	movem.l d1/a0-a1, -(sp)
	movea.l Session+session.State.Frame, a1
	move.l Frame.Emitted(a1), d1
	tst.w SectionState+sections.State.Mode
	bne.w sectionPadding
	clr.l Frame.Emitted(a1)
sectionPadding
	suba.l a0, a0
	bsr.w emitBytes
	move.l d1, Frame.Emitted(a1)
	movem.l (sp)+, d1/a0-a1
done
	tst.l d0
	rts
	.bend  ; emitGap

; A0=initialized bytes (zero means initialized zero-fill),D0=count,A2=Context.
; Updates PC and Used; only pass two copies output.
; D0=status; other registers preserved. CCR reflects D0.
emitBytes	.block
	movem.l d1-d4/a0-a3, -(sp)
	tst.l d0
	beq.w noPendingGap
	tst.l PendingFlatGap
	beq.w noPendingGap
	movem.l d0/a0, -(sp)
	move.l PendingFlatGap, d0
	clr.l PendingFlatGap
	sub.l d0, pkg.Context.Pc(a2)
	bsr.w emitGap
	movem.l (sp)+, d0/a0
	bne.w fail
noPendingGap
	movea.l a0, a3
	move.l d0, d4
	lea SectionState, a0
	movea.l a2, a1
	jsr sections.checkEmit
	bne.w fail
	movea.l a3, a0
	move.l d4, d0
	movea.l Session+session.State.Frame, a3
	move.l Frame.Used(a3), d1
	lea SectionState, a1
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a1)
	bne.w ordinaryOffset
	movem.l d0/a0-a1, -(sp)
	movea.l a1, a0
	movea.l a2, a1
	jsr sections.routeHunk
	movem.l (sp)+, d0/a0-a1
	bne.w fail
	tst.w d2
	beq.w unselectedEmit
	bra.w routedOffset
ordinaryOffset
	move.l Frame.Used(a3), d1
routedOffset
	move.l Frame.Used(a3), d2
	add.l d0, d2
	bcs.w fail
	cmpi.w #1, pkg.Context.Pass(a2)
	beq.w capacityReady
	cmp.l Frame.Capacity(a3), d2
	bhi.w fail
	move.l d1, d4
	add.l d0, d4
	bcs.w fail
	cmp.l Frame.Capacity(a3), d4
	bhi.w fail
capacityReady
	move.l pkg.Context.Pc(a2), d3
	add.l d0, d3
	bcs.w fail
	movea.l pkg.Context.Package(a2), a1
	move.l d3, d4
	tst.l d0
	beq.w checked
	subq.l #1, d4
	cmp.l pkg.Header.MaxAddress(a1), d4
	bhi.w fail
checked
	; Passive numeric observation is requested only by artifact consumers.
	cmpi.w #1, pkg.Context.Pass(a2)
	beq.w observed
	tst.l d0
	beq.w observed
	move.l Frame.Emitted(a3), d4
	beq.w observed
	movem.l d0-d4/a0-a3, -(sp)
	movea.l d4, a1
	move.l d0, d3
	move.l d1, d2
	move.l pkg.Context.Pc(a2), d1
	movea.l a3, a0
	jsr (a1)
	tst.l d0
	movem.l (sp)+, d0-d4/a0-a3
	bne.w fail
observed
	move.l d3, pkg.Context.Pc(a2)
	move.l d2, Frame.Used(a3)
	cmpi.w #1, pkg.Context.Pass(a2)
	beq.w ok
	movea.l Frame.Output(a3), a1
	adda.l d1, a1
	tst.l d0
	beq.w ok
	move.l a0, d3
	beq.w zeroCopy
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
ok
	moveq #0, d0
	bra.w done
zeroCopy
	clr.b (a1)+
	subq.l #1, d0
	bne.w zeroCopy
	bra.w ok
unselectedEmit
	; Omitted payload still executes with a section-local PC and symbol state.
	move.l pkg.Context.Pc(a2), d3
	add.l d0, d3
	bcs.w fail
	move.l d3, pkg.Context.Pc(a2)
	bra.w ok
fail
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a3
	tst.l d0
	rts
	.bend  ; emit

	.include "binary_data_emit.asm"
	.endsection
	.endmodule
