; Fixed bounded capture ownership/work observer. No retained owner pointers.
; Imported only by the three-gated capture telemetry wrappers.
; @opforge-owner: debug.amigaos.capture_profile
	.module debug.amigaos.capture_profile
	.cpu 68020
.ifdef OPFORGE_PREPARATION_PROGRESS
	.use debug.amigaos.memory_profile as memory_profile
.endif
	.pub
SCHEMA_PHASE = 400
COUNTERS_PHASE = 401
ACQUISITION_PHASE = 416
REPLAY_PHASE = 424
RELEASE_PHASE = 432
OUTCOME_COPY_BYTES = 7*4
BlockView	.struct
Pointer	.long ?
Capacity	.long ?
Used	.long ?
.endstruct
OwnerSnapshot	.struct
Regions	.long ?
RetainedUsed	.long ?
RetainedCapacity	.long ?
RetainedSlack	.long ?
TemporaryUsed	.long ?
TemporaryCapacity	.long ?
TableUsed	.long ?
TableCapacity	.long ?
OrderUsed	.long ?
OrderCapacity	.long ?
TemporarySlack	.long ?
TableSlack	.long ?
OrderSlack	.long ?
Reserved0	.long ?
Reserved1	.long ?
.endstruct
SNAPSHOT_BYTES = OwnerSnapshot.Reserved1+4
Fields	.struct
Errors	.long ?
Mask	.long ?
Records	.long ?
Headers	.long ?
Tokens	.long ?
Lexemes	.long ?
Plans	.long ?
Padding	.long ?
Source	.long ?
Constructed	.long ?
Cleared	.long ?
ConstructionCopies	.long ?
AppendCopies	.long ?
ConfigurationCopies	.long ?
ReplayCopies	.long ?
GlobalGrowCalls	.long ?
GlobalGrowCopies	.long ?
RetainedGrowCalls	.long ?
RetainedGrowCopies	.long ?
RetainedPeak	.long ?
RetainedLive	.long ?
ReplayMode	.long ?
GrowMode	.long ?
Snapshots	.res 3*SNAPSHOT_BYTES
.endstruct
STATE_BYTES = Fields.Snapshots+3*SNAPSHOT_BYTES
OVERFLOW = 1
INVALID_EXTENT = 2
OWNER_UNDERFLOW = 4
; These local macros only serve the fully preserving observer routines.
ADD_FIELD	.macro field, amount
	move.l .amount, d0
	move.l #.field, d7
	bsr.w addValue
.endmacro
SUM	.macro amount, total
	add.l .amount, .total
	bcc.w summed
	moveq #-1, .total
	ori.l #OVERFLOW, Fields.Errors(a6)
summed
.endmacro
	.section code, kind=code
; Reset one execute's observer. All passive APIs preserve every register, CCR
; and stack depth. Storage is dedicated; no allocation, clocks or I/O here.
clear	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea State, a0
	move.w #STATE_BYTES/4-1, d0
next
	clr.l (a0)+
	dbra d0, next
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; clear

; Bracket only a retained arena reserve; no owner address survives relocation.
growBegin	.block
	move.w ccr, -(sp)
	move.l a0, -(sp)
	lea State, a0
	move.l #1, Fields.GrowMode(a0)
	movea.l (sp)+, a0
	move.w (sp)+, ccr
	rts
	.bend  ; growBegin

growEnd	.block
	move.w ccr, -(sp)
	move.l a0, -(sp)
	lea State, a0
	clr.l Fields.GrowMode(a0)
	movea.l (sp)+, a0
	move.w (sp)+, ccr
	rts
	.bend  ; growEnd

; Following acquisition, materializations belong to semantic replay.
replayBegin	.block
	move.w ccr, -(sp)
	move.l a0, -(sp)
	lea State, a0
	move.l #1, Fields.ReplayMode(a0)
	movea.l (sp)+, a0
	move.w (sp)+, ccr
	rts
	.bend  ; replayBegin

; D0=actual cleared bytes,D1=tokens,D2=lexemes,D3=plans. Count actual
; constructor work, including records later filtered from retained capture.
construction	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea State, a6
	move.l d0, d4
	.ADD_FIELD Fields.Constructed, #1
	.ADD_FIELD Fields.Cleared, d4
	mulu.w #20, d1
	.ADD_FIELD Fields.ConstructionCopies, #OUTCOME_COPY_BYTES
	.ADD_FIELD Fields.ConstructionCopies, d1
	.ADD_FIELD Fields.ConstructionCopies, d2
	.ADD_FIELD Fields.ConstructionCopies, d3
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; construction

; D1=tokens,D2=lexemes,D3=plans,D4=source,D5=appended bytes,D6=header
; extent. Called only after the retained region publishes the copied record.
append	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea State, a6
	.ADD_FIELD Fields.Records, #1
	.ADD_FIELD Fields.Headers, d6
	mulu.w #20, d1
	.ADD_FIELD Fields.Tokens, d1
	.ADD_FIELD Fields.Lexemes, d2
	.ADD_FIELD Fields.Plans, d3
	.ADD_FIELD Fields.Source, d4
	.ADD_FIELD Fields.AppendCopies, d5
	sub.l d6, d5
	bcs.w invalid
	sub.l d1, d5
	bcs.w invalid
	sub.l d2, d5
	bcs.w invalid
	sub.l d3, d5
	bcs.w invalid
	.ADD_FIELD Fields.Padding, d5
	bra.w done
invalid
	ori.l #INVALID_EXTENT, Fields.Errors(a6)
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; append

; D0=bytes copied from capture into frontend scratch; observes both preview
; and ordinary materialization, separated by the acquisition/replay boundary.
materialize	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea State, a6
	move.l #Fields.ConfigurationCopies, d7
	tst.l Fields.ReplayMode(a6)
	beq.w count
	move.l #Fields.ReplayCopies, d7
count
	bsr.w addValue
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; materialize

; D1=Used bytes copied,D2=old capacity,D3=new capacity. Called after a
; successful allocation, before old storage is freed. Global counts cover all
; binary_memory owners in this execute, not only capture. Context attributes
; retained arena growth without storing a movable descriptor pointer.
grow	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea State, a6
	.ADD_FIELD Fields.GlobalGrowCalls, #1
	.ADD_FIELD Fields.GlobalGrowCopies, d1
	tst.l Fields.GrowMode(a6)
	beq.w done
	.ADD_FIELD Fields.RetainedGrowCalls, #1
	.ADD_FIELD Fields.RetainedGrowCopies, d1
	move.l Fields.RetainedLive(a6), d4
	.SUM d3, d4
	cmp.l Fields.RetainedPeak(a6), d4
	bls.w update
	move.l d4, Fields.RetainedPeak(a6)
update
	move.l Fields.RetainedLive(a6), d4
	sub.l d2, d4
	bcs.w invalid
	.SUM d3, d4
	move.l d4, Fields.RetainedLive(a6)
	bra.w done
invalid
	ori.l #OWNER_UNDERFLOW, Fields.Errors(a6)
	move.l #$ffffffff, Fields.RetainedLive(a6)
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; grow

; D0=capacity about to be freed. Zero-capacity repeated cleanup is harmless.
retainedRelease	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea State, a0
	sub.l d0, Fields.RetainedLive(a0)
	bcc.w done
	clr.l Fields.RetainedLive(a0)
	ori.l #OWNER_UNDERFLOW, Fields.Errors(a0)
done
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; retainedRelease

; D0=boundary0..2,D1=region count,D2=descriptor stride,D3=arena offset;
; D4=published order count,D5=order stride; A0=region table Block,
; A1=temporary Block,A2=order Block. Order used is logical count*stride
; because its writer does not publish memory.Block.Used. Boundaries scan
; descriptors once, never per physical line. Snapshots are latest per stage.
; BlockView names the shared memory.Block ABI without a cyclic owner import.
; Descriptors are bounded
; by both the published table extent and its capacity before any region read.
snapshot	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea State, a6
	cmpi.l #2, d0
	bhi.w invalid
	bset d0, Fields.Mask+3(a6)
	mulu.w #SNAPSHOT_BYTES, d0
	lea Fields.Snapshots(a6), a5
	adda.l d0, a5
	move.l d1, OwnerSnapshot.Regions(a5)
	move.l BlockView.Used(a0), OwnerSnapshot.TableUsed(a5)
	move.l BlockView.Capacity(a0), OwnerSnapshot.TableCapacity(a5)
	move.l BlockView.Used(a1), OwnerSnapshot.TemporaryUsed(a5)
	move.l BlockView.Capacity(a1), OwnerSnapshot.TemporaryCapacity(a5)
	move.l BlockView.Capacity(a2), OwnerSnapshot.OrderCapacity(a5)
	cmpi.l #65535, d4
	bhi.w invalid
	cmpi.l #65535, d5
	bhi.w invalid
	mulu.w d5, d4
	cmp.l BlockView.Capacity(a2), d4
	bhi.w invalid
	move.l d4, OwnerSnapshot.OrderUsed(a5)
	cmpi.l #65535, d1
	bhi.w invalid
	cmpi.l #65535, d2
	bhi.w invalid
	move.l d3, d4
	addi.l #12, d4
	bcs.w invalid
	cmp.l d2, d4
	bhi.w invalid
	move.l d1, d4
	mulu.w d2, d4
	cmp.l BlockView.Used(a0), d4
	bhi.w invalid
	move.l BlockView.Used(a0), d4
	cmp.l BlockView.Capacity(a0), d4
	bhi.w invalid
	movea.l BlockView.Pointer(a0), a3
	moveq #0, d4
	moveq #0, d5
next
	tst.l d1
	beq.w totals
	lea 0(a3, d3.l), a4
	move.l BlockView.Used(a4), d0
	cmp.l BlockView.Capacity(a4), d0
	bhi.w invalid
	.SUM d0, d4
	.SUM BlockView.Capacity(a4), d5
	adda.l d2, a3
	subq.l #1, d1
	bra.w next
totals
	move.l d4, OwnerSnapshot.RetainedUsed(a5)
	move.l d5, OwnerSnapshot.RetainedCapacity(a5)
	sub.l d4, d5
	bcs.w invalid
	move.l d5, OwnerSnapshot.RetainedSlack(a5)
	move.l OwnerSnapshot.TemporaryCapacity(a5), d0
	sub.l OwnerSnapshot.TemporaryUsed(a5), d0
	bcs.w invalid
	move.l d0, OwnerSnapshot.TemporarySlack(a5)
	move.l OwnerSnapshot.TableCapacity(a5), d0
	sub.l OwnerSnapshot.TableUsed(a5), d0
	bcs.w invalid
	move.l d0, OwnerSnapshot.TableSlack(a5)
	move.l OwnerSnapshot.OrderCapacity(a5), d0
	sub.l OwnerSnapshot.OrderUsed(a5), d0
	bcs.w invalid
	move.l d0, OwnerSnapshot.OrderSlack(a5)
	bra.w done
invalid
	ori.l #INVALID_EXTENT, Fields.Errors(a6)
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; snapshot

.ifdef OPFORGE_PREPARATION_PROGRESS
; A0=DOS base. Emit all 22 schema rows once at terminal save, even failure.
; This is the only observer I/O, through the existing progress bridge.
report	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a4
	lea State, a6
	move.l #SCHEMA_PHASE, d0
	moveq #1, d1
	move.l Fields.Errors(a6), d2
	move.l Fields.Mask(a6), d3
	jsr memory_profile.progress
	lea Fields.Records(a6), a5
	move.l #COUNTERS_PHASE, d4
	moveq #5, d5
counters
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d5, counters
	lea Fields.Snapshots(a6), a5
	move.l #ACQUISITION_PHASE, d4
	moveq #2, d6
boundary
	moveq #4, d5
rows
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d5, rows
	addq.l #3, d4
	dbra d6, boundary
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; report
.endif
	.priv
; D0=unsigned increment,D7=field offset,A6=State. Saturate every cumulative
; counter and expose overflow. No byte/token probes and no wrapping counters.
addValue	.block
	lea 0(a6, d7.l), a0
	add.l d0, (a0)
	bcc.w done
	move.l #$ffffffff, (a0)
	ori.l #OVERFLOW, Fields.Errors(a6)
done
	rts
	.bend  ; addValue
	.endsection
	.section bss, kind=bss
State	.res byte, STATE_BYTES
	.endsection
	.endmodule
