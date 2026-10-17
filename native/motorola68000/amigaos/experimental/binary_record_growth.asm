; Prepared-record ownership: optional disk staging avoids large growth overlap.
; @opforge-owner: experimental.amigaos.binary_record_growth
	.module experimental.amigaos.binary_record_growth
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_capture_io as io
	.include "memory_telemetry.i"
	.pub
State	.struct
Dos	.long ?
Directory	.long ?
Io	.res io.CONTEXT_BYTES
Failure	.long ?
.endstruct
STATE_BYTES = State.Failure+4
THRESHOLD = 1048576
FAILURE_IO = 1
FAILURE_ALLOCATION = 2
FAILURE_INPUT = 3
PROGRESS_WORK = 365
PROGRESS_CAPACITY = 366

; The existing capture gates own these aggregate work counters. Release builds
; retain neither counter storage nor calls to the progress observer.
GROWTH_COUNT	.macro counter, amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	.MEMORY_COUNTER_ADD .counter, .amount
.endif
.endif
.endif
.endmacro
GROWTH_REPORT	.macro dosbase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	.MEMORY_PROGRESS .dosbase, #PROGRESS_WORK, Completed, WrittenBytes, ReadBytes
	.MEMORY_PROGRESS .dosbase, #PROGRESS_CAPACITY, ReleasedCapacity, #0, #0
	.MEMORY_COUNTER_CLEAR Completed
	.MEMORY_COUNTER_CLEAR WrittenBytes
	.MEMORY_COUNTER_CLEAR ReadBytes
	.MEMORY_COUNTER_CLEAR ReleasedCapacity
.endif
.endif
.endif
.endmacro
	.section code, kind=code
; A0=Block,A1=State,D0=minimum capacity,D1=capacity limit. D0/CCR=status;
; other registers preserved. Caller initializes Dos/Directory; zero Directory
; keeps ordinary memory growth. Only actual growth of a nonempty large block
; uses disk. No borrowed pointer may survive growth (same as memory.reserve).
;
; Invalid requests/configuration/write failures preserve the old block. Once
; staged bytes are durable, release old RAM before allocation. Allocation/read
; failure then leaves an empty block and requires the caller to abort preparation.
; Used is published only after exact restoration; State owns scratch until cleanup.
reserve	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	movea.l a1, a5
	cmp.l d1, d0
	bhi.w ordinary
	cmp.l memory.Block.Capacity(a4), d0
	bls.w good
	tst.l State.Directory(a5)
	beq.w ordinary
	cmpi.l #THRESHOLD, memory.Block.Capacity(a4)
	blo.w ordinary
	move.l memory.Block.Used(a4), d4
	beq.w ordinary
	cmp.l memory.Block.Capacity(a4), d4
	bhi.w invalid
	tst.l memory.Block.Pointer(a4)
	beq.w invalid
	move.l d0, d6
	move.l d1, d7
	move.l memory.Block.Capacity(a4), d5
	tst.l State.Dos(a5)
	beq.w invalid
	tst.l State.Io+io.Context.File(a5)
	bne.w write
	lea State.Io(a5), a0
	movea.l State.Dos(a5), a1
	movea.l State.Directory(a5), a2
	jsr io.configure
	bne.w ioFailed
write
	lea State.Io(a5), a0
	moveq #0, d1
	movea.l memory.Block.Pointer(a4), a1
	move.l d4, d2
	jsr io.write
	bne.w ioFailed
	.GROWTH_COUNT WrittenBytes, d4
	; Used must be zero before reserve: release deliberately retains it.
	movea.l a4, a0
	jsr memory.release
	clr.l memory.Block.Used(a4)
	.GROWTH_COUNT ReleasedCapacity, d5
	move.l d6, d0
	move.l d7, d1
	jsr memory.reserveBounded
	bne.w allocationFailed
	lea State.Io(a5), a0
	moveq #0, d1
	movea.l memory.Block.Pointer(a4), a1
	move.l d4, d2
	jsr io.read
	bne.w restoreFailed
	.GROWTH_COUNT ReadBytes, d4
	move.l d4, memory.Block.Used(a4)
	.GROWTH_COUNT Completed, #1
	bra.w good
ordinary
	movea.l a4, a0
	jsr memory.reserveBounded
	bra.w done
restoreFailed
	movea.l a4, a0
	jsr memory.release
	clr.l memory.Block.Used(a4)
ioFailed
	move.l #FAILURE_IO, State.Failure(a5)
	bra.w bad
allocationFailed
	move.l #FAILURE_ALLOCATION, State.Failure(a5)
	bra.w bad
invalid
	move.l #FAILURE_INPUT, State.Failure(a5)
bad
	moveq #1, d0
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; reserve

; A0=State. Close/delete only its owned scratch and zero state. Idempotent;
; preserves registers, CCR unspecified. Call before closing the DOS library.
cleanup	.block
	movem.l d0-d1/a0-a2, -(sp)
	movea.l a0, a2
	tst.l State.Dos(a2)
	beq.w clear
	.GROWTH_REPORT State.Dos(a2)
	lea State.Io(a2), a0
	jsr io.cleanup
clear
	movea.l a2, a0
	move.w #STATE_BYTES/4-1, d1
next
	clr.l (a0)+
	dbra d1, next
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; cleanup
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	.section bss, kind=bss
Completed	.res long, 1
WrittenBytes	.res long, 1
ReadBytes	.res long, 1
ReleasedCapacity	.res long, 1
	.endsection
.endif
.endif
.endif
	.endmodule
