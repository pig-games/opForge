; Bounded sampled inclusive/exclusive routine observer; no owner pointers.
; @opforge-owner: debug.amigaos.hotspot_profile
	.module debug.amigaos.hotspot_profile
	.cpu 68020
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_HOTSPOT_TELEMETRY
	.use debug.amigaos.memory_profile as memory_profile
.ifndef OPFORGE_HOTSPOT_SAMPLE_PERIOD
OPFORGE_HOTSPOT_SAMPLE_PERIOD = 128
.endif
.ifndef OPFORGE_HOTSPOT_PHASE_SEED
OPFORGE_HOTSPOT_PHASE_SEED = 0
.endif
	.pub
SLOT_COUNT = 16
STACK_CAPACITY = 32
SCHEMA_PHASE = 500
STATE_PHASE = 501
CONFIG_PHASE = 502
SLOT_PHASE = 512
	.priv
TIMED = 1
OWN_SAMPLE = 2
INVALID_SLOT = 1
STACK_OVERFLOW = 2
MISMATCH_ERROR = 4
CLOCK_UNAVAILABLE = 8
FREQUENCY_CHANGED = 16
ARITHMETIC_OVERFLOW = 32
INVALID_PERIOD = 64
UNFINISHED = 128
TickPair	.struct
High	.long ?
Low	.long ?
.endstruct
TICKS_BYTES = TickPair.Low+4
Slot	.struct
Calls	.long ?
Samples	.long ?
Windows	.long ?
Inclusive	.res TICKS_BYTES
Exclusive	.res TICKS_BYTES
.endstruct
SLOT_BYTES = Slot.Exclusive+TICKS_BYTES
Frame	.struct
Identity	.long ?
Flags	.long ?
Start	.res TICKS_BYTES
Children	.res TICKS_BYTES
.endstruct
FRAME_BYTES = Frame.Children+TICKS_BYTES
Fields	.struct
Errors	.long ?
Frequency	.long ?
MaxDepth	.long ?
Depth	.long ?
Stopped	.long ?
Stamp	.res TICKS_BYTES
Slots	.res SLOT_COUNT*SLOT_BYTES
Frames	.res STACK_CAPACITY*FRAME_BYTES
.endstruct
STATE_BYTES = Fields.Frames+STACK_CAPACITY*FRAME_BYTES
	.section code, kind=code
	.pub
; Reset at one execute entry. All passive APIs preserve D0-D7/A0-A6, CCR
; and stack depth, and use only dedicated bounded storage. No allocations.
clear	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea State, a0
	move.w #STATE_BYTES/4-1, d0
next
	clr.l (a0)+
	dbra d0, next
	move.l #OPFORGE_HOTSPOT_SAMPLE_PERIOD, d0
	cmpi.l #64, d0
	beq.w done
	cmpi.l #128, d0
	beq.w done
	cmpi.l #256, d0
	beq.w done
	lea State, a0
	move.l #INVALID_PERIOD, Fields.Errors(a0)
	move.l #1, Fields.Stopped(a0)
done
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; clear

; D0=slot 0..15. Each actual invocation is counted, including recursion.
; Independently sample a per-slot fixed phase; deterministic periodic samples
; can alias periodic workloads. A different seed/period is a sensitivity check.
; Only an own sample or direct child of an own sample needs an E-clock read:
; a child's complete wall interval already includes its deeper descendants.
enter	.block
	move.w ccr, -(sp)
	movem.l d0-d3/a0-a3, -(sp)
	lea State, a2
	tst.l Fields.Stopped(a2)
	bne.w done
	cmpi.l #SLOT_COUNT-1, d0
	bhi.w invalid
	move.l d0, d1
	mulu.w #SLOT_BYTES, d1
	lea Fields.Slots(a2), a3
	adda.l d1, a3
	lea Slot.Calls(a3), a0
	bsr.w increment
	bcs.w done
	move.l Fields.Depth(a2), d1
	cmpi.l #STACK_CAPACITY, d1
	bhs.w overflow
	mulu.w #FRAME_BYTES, d1
	lea Fields.Frames(a2), a1
	adda.l d1, a1
	move.l d0, Frame.Identity(a1)
	clr.l Frame.Flags(a1)
	clr.l Frame.Children+TickPair.High(a1)
	clr.l Frame.Children+TickPair.Low(a1)
	move.l Slot.Calls(a3), d1
	subq.l #1, d1
	mulu.w #17, d0
	add.l d0, d1
	addi.l #OPFORGE_HOTSPOT_PHASE_SEED, d1
	andi.l #OPFORGE_HOTSPOT_SAMPLE_PERIOD-1, d1
	bne.w parent
	move.l #TIMED+OWN_SAMPLE, Frame.Flags(a1)
parent
	tst.l Fields.Depth(a2)
	beq.w pushed
	lea -FRAME_BYTES(a1), a0
	move.l Frame.Flags(a0), d1
	andi.l #OWN_SAMPLE, d1
	beq.w pushed
	ori.l #TIMED, Frame.Flags(a1)
pushed
	addq.l #1, Fields.Depth(a2)
	move.l Fields.Depth(a2), d1
	cmp.l Fields.MaxDepth(a2), d1
	bls.w clock
	move.l d1, Fields.MaxDepth(a2)
clock
	tst.l Frame.Flags(a1)
	beq.w done
	lea Frame.Start(a1), a0
	bsr.w getClock
	bra.w done
invalid
	moveq #INVALID_SLOT, d0
	bra.w fail
overflow
	moveq #STACK_OVERFLOW, d0
fail
	bsr.w stop
done
	movem.l (sp)+, d0-d3/a0-a3
	move.w (sp)+, ccr
	rts
	.bend  ; enter

; D0=matching slot. Completed own samples alone add inclusive/exclusive
; totals; forced-only child windows only subtract from their sampled parent.
; No output or clock read on an ordinary unsampled enter/leave path.
leave	.block
	move.w ccr, -(sp)
	movem.l d0-d3/a0-a3, -(sp)
	lea State, a2
	tst.l Fields.Stopped(a2)
	bne.w done
	cmpi.l #SLOT_COUNT-1, d0
	bhi.w invalid
	move.l Fields.Depth(a2), d1
	beq.w mismatch
	subq.l #1, d1
	move.l d1, d2
	mulu.w #FRAME_BYTES, d1
	lea Fields.Frames(a2), a1
	adda.l d1, a1
	cmp.l Frame.Identity(a1), d0
	bne.w mismatch
	move.l d2, Fields.Depth(a2)
	tst.l Frame.Flags(a1)
	beq.w done
	mulu.w #SLOT_BYTES, d0
	lea Fields.Slots(a2), a3
	adda.l d0, a3
	lea Fields.Stamp(a2), a0
	bsr.w getClock
	bcs.w done
	move.l TickPair.High(a0), d2
	move.l TickPair.Low(a0), d3
	sub.l Frame.Start+TickPair.Low(a1), d3
	move.l Frame.Start+TickPair.High(a1), d1
	subx.l d1, d2
	bcs.w arithmetic
	lea Slot.Windows(a3), a0
	bsr.w increment
	bcs.w done
	move.l Frame.Flags(a1), d1
	andi.l #OWN_SAMPLE, d1
	beq.w parent
	lea Slot.Samples(a3), a0
	bsr.w increment
	bcs.w done
	lea Slot.Inclusive(a3), a0
	bsr.w addTicks
	bcs.w done
parent
	tst.l Fields.Depth(a2)
	beq.w exclusive
	lea -FRAME_BYTES(a1), a0
	move.l Frame.Flags(a0), d1
	andi.l #OWN_SAMPLE, d1
	beq.w exclusive
	lea Frame.Children(a0), a0
	bsr.w addTicks
	bcs.w done
exclusive
	move.l Frame.Flags(a1), d1
	andi.l #OWN_SAMPLE, d1
	beq.w done
	sub.l Frame.Children+TickPair.Low(a1), d3
	move.l Frame.Children+TickPair.High(a1), d1
	subx.l d1, d2
	bcs.w arithmetic
	lea Slot.Exclusive(a3), a0
	bsr.w addTicks
	bra.w done
invalid
	moveq #INVALID_SLOT, d0
	bra.w fail
mismatch
	moveq #MISMATCH_ERROR, d0
	bra.w fail
arithmetic
	moveq #ARITHMETIC_OVERFLOW, d0
fail
	bsr.w stop
done
	movem.l (sp)+, d0-d3/a0-a3
	move.w (sp)+, ccr
	rts
	.bend  ; leave

.ifdef OPFORGE_PREPARATION_PROGRESS
; A0=DOS base. Fixed schema: three metadata rows plus three rows per slot.
; Report once at terminal save before memory_profile closes its timer.
report	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a4
	lea State, a6
	tst.l Fields.Depth(a6)
	beq.w metadata
	ori.l #UNFINISHED, Fields.Errors(a6)
metadata
	move.l #SCHEMA_PHASE, d0
	moveq #1, d1
	move.l Fields.Errors(a6), d2
	move.l #OPFORGE_HOTSPOT_SAMPLE_PERIOD, d3
	jsr memory_profile.progress
	move.l #STATE_PHASE, d0
	move.l Fields.Frequency(a6), d1
	move.l Fields.MaxDepth(a6), d2
	move.l Fields.Depth(a6), d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #CONFIG_PHASE, d0
	moveq #STACK_CAPACITY, d1
	move.l #OPFORGE_HOTSPOT_PHASE_SEED, d2
	move.l Fields.Stopped(a6), d3
	movea.l a4, a0
	jsr memory_profile.progress
	lea Fields.Slots(a6), a5
	move.l #SLOT_PHASE, d4
	moveq #0, d5
next
	move.l d4, d0
	move.l d5, d1
	move.l Slot.Calls(a5), d2
	move.l Slot.Samples(a5), d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	move.l d4, d0
	move.l Slot.Windows(a5), d1
	move.l Slot.Inclusive+TickPair.High(a5), d2
	move.l Slot.Inclusive+TickPair.Low(a5), d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	move.l d4, d0
	move.l Slot.Exclusive+TickPair.High(a5), d1
	move.l Slot.Exclusive+TickPair.Low(a5), d2
	moveq #0, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	adda.l #SLOT_BYTES, a5
	addq.l #1, d5
	cmpi.l #SLOT_COUNT, d5
	blo.w next
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; report
.endif
	.priv
; A0=timestamp, A2=state. D0=frequency; other registers preserved.
; C set means observation stopped, clear means consistent positive clock.
getClock	.block
	jsr memory_profile.readClock
	tst.l d0
	beq.w unavailable
	tst.l Fields.Frequency(a2)
	beq.w first
	cmp.l Fields.Frequency(a2), d0
	bne.w changed
	rts
first
	move.l d0, Fields.Frequency(a2)
	rts
unavailable
	moveq #CLOCK_UNAVAILABLE, d0
	bra.w failed
changed
	moveq #FREQUENCY_CHANGED, d0
failed
	bsr.w stop
	cmpi.l #2, Fields.Stopped(a2)  ; stopped=1: set carry without changing registers
	rts
	.bend  ; getClock

; A0=u32 counter, A2=state; preserves D0-D3. C set on saturation/failure.
increment	.block
	addq.l #1, (a0)
	bcc.w done
	move.l #$ffffffff, (a0)
	ori.l #ARITHMETIC_OVERFLOW, Fields.Errors(a2)
	move.l #1, Fields.Stopped(a2)
	cmpi.l #2, Fields.Stopped(a2)  ; stopped=1: set carry without changing registers
done
	rts
	.bend  ; increment

; A0=u64 total, D2:D3=increment, A2=state. Clobbers D1, C=overflow.
addTicks	.block
	add.l d3, TickPair.Low(a0)
	move.l TickPair.High(a0), d1
	addx.l d2, d1
	bcs.w overflow
	move.l d1, TickPair.High(a0)
	rts
overflow
	move.l #$ffffffff, TickPair.High(a0)
	move.l #$ffffffff, TickPair.Low(a0)
	ori.l #ARITHMETIC_OVERFLOW, Fields.Errors(a2)
	move.l #1, Fields.Stopped(a2)
	cmpi.l #2, Fields.Stopped(a2)  ; stopped=1: set carry without changing registers
	rts
	.bend  ; addTicks

; D0=error bit, A2=state. Retain observations and stop only this observer.
stop	.block
	or.l d0, Fields.Errors(a2)
	move.l #1, Fields.Stopped(a2)
	rts
	.bend  ; stop
	.endsection
	.section bss, kind=bss
	.align 4
State	.res byte, STATE_BYTES
	.endsection
.endif
.endif
.endif
	.endmodule
