; Native component checks for bounded record growth using real DOS scratch IO.
; @opforge-owner: tests.amigaos.record_growth_probe
	.module tests.amigaos.record_growth_probe
	.cpu 68020
	.use experimental.amigaos.binary_record_growth as growth
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_capture_io as io
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
BASE_BYTES = 1048576
GROWN_BYTES = 2097152
PROBE_FAILED = 88
	.pub
	.section code, kind=code
; No inputs. D0/CCR=status; all other registers preserved. No surviving resources.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #PROBE_FAILED, d7
	movea.l 4.w, a6
	lea DosName, a1
	moveq #36, d0
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	bsr.w reset
.ifdef OPFORGE_GROWTH_EXPECT_WRITE_FAILURE
	bra.w transportFailure
.endif
.ifdef OPFORGE_GROWTH_EXPECT_READ_FAILURE
	bra.w transportFailure
.endif
	; Small growth must bypass scratch even when the parent is invalid.
	lea MissingScratch, a2
	move.l a2, growth.State.Directory(a1)
	move.l #256, d0
	move.l #GROWN_BYTES, d1
	jsr growth.reserve
	bne.w finish
	movea.l memory.Block.Pointer(a0), a2
	move.b #$a5, (a2)
	move.l #1, memory.Block.Used(a0)
	move.l #512, d0
	jsr growth.reserve
	bne.w finish
	movea.l memory.Block.Pointer(a0), a2
	cmpi.b #$a5, (a2)
	bne.w finish
	tst.l growth.State.Io+io.Context.File(a1)
	bne.w finish
	; A zero directory keeps even large growth resident.
	bsr.w reset
	bsr.w seed
	bne.w finish
	move.l #BASE_BYTES+1, d0
	move.l #GROWN_BYTES, d1
	jsr growth.reserve
	bne.w finish
	bsr.w verify
	bne.w finish
	tst.l growth.State.Io+io.Context.File(a1)
	bne.w finish
	; Successful eligible growth restores every byte, then accepts an appended tail.
	bsr.w reset
	bsr.w seed
	bne.w finish
	lea Scratch, a2
	move.l a2, growth.State.Directory(a1)
	move.l #BASE_BYTES+1, d0
	move.l #GROWN_BYTES, d1
	jsr growth.reserve
	bne.w finish
	bsr.w verify
	bne.w finish
	tst.l growth.State.Io+io.Context.File(a1)
	beq.w finish
	movea.l memory.Block.Pointer(a0), a2
	adda.l #BASE_BYTES, a2
	move.l #$89abcdef, (a2)
	move.l #BASE_BYTES+4, memory.Block.Used(a0)
	move.l #BASE_BYTES+4, d0
	jsr growth.reserve
	bne.w finish
	movea.l memory.Block.Pointer(a0), a2
	adda.l #BASE_BYTES, a2
	cmpi.l #$89abcdef, (a2)
	bne.w finish
	; Over-limit rejects before changing the old block or its appended bytes.
	bsr.w snapshot
	move.l #GROWN_BYTES+1, d0
	jsr growth.reserve
	beq.w finish
	bsr.w unchanged
	bne.w finish
	; Scratch setup failure must preserve a full live allocation.
	bsr.w reset
	bsr.w seed
	bne.w finish
	lea MissingScratch, a2
	move.l a2, growth.State.Directory(a1)
	bsr.w snapshot
	move.l #BASE_BYTES+1, d0
	move.l #GROWN_BYTES, d1
	jsr growth.reserve
	beq.w finish
	bsr.w unchanged
	bne.w finish
	bsr.w verify
	bne.w finish
	; Once staged, an impossible allocation leaves an empty, abortable block.
	bsr.w reset
	bsr.w seed
	bne.w finish
	lea Scratch, a2
	move.l a2, growth.State.Directory(a1)
	move.l #$7fffffff, d0
	move.l d0, d1
	jsr growth.reserve
	beq.w finish
	tst.l memory.Block.Pointer(a0)
	bne.w finish
	tst.l memory.Block.Capacity(a0)
	bne.w finish
	tst.l memory.Block.Used(a0)
	bne.w finish
	moveq #0, d7
	bra.w finish

; Probe-only variants use separately patched temporary transport sources.
; They never select behavior inside the production growth implementation.
transportFailure
	bsr.w seed
	bne.w finish
	lea Scratch, a2
	move.l a2, growth.State.Directory(a1)
	bsr.w snapshot
	move.l #BASE_BYTES+1, d0
	move.l #GROWN_BYTES, d1
	jsr growth.reserve
	beq.w finish
.ifdef OPFORGE_GROWTH_EXPECT_READ_FAILURE
	tst.l memory.Block.Pointer(a0)
	bne.w finish
	tst.l memory.Block.Capacity(a0)
	bne.w finish
	tst.l memory.Block.Used(a0)
	bne.w finish
.else
	bsr.w unchanged
	bne.w finish
	bsr.w verify
	bne.w finish
.endif
	moveq #0, d7
finish
	bsr.w reset
	; cleanup is idempotent and clears every owned-context field.
	lea State, a0
	jsr growth.cleanup
	lea State, a2
	move.l #growth.STATE_BYTES, d2
cleared
	tst.b (a2)+
	bne.w cleanupFailed
	subq.l #1, d2
	bne.w cleared
	bra.w closeDos
cleanupFailed
	moveq #PROBE_FAILED, d7
closeDos
	movea.l DosBase, a1
	movea.l 4.w, a6
	jsr CLOSE_LIBRARY(a6)
	clr.l DosBase
done
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run

; Release all resources and return A0=empty Block,A1=State with DOS installed.
reset	.block
	lea Block, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State, a0
	jsr growth.cleanup
	move.l DosBase, growth.State.Dos(a0)
	movea.l a0, a1
	lea Block, a0
	rts
	.bend  ; reset

; Allocate and fill 1 MiB with distinct longword indices, including page identity.
seed	.block
	move.l #BASE_BYTES, d0
	move.l #GROWN_BYTES, d1
	jsr growth.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a2
	move.l #BASE_BYTES/4, d2
	moveq #0, d3
fill
	move.l d3, (a2)+
	addq.l #1, d3
	subq.l #1, d2
	bne.w fill
	move.l #BASE_BYTES, memory.Block.Used(a0)
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; seed

; Check all bytes across the threshold, including both ends and every page.
verify	.block
	cmpi.l #BASE_BYTES, memory.Block.Used(a0)
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	move.l #BASE_BYTES/4, d2
	moveq #0, d3
check
	cmp.l (a2)+, d3
	bne.w bad
	addq.l #1, d3
	subq.l #1, d2
	bne.w check
	moveq #0, d0
	rts
bad
	moveq #PROBE_FAILED, d0
	rts
	.bend  ; verify
snapshot	.block
	lea SavedBlock, a2
	move.l memory.Block.Pointer(a0), (a2)+
	move.l memory.Block.Capacity(a0), (a2)+
	move.l memory.Block.Used(a0), (a2)+
	rts
	.bend  ; snapshot
unchanged	.block
	lea SavedBlock, a2
	move.l (a2)+, d0
	cmp.l memory.Block.Pointer(a0), d0
	bne.w bad
	move.l (a2)+, d0
	cmp.l memory.Block.Capacity(a0), d0
	bne.w bad
	move.l (a2)+, d0
	cmp.l memory.Block.Used(a0), d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #PROBE_FAILED, d0
	rts
	.bend  ; unchanged
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
Scratch	.byte "Work:", 0
MissingScratch	.byte "Work:opforge-missing-parent/growth", 0
	.endsection
	.section bss, kind=bss
DosBase	.res long, 1
Block	.res byte, memory.Block.Used+4
SavedBlock	.res byte, memory.Block.Used+4
State	.res byte, growth.STATE_BYTES
	.endsection
	.endmodule
