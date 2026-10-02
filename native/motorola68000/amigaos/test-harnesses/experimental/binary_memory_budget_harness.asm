; Caller-owned budget, payload ownership and public allocator ABI proof.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent
	.module main
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.section entry, kind=code
	.pub
start	.block
	movem.l d1-d7/a0-a6, -(sp)
	bsr.w exercise
	move.l d0, d7
	lea Storage, a0
	jsr memory.release
	lea Empty, a0
	jsr memory.release
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; start
	.priv

exercise	.block
	lea Storage, a0
	moveq #0, d0
	moveq #0, d1
	bsr.w checkedReserve
	bne.w bad
	tst.l memory.Block.Pointer(a0)
	bne.w bad
	tst.l memory.Block.Capacity(a0)
	bne.w bad
	move.l #257, d0
	move.l #300, d1
	bsr.w checkedReserve
	bne.w bad
	cmpi.l #300, memory.Block.Capacity(a0)
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	cmpa.l #0, a1
	beq.w bad
	move.l #$12345678, (a1)
	move.l #4, memory.Block.Used(a0)
	move.l a1, PreviousPointer
	move.l #301, d0
	move.l #300, d1
	bsr.w checkedReserve
	cmpi.l #1, d0
	bne.w bad
	move.l memory.Block.Pointer(a0), d0
	cmp.l PreviousPointer, d0
	bne.w bad
	cmpi.l #300, memory.Block.Capacity(a0)
	bne.w bad
	bsr.w payload
	bne.w bad
	; A high-bit cap is unsigned; a small request must still grow normally.
	move.l #301, d0
	move.l #$80000001, d1
	bsr.w checkedReserve
	bne.w bad
	cmpi.l #512, memory.Block.Capacity(a0)
	bne.w bad
	bsr.w payload
	bne.w bad
	; Default APIs retain their existing one-MiB allocation limit.
	lea Empty, a0
	move.l #memory.LIMIT+1, d0
	jsr memory.reserve
	cmpi.l #1, d0
	bne.w bad
	move.l #memory.LIMIT+1, d0
	jsr memory.reserveExact
	cmpi.l #1, d0
	bne.w bad
	tst.l memory.Block.Pointer(a0)
	bne.w bad
	lea Storage, a0
	move.l #memory.LIMIT, d0
	move.l #2*memory.LIMIT, d1
	bsr.w checkedReserve
	bne.w bad
	bsr.w payload
	bne.w bad
	move.l #memory.LIMIT, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	adda.l #memory.LIMIT-1, a1
	move.b #$a5, (a1)
	move.l #memory.LIMIT+1, d0
	move.l #2*memory.LIMIT, d1
	bsr.w checkedReserve
	bne.w bad
	cmpi.l #2*memory.LIMIT, memory.Block.Capacity(a0)
	bne.w bad
	cmpi.l #memory.LIMIT, memory.Block.Used(a0)
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	cmpi.l #$12345678, (a1)
	bne.w bad
	adda.l #memory.LIMIT-1, a1
	cmpi.b #$a5, (a1)
	bne.w bad
	move.l #$ffffffff, d0
	move.l #2*memory.LIMIT, d1
	bsr.w checkedReserve
	cmpi.l #1, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #20, d0
	rts
	.bend  ; exercise

; Verify the preserved byte prefix and Used after each small relocation.
payload	.block
	cmpi.l #4, memory.Block.Used(a0)
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	cmpi.l #$12345678, (a1)
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; payload

; A0/D0/D1 match reserveBounded. Check all preserved registers and stack on
; both success and rejection, then restore the probe caller's own registers.
checkedReserve	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #$22334455, d2
	move.l #$33445566, d3
	move.l #$44556677, d4
	move.l #$55667788, d5
	move.l #$66778899, d6
	move.l #$778899aa, d7
	movea.l #$11223344, a1
	movea.l #$22334455, a2
	movea.l #$33445566, a3
	movea.l #$44556677, a4
	movea.l #$55667788, a5
	movea.l #$66778899, a6
	movem.l d1-d7/a0-a6, Before
	move.l sp, PreviousStack
	jsr memory.reserveBounded
	beq.w zeroStatus
	cmpi.l #1, d0
	bne.w bad
	bra.w statusReady
zeroStatus
	tst.l d0
	bne.w bad
statusReady
	move.l d0, Status
	movem.l d1-d7/a0-a6, After
	cmpa.l PreviousStack, sp
	bne.w bad
	lea Before, a0
	lea After, a1
	moveq #13, d0
compare
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w bad
	dbf d0, compare
	move.l Status, d0
	bra.w done
bad
	moveq #-1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; checkedReserve
	.endsection
	.section bss, kind=bss
Storage	.res byte, memory.Block.Used+4
Empty	.res byte, memory.Block.Used+4
Before	.res long, 14
After	.res long, 14
PreviousStack	.res long, 1
PreviousPointer	.res long, 1
Status	.res long, 1
	.endsection
	.output "build/binary_memory_budget_harness", format=hunk, sections=entry, code, bss
	.endmodule
	.end
