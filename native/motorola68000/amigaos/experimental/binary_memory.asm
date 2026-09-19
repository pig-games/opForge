; Bounded raw blocks owned by the experimental caller.
; @opforge-owner: experimental.amigaos.binary_memory
	.module experimental.amigaos.binary_memory
	.cpu 68020
	.include "memory_telemetry.i"
	.pub
Block	.struct
Pointer	.long ?
Capacity	.long ?
Used	.long ?
.endstruct
LIMIT = 1048576
	.section code, kind=code
; A0=zero-initialized Block, D0=minimum capacity. Preserves other registers.
; D0/CCR=status. Growth doubles from 256 bytes, capped at LIMIT. Old storage
; survives allocation failure; copy overlap contributes to telemetry peak.
reserve	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	cmp.l Block.Capacity(a4), d0
	bls.w good
	cmpi.l #LIMIT, d0
	bhi.w bad
	move.l #256, d4
sizeLoop
	cmp.l d0, d4
	bhs.w allocate
	add.l d4, d4
	bra.w sizeLoop
allocate
	move.l d4, d0
	move.l #$10001, d1
	movea.l 4.w, a6
	jsr -198(a6)
	tst.l d0
	beq.w bad
	movea.l d0, a5
	.MEMORY_ALLOC d4
	movea.l Block.Pointer(a4), a0
	movea.l a5, a1
	move.l Block.Used(a4), d2
	beq.w copied
copy
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bne.w copy
copied
	movea.l a4, a0
	bsr.w release
	move.l a5, Block.Pointer(a4)
	move.l d4, Block.Capacity(a4)
; release deliberately retains Used so growth preserves the logical extent.
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; reserve
; A0=Block. Free owned allocation and clear pointer/capacity; Used retained.
; Preserves all registers; CCR unspecified. Safe for an empty block.
release	.block
	movem.l d0-d1/a0-a2/a6, -(sp)
	movea.l a0, a2
	move.l Block.Pointer(a0), d0
	beq.w done
	movea.l d0, a1
	move.l Block.Capacity(a0), d0
	.MEMORY_FREE d0
	movea.l 4.w, a6
	jsr -210(a6)
	clr.l Block.Pointer(a2)
	clr.l Block.Capacity(a2)
done
	movem.l (sp)+, d0-d1/a0-a2/a6
	rts
	.bend  ; release
	.endsection
	.endmodule
