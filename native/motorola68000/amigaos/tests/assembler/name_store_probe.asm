; Native paged-name component checks; no surviving allocations.
; @opforge-owner: opforge.tests.assembler.name_store_probe
	.module opforge.tests.assembler.name_store_probe
	.cpu 68020
	.use opforge.assembler.symbols.name_store as names
	.use opforge.platform.memory as memory
PROBE_FAILED = 88
	.pub
	.section code, kind=code
; No inputs. D0/CCR=0 success or 88 failure; all other registers preserved.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #PROBE_FAILED, d7
	lea State, a0
	jsr names.release
	.ifdef OPFORGE_NAME_STORE_EXPECT_FIRST_FAILURE
	bsr.w allocationFailure
	bne.w finish
	moveq #0, d7
	bra.w finish
	.endif
	.ifdef OPFORGE_NAME_STORE_EXPECT_PAGE_FAILURE
	bsr.w allocationFailure
	bne.w finish
	moveq #0, d7
	bra.w finish
	.endif
	lea Pattern, a1
	moveq #0, d2
fillPattern
	move.b d2, (a1)+
	addq.l #1, d2
	cmpi.l #names.MAX_NAME_BYTES, d2
	blo.w fillPattern
	bsr.w appendPattern
	bne.w finish
	tst.l d1
	bne.w finish
	cmpi.l #256, State+names.State.First+memory.Block.Capacity
	bne.w finish
	; Save an alias before first-page relocation and append its copied bytes.
	moveq #0, d0
	jsr names.pointer
	movea.l a0, a1
	lea Alias, a2
	move.l #254, d2
aliasCopy
	move.b (a1)+, (a2)+
	dbra d2, aliasCopy
	lea State, a0
	lea Alias, a1
	move.l #255, d0
	move.l #names.MAX_PAYLOAD_BYTES, d1
	jsr names.append
	bne.w finish
	cmpi.l #255, d1
	bne.w finish
	cmpi.l #512, State+names.State.First+memory.Block.Capacity
	bne.w finish
	moveq #0, d0
	bsr.w verify
	bne.w finish
	move.l #255, d0
	bsr.w verify
	bne.w finish
	lea State, a0
	jsr names.release
	; 257 complete names leave one unused byte in the first page.
	move.l #256, d6
fillFirst
	bsr.w appendPattern
	bne.w finish
	dbra d6, fillFirst
	cmpi.l #65535, State+names.State.Bytes
	bne.w finish
	cmpi.l #65535, State+names.State.End
	bne.w finish
	cmpi.l #65536, State+names.State.First+memory.Block.Capacity
	bne.w finish
	tst.l State+names.State.Pages+memory.Block.Pointer
	bne.w finish
	bsr.w appendPattern
	bne.w finish
	cmpi.l #65536, d1
	bne.w finish
	movea.l State+names.State.First+memory.Block.Pointer, a4
	movea.l State+names.State.Pages+memory.Block.Pointer, a2
	movea.l memory.Block.Pointer(a2), a5
	cmpi.l #65536, memory.Block.Capacity(a2)
	bne.w finish
	; Another contiguous name on page one preserves the prior page pointer.
	bsr.w appendPattern
	bne.w finish
	cmpi.l #65536+255, d1
	bne.w finish
	cmpa.l State+names.State.First+memory.Block.Pointer, a4
	bne.w finish
	movea.l State+names.State.Pages+memory.Block.Pointer, a2
	cmpa.l memory.Block.Pointer(a2), a5
	bne.w finish
	cmpi.l #510, memory.Block.Used(a2)
	bne.w finish
	cmpi.l #1, State+names.State.Last
	bne.w finish
	cmpi.l #65535+510, State+names.State.Bytes
	bne.w finish
	cmpi.l #65536+510, State+names.State.End
	bne.w finish
	; Old IDs and both page-one names retain the complete byte pattern.
	.for handle in {0, 128*255, 256*255, 65536, 65536+255}
	move.l #handle, d0
	bsr.w verify
	bne.w finish
	.endfor
	; End includes a gap, but a limit of Bytes+1 still accepts one payload byte.
	lea State, a0
	lea Pattern, a1
	moveq #1, d0
	move.l State+names.State.Bytes, d1
	addq.l #1, d1
	jsr names.append
	bne.w finish
	cmpi.l #65536+510, d1
	bne.w finish
	cmpi.l #65535+511, State+names.State.Bytes
	bne.w finish
	cmpi.l #65536+511, State+names.State.End
	bne.w finish
	; A payload limit equal to actual bytes permits no further byte.
	bsr.w snapshot
	lea State, a0
	lea Pattern, a1
	moveq #1, d0
	move.l State+names.State.Bytes, d1
	jsr names.append
	beq.w finish
	bsr.w unchanged
	bne.w finish
	; Invalid append lengths and limits preserve every state word.
	.for length in {0, 256, -1}
	move.l #length, d0
	move.l #names.MAX_PAYLOAD_BYTES, d1
	jsr names.append
	beq.w finish
	bsr.w unchanged
	bne.w finish
	.endfor
	moveq #1, d0
	move.l #names.MAX_PAYLOAD_BYTES+1, d1
	jsr names.append
	beq.w finish
	bsr.w unchanged
	bne.w finish
	moveq #0, d0
	bsr.w verify
	bne.w finish
	; Reject the page-tail hole, crossing the tail, page-one end and absent page.
	.for handle in {65535, 65534, 65536+511, 131072}
	lea State, a0
	move.l #handle, d0
	moveq #2, d1
	jsr names.resolve
	beq.w finish
	move.l a1, d2
	bne.w finish
	.endfor
	.for length in {0, 256, -1}
	moveq #0, d0
	move.l #length, d1
	jsr names.resolve
	beq.w finish
	move.l a1, d2
	bne.w finish
	.endfor
	lea State, a0
	jsr names.release
	bsr.w zeroState
	bne.w finish
	jsr names.release
	bsr.w zeroState
	bne.w finish
	; A retired First can own a generic allocation and still be released.
	move.l #4096, d0
	jsr memory.reserve
	bne.w finish
	move.l #4096, memory.Block.Used(a0)
	jsr names.release
	bsr.w zeroState
	bne.w finish
	; A tiny payload budget caps first allocation without rounding to a page.
	lea Pattern, a1
	moveq #1, d0
	moveq #1, d1
	jsr names.append
	bne.w finish
	cmpi.l #1, State+names.State.First+memory.Block.Capacity
	bne.w finish
	moveq #0, d7
finish
	lea State, a0
	jsr names.release
	bsr.w zeroState
	beq.w done
	moveq #PROBE_FAILED, d7
done
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
; Fixture-only faults replace exactly one allocator call in a copied module.
; Seed directly through memory so the production append replacement is isolated.
allocationFailure	.block
	lea State, a0
	move.l #256, d0
	move.l #255, d6
.ifdef OPFORGE_NAME_STORE_EXPECT_PAGE_FAILURE
	move.l #65536, d0
	move.l #65535, d6
.endif
	jsr memory.reserve
	bne.w bad
	move.l d6, memory.Block.Used(a0)
	move.l d6, names.State.Bytes(a0)
	move.l d6, names.State.End(a0)
	movea.l memory.Block.Pointer(a0), a1
	moveq #0, d2
seedBytes
	move.b d2, (a1)+
	addq.l #1, d2
	cmp.l d6, d2
	blo.w seedBytes
	bsr.w snapshot
	lea Pattern, a1
	moveq #2, d0
	move.l #names.MAX_PAYLOAD_BYTES, d1
	jsr names.append
	beq.w bad
.ifdef OPFORGE_NAME_STORE_EXPECT_FIRST_FAILURE
	bsr.w unchanged
	bne.w bad
.endif
.ifdef OPFORGE_NAME_STORE_EXPECT_PAGE_FAILURE
	; Directory capacity may grow, but no page row was published or leaked.
	lea SavedState, a2
	.for offset in {names.State.First+memory.Block.Pointer, names.State.First+memory.Block.Capacity, names.State.First+memory.Block.Used, names.State.Last, names.State.End, names.State.Bytes}
	move.l offset(a0), d0
	cmp.l offset(a2), d0
	bne.w bad
	.endfor
	tst.l names.State.Pages+memory.Block.Used(a0)
	bne.w bad
	tst.l names.State.Pages+memory.Block.Pointer(a0)
	beq.w bad
	cmpi.l #256, names.State.Pages+memory.Block.Capacity(a0)
	bne.w bad
	movea.l names.State.Pages+memory.Block.Pointer(a0), a2
	.for offset in {memory.Block.Pointer, memory.Block.Capacity, memory.Block.Used}
	tst.l offset(a2)
	bne.w bad
	.endfor
.endif
	movea.l memory.Block.Pointer(a0), a1
	moveq #0, d2
checkBytes
	move.b (a1)+, d0
	cmp.b d2, d0
	bne.w bad
	addq.l #1, d2
	cmp.l d6, d2
	blo.w checkBytes
	moveq #0, d0
	rts
bad
	moveq #PROBE_FAILED, d0
	rts
	.bend  ; allocationFailure
; Append a complete pattern using the maximum supported payload budget.
appendPattern	.block
	lea State, a0
	lea Pattern, a1
	move.l #255, d0
	move.l #names.MAX_PAYLOAD_BYTES, d1
	jsr names.append
	rts
	.bend  ; appendPattern
; D0=handle. Verify checked and trusted lookups agree and every name byte matches.
; Clobbers D0-D3/A0-A3; D0/CCR=status.
verify	.block
	move.l d0, d3
	lea State, a0
	move.l #255, d1
	jsr names.resolve
	bne.w bad
	movea.l a1, a3
	move.l d3, d0
	jsr names.pointer
	cmpa.l a3, a0
	bne.w bad
	cmp.l d3, d0
	bne.w bad
	lea Pattern, a2
	move.l #254, d2
check
	move.b (a2)+, d0
	cmp.b (a3)+, d0
	bne.w bad
	dbra d2, check
	moveq #0, d0
	rts
bad
	moveq #PROBE_FAILED, d0
	rts
	.bend  ; verify
snapshot	.block
	lea State, a0
	lea SavedState, a2
	.for offset in 0..names.STATE_BYTES/4
	move.l offset*4(a0), (a2)+
	.endfor
	rts
	.bend  ; snapshot
; Compare all state, restoring A0=State for another attempted append.
unchanged	.block
	lea State, a0
	lea SavedState, a2
	.for offset in 0..names.STATE_BYTES/4
	move.l offset*4(a0), d0
	cmp.l (a2)+, d0
	bne.w bad
	.endfor
	moveq #0, d0
	rts
bad
	moveq #PROBE_FAILED, d0
	rts
	.bend  ; unchanged
; A0=State remains unchanged; D0/CCR=status, clobbers D2/A2.
zeroState	.block
	movea.l a0, a2
	moveq #names.STATE_BYTES/4-1, d2
check
	tst.l (a2)+
	bne.w bad
	dbra d2, check
	moveq #0, d0
	rts
bad
	moveq #PROBE_FAILED, d0
	rts
	.bend  ; zeroState
	.endsection
	.section bss, kind=bss
	.align 4
State	.res byte, names.STATE_BYTES
SavedState	.res byte, names.STATE_BYTES
Pattern	.res byte, 255
Alias	.res byte, 255
	.endsection
	.endmodule
