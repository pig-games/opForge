; Callable component probe; temporary qualification caller supplies production
; grow arguments. The component uses independent owners and releases them.
; @opforge-owner: opforge.tests.assembler.symbol_storage_probe
	.module opforge.tests.assembler.symbol_storage_probe
	.cpu 68020
	.use opforge.assembler.symbols.storage as storage
	.use opforge.platform.memory as memory
	.use opforge.assembler.values.values as values
	.use opforge.packages.format as package
	.pub
Pointers	.struct
Values	.long ?
Defined	.long ?
Sections	.long ?
MemberKeys	.long ?
Kinds	.long ?
.endstruct
POINTER_BYTES = Pointers.Kinds+4
	.section code, kind=code
; Same grow ABI as storage.grow. Component checks must pass before the real
; caller is processed. Production never imports this qualification wrapper.
grow	.block
	movem.l d0-d7/a0-a6, -(sp)
	bsr.w component
	tst.l d0
	bne.w failed
	movem.l (sp)+, d0-d7/a0-a6
	jmp storage.grow
failed
	movem.l (sp)+, d0-d7/a0-a6
	moveq #1, d0
	rts
	.bend  ; grow
	.priv
component	.block
	lea TestContext, a1
	move.l #TypedOwner, package.Context.Owner(a1)
	lea TestStorage, a0
	move.l #memory.reserveExact, storage.State.Reserve(a0)
	moveq #1, d0
	jsr storage.grow
	bne.w bad
	; An absent kinds table stays lazy through scalar-only growth.
	tst.l TypedOwner+values.Owner.Kinds+memory.Block.Pointer
	bne.w bad
	moveq #2, d0
	jsr storage.grow
	bne.w bad
	tst.l TypedOwner+values.Owner.Kinds+memory.Block.Pointer
	bne.w bad
	; Exact initial capacities force all five replacements at Count=9.
	lea TypedOwner+values.Owner.Kinds, a0
	moveq #2, d0
	jsr memory.reserveExact
	bne.w bad
	move.l #2, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	move.b #values.LIST, (a1)+
	clr.b (a1)
	lea TestStorage, a0
	movea.l storage.State.Values+memory.Block.Pointer(a0), a1
	move.l #$12345678, (a1)
	move.l #$76543210, 4(a1)
	movea.l storage.State.Defined+memory.Block.Pointer(a0), a1
	move.b #2, (a1)
	movea.l storage.State.Sections+memory.Block.Pointer(a0), a1
	move.b #3, (a1)
	movea.l storage.State.MemberKeys+memory.Block.Pointer(a0), a1
	move.w #$4321, (a1)
	move.l storage.State.Values+memory.Block.Pointer(a0), Saved+Pointers.Values
	move.l storage.State.Defined+memory.Block.Pointer(a0), Saved+Pointers.Defined
	move.l storage.State.Sections+memory.Block.Pointer(a0), Saved+Pointers.Sections
	move.l storage.State.MemberKeys+memory.Block.Pointer(a0), Saved+Pointers.MemberKeys
	move.l TypedOwner+values.Owner.Kinds+memory.Block.Pointer, Saved+Pointers.Kinds
	move.l #failReserve, storage.State.Reserve(a0)
	moveq #1, d7
failurePosition
	move.l d7, FailAt
	clr.l ReserveCalls
	lea TestStorage, a0
	lea TestContext, a1
	moveq #9, d0
	jsr storage.grow
	tst.l d0
	beq.w bad
	cmp.l ReserveCalls, d7
	bne.w bad
	bsr.w checkOld
	bne.w bad
	addq.l #1, d7
	cmpi.l #6, d7
	blo.w failurePosition
	lea TestStorage, a0
	clr.l storage.State.Reserve(a0)
	lea TestContext, a1
	moveq #9, d0
	jsr storage.grow
	bne.w bad
	; Earlier scalar, availability, provenance, canonical keys and typed kind
	; survive relocation; all newly appended cells begin zero.
	bsr.w checkCells
	bne.w bad
	lea TestStorage, a0
	cmpi.l #9, storage.State.Count(a0)
	bne.w bad
	lea TestContext, a1
	cmpi.l #9, package.Context.Count(a1)
	bne.w bad
	cmpi.l #9, TypedOwner+values.Owner.Count
	bne.w bad
	moveq #8, d0
	jsr storage.grow
	tst.l d0
	beq.w bad
	move.l #storage.MAX_SYMBOLS, d0
	jsr storage.grow
	bne.w bad
	cmpi.l #storage.MAX_SYMBOLS, storage.State.Count(a0)
	bne.w bad
	cmpi.l #storage.MAX_SYMBOLS, package.Context.Count(a1)
	bne.w bad
	cmpi.l #storage.MAX_SYMBOLS, TypedOwner+values.Owner.Count
	bne.w bad
	move.l #storage.MAX_SYMBOLS+1, d0
	jsr storage.grow
	tst.l d0
	beq.w bad
	bsr.w checkCells
	bne.w bad
	bsr.w prepared
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l d0, -(sp)
	lea TestStorage, a0
	jsr storage.release
	jsr storage.release
	lea TypedOwner, a0
	jsr values.release
	move.l (sp)+, d0
	tst.l d0
	rts
	.bend  ; component
; Preparation may publish keys/kinds before the first scalar context exists.
prepared	.block
	lea TestStorage, a0
	jsr storage.release
	lea TypedOwner, a0
	jsr values.release
	moveq #2, d0
	jsr values.init
	bne.w bad
	lea TypedOwner+values.Owner.Kinds, a0
	moveq #2, d0
	jsr memory.reserveExact
	bne.w bad
	move.l #2, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	move.b #values.LIST, (a1)+
	move.b #values.STRUCT_TYPE, (a1)
	lea TestStorage+storage.State.MemberKeys, a0
	moveq #4, d0
	jsr memory.reserveExact
	bne.w bad
	move.l #4, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	move.w #$1234, (a1)+
	move.w #$5678, (a1)
	lea TestContext, a1
	clr.l package.Context.Count(a1)
	clr.l package.Context.Values(a1)
	clr.l package.Context.Defined(a1)
	clr.l package.Context.SectionIds(a1)
	lea TestStorage, a0
	clr.l storage.State.Reserve(a0)
	moveq #2, d0
	jsr storage.grow
	bne.w bad
	movea.l storage.State.MemberKeys+memory.Block.Pointer(a0), a2
	cmpi.w #$1234, (a2)+
	bne.w bad
	cmpi.w #$5678, (a2)
	bne.w bad
	movea.l TypedOwner+values.Owner.Kinds+memory.Block.Pointer, a2
	cmpi.b #values.LIST, (a2)+
	bne.w bad
	cmpi.b #values.STRUCT_TYPE, (a2)
	bne.w bad
	movea.l package.Context.Values(a1), a2
	moveq #3, d0
clearValues
	tst.l (a2)+
	bne.w bad
	dbra d0, clearValues
	movea.l package.Context.Defined(a1), a2
	tst.w (a2)
	bne.w bad
	movea.l package.Context.SectionIds(a1), a2
	tst.w (a2)
	bne.w bad
	; Same Count must neither allocate nor rewrite existing cells.
	move.l #failReserve, storage.State.Reserve(a0)
	move.l #1, FailAt
	clr.l ReserveCalls
	moveq #2, d0
	jsr storage.grow
	bne.w bad
	tst.l ReserveCalls
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; prepared
; Injection uses the normal allocation boundary, not a production identity.
failReserve	.block
	addq.l #1, ReserveCalls
	move.l d1, -(sp)
	move.l ReserveCalls, d1
	cmp.l FailAt, d1
	movem.l (sp)+, d1
	beq.w failed
	jmp memory.reserveExact
failed
	moveq #1, d0
	tst.l d0
	rts
	.bend  ; failReserve
checkOld	.block
	lea TestStorage, a0
	cmpi.l #2, storage.State.Count(a0)
	bne.w bad
	lea TestContext, a1
	cmpi.l #2, package.Context.Count(a1)
	bne.w bad
	cmpi.l #2, TypedOwner+values.Owner.Count
	bne.w bad
	move.l storage.State.MemberKeys+memory.Block.Pointer(a0), d0
	cmp.l Saved+Pointers.MemberKeys, d0
	bne.w bad
	move.l TypedOwner+values.Owner.Kinds+memory.Block.Pointer, d0
	cmp.l Saved+Pointers.Kinds, d0
	bne.w bad
	move.l package.Context.Values(a1), d0
	cmp.l Saved+Pointers.Values, d0
	bne.w bad
	cmp.l storage.State.Values+memory.Block.Pointer(a0), d0
	bne.w bad
	move.l package.Context.Defined(a1), d0
	cmp.l Saved+Pointers.Defined, d0
	bne.w bad
	cmp.l storage.State.Defined+memory.Block.Pointer(a0), d0
	bne.w bad
	move.l package.Context.SectionIds(a1), d0
	cmp.l Saved+Pointers.Sections, d0
	bne.w bad
	cmp.l storage.State.Sections+memory.Block.Pointer(a0), d0
	bne.w bad
	bra.w checkCells
bad
	moveq #1, d0
	rts
	.bend  ; checkOld
checkCells	.block
	lea TestStorage, a0
	movea.l storage.State.Values+memory.Block.Pointer(a0), a1
	cmpi.l #$12345678, (a1)+
	bne.w bad
	cmpi.l #$76543210, (a1)+
	bne.w bad
	move.l storage.State.Values+memory.Block.Used(a0), d0
	subq.l #storage.VALUE_BYTES, d0
	bsr.w zeroTail
	bne.w bad
	movea.l storage.State.Defined+memory.Block.Pointer(a0), a1
	cmpi.b #2, (a1)+
	bne.w bad
	move.l storage.State.Defined+memory.Block.Used(a0), d0
	subq.l #1, d0
	bsr.w zeroTail
	bne.w bad
	movea.l storage.State.Sections+memory.Block.Pointer(a0), a1
	cmpi.b #3, (a1)+
	bne.w bad
	move.l storage.State.Sections+memory.Block.Used(a0), d0
	subq.l #1, d0
	bsr.w zeroTail
	bne.w bad
	movea.l storage.State.MemberKeys+memory.Block.Pointer(a0), a1
	cmpi.w #$4321, (a1)+
	bne.w bad
	move.l storage.State.MemberKeys+memory.Block.Used(a0), d0
	subq.l #storage.MEMBER_BYTES, d0
	bsr.w zeroTail
	bne.w bad
	movea.l TypedOwner+values.Owner.Kinds+memory.Block.Pointer, a1
	cmpi.b #values.LIST, (a1)+
	bne.w bad
	move.l TypedOwner+values.Owner.Kinds+memory.Block.Used, d0
	subq.l #1, d0
	bsr.w zeroTail
	rts
bad
	moveq #1, d0
	rts
	.bend  ; checkCells
zeroTail	.block
	tst.l d0
	beq.w done
next
	tst.b (a1)+
	bne.w bad
	subq.l #1, d0
	bne.w next
done
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; zeroTail
	.endsection
	.section bss, kind=bss
TestStorage	.res byte, storage.STATE_BYTES
TestContext	.res byte, package.CONTEXT_BYTES
TypedOwner	.res byte, values.OWNER_BYTES
Saved	.res byte, POINTER_BYTES
FailAt	.res long, 1
ReserveCalls	.res long, 1
	.endsection
	.endmodule
