; Offset-view component checks followed by real budgeted lifecycle execution.
; @opforge-owner: opforge.tests.assembler.record_views_probe
	.module opforge.tests.assembler.record_views_probe
	.cpu 68020
	.use opforge.platform.memory as memory
	.use opforge.packages.format as package
	.use opforge.assembler.language.repetition as repetition
	.use opforge.assembler.language.repeat_frames as frames
	.use opforge.assembler.language.repeat_inventory as inventory
	.use opforge.assembler.values.guards as guards
	.use opforge.assembler.language.branches as branches
	.use opforge.tests.assembler.assembly_session_probe as lifecycle
	.use opforge.vm.prvm.abi as abi
	.pub
RECORD_BYTES = 64
FIRST_HEADER = 0
FIRST_BODY = 16
SECOND_HEADER = 8
	.section code, kind=code
; A0=normal frozen assembly Frame. Preserve the caller and qualify component
; rebasing independently; production session pins still reject changed bases.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	bsr.w component
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	bne.w bad
	jmp lifecycle.run
bad
	moveq #1, d0
	rts
	.bend  ; run
	.priv
component	.block
	lea FirstRecords, a0
	moveq #RECORD_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	lea SecondRecords, a0
	moveq #RECORD_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	movea.l FirstRecords+memory.Block.Pointer, a0
	move.b #15, FIRST_HEADER(a0)
	move.b #15, SECOND_HEADER(a0)
	movea.l SecondRecords+memory.Block.Pointer, a0
	move.b #15, FIRST_HEADER(a0)
	move.b #15, SECOND_HEADER(a0)
	move.l #1, Context+package.Context.Count
	move.l #1, Inventory+inventory.State.Symbols
	move.l #Inventory, Loops+repetition.State.Scopes+frames.State.Inventory
	move.l #Context, Loops+repetition.State.Scopes+frames.State.Context
	move.l #Inventory, Loops+repetition.State.Guard+guards.State.Inventory
	move.w #2, Loops+repetition.State.Depth
	lea Loops+repetition.SLOTS, a0
	move.l #abi.PRVM_LOOP_MATCHED, repetition.Slot.Kind(a0)
	move.l #FIRST_HEADER, repetition.Slot.Header(a0)
	move.l #FIRST_BODY, repetition.Slot.Body(a0)
	adda.w #repetition.SLOT_BYTES, a0
	move.w #1, repetition.Slot.Materialized(a0)
	move.w #1, repetition.Slot.IterationActive(a0)
	move.l #SECOND_HEADER, repetition.Slot.Header(a0)
	; Materialized bodies never rewind and do not borrow a source body view.
	move.l #-1, repetition.Slot.Body(a0)
	lea Loops, a0
	movea.l FirstRecords+memory.Block.Pointer, a1
	moveq #RECORD_BYTES, d0
	jsr repetition.refresh
	bne.w bad
	movea.l SecondRecords+memory.Block.Pointer, a1
	moveq #RECORD_BYTES, d0
	jsr repetition.refresh
	bne.w bad
	move.l a1, d1
	cmp.l repetition.State.SourceBase(a0), d1
	bne.w bad
	cmp.l repetition.State.Scopes+frames.State.Records(a0), d1
	bne.w bad
	cmp.l repetition.State.Guard+guards.State.Records(a0), d1
	bne.w bad
	cmpi.l #RECORD_BYTES, repetition.State.SourceBytes(a0)
	bne.w bad
	cmpi.w #2, repetition.State.Depth(a0)
	bne.w bad
	cmpi.l #FIRST_BODY, repetition.SLOTS+repetition.Slot.Body(a0)
	bne.w bad
	cmpi.l #SECOND_HEADER, repetition.SLOTS+repetition.SLOT_BYTES+repetition.Slot.Header(a0)
	bne.w bad
	; A body at the complete frontier is valid, including an empty body.
	move.l #RECORD_BYTES, repetition.SLOTS+repetition.Slot.Body(a0)
	moveq #RECORD_BYTES, d0
	jsr repetition.refresh
	bne.w bad
	move.l #FIRST_BODY, repetition.SLOTS+repetition.Slot.Body(a0)
	; Header starts are within nine bytes, but their full spans are not.
	moveq #9, d0
	jsr repetition.refresh
	tst.l d0
	beq.w bad
	move.l #RECORD_BYTES, repetition.SLOTS+repetition.SLOT_BYTES+repetition.Slot.Header(a0)
	moveq #RECORD_BYTES, d0
	jsr repetition.refresh
	tst.l d0
	beq.w bad
	move.l #SECOND_HEADER, repetition.SLOTS+repetition.SLOT_BYTES+repetition.Slot.Header(a0)
	move.l #RECORD_BYTES+1, repetition.SLOTS+repetition.Slot.Body(a0)
	moveq #RECORD_BYTES, d0
	jsr repetition.refresh
	tst.l d0
	beq.w bad
	move.l #FIRST_BODY, repetition.SLOTS+repetition.Slot.Body(a0)
	movea.l #0, a1
	moveq #RECORD_BYTES, d0
	jsr repetition.refresh
	tst.l d0
	beq.w bad
	movea.l #-16, a1
	moveq #RECORD_BYTES, d0
	jsr repetition.refresh
	tst.l d0
	beq.w bad
	; Every rejected refresh leaves the last accepted allocation view intact.
	move.l SecondRecords+memory.Block.Pointer, d1
	cmp.l repetition.State.SourceBase(a0), d1
	bne.w bad
	lea Inventory, a0
	movea.l SecondRecords+memory.Block.Pointer, a1
	lea Context, a2
	lea MemberKeys, a3
	moveq #RECORD_BYTES, d0
	jsr inventory.refresh
	bne.w bad
	move.l a1, d1
	cmp.l inventory.State.SourceBase(a0), d1
	bne.w bad
	move.l a3, d1
	cmp.l inventory.State.MemberKeys(a0), d1
	bne.w bad
	move.l #2, package.Context.Count(a2)
	moveq #RECORD_BYTES, d0
	jsr inventory.refresh
	tst.l d0
	beq.w bad
	lea Branches, a0
	jsr branches.refresh
	bne.w bad
	move.l a1, d1
	cmp.l branches.State.Records(a0), d1
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l d0, -(sp)
	lea FirstRecords, a0
	jsr memory.release
	lea SecondRecords, a0
	jsr memory.release
	move.l (sp)+, d0
	tst.l d0
	rts
	.bend  ; component
	.endsection
	.section bss, kind=bss
FirstRecords	.res byte, memory.Block.Used+4
SecondRecords	.res byte, memory.Block.Used+4
Loops	.res byte, repetition.STATE_BYTES
Inventory	.res byte, inventory.STATE_BYTES
Context	.res byte, package.CONTEXT_BYTES
MemberKeys	.res word, 1
Branches	.res byte, branches.STATE_BYTES
	.endsection
	.endmodule
