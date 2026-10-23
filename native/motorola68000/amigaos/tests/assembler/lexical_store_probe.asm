; Owned lexical copies, physical module retirement and cross-module late edits.
; @opforge-owner: opforge.tests.assembler.lexical_store_probe
	.module opforge.tests.assembler.lexical_store_probe
	.cpu 68020
	.use opforge.assembler.symbols.lexical_store as store
	.use opforge.assembler.symbols.binding_records as records
	.use opforge.platform.memory as memory
	.use opforge.assembler.prepare.record_store as backing
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
; Private entry/name payload exceeds the prepared-source store's old 3 MiB cap.
COUNT = 12000
NAME_LENGTH = 255
	.pub
	.section code, kind=code
; D0/CCR=status; others preserved. Always releases storage and DOS ownership.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #88, d7
	lea State, a0
	jsr store.release
	movea.l 4.w, a6
	lea DosName, a1
	moveq #36, d0
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	lea Name, a1
	move.w #NAME_LENGTH-1, d0
fillName
	move.b #65, (a1)+
	dbra d0, fillName
	; Frontend initialization binds names before disk policy is configured.
	clr.b Name
	lea State, a0
	lea Name, a1
	move.l #NAME_LENGTH, d0
	moveq #0, d1
	jsr store.appendName
	bne.w finish
	lea Entry, a1
	move.l d1, records.Entry.Name(a1)
	move.w #NAME_LENGTH, records.Entry.Length(a1)
	clr.w records.Entry.Target(a1)
	moveq #0, d0
	jsr store.appendEntry
	bne.w finish
	move.w #1, Owners
	movea.l DosBase, a1
	lea Scratch, a2
	jsr store.configure
	bne.w finish
	cmpi.l #store.MAX_LOGICAL_BYTES, store.State.Backing+backing.State.Limit(a0)
	bne.w finish
	moveq #1, d6
append
	lea State, a0
	lea Name, a1
	move.b d6, (a1)
	move.l #NAME_LENGTH, d0
	move.l d6, d1
	jsr store.appendName
	bne.w finish
	lea Entry, a1
	move.l d1, records.Entry.Name(a1)
	move.w #NAME_LENGTH, records.Entry.Length(a1)
	move.w d6, records.Entry.Target(a1)
	move.l d6, d0
	jsr store.appendEntry
	bne.w finish
	move.l d6, d0
	add.l d0, d0
	lea Owners, a1
	move.w #1, 0(a1, d0.l)
	addq.l #1, d6
	cmpi.l #COUNT, d6
	blo.w append
	; The first interface remains resident; all other names/entries spill.
	move.w #1, Flags
	lea Owners, a1
	lea Flags, a2
	moveq #9, d0  ; public/module interface facets in this metadata fixture
	jsr store.seal
	bne.w finish
	cmpi.l #3*memory.LIMIT, store.State.Backing+backing.State.LogicalUsed(a0)
	bls.w finish
	tst.l store.State.ActiveEntries+memory.Block.Pointer(a0)
	bne.w finish
	tst.l store.State.ActiveNames+memory.Block.Pointer(a0)
	bne.w finish
	cmpi.l #backing.PAYLOAD_BUDGET, store.State.Backing+backing.State.Reserved(a0)
	bhi.w finish
	moveq #0, d6
read
	lea Entry, a1
	move.l d6, d0
	jsr store.readEntry
	bne.w finish
	cmp.w records.Entry.Target(a1), d6
	bne.w finish
	move.l records.Entry.Name(a1), d0
	moveq #0, d1
	move.w records.Entry.Length(a1), d1
	lea Copy, a1
	jsr store.copyName
	bne.w finish
	cmp.b (a1), d6
	bne.w finish
	cmpi.b #65, NAME_LENGTH-1(a1)
	bne.w finish
	tst.b NAME_LENGTH(a1)
	bne.w finish
	addq.l #1, d6
	cmpi.l #COUNT, d6
	blo.w read
	; A private canonical name can become an exported alias in a later module.
	lea Entry, a1
	move.w #1234, records.Entry.Target(a1)
	move.l #COUNT-1, d0
	jsr store.writeEntry
	bne.w finish
	move.l #COUNT, d0
	jsr store.appendEntry
	bne.w finish
	move.w #1, Owners+COUNT*2
	move.w #1, Flags+COUNT*2
	lea Owners, a1
	lea Flags, a2
	moveq #9, d0
	jsr store.seal
	bne.w finish
	lea Entry, a1
	move.l #COUNT-1, d0
	jsr store.readEntry
	bne.w finish
	cmpi.w #1234, records.Entry.Target(a1)
	bne.w finish
	move.l #COUNT, d0
	jsr store.readEntry
	bne.w finish
	move.l records.Entry.Name(a1), d0
	move.l #NAME_LENGTH, d1
	lea Copy, a1
	jsr store.copyName
	bne.w finish
	cmpi.b #65, NAME_LENGTH-1(a1)
	bne.w finish
	; The numeric post-binding frontier retires names while entries remain valid.
	jsr store.retireNames
	tst.l store.State.RetainedNames+memory.Block.Pointer(a0)
	bne.w finish
	move.l #COUNT-1, d0
	lea Entry, a1
	jsr store.readEntry
	bne.w finish
	cmpi.w #1234, records.Entry.Target(a1)
	bne.w finish
	move.l records.Entry.Name(a1), d0
	move.l #NAME_LENGTH, d1
	lea Copy, a1
	jsr store.copyName
	beq.w finish
	jsr store.check
	beq.w finish
	lea Owners, a1
	lea Flags, a2
	moveq #9, d0
	jsr store.seal
	beq.w finish
	; Count is exclusive; invalid numeric access cannot expose storage.
	move.l #COUNT+1, d0
	lea Entry, a1
	jsr store.readEntry
	beq.w finish
	moveq #0, d7
finish
	lea State, a0
	jsr store.release
	move.w #store.STATE_BYTES/4-1, d0
cleaned
	tst.l (a0)+
	bne.w cleanupFailed
	dbra d0, cleaned
	bra.w closeLibrary
cleanupFailed
	moveq #89, d7
closeLibrary
	movea.l 4.w, a6
	movea.l DosBase, a1
	jsr CLOSE_LIBRARY(a6)
done
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
Scratch	.byte "Work:", 0
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
State	.res byte, store.STATE_BYTES
Entry	.res byte, records.ENTRY_BYTES
Name	.res byte, records.NAME_BYTES
Copy	.res byte, records.NAME_BYTES
Owners	.res word, COUNT+1
Flags	.res word, COUNT+1
	.endsection
	.endmodule
