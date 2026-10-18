; Numeric lexical identities with owned copies and bounded sealed payloads.
; @opforge-owner: experimental.amigaos.binary_lexical_store
	.module experimental.amigaos.binary_lexical_store
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_prepared_store as backing
	.use experimental.amigaos.binary_name_store as names
	.pub
Bridge	.struct
Entry	.long ?
Name	.long ?
.endstruct
BRIDGE_BYTES = Bridge.Name+4
State	.struct
Bridge	.res memory.Block.Used+4
ActiveEntries	.res memory.Block.Used+4
ActiveNames	.res memory.Block.Used+4
RetainedEntries	.res memory.Block.Used+4
RetainedNames	.res memory.Block.Used+4
ResidentNames	.res names.STATE_BYTES
Backing	.res backing.STATE_BYTES
Count	.long ?
Start	.long ?
Disk	.long ?
Failure	.long ?  ; sticky: numeric lookup absence must not swallow storage errors
NamesRetired	.long ?
.endstruct
STATE_BYTES = State.NamesRetired+4
TAG_ACTIVE = $80000000
TAG_RETAINED = $40000000
TAG_RESIDENT_NAMES = $c0000000
OFFSET_MASK = $3fffffff
LIMIT = 65535
MAX_LOGICAL_BYTES = LIMIT*(records.NAME_BYTES-1+records.ENTRY_BYTES)
OWNED_BLOCKS = (State.ResidentNames-State.Bridge)/(memory.Block.Used+4)
	.section code, kind=code
; A0=zero State,A1=DOS base,A2=existing scratch parent. Other registers kept.
; D0/CCR=status. Configuration owns its backing file until release.
configure	.block
	movem.l a0, -(sp)
	lea State.Backing(a0), a0
	move.l #MAX_LOGICAL_BYTES, d0
	jsr backing.configureBounded
	movea.l (sp)+, a0
	tst.l d0
	bne.w done
	move.l #1, State.Disk(a0)
done
	tst.l d0
	beq.w returned
	move.l #1, State.Failure(a0)
returned
	tst.l d0
	rts
	.bend  ; configure

; A0=State,D0=minimum identity count. D0/CCR=status; others preserved.
reserve	.block
	movem.l d1/a0, -(sp)
	cmpi.l #LIMIT, d0
	bhi.w bad
	lsl.l #3, d0
	lea State.Bridge(a0), a0
	move.l #LIMIT*BRIDGE_BYTES, d1
	jsr memory.reserveBounded
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	beq.w returned
	move.l #1, State.Failure(a0)
returned
	tst.l d0
	rts
	.bend  ; reserve

; A0=State,A1=name bytes,D0=length,D1=next lexical index.
; D0/CCR=status,D1=opaque canonical-name handle on success; others kept.
; Unpublished rows remain owned on failure. Only appendEntry publishes Count.
appendName	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d7
	move.l d1, d6
	tst.l State.NamesRetired(a6)
	bne.w bad
	cmp.l State.Count(a6), d6
	bne.w bad
	cmpi.l #1, d7
	blo.w bad
	cmpi.l #records.NAME_BYTES-1, d7
	bhi.w bad
	move.l d6, d0
	addq.l #1, d0
	bsr.w reserve
	bne.w bad
	tst.l State.Disk(a6)
	bne.w activeName
	lea State.ResidentNames(a6), a0
	movea.l a5, a1
	move.l d7, d0
	move.l #LIMIT*(records.NAME_BYTES-1), d1
	jsr names.append
	bne.w bad
	move.l d1, d5
	ori.l #TAG_RESIDENT_NAMES, d5
	bra.w publishName
activeName
	lea State.ActiveNames(a6), a0
	move.l memory.Block.Used(a0), d5
	move.l d5, d0
	add.l d7, d0
	move.l #LIMIT*(records.NAME_BYTES-1), d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l State.ActiveNames+memory.Block.Pointer(a6), a1
	adda.l d5, a1
	movea.l a5, a0
	move.l d7, d0
	bsr.w copyBytes
	add.l d7, State.ActiveNames+memory.Block.Used(a6)
	ori.l #TAG_ACTIVE, d5
publishName
	move.l d6, d0
	lsl.l #3, d0
	movea.l State.Bridge+memory.Block.Pointer(a6), a0
	move.l d5, Bridge.Name(a0, d0.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	beq.w returned
	move.l #1, State.Failure(a0)
returned
	tst.l d0
	rts
	.bend  ; appendName

; A0=State,A1=complete Entry,D0=next lexical index.
; D0/CCR=status; all other registers preserved. Entry contains IDs/handles only.
appendEntry	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d6
	cmp.l State.Count(a6), d6
	bne.w bad
	addq.l #1, d0
	bsr.w reserve
	bne.w bad
	lea State.ActiveEntries(a6), a0
	move.l memory.Block.Used(a0), d5
	move.l d5, d0
	addi.l #records.ENTRY_BYTES, d0
	move.l #LIMIT*records.ENTRY_BYTES, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l State.ActiveEntries+memory.Block.Pointer(a6), a1
	adda.l d5, a1
	movea.l a5, a0
	moveq #records.ENTRY_BYTES, d0
	bsr.w copyBytes
	addi.l #records.ENTRY_BYTES, State.ActiveEntries+memory.Block.Used(a6)
	move.l d6, d0
	lsl.l #3, d0
	movea.l State.Bridge+memory.Block.Pointer(a6), a0
	ori.l #TAG_ACTIVE, d5
	move.l d5, Bridge.Entry(a0, d0.l)
	addq.l #1, State.Count(a6)
	move.l State.Count(a6), d0
	lsl.l #3, d0
	move.l d0, State.Bridge+memory.Block.Used(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	beq.w returned
	move.l #1, State.Failure(a0)
returned
	tst.l d0
	rts
	.bend  ; appendEntry

; A0=State,A1=caller-owned Entry,D0=zero-based lexical index.
; D0/CCR=status; all other registers kept. No borrowed payload pointer escapes.
readEntry	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	bsr.w row
	bne.w bad
	move.l Bridge.Entry(a2), d1
	lea State.ActiveEntries(a6), a3
	lea State.RetainedEntries(a6), a4
	moveq #records.ENTRY_BYTES, d7
	bsr.w readPayload
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	beq.w returned
	move.l #1, State.Failure(a0)
returned
	tst.l d0
	rts
	.bend  ; readEntry

; Same inputs as readEntry; overwrite one existing checked Entry.
; Flush copies before nested semantic mutation; reload before subsequent edits.
writeEntry	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	bsr.w row
	bne.w bad
	move.l Bridge.Entry(a2), d1
	lea State.ActiveEntries(a6), a3
	lea State.RetainedEntries(a6), a4
	moveq #records.ENTRY_BYTES, d7
	bsr.w locate
	bne.w bad
	movea.l a5, a0
	move.l d7, d0
	bsr.w copyBytes
	tst.l d3
	beq.w good
	lea State.Backing(a6), a0
	move.l d3, d1
	jsr backing.commitView
	move.l d0, d6
	jsr backing.releaseView
	or.l d6, d0
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	beq.w returned
	move.l #1, State.Failure(a0)
returned
	tst.l d0
	rts
	.bend  ; writeEntry

; A0=State,A1=caller-owned NAME_BYTES buffer,D0=opaque handle,D1=length.
; D0/CCR=status; others kept. Copy includes a newly written zero terminator.
copyName	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	tst.l State.NamesRetired(a6)
	bne.w bad
	move.l d1, d7
	cmpi.l #1, d7
	blo.w bad
	cmpi.l #records.NAME_BYTES-1, d7
	bhi.w bad
	bsr.w row
	bne.w bad
	move.l Bridge.Name(a2), d0
	andi.l #TAG_RESIDENT_NAMES, d0
	cmpi.l #TAG_RESIDENT_NAMES, d0
	bne.w ownedName
	move.l Bridge.Name(a2), d0
	andi.l #OFFSET_MASK, d0
	move.l d7, d1
	lea State.ResidentNames(a6), a0
	jsr names.resolve
	bne.w bad
	movea.l a1, a0
	movea.l a5, a1
	move.l d7, d0
	bsr.w copyBytes
	bra.w terminated
ownedName
	move.l Bridge.Name(a2), d1
	lea State.ActiveNames(a6), a3
	lea State.RetainedNames(a6), a4
	bsr.w readPayload
	bne.w bad
terminated
	clr.b 0(a5, d7.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	beq.w returned
	move.l #1, State.Failure(a0)
returned
	tst.l d0
	rts
	.bend  ; copyName

; A0=State,A1=per-ID module owners words,A2=per-ID module flags words,
; D0=module flags retaining interface entries (supplied by module owner).
; D0/CCR=status; others kept. Publish the complete active range in source order.
; Failed sealing retains every active allocation; release remains sufficient.
; Disk mode retains interface/global facets and spills private payloads. Resident
; mode is the contiguous reference and does not retire module arenas.
seal	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d7
	movea.l a0, a6
	movea.l a1, a3
	movea.l a2, a4
	tst.l State.Failure(a6)
	beq.w healthy
	moveq #1, d0
	bra.w done
healthy
	tst.l State.Disk(a6)
	beq.w good
	suba.w #records.ENTRY_BYTES+records.NAME_BYTES, sp
	movea.l sp, a5
	moveq #0, d5
pass
	move.l State.Start(a6), d6
next
	cmp.l State.Count(a6), d6
	bhs.w passDone
	movea.l a6, a0
	movea.l a5, a1
	move.l d6, d0
	bsr.w readEntry
	bne.w failed
	move.l d6, d0
	add.l d0, d0
	moveq #0, d4
	tst.w 0(a3, d0.l)
	bne.w visibility
	tst.w records.Entry.Owner(a5)
	beq.w retained
visibility
	move.w 0(a4, d0.l), d1
	and.w d7, d1
	bne.w retained
	move.w records.Entry.TemplateFlags(a5), d1
	andi.w #records.TEMPLATE_PUBLIC+records.STATEMENT_PUBLIC, d1
	beq.w classified
retained
	moveq #1, d4
classified
	; Interface names move first so aliases never force a private duplicate.
	tst.l d5
	bne.w name
	tst.l d4
	beq.w advance
name
	move.l records.Entry.Name(a5), d0
	bsr.w row
	bne.w failed
	move.l Bridge.Name(a2), d1
	btst #31, d1
	beq.w entry
	move.l a2, -(sp)
	movea.l a6, a0
	lea records.ENTRY_BYTES(a5), a1
	move.l records.Entry.Name(a5), d0
	moveq #0, d1
	move.w records.Entry.Length(a5), d1
	bsr.w copyName
	movea.l (sp)+, a2
	bne.w failed
	move.l a2, -(sp)
	lea records.ENTRY_BYTES(a5), a1
	moveq #0, d0
	move.w records.Entry.Length(a5), d0
	lea State.RetainedNames(a6), a0
	bsr.w publishPayload
	movea.l (sp)+, a2
	bne.w failed
	move.l d1, Bridge.Name(a2)
entry
	tst.l d5
	beq.w advance
	move.l d6, d0
	bsr.w row
	bne.w failed
	move.l a2, -(sp)
	movea.l a5, a1
	moveq #records.ENTRY_BYTES, d0
	lea State.RetainedEntries(a6), a0
	bsr.w publishPayload
	movea.l (sp)+, a2
	bne.w failed
	move.l d1, Bridge.Entry(a2)
advance
	addq.l #1, d6
	bra.w next
passDone
	addq.l #1, d5
	cmpi.l #2, d5
	blo.w pass
	lea State.Backing(a6), a0
	jsr backing.seal
	bne.w failed
	lea State.ActiveEntries(a6), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.ActiveNames(a6), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.ResidentNames(a6), a0
	jsr names.release
	move.l State.Count(a6), State.Start(a6)
	adda.w #records.ENTRY_BYTES+records.NAME_BYTES, sp
	bra.w good
failed
	adda.w #records.ENTRY_BYTES+records.NAME_BYTES, sp
	moveq #1, d0
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	beq.w returned
	move.l #1, State.Failure(a0)
returned
	tst.l d0
	rts
	.bend  ; seal

; A0=State -> D0/CCR=sticky storage status. Other registers preserved.
; Frontend success boundaries check this after boolean semantic probes.
check	.block
	move.l State.Failure(a0), d0
	tst.l d0
	rts
	.bend  ; check

; A0=State. Retire name payloads at the existing post-binding numeric frontier.
; Entry copies and sealed entry backing remain live for block/member consumers.
; Registers preserved; CCR unspecified. Repeated retirement is safe.
retireNames	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea State.ActiveNames(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.RetainedNames(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.ResidentNames(a1), a0
	jsr names.release
	move.l #1, State.NamesRetired(a1)
	movem.l (sp)+, a0-a1
	rts
	.bend  ; retireNames

; A0=State. Release every arena and backing file, then zero State. Registers kept.
release	.block
	movem.l d0-d1/a0-a2, -(sp)
	movea.l a0, a2
	moveq #OWNED_BLOCKS-1, d1
blocks
	jsr memory.release
	clr.l memory.Block.Used(a0)
	adda.w #memory.Block.Used+4, a0
	dbra d1, blocks
	lea State.ResidentNames(a2), a0
	jsr names.release
	lea State.Backing(a2), a0
	jsr backing.cleanup
	movea.l a2, a0
	move.w #STATE_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; release
	.priv
; D0=index,A6=State -> A2=bridge row,D0/CCR=status. D1 scratch.
row	.block
	cmp.l State.Count(a6), d0
	bhs.w bad
	lsl.l #3, d0
	movea.l State.Bridge+memory.Block.Pointer(a6), a2
	adda.l d0, a2
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; row

; A6=State,D1=location,A3=active Block,A4=retained Block,D7=length.
; A1=checked pointer,D3=lease (zero for arenas),D0/CCR=status.
locate	.block
	moveq #0, d3
	btst #31, d1
	bne.w active
	btst #30, d1
	bne.w retained
	lea State.Backing(a6), a0
	jsr backing.acquire
	tst.l d0
	bne.w bad
	cmp.l d7, d2
	bhs.w good
	move.l d3, d1
	jsr backing.releaseView
	bra.w bad
active
	movea.l a3, a0
	bra.w arena
retained
	movea.l a4, a0
arena
	andi.l #OFFSET_MASK, d1
	move.l d1, d2
	add.l d7, d2
	bcs.w bad
	cmp.l memory.Block.Used(a0), d2
	bhi.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d1, a1
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; locate

; Inputs as locate,A5=destination. No lease escapes. Other scratch caller-owned.
readPayload	.block
	bsr.w locate
	bne.w done
	movea.l a1, a0
	movea.l a5, a1
	move.l d7, d0
	bsr.w copyBytes
	tst.l d3
	beq.w good
	lea State.Backing(a6), a0
	move.l d3, d1
	jsr backing.releaseView
	bra.w done
good
	moveq #0, d0
done
	rts
	.bend  ; readPayload

; A0=retained Block,A1=payload,D0=length,D4=retention flag,A6=State.
; D0/CCR=status,D1=tagged location. Other caller loop state preserved.
publishPayload	.block
	movem.l d2-d3/d5-d7/a0-a5, -(sp)
	tst.l d4
	bne.w retained
	lea State.Backing(a6), a0
	jsr backing.append
	bra.w done
retained
	movea.l a0, a4
	movea.l a1, a3
	move.l d0, d7
	move.l memory.Block.Used(a4), d6
	add.l d6, d0
	move.l #LIMIT*(records.NAME_BYTES-1), d1
	jsr memory.reserveBounded
	bne.w done
	movea.l memory.Block.Pointer(a4), a1
	adda.l d6, a1
	movea.l a3, a0
	move.l d7, d0
	bsr.w copyBytes
	add.l d7, memory.Block.Used(a4)
	move.l d6, d1
	ori.l #TAG_RETAINED, d1
	moveq #0, d0
done
	movem.l (sp)+, d2-d3/d5-d7/a0-a5
	tst.l d0
	rts
	.bend  ; publishPayload

; A0=source,A1=destination,D0=positive bytes. D0/A0/A1 clobbered; CCR unspecified.
copyBytes	.block
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyBytes
	rts
	.bend  ; copyBytes
	.endsection
	.endmodule
