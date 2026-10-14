; Disposable component probe. No source identity selects production behavior.
; @opforge-owner: tests.amigaos.binary_lazy_template_probe
	.module tests.amigaos.binary_lazy_template_probe
	.cpu 68020
	.include "lazy_work.i"
Step	.struct
Routine	.long ?
Packet	.long ?
D0	.long ?
D1	.long ?
D2	.long ?
D3	.long ?
.endstruct
STEP_BYTES = Step.D3+4
; Record around an indirect passive entry. The comparator returns 26 or zero.
CHECK_PASSIVE	.macro
	move.l sp, BeforeSp
	movem.l d0-d7/a0-a6, Before
	move.w #$15, ccr
	move.w ccr, BeforeFlags
	jsr (a3)
	move.w ccr, AfterFlags
	movem.l d0-d7/a0-a6, After
	move.l sp, AfterSp
	bsr.w preservation
.endmacro
CHECK_FIELDS	.macro table, status
	jsr lazy_work.getTemplates
	lea .table, a1
	moveq #.status, d7
	bsr.w fields
.endmacro
	.section code, kind=code
	.pub
; No inputs. D0/CCR=0 success or 26..37 failed invariant; others preserved.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #1, d0
	bsr.w setPacketSession
	lea Steps, a0
	move.l a0, CaseCursor
matrix
	movea.l CaseCursor, a0
	tst.l Step.Routine(a0)
	beq.w accounted
	movem.l Seed, d0-d7/a0-a6
	movea.l CaseCursor, a3
	movea.l Step.Packet(a3), a0
	move.l Step.d0(a3), d0
	move.l Step.d1(a3), d1
	move.l Step.d2(a3), d2
	move.l Step.d3(a3), d3
	movea.l Step.Routine(a3), a3
	.CHECK_PASSIVE
	bne.w done
	addi.l #STEP_BYTES, CaseCursor
	bra.w matrix
accounted
	.CHECK_FIELDS Accounting, 27
	bne.w done
	.CHECK_FIELDS Consumers, 28
	bne.w done
	.CHECK_FIELDS Residuals, 29
	bne.w done
	; Handle one is reusable after finish but must produce a new epoch/row.
	lea OwnerA, a0
	jsr lazy_work.templateBegin
	bsr.w setSession
	lea InternalDef, a0
	jsr lazy_work.templateDefine
	moveq #1, d0
	moveq #0, d1
	moveq #10, d2
	move.l OwnerA, d3
	jsr lazy_work.templateClose
	lea RootCall, a0
	jsr lazy_work.templateCall
	lea Pools, a0
	jsr lazy_work.templatePools
	.CHECK_FIELDS Epochs, 30
	bne.w done
	; Getter has the sole documented A0 output; compare every other register.
	movem.l Seed, d0-d7/a0-a6
	lea lazy_work.getTemplates, a3
	.CHECK_PASSIVE
	; The first comparator is expected to reject changed A0; accept only that
	; output and rerun the same register/CCR/SP comparison.
	move.l After+8*4, Before+8*4
	bsr.w preservation
	bne.w done
	jsr lazy_work.clear
	.CHECK_FIELDS Cleared, 31
	bne.w done
	bsr.w freshDefinition
	lea InternalDef, a0
	lea lazy_work.templateDefine, a3
	.CHECK_PASSIVE
	bne.w done
	.CHECK_FIELDS Duplicate, 32
	bne.w done
	bsr.w freshDefinition
	moveq #2, d0
	moveq #0, d1
	moveq #10, d2
	move.l OwnerA, d3
	lea lazy_work.templateClose, a3
	.CHECK_PASSIVE
	bne.w done
	.CHECK_FIELDS Duplicate, 32
	bne.w done
	bsr.w freshDefinition
	lea ExternalDef, a0
	jsr lazy_work.templateDefine
	moveq #1, d0
	moveq #1, d1
	moveq #9, d2  ; below First=10, a negative body extent
	move.l OwnerA, d3
	lea lazy_work.templateClose, a3
	.CHECK_PASSIVE
	bne.w done
	.CHECK_FIELDS BadRange, 33
	bne.w done
	bsr.w freshDefinition
	moveq #0, d0
	jsr lazy_work.contextSet
	moveq #0, d1
	moveq #2, d2
	jsr lazy_work.templateCount
	.CHECK_FIELDS MissingOwner, 32
	bne.w done
	; Fill the real direct map/table, then attempt one extra handle.
	bsr.w freshDefinition
	moveq #2, d7
definitionCapacity
	move.l d7, InternalDef+lazy_work.TemplateDefinePacket.Handle
	lea InternalDef, a0
	jsr lazy_work.templateDefine
	addq.l #1, d7
	cmpi.l #lazy_work.TEMPLATE_DEFINITION_LIMIT+1, d7
	bls.w definitionCapacity
	move.l #1, InternalDef+lazy_work.TemplateDefinePacket.Handle
	.CHECK_FIELDS FullDefinitions, 34
	bne.w done
	; Every distinct source origin is a distinct consumer edge.
	bsr.w freshDefinition
	moveq #1, d0
	moveq #0, d1
	moveq #10, d2
	move.l OwnerA, d3
	jsr lazy_work.templateClose
	moveq #1, d7
edgeCapacity
	move.l d7, RootCall+lazy_work.TemplateCallPacket.Origin
	lea RootCall, a0
	jsr lazy_work.templateCall
	addq.l #1, d7
	cmpi.l #lazy_work.TEMPLATE_EDGE_LIMIT+1, d7
	bls.w edgeCapacity
	move.l #4, RootCall+lazy_work.TemplateCallPacket.Origin
	.CHECK_FIELDS FullEdges, 34
	bne.w done
	bsr.w freshDefinition
	moveq #1, d0
	moveq #0, d1
	move.l OwnerA, d2
	jsr lazy_work.templateOwner
	moveq #0, d0
	jsr lazy_work.contextSet
	moveq #0, d1
	moveq #-1, d2
	jsr lazy_work.templateCount
	moveq #1, d2
	lea lazy_work.templateCount, a3
	.CHECK_PASSIVE
	bne.w done
	.CHECK_FIELDS Overflowed, 35
	bne.w done
	jsr lazy_work.clear
	lea ProbeOwners, a0
	moveq #lazy_work.TEMPLATE_OWNER_LIMIT, d7
ownerCapacity
	jsr lazy_work.templateBegin
	addq.l #4, a0
	dbra d7, ownerCapacity
	.CHECK_FIELDS FullOwners, 36
	bne.w done
	bsr.w freshDefinition
	lea Pools, a0
	jsr lazy_work.templatePools
	.CHECK_FIELDS Unclosed, 36
	bne.w done
	bsr.w overlappingOwners
	bne.w done
	jsr lazy_work.clear
	.CHECK_FIELDS Cleared, 31
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
freshDefinition	.block
	jsr lazy_work.clear
	lea OwnerA, a0
	jsr lazy_work.templateBegin
	bsr.w setSession
	lea InternalDef, a0
	jsr lazy_work.templateDefine
	rts
	.bend  ; freshDefinition
setSession	.block
	move.l OwnerA, d0
	bra.w setPacketSession
	.bend  ; setSession
; D0=numeric session; packets are probe-owned mutable fixtures.
setPacketSession	.block
	move.l d0, InternalDef+lazy_work.TemplateDefinePacket.Session
	move.l d0, ExternalDef+lazy_work.TemplateDefinePacket.Session
	move.l d0, RootCall+lazy_work.TemplateCallPacket.Session
	move.l d0, NestedCall+lazy_work.TemplateCallPacket.Session
	move.l d0, RepeatedCall+lazy_work.TemplateCallPacket.Session
	move.l d0, Pools+lazy_work.TemplatePoolsPacket.Session
	rts
	.bend  ; setPacketSession
; A0=inventory,A1=offset/value pairs ending with -1,D7=failure status.
fields	.block
	move.l d7, d0
next
	move.l (a1)+, d1
	bmi.w success
	move.l (a1)+, d2
	cmp.l 0(a0, d1.l), d2
	bne.w done
	bra.w next
success
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; fields
preservation	.block
	moveq #26, d0
	move.l BeforeSp, d1
	cmp.l AfterSp, d1
	bne.w done
	move.w BeforeFlags, d1
	cmp.w AfterFlags, d1
	bne.w done
	lea Before, a0
	lea After, a1
	moveq #14, d2
next
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w done
	dbra d2, next
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; preservation
	.endsection
	.include "lazy_template_probe_data.i"
	.include "lazy_template_probe_owners.i"
	.endmodule
