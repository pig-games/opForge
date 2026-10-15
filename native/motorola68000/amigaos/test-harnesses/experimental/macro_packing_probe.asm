; Component checks for the passive observer, isolated from actual assembly work.
; @opforge-owner: tests.amigaos.macro_packing_probe
	.module tests.amigaos.macro_packing_probe
	.cpu 68020
	.use debug.amigaos.lazy_work as work
; Exercise each public passive operation with seeded registers and CCR=$15.
; Probe-owned snapshots never enter an observer record or production structure.
PACKING_PROBE_CALL	.macro routine
	movem.l d0-d7/a0-a6, Before
	move.l sp, SavedSp
	move.w #$15, ccr
	jsr .routine
	move.w ccr, AfterFlags
	movem.l d0-d7/a0-a6, After
	move.l sp, d0
	cmp.l SavedSp, d0
	bne.w invalid
	cmpi.w #$15, AfterFlags
	bne.w invalid
	lea Before, a0
	lea After, a1
	moveq #14, d0
next
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w invalid
	dbra d0, next
	bra.w done
invalid
	move.l #80, Failure
done
	movea.l SavedSp, sp
	movem.l Before, d0-d7/a0-a6
.endmacro
	.section code, kind=code
	.pub
; D0/CCR=status (80 preservation,81 counters,82 invalid/overflow cases).
; Preserves other registers; clears test observations before returning success.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	clr.l Failure
	movem.l Seed, d0-d7/a0-a6
	.PACKING_PROBE_CALL work.packingClear
	moveq #work.PackingRow.SourceBinds, d0
	moveq #3, d1
	.PACKING_PROBE_CALL work.packingCount
	moveq #0, d0
	moveq #0, d1
	moveq #0, d2
	moveq #3, d3
	moveq #7, d4
	moveq #1, d5
	.PACKING_PROBE_CALL work.packingBegin
	moveq #work.PackingRow.Names, d0
	moveq #2, d1
	.PACKING_PROBE_CALL work.packingCount
	moveq #0, d0
	moveq #0, d1
	moveq #0, d2
	moveq #1, d3
	moveq #4, d4
	moveq #2, d5
	.PACKING_PROBE_CALL work.packingBegin
	moveq #work.PackingRow.CaptureBinds, d0
	moveq #1, d1
	.PACKING_PROBE_CALL work.packingCount
	moveq #1, d0
	move.l #999, d1
	.PACKING_PROBE_CALL work.packingEnd
	moveq #0, d0
	moveq #9, d1
	.PACKING_PROBE_CALL work.packingEnd
	jsr work.packingGetState
	moveq #81, d0
	tst.l work.PackingFields.Depth(a0)
	bne.w done
	cmpi.l #2, work.PackingFields.MaxDepth(a0)
	bne.w done
	cmpi.l #2, work.PackingFields.Completed(a0)
	bne.w done
	cmpi.l #3, work.PackingFields.Outside+work.PackingRow.SourceBinds(a0)
	bne.w done
	tst.l work.PackingFields.Gross+work.PackingRow.SourceBinds(a0)
	bne.w done
	cmpi.l #2, work.PackingFields.Gross+work.PackingRow.Writes(a0)
	bne.w done
	cmpi.l #4, work.PackingFields.Gross+work.PackingRow.Tokens(a0)
	bne.w done
	cmpi.l #11, work.PackingFields.Gross+work.PackingRow.LexemeBytes(a0)
	bne.w done
	cmpi.l #12, work.PackingFields.Gross+work.PackingRow.MapBytes(a0)
	bne.w done
	cmpi.l #9, work.PackingFields.Gross+work.PackingRow.PackedBytes(a0)
	bne.w done
	cmpi.l #2, work.PackingFields.Gross+work.PackingRow.Names(a0)
	bne.w done
	cmpi.l #1, work.PackingFields.Gross+work.PackingRow.CaptureBinds(a0)
	bne.w done
	cmpi.l #1, work.PackingFields.Gross+work.PackingRow.Failures(a0)
	bne.w done
	cmpi.l #1, work.PackingFields.Gross+work.PackingRow.PrefixWrites(a0)
	bne.w done
	; Compare the ordinary row to independently accumulated gross, including
	; timing fields when this probe is run after the framework clock opens.
	lea work.PackingFields.Gross(a0), a1
	adda.l #work.PackingFields.Rows, a0
	moveq #work.PACKING_ROW_BYTES/4-1, d2
compare
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w done
	dbra d2, compare
	; Stored needs an open inventory identity: zero is always invalid.
	moveq #0, d0
	moveq #0, d1
	moveq #0, d2
	moveq #10, d3
	.PACKING_PROBE_CALL work.packingStored
	jsr work.packingGetState
	moveq #82, d0
	move.l work.PackingFields.Errors(a0), d1
	andi.l #work.PACKING_IDENTITY, d1
	beq.w done
	.PACKING_PROBE_CALL work.packingClear
	; A rejected 33rd begin consumes its matching end without losing the
	; 32nd frame; bounded frames and stack remain intact.
	moveq #32, d7
nested
	moveq #0, d0
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
	moveq #0, d4
	moveq #0, d5
	.PACKING_PROBE_CALL work.packingBegin
	dbra d7, nested
	jsr work.packingGetState
	moveq #82, d0
	cmpi.l #32, work.PackingFields.Depth(a0)
	bne.w done
	cmpi.l #1, work.PackingFields.OverflowDepth(a0)
	bne.w done
	moveq #32, d7
unwind
	moveq #0, d0
	moveq #0, d1
	.PACKING_PROBE_CALL work.packingEnd
	dbra d7, unwind
	jsr work.packingGetState
	moveq #82, d0
	tst.l work.PackingFields.Depth(a0)
	bne.w done
	tst.l work.PackingFields.OverflowDepth(a0)
	bne.w done
	cmpi.l #32, work.PackingFields.Completed(a0)
	bne.w done
	move.l work.PackingFields.Errors(a0), d1
	andi.l #work.PACKING_NESTING, d1
	beq.w done
	.PACKING_PROBE_CALL work.packingClear
	; Counter saturation and invalid metric never wrap into plausible work.
	moveq #0, d0
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
	moveq #0, d4
	moveq #0, d5
	.PACKING_PROBE_CALL work.packingBegin
	moveq #work.PackingRow.Names, d0
	moveq #-1, d1
	.PACKING_PROBE_CALL work.packingCount
	moveq #1, d1
	.PACKING_PROBE_CALL work.packingCount
	moveq #0, d0
	.PACKING_PROBE_CALL work.packingCount
	moveq #0, d1
	.PACKING_PROBE_CALL work.packingEnd
	jsr work.packingGetState
	moveq #82, d0
	cmpi.l #-1, work.PackingFields.Gross+work.PackingRow.Names(a0)
	bne.w done
	move.l work.PackingFields.Errors(a0), d1
	andi.l #work.PACKING_OVERFLOW+work.PACKING_BOUNDS, d1
	cmpi.l #work.PACKING_OVERFLOW+work.PACKING_BOUNDS, d1
	bne.w done
	; No borrowed lookup or previous test mutation can silently invent a row.
	.PACKING_PROBE_CALL work.packingClear
	moveq #1, d0
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
	moveq #0, d4
	moveq #0, d5
	.PACKING_PROBE_CALL work.packingBegin
	moveq #0, d0
	.PACKING_PROBE_CALL work.packingEnd
	jsr work.packingGetState
	moveq #82, d0
	move.l work.PackingFields.Errors(a0), d1
	andi.l #work.PACKING_IDENTITY, d1
	beq.w done
	tst.l work.PackingFields.Gross+work.PackingRow.Writes(a0)
	bne.w done
	; Equal handles in internal/external pools must join different inventory
	; ordinals. Stored publication counts are independent of writer windows.
	jsr work.clear
	lea Owner, a0
	jsr work.templateBegin
	move.l Owner, Definition+work.TemplateDefinePacket.Session
	move.l #1, Definition+work.TemplateDefinePacket.Handle
	move.l #10, Definition+work.TemplateDefinePacket.First
	clr.l Definition+work.TemplateDefinePacket.Pool
	clr.l Definition+work.TemplateDefinePacket.Kind
	lea Definition, a0
	jsr work.templateDefine
	move.l #1, Definition+work.TemplateDefinePacket.Pool
	move.l #2, Definition+work.TemplateDefinePacket.Kind
	jsr work.templateDefine
	moveq #1, d0
	moveq #0, d1
	move.l Owner, d2
	moveq #2, d3
	moveq #5, d4
	moveq #0, d5
	.PACKING_PROBE_CALL work.packingBegin
	moveq #work.PackingRow.Names, d0
	moveq #1, d1
	.PACKING_PROBE_CALL work.packingCount
	moveq #0, d0
	moveq #8, d1
	.PACKING_PROBE_CALL work.packingEnd
	moveq #1, d0
	moveq #0, d1
	move.l Owner, d2
	moveq #13, d3
	.PACKING_PROBE_CALL work.packingStored
	moveq #1, d0
	moveq #1, d1
	move.l Owner, d2
	moveq #1, d3
	moveq #3, d4
	moveq #1, d5
	.PACKING_PROBE_CALL work.packingBegin
	moveq #0, d0
	moveq #6, d1
	.PACKING_PROBE_CALL work.packingEnd
	moveq #1, d0
	moveq #1, d1
	move.l Owner, d2
	moveq #7, d3
	.PACKING_PROBE_CALL work.packingStored
	jsr work.packingGetState
	moveq #82, d0
	lea work.PackingFields.Rows+work.PACKING_ROW_BYTES(a0), a1
	cmpi.l #8, work.PackingRow.PackedBytes(a1)
	bne.w done
	cmpi.l #13, work.PackingRow.StoredBytes(a1)
	bne.w done
	cmpi.l #1, work.PackingRow.StoredRecords(a1)
	bne.w done
	cmpi.l #1, work.PackingRow.Names(a1)
	bne.w done
	adda.w #work.PACKING_ROW_BYTES, a1
	cmpi.l #6, work.PackingRow.PackedBytes(a1)
	bne.w done
	cmpi.l #7, work.PackingRow.StoredBytes(a1)
	bne.w done
	cmpi.l #1, work.PackingRow.StoredRecords(a1)
	bne.w done
	cmpi.l #1, work.PackingRow.PrefixWrites(a1)
	bne.w done
	move.l work.PackingFields.Errors(a0), d1
	andi.l #work.PACKING_IDENTITY+work.PACKING_BOUNDS+work.PACKING_RECONCILE, d1
	bne.w done
	moveq #1, d0
	moveq #0, d1
	moveq #23, d2
	move.l Owner, d3
	jsr work.templateClose
	moveq #1, d0
	moveq #1, d1
	moveq #17, d2
	move.l Owner, d3
	jsr work.templateClose
	move.l Failure, d0
	bne.w done
	jsr work.clear
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.endsection
	.section data, kind=data
Seed	.long $10101010, $21212121, $32323232, $43434343
	.long $54545454, $65656565, $76767676, $87878787
	.long $18181818, $29292929, $3a3a3a3a, $4b4b4b4b
	.long $5c5c5c5c, $6d6d6d6d, $7e7e7e7e
	.endsection
	.section bss, kind=bss
Before	.res long, 15
After	.res long, 15
AfterFlags	.res word, 1
SavedSp	.res long, 1
Failure	.res long, 1
Owner	.res long, 1
Definition	.res byte, work.TEMPLATE_DEFINE_PACKET_BYTES
	.endsection
	.endmodule
