; Isolated passive-observer invariant probe; never selected by production input.
; @opforge-owner: tests.amigaos.binary_lazy_audit_probe
	.module tests.amigaos.binary_lazy_audit_probe
	.cpu 68020
	.include "lazy_audit.i"
	.include "binary_block_span.i"
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_graph as graph
	.section code, kind=code
	.pub
; No inputs. D0/CCR=0 on success, otherwise the failed invariant number.
; Preserves all other registers. Runs only in a disposable probe executable.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l sp, SavedSp
	movem.l Seed, d0-d7/a0-a6
	lea BlockRows, a4
	move.w #$15, ccr
	movem.l d0-d7/a0-a6, Before
	move.w ccr, BeforeFlags
	.LAZY_CLEAR
	.LAZY_COUNT lazy_audit.PHYSICAL, #1
	.LAZY_REGION Descriptor, #0
	.LAZY_PIN 1
	.LAZY_PIN -1
	.LAZY_SPAN #0, #8, #1, #10
	.LAZY_SPAN_END #20
	.LAZY_SCHEDULE Schedule, #1, graph.SPAN_BYTES
	.LAZY_CLASSIFY
	.LAZY_BLOCKS a4, #1
	move.w ccr, AfterFlags
	movem.l d0-d7/a0-a6, After
	moveq #1, d0
	move.l sp, d1
	cmp.l SavedSp, d1
	bne.w done
	move.w BeforeFlags, d1
	cmp.w AfterFlags, d1
	bne.w done
	lea Before, a0
	lea After, a1
	moveq #14, d2
preserved
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w done
	dbra d2, preserved
	jsr lazy_audit.clear
	.LAZY_COUNT lazy_audit.PHYSICAL, #$ffffffff
	.LAZY_COUNT lazy_audit.PHYSICAL, #1
	jsr lazy_audit.getState
	moveq #2, d0
	cmpi.l #$ffffffff, lazy_audit.Fields.Physical(a0)
	bne.w done
	cmpi.l #lazy_audit.OVERFLOW, lazy_audit.Fields.Errors(a0)
	bne.w done
	jsr lazy_audit.clear
	.LAZY_PIN -1
	jsr lazy_audit.getState
	moveq #3, d0
	cmpi.l #lazy_audit.PIN_UNDERFLOW, lazy_audit.Fields.Errors(a0)
	bne.w done
	jsr lazy_audit.clear
	.LAZY_PIN 1
	.LAZY_PIN 1
	.LAZY_PIN -1
	.LAZY_PIN -1
	jsr lazy_audit.getState
	moveq #4, d0
	tst.l lazy_audit.Fields.Pins(a0)
	bne.w done
	cmpi.l #2, lazy_audit.Fields.MaxPins(a0)
	bne.w done
	tst.l lazy_audit.Fields.Errors(a0)
	bne.w done
	jsr lazy_audit.clear
	moveq #0, d7
regions
	.LAZY_REGION Descriptor, d7
	addq.l #1, d7
	cmpi.l #lazy_audit.REGION_LIMIT+1, d7
	blo.w regions
	jsr lazy_audit.getState
	moveq #5, d0
	cmpi.l #lazy_audit.REGION_LIMIT, lazy_audit.Fields.RegionCount(a0)
	bne.w done
	cmpi.l #lazy_audit.EXHAUSTED, lazy_audit.Fields.Errors(a0)
	bne.w done
	jsr lazy_audit.clear
	.LAZY_REGION InvalidDescriptor, #0
	jsr lazy_audit.getState
	moveq #6, d0
	cmpi.l #lazy_audit.INVALID, lazy_audit.Fields.Errors(a0)
	bne.w done
	jsr lazy_audit.clear
	move.l #lazy_audit.MAP_LIMIT, d7
maps
	.LAZY_SPAN #0, #8, #1, #10
	.LAZY_SPAN_END #20
	subq.l #1, d7
	bne.w maps
	.LAZY_SPAN #0, #8, #1, #10
	.LAZY_SPAN_END #99
	jsr lazy_audit.getState
	moveq #7, d0
	cmpi.l #lazy_audit.MAP_LIMIT, lazy_audit.Fields.Completed(a0)
	bne.w done
	cmpi.l #lazy_audit.EXHAUSTED, lazy_audit.Fields.Errors(a0)
	bne.w done
	adda.l #lazy_audit.Fields.MapRows+(lazy_audit.MAP_LIMIT-1)*lazy_audit.MAPPING_BYTES, a0
	cmpi.l #20, lazy_audit.Mapping.PreparedEnd(a0)
	bne.w done
	jsr lazy_audit.clear
	lea Prepared, a0
	moveq #8, d0
	moveq #10, d1
	moveq #5, d2
	moveq #1, d3
	moveq #2, d4
	moveq #0, d5
	jsr lazy_units.prepared
	.LAZY_REMAP #10, #18, #30
	; A later disjoint span must not overwrite translated boundary positions.
	.LAZY_REMAP #20, #28, #0
	lea BlockRows, a0
	moveq #1, d0
	jsr lazy_units.reconcile
	jsr lazy_units.getState
	moveq #8, d0
	cmpi.l #1, lazy_units.Fields.Matched(a0)
	bne.w done
	tst.l lazy_units.Fields.Unmatched(a0)
	bne.w done
	tst.l lazy_units.Fields.Errors(a0)
	bne.w done
	tst.l lazy_units.Fields.Depth(a0)
	bne.w done
	cmpi.l #30, lazy_units.Fields.Events+lazy_units.Event.FinalOpen(a0)
	bne.w done
	cmpi.l #38, lazy_units.Fields.Events+lazy_units.Event.FinalEnd(a0)
	bne.w done
	cmpi.l #1, lazy_units.Fields.Events+lazy_units.Event.Unit(a0)
	bne.w done
	jsr lazy_audit.clear
	.LAZY_REGION Descriptor, #0
	.LAZY_SCHEDULE Schedule, #1, graph.SPAN_BYTES
	.LAZY_SPAN #0, #8, #1, #10
	.LAZY_SPAN_END #20
	jsr lazy_audit.getState
	moveq #9, d0
	cmpi.l #16, lazy_audit.Fields.Eligible(a0)
	bne.w done
	cmpi.l #1, lazy_audit.Fields.EligibleRegions(a0)
	bne.w done
	cmpi.l #16, lazy_audit.Fields.Regions+lazy_audit.Region.Reasons(a0)
	bne.w done
	lea Descriptor, a0
	moveq #0, d1
	.LAZY_LOOKUP
	jsr lazy_audit.getState
	cmpi.l #1, lazy_audit.Fields.Late(a0)
	bne.w done
	jsr lazy_audit.clear
	jsr lazy_audit.getState
	moveq #10, d0
	tst.l lazy_audit.Fields.Errors(a0)
	bne.w done
	tst.l lazy_audit.Fields.RegionCount(a0)
	bne.w done
	jsr lazy_units.getState
	tst.l lazy_units.Fields.Count(a0)
	bne.w done
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
ScheduledSpan	.long 0, 8, 1
Schedule	.long ScheduledSpan, graph.SPAN_BYTES, graph.SPAN_BYTES
Descriptor	.long 0, 8, 1, 0, 0, 16, 8
InvalidDescriptor	.long 0, 9, 1, 0, 0, 16, 8
Prepared	.byte source.RECORD_HEADER_BYTES-1, source.FLAG_BLOCK_OPEN
	.word 2
	.byte source.RECORD_HEADER_BYTES-1, source.FLAG_BLOCK_CLOSE
	.word 3
BlockRows	.long 30, 38
	.word 1, 0, 1
	.endsection
	.section bss, kind=bss
Before	.res long, 15
After	.res long, 15
BeforeFlags	.res word, 1
AfterFlags	.res word, 1
SavedSp	.res long, 1
	.endsection
	.endmodule
