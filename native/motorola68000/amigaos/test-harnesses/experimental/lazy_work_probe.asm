; Isolated work-observer invariants; production never selects this probe.
; @opforge-owner: tests.amigaos.binary_lazy_work_probe
	.module tests.amigaos.binary_lazy_work_probe
	.cpu 68020
	.include "lazy_audit.i"
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_source as source
	.section code, kind=code
	.pub
; D0/CCR=status (11..19 invariant failure); preserves other registers.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l sp, SavedSp
	movem.l Seed, d0-d7/a0-a6
	lea Request, a4
	move.w #$15, ccr
	movem.l d0-d7/a0-a6, Before
	move.w ccr, BeforeFlags
	.WORK_CALL lazy_work.clear
	.WORK_CAPTURE_RESET a4
	.WORK_ACQUIRE capture.CANDIDATE_COUNT, a4
	.WORK_ACQUIRE capture.DESCRIPTOR_COUNT, a4
	.WORK_ACQUIRE capture.FRAGMENT_COUNT, a4
	.WORK_ACQUIRE capture.PLAN_CREATE_COUNT, a4
	.WORK_COUNT lazy_work.BIND, #3
	.WORK_BOUND_BEGIN #10
	.WORK_BOUND_END #14
	.WORK_TEMPLATE_DELTA #10, #14
	.WORK_CALL lazy_work.sampleBegin
	move.w ccr, AfterFlags
	movem.l d0-d7/a0-a6, After
	moveq #11, d0
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
	cmpi.l #$01010101, Request+capture.Frame.WorkCounts
	bne.w done
	jsr lazy_work.clear
	moveq #10, d1
	moveq #14, d2
	move.w #$15, ccr
	.WORK_TEMPLATE_DELTA d1, d2
	move.w ccr, AfterFlags
	moveq #19, d0
	cmpi.w #$15, AfterFlags
	bne.w done
	cmpi.l #10, d1
	bne.w done
	cmpi.l #14, d2
	bne.w done
	jsr lazy_work.getState
	cmpi.l #4, lazy_work.WorkFields.Totals+lazy_work.TEMPLATE_BYTES(a0)
	bne.w done
	jsr lazy_work.clear
	moveq #17, d0
	.WORK_COUNT lazy_work.REPLAY_COPY, d0
	jsr lazy_work.getState
	moveq #18, d0
	cmpi.l #17, lazy_work.WorkFields.Totals+lazy_work.REPLAY_COPY(a0)
	bne.w done
	jsr lazy_work.clear
	.WORK_COUNT lazy_work.BIND, #$ffffffff
	.WORK_COUNT lazy_work.BIND, #1
	jsr lazy_work.getState
	moveq #12, d0
	cmpi.l #$ffffffff, lazy_work.WorkFields.Totals+lazy_work.BIND(a0)
	bne.w done
	cmpi.l #lazy_units.OVERFLOW, lazy_work.WorkFields.Errors(a0)
	bne.w done
	jsr lazy_work.clear
	lea Request, a4
	.WORK_CAPTURE_RESET a4
	move.w #255, d7
acquisitions
	.WORK_ACQUIRE capture.CANDIDATE_COUNT, a4
	dbra d7, acquisitions
	jsr lazy_work.getState
	moveq #13, d0
	cmpi.l #256, lazy_work.WorkFields.Totals+lazy_work.CANDIDATE(a0)
	bne.w done
	cmpi.l #lazy_units.OVERFLOW, lazy_work.WorkFields.Errors(a0)
	bne.w done
	cmpi.b #255, Request+capture.Frame.WorkCounts
	bne.w done
	jsr lazy_work.clear
	jsr lazy_units.clear
	.WORK_CALL lazy_work.sampleBegin
	.WORK_COUNT lazy_work.BIND, #3
	.WORK_COUNT lazy_work.COMPILE, #2
	.WORK_COUNT lazy_work.TEMPLATE_BYTES, #7
	lea Prepared, a0
	moveq #8, d0
	moveq #10, d1
	moveq #5, d2
	moveq #1, d3
	moveq #2, d4
	moveq #0, d5
	jsr lazy_units.prepared
	lea BlockRows, a0
	moveq #1, d0
	jsr lazy_units.reconcile
	lea BlockRows, a4
	moveq #1, d0
	.WORK_SELECTED a4, d0
	jsr lazy_work.getState
	moveq #14, d0
	tst.l lazy_work.WorkFields.Errors(a0)
	bne.w done
	cmpi.l #3, lazy_work.WorkFields.Unreferenced+lazy_work.BIND(a0)
	bne.w done
	cmpi.l #2, lazy_work.WorkFields.Unreferenced+lazy_work.COMPILE(a0)
	bne.w done
	cmpi.l #7, lazy_work.WorkFields.Shared+lazy_work.TEMPLATE_BYTES(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Unreferenced+lazy_work.TEMPLATE_BYTES(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Unknown+lazy_work.BIND(a0)
	bne.w done
	jsr lazy_work.clear
	jsr lazy_units.clear
	lea Nested, a0
	moveq #4, d0
	moveq #10, d1
	moveq #5, d2
	moveq #1, d3
	moveq #2, d4
	moveq #0, d5
	jsr lazy_units.prepared
	.WORK_COUNT lazy_work.BIND, #2
	lea Nested+4, a0
	moveq #8, d0
	moveq #14, d1
	moveq #6, d2
	jsr lazy_units.prepared
	lea Nested+12, a0
	moveq #4, d0
	moveq #22, d1
	moveq #7, d2
	jsr lazy_units.prepared
	lea NestedRows, a0
	moveq #2, d0
	jsr lazy_units.reconcile
	lea NestedRows, a4
	.WORK_SELECTED a4, #2
	jsr lazy_work.getState
	moveq #15, d0
	cmpi.l #1, lazy_work.WorkFields.Rows+lazy_work.ROW_BYTES+lazy_work.Row.Class(a0)
	bne.w done
	cmpi.l #2, lazy_work.WorkFields.Selected+lazy_work.BIND(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Unreferenced+lazy_work.BIND(a0)
	bne.w done
	jsr lazy_work.clear
	jsr lazy_units.clear
	move.l #capture.MAGIC, Malformed+capture.Record.Magic
	move.l #capture.HEADER_BYTES, Malformed+capture.Record.Bytes
	move.l #capture.HEADER_BYTES+1, Malformed+capture.Record.LexemeBytes
	lea InvalidRegion, a4
	.WORK_CAPTURE_REGION a4
	jsr lazy_work.getState
	moveq #16, d0
	cmpi.l #lazy_units.INVALID, lazy_work.WorkFields.Errors(a0)
	bne.w done
	jsr lazy_work.clear
	jsr lazy_work.getState
	moveq #17, d0
	tst.l lazy_work.WorkFields.Errors(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Count(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Totals+lazy_work.BIND(a0)
	bne.w done
	jsr lazy_units.clear
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.endsection
	.section data, kind=data
InvalidRegion	.long 0, capture.HEADER_BYTES, 1, 0
	.long Malformed, capture.HEADER_BYTES, capture.HEADER_BYTES
Seed	.long $10101010, $21212121, $32323232, $43434343
	.long $54545454, $65656565, $76767676, $87878787
	.long $18181818, $29292929, $3a3a3a3a, $4b4b4b4b
	.long $5c5c5c5c, $6d6d6d6d, $7e7e7e7e
Prepared	.byte source.RECORD_HEADER_BYTES-1, source.FLAG_BLOCK_OPEN
	.word 2
	.byte source.RECORD_HEADER_BYTES-1, source.FLAG_BLOCK_CLOSE
	.word 3
Nested	.byte source.RECORD_HEADER_BYTES-1, source.FLAG_BLOCK_OPEN
	.word 2
	.byte source.RECORD_HEADER_BYTES-1, source.FLAG_BLOCK_OPEN
	.word 3
	.byte source.RECORD_HEADER_BYTES-1, source.FLAG_BLOCK_CLOSE
	.word 4
	.byte source.RECORD_HEADER_BYTES-1, source.FLAG_BLOCK_CLOSE
	.word 5
NestedRows	.long 10, 26
	.word 1, 0, 1
	.long 14, 22
	.word 2, 1, 0
BlockRows	.long 10, 18
	.word 1, 0, 0
	.endsection
	.section bss, kind=bss
Malformed	.res byte, capture.HEADER_BYTES
Request	.res byte, capture.FRAME_BYTES
Before	.res long, 15
After	.res long, 15
BeforeFlags	.res word, 1
AfterFlags	.res word, 1
SavedSp	.res long, 1
	.endsection
	.endmodule
