; Isolated work-observer invariants; production never selects this probe.
; @opforge-owner: tests.amigaos.binary_lazy_work_probe
	.module tests.amigaos.binary_lazy_work_probe
	.cpu 68020
	.include "lazy_audit.i"
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_source as source
	.section code, kind=code
	.pub
; D0/CCR=status (11..25 invariant failure); preserves other registers.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l sp, SavedSp
	jsr lazy_units.clear
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
	.WORK_PLAN_CONTEXT #1
	.WORK_CONTEXT 0, #3
	.WORK_SAMPLE_BEGIN #1
	.WORK_CALL lazy_work.sampleEnd
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
	.WORK_SAMPLE_BEGIN #1
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
	; No publication occurs during this invocation. Lifecycle completion owns
	; its preparation exactly once, including plan binding and committed bytes.
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
	.WORK_SAMPLE_BEGIN #1
	.WORK_COUNT lazy_work.PLAN_BIND, #2
	.WORK_COUNT lazy_work.BOUND_BYTES, #11
	.WORK_CALL lazy_work.sampleEnd
	.WORK_CALL lazy_work.sampleEnd
	jsr lazy_work.getState
	moveq #20, d0
	cmpi.l #2, lazy_work.WorkFields.Rows+lazy_work.Row.Values+lazy_work.PLAN_BIND(a0)
	bne.w done
	cmpi.l #11, lazy_work.WorkFields.Rows+lazy_work.Row.Values+lazy_work.BOUND_BYTES(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Sample+lazy_work.PLAN_BIND(a0)
	bne.w done
	tst.l lazy_work.WorkFields.SampleOwner(a0)
	bne.w done
	.WORK_SAMPLE_BEGIN #2
	.WORK_COUNT lazy_work.BIND, #1
	.WORK_CALL lazy_work.sampleEnd
	jsr lazy_work.getState
	cmpi.l #lazy_work.MULTIFILE, lazy_work.WorkFields.Rows+lazy_work.Row.Reasons(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Errors(a0)
	bne.w done
	jsr lazy_work.clear
	jsr lazy_units.clear
	move.l #capture.MAGIC, Malformed+capture.Record.Magic
	move.l #capture.HEADER_BYTES, Malformed+capture.Record.Bytes
	move.l #capture.HEADER_BYTES+1, Malformed+capture.Record.LexemeBytes
	lea InvalidRegion, a4
	.WORK_CAPTURE_REGION a4, #capture.HEADER_BYTES
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
	; Quantitative unknown split, stable largest rows, scalar range causes.
	jsr lazy_work.clear
	jsr lazy_units.clear
	jsr lazy_units.getState
	move.l #4, lazy_units.Fields.Count(a0)
	lea lazy_units.Fields.Events(a0), a1
	moveq #3, d2
seedRanges
	move.l #10, lazy_units.Event.CaptureOpen(a1)
	move.l #10, lazy_units.Event.CaptureClose(a1)
	move.l #20, lazy_units.Event.OriginalEnd(a1)
	move.l #1, lazy_units.Event.Origin(a1)
	move.l #2, lazy_units.Event.Line(a1)
	move.l #1, lazy_units.Event.Unit(a1)
	adda.w #lazy_units.EVENT_BYTES, a1
	dbra d2, seedRanges
	lea lazy_units.Fields.Events(a0), a1
	move.l #9, lazy_units.Event.CaptureClose(a1)
	move.l #14+capture.HEADER_BYTES, lazy_units.EVENT_BYTES+lazy_units.Event.CaptureClose(a1)
	clr.l lazy_units.EVENT_BYTES*2+lazy_units.Event.OriginalEnd(a1)
	move.l #5, lazy_units.EVENT_BYTES*3+lazy_units.Event.CaptureOpen(a1)
	move.l #10, lazy_units.EVENT_BYTES*3+lazy_units.Event.PreviousCapture(a1)
	; The preceding malformed-record probe deliberately left invalid lengths.
	; This independent valid owner starts from a fully cleared capture header.
	lea Malformed, a1
	moveq #capture.HEADER_BYTES/4-1, d2
clearCapture
	clr.l (a1)+
	dbra d2, clearCapture
	move.l #capture.MAGIC, Malformed+capture.Record.Magic
	move.l #capture.HEADER_BYTES, Malformed+capture.Record.Bytes
	move.l #1, Malformed+capture.Record.Origin
	move.w #2, Malformed+capture.Record.Line
	lea AuditRegion, a1
	move.l #10+capture.HEADER_BYTES, d0
	jsr lazy_work.captureRegion
	jsr lazy_work.getState
	moveq #22, d0
	tst.l lazy_work.WorkFields.Errors(a0)
	bne.w done
	cmpi.l #lazy_work.BAD_RANGE, lazy_work.WorkFields.Rows+lazy_work.ROW_BYTES+lazy_work.Row.Reasons(a0)
	bne.w done
	lea lazy_work.WorkFields.RangeCounts(a0), a1
	moveq #3, d2
checkRanges
	cmpi.l #1, (a1)+
	bne.w done
	dbra d2, checkRanges
	cmpi.l #10, lazy_work.WorkFields.RangeExamples+28(a0)
	bne.w done
	cmpi.l #10+capture.HEADER_BYTES, lazy_work.WorkFields.RangeExamples+32(a0)
	bne.w done
	; Force one row-owned cost with another global remainder. These observations
	; do not claim that the latter happened outside an eventual reconciled unit.
	move.l #2, lazy_work.WorkFields.Totals+lazy_work.PLAN_BIND(a0)
	move.l #1, lazy_work.WorkFields.Rows+lazy_work.Row.Values+lazy_work.PLAN_BIND(a0)
	move.l #40, lazy_work.WorkFields.Totals+lazy_work.BOUND_BYTES(a0)
	move.l #30, lazy_work.WorkFields.Rows+lazy_work.Row.Values+lazy_work.BOUND_BYTES(a0)
	lea BlockRows, a4
	.WORK_SELECTED a4, #1
	jsr lazy_work.getState
	moveq #23, d0
	tst.l lazy_work.WorkFields.Errors(a0)
	bne.w done
	cmpi.l #1, lazy_work.WorkFields.UnknownRows+lazy_work.PLAN_BIND(a0)
	bne.w done
	cmpi.l #1, lazy_work.WorkFields.Residual+lazy_work.PLAN_BIND(a0)
	bne.w done
	cmpi.l #30, lazy_work.WorkFields.UnknownRows+lazy_work.BOUND_BYTES(a0)
	bne.w done
	cmpi.l #10, lazy_work.WorkFields.Residual+lazy_work.BOUND_BYTES(a0)
	bne.w done
	cmpi.l #1, lazy_work.WorkFields.TopRows+lazy_work.TOP_BYTES+lazy_work.Top.Event(a0)
	bne.w done
	; One complete physical unit spans two independently retained arenas. The
	; close equals the first region endpoint but is below the actual global end.
	jsr lazy_work.clear
	jsr lazy_units.clear
	jsr lazy_units.getState
	move.l #1, lazy_units.Fields.Count(a0)
	lea lazy_units.Fields.Events(a0), a1
	move.l #10, lazy_units.Event.CaptureOpen(a1)
	move.l #10+capture.HEADER_BYTES, lazy_units.Event.CaptureClose(a1)
	move.l #10, lazy_units.Event.OriginalOpen(a1)
	move.l #20, lazy_units.Event.OriginalEnd(a1)
	move.l #10, lazy_units.Event.FinalOpen(a1)
	move.l #1, lazy_units.Event.Origin(a1)
	move.l #2, lazy_units.Event.Line(a1)
	move.l #1, lazy_units.Event.Unit(a1)
	lea AuditRegion, a4
	.WORK_CAPTURE_REGION a4, #10+capture.HEADER_BYTES*2
	lea SecondRegion, a4
	.WORK_CAPTURE_REGION a4, #10+capture.HEADER_BYTES*2
	jsr lazy_work.getState
	moveq #25, d0
	tst.l lazy_work.WorkFields.Errors(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Rows+lazy_work.Row.Reasons(a0)
	bne.w done
	cmpi.l #capture.HEADER_BYTES*2, lazy_work.WorkFields.Rows+lazy_work.Row.Values+lazy_work.CAPTURE_BYTES(a0)
	bne.w done
	cmpi.l #capture.HEADER_BYTES*2, lazy_work.WorkFields.Totals+lazy_work.CAPTURE_BYTES(a0)
	bne.w done
	cmpi.l #2, lazy_work.WorkFields.CaptureRecords(a0)
	bne.w done
	cmpi.l #1, lazy_work.WorkFields.RangeCounts+4(a0)
	bne.w done
	cmpi.l #1, lazy_work.WorkFields.RangeCounts+12(a0)
	bne.w done
	lea BlockRows, a4
	.WORK_SELECTED a4, #1
	jsr lazy_work.getState
	cmpi.l #capture.HEADER_BYTES*2, lazy_work.WorkFields.Unreferenced+lazy_work.CAPTURE_BYTES(a0)
	bne.w done
	tst.l lazy_work.WorkFields.Unknown+lazy_work.CAPTURE_BYTES(a0)
	bne.w done
	jsr lazy_work.clear
	.WORK_PLAN_CONTEXT #1
	.WORK_CONTEXT 0, #3
	moveq #0, d0
	.WORK_BOUND_BEGIN #10
	.WORK_BOUND_END #17
	jsr lazy_work.getState
	moveq #24, d0
	cmpi.l #3, lazy_work.WorkFields.Contexts+8(a0)
	bne.w done
	cmpi.l #7, lazy_work.WorkFields.Contexts+12(a0)
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
SecondRegion	.long 10+capture.HEADER_BYTES, 10+capture.HEADER_BYTES*2, 1, 0
	.long Malformed, capture.HEADER_BYTES, capture.HEADER_BYTES
AuditRegion	.long 10, 10+capture.HEADER_BYTES, 1, 0
	.long Malformed, capture.HEADER_BYTES, capture.HEADER_BYTES
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
