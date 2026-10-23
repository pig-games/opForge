; Isolated passive-observer component checks; no production path selects a test.
; @opforge-owner: opforge.tests.assembler.capture_lifetime_probe
	.module opforge.tests.assembler.capture_lifetime_probe
	.cpu 68020
	.use opforge.debug.capture_lifetime as life
; Synthetic typed record format, independent of production module imports.
Header	.struct
Count	.byte ?
Padding	.res 3
Bytes	.long ?
Lexemes	.long ?
Plans	.long ?
Source	.long ?
Reserved	.long ?
.endstruct
HEADER_BYTES = Header.Reserved+4
Token	.struct
Kind	.word ?
Reserved	.word ?
Start	.long ?
End	.long ?
Offset	.long ?
Length	.long ?
.endstruct
TOKEN_BYTES = Token.Length+4
LIFE_PROBE_CALL	.macro routine
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
LIFE_PROBE_RECORD	.macro record
	lea .record, a0
	lea Packet, a1
	move.l Header.Bytes(a0), d0
	moveq #0, d1
	move.l #256, d2
	.LIFE_PROBE_CALL life.inspect
.endmacro
	.section code, kind=code
	.pub
; No inputs. D0/CCR=status:80 preservation,81 heap/curve,82 records,83invalid.
; Other registers preserved. Every return clears all synthetic observations.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	clr.l Failure
	movem.l Seed, d0-d7/a0-a6
	.LIFE_PROBE_CALL life.clear
	moveq #life.ACQUISITION, d0
	.LIFE_PROBE_CALL life.phase
	.LIFE_PROBE_CALL life.retainedBegin
	move.l #256, d0
	.LIFE_PROBE_CALL life.allocate
	.LIFE_PROBE_CALL life.retainedEnd
	moveq #64, d0
	.LIFE_PROBE_CALL life.allocate
	moveq #life.CONFIGURATION, d0
	.LIFE_PROBE_CALL life.phase
	moveq #0, d0
	moveq #7, d1
	.LIFE_PROBE_CALL life.region
	.LIFE_PROBE_RECORD Narrow
	.LIFE_PROBE_RECORD WideKind
	.LIFE_PROBE_RECORD WideField
	.LIFE_PROBE_RECORD Empty
	jsr life.getState
	moveq #82, d0
	tst.l life.Fields.Errors(a0)
	bne.w done
	cmpi.l #4, life.Fields.Records(a0)
	bne.w done
	cmpi.l #5, life.Fields.Tokens(a0)
	bne.w done
	cmpi.l #204, life.Fields.OriginalBytes(a0)
	bne.w done
	cmpi.l #188, life.Fields.CompactBytes(a0)
	bne.w done
	cmpi.l #2, life.Fields.FallbackRecords(a0)
	bne.w done
	cmpi.l #3, life.Fields.FallbackTokens(a0)
	bne.w done
	cmpi.l #96, life.Fields.HeaderBytes(a0)
	bne.w done
	cmpi.l #3, life.Fields.LexemeBytes(a0)
	bne.w done
	cmpi.l #2, life.Fields.PlanBytes(a0)
	bne.w done
	cmpi.l #HEADER_BYTES, life.Fields.RecordHeaderWidth(a0)
	bne.w done
	cmpi.l #2, life.Fields.Maxima+life.Maxima.Count(a0)
	bne.w done
	cmpi.l #70000, life.Fields.Maxima+life.Maxima.SourceBytes(a0)
	bne.w done
	cmpi.l #256, life.Fields.Maxima+life.Maxima.Kind(a0)
	bne.w done
	cmpi.l #65535, life.Fields.Maxima+life.Maxima.Reserved(a0)
	bne.w done
	cmpi.l #65536, life.Fields.Maxima+life.Maxima.Start(a0)
	bne.w done
	cmpi.l #65535, life.Fields.Maxima+life.Maxima.End(a0)
	bne.w done
	cmpi.l #65535, life.Fields.Maxima+life.Maxima.Offset(a0)
	bne.w done
	cmpi.l #65535, life.Fields.Maxima+life.Maxima.Length(a0)
	bne.w done
	lea life.Fields.Regions(a0), a1
	cmpi.l #7, life.RegionRow.File(a1)
	bne.w done
	cmpi.l #4, life.RegionRow.Records(a1)
	bne.w done
	cmpi.l #5, life.RegionRow.Tokens(a1)
	bne.w done
	cmpi.l #204, life.RegionRow.OriginalUsed(a1)
	bne.w done
	cmpi.l #188, life.RegionRow.CompactUsed(a1)
	bne.w done
	cmpi.l #256, life.RegionRow.ActualCapacity(a1)
	bne.w done
	cmpi.l #2, life.RegionRow.FallbackRecords(a1)
	bne.w done
	cmpi.l #3, life.RegionRow.FallbackTokens(a1)
	bne.w done
	; One synchronized L1 closure precedes a later heap peak. Phase entry
	; retains inherited residency without inventing an allocation event.
	moveq #1, d0
	move.l #128, d1
	move.l #256, d2
	moveq #2, d3
	moveq #1, d4
	.LIFE_PROBE_CALL life.curve
	move.l #128, d0
	.LIFE_PROBE_CALL life.allocate
	moveq #64, d0
	.LIFE_PROBE_CALL life.release
	.LIFE_PROBE_CALL life.retainedBegin
	move.l #256, d0
	.LIFE_PROBE_CALL life.release
	.LIFE_PROBE_CALL life.retainedEnd
	move.l #128, d0
	.LIFE_PROBE_CALL life.release
	jsr life.getState
	moveq #81, d0
	tst.l life.Fields.Errors(a0)
	bne.w done
	tst.l life.Fields.HeapLive(a0)
	bne.w done
	tst.l life.Fields.CaptureLive(a0)
	bne.w done
	tst.l life.Fields.Mode(a0)
	bne.w done
	cmpi.l #448, life.Fields.Allocated(a0)
	bne.w done
	cmpi.l #448, life.Fields.Freed(a0)
	bne.w done
	cmpi.l #3, life.Fields.AllocCalls(a0)
	bne.w done
	cmpi.l #3, life.Fields.FreeCalls(a0)
	bne.w done
	cmpi.l #6, life.Fields.Event(a0)
	bne.w done
	cmpi.l #448, life.Fields.HeapPeak(a0)
	bne.w done
	cmpi.l #256, life.Fields.CapturePeak(a0)
	bne.w done
	cmpi.l #3, life.Fields.HeapPeakRow+life.PeakRow.Event(a0)
	bne.w done
	cmpi.l #128, life.Fields.HeapPeakRow+life.PeakRow.Eligible(a0)
	bne.w done
	cmpi.l #1, life.Fields.HeapPeakRow+life.PeakRow.Span(a0)
	bne.w done
	cmpi.l #1, life.Fields.CapturePeakRow+life.PeakRow.Event(a0)
	bne.w done
	cmpi.l #1, life.Fields.CurveCount(a0)
	bne.w done
	cmpi.l #2, life.Fields.Curves+life.CurveRow.Event(a0)
	bne.w done
	cmpi.l #320, life.Fields.Curves+life.CurveRow.Heap(a0)
	bne.w done
	cmpi.l #128, life.Fields.Curves+life.CurveRow.Eligible(a0)
	bne.w done
	cmpi.l #320, life.Fields.PhasePeaks+life.ACQUISITION*life.PEAK_BYTES+life.PeakRow.Heap(a0)
	bne.w done
	cmpi.l #448, life.Fields.PhasePeaks+life.CONFIGURATION*life.PEAK_BYTES+life.PeakRow.Heap(a0)
	bne.w done
	; Underflow, invalid phase and unbalanced/nested modes remain explicit.
	.LIFE_PROBE_CALL life.clear
	moveq #1, d0
	.LIFE_PROBE_CALL life.release
	.LIFE_PROBE_CALL life.retainedEnd
	.LIFE_PROBE_CALL life.retainedBegin
	.LIFE_PROBE_CALL life.retainedBegin
	.LIFE_PROBE_CALL life.retainedEnd
	moveq #life.PHASE_COUNT, d0
	.LIFE_PROBE_CALL life.phase
	jsr life.getState
	moveq #83, d0
	cmpi.l #life.UNDERFLOW+life.BAD_MODE+life.BOUNDS, life.Fields.Errors(a0)
	bne.w done
	tst.l life.Fields.HeapLive(a0)
	bne.w done
	tst.l life.Fields.Mode(a0)
	bne.w done
	; Saturation does not produce an apparently small heap or event count.
	.LIFE_PROBE_CALL life.clear
	moveq #-1, d0
	.LIFE_PROBE_CALL life.allocate
	moveq #1, d0
	.LIFE_PROBE_CALL life.allocate
	jsr life.getState
	moveq #83, d0
	cmpi.l #life.OVERFLOW, life.Fields.Errors(a0)
	bne.w done
	cmpi.l #-1, life.Fields.HeapLive(a0)
	bne.w done
	cmpi.l #-1, life.Fields.Allocated(a0)
	bne.w done
	; Malformed typed offsets and exact extents cannot be accepted as records.
	.LIFE_PROBE_CALL life.clear
	moveq #0, d0
	moveq #7, d1
	.LIFE_PROBE_CALL life.region
	move.w Packet+life.Layout.KindOffset, d7
	move.w #1, Packet+life.Layout.KindOffset
	.LIFE_PROBE_RECORD Narrow
	move.w d7, Packet+life.Layout.KindOffset
	jsr life.getState
	moveq #83, d0
	cmpi.l #life.BOUNDS, life.Fields.Errors(a0)
	bne.w done
	tst.l life.Fields.Records(a0)
	bne.w done
	lea Narrow, a0
	lea Packet, a1
	move.l #NarrowEnd-Narrow-4, d0
	moveq #0, d1
	move.l #256, d2
	.LIFE_PROBE_CALL life.inspect
	jsr life.getState
	moveq #83, d0
	tst.l life.Fields.Records(a0)
	bne.w done
	; Registered-but-empty regions are retained and bounded; a nonsequential
	; region cannot write past the fixed table.
	.LIFE_PROBE_CALL life.clear
	moveq #1, d0
	moveq #7, d1
	.LIFE_PROBE_CALL life.region
	jsr life.getState
	moveq #83, d0
	cmpi.l #life.BOUNDS, life.Fields.Errors(a0)
	bne.w done
	tst.l life.Fields.RegionCount(a0)
	bne.w done
	move.l Failure, d0
done
	move.l d0, Result
	jsr life.clear
	move.l Result, d0
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
Packet	.word HEADER_BYTES, Header.Count, Header.Bytes, Header.Lexemes
	.word Header.Plans, Header.Source, TOKEN_BYTES, Token.Kind, Token.Reserved
	.word Token.Start, Token.End, Token.Offset, Token.Length
	.align 4
Narrow	.byte 2, 0, 0, 0
	.long NarrowEnd-Narrow, 3, 2, 70000, 0
	.word 255, 65535
	.long 65535, 0, 65535, 1
	.word 1, $8000
	.long 0, 65535, 1, 65535
	.byte 1, 2, 3
	.align 4
	.byte 4, 5
	.align 4
NarrowEnd
WideKind	.byte 2, 0, 0, 0
	.long WideKindEnd-WideKind, 0, 0, 12, 0
	.word 256, 2
	.long 1, 2, 3, 4
	.word 1, 3
	.long 5, 6, 7, 8
WideKindEnd
WideField	.byte 1, 0, 0, 0
	.long WideFieldEnd-WideField, 0, 0, 12, 0
	.word 2, 5
	.long 65536, 0, 0, 0
WideFieldEnd
Empty	.byte 0, 0, 0, 0
	.long EmptyEnd-Empty, 0, 0, 0, 0
EmptyEnd
	.endsection
	.section bss, kind=bss
Before	.res long, 15
After	.res long, 15
AfterFlags	.res word, 1
SavedSp	.res long, 1
Failure	.res long, 1
Result	.res long, 1
	.endsection
	.endmodule
