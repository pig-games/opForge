; Typed capture-record inspection. No token/source/capture module import and no
; borrowed address retained. The hypothetical narrow budget is observational.
Scan	.struct
Bytes	.long ?
Capacity	.long ?
Count	.long ?
Lexemes	.long ?
Plans	.long ?
Source	.long ?
Header	.long ?
Compact	.long ?
Fallback	.long ?
.endstruct
SCAN_BYTES = Scan.Fallback+4
LIFE_MAX	.macro field, value
	move.l .value, d0
	lea Fields.Maxima+.field(a6), a0
	bsr.w maxScalar
.endmacro
LIFE_TOKEN_WORD	.macro layoutField, maximum, narrowLimit
	moveq #0, d1
	move.w .layoutField(a5), d1
	moveq #0, d0
	move.w 0(a2, d1.l), d0
	lea Fields.Maxima+.maximum(a6), a0
	bsr.w maxScalar
	cmpi.l #.narrowLimit, d0
	bls.w fits
	move.l #1, Scan.Fallback(sp)
fits
.endmacro
LIFE_TOKEN_LONG	.macro layoutField, maximum
	moveq #0, d1
	move.w .layoutField(a5), d1
	move.l 0(a2, d1.l), d0
	lea Fields.Maxima+.maximum(a6), a0
	bsr.w maxScalar
	cmpi.l #65535, d0
	bls.w fits
	move.l #1, Scan.Fallback(sp)
fits
.endmacro
	.pub
; A0=retained record,A1=Layout packet,D0=exact bytes,D1=registered region,
; D2=current retained arena capacity. Preserves every register, CCR and SP.
inspect	.block
	.LIFE_SAVE
	suba.w #SCAN_BYTES, sp
	movea.l a0, a4
	movea.l a1, a5
	lea State, a6
	move.l d0, Scan.Bytes(sp)
	move.l d2, Scan.Capacity(sp)
	clr.l Scan.Fallback(sp)
	cmpa.w #0, a4
	beq.w invalid
	cmpa.w #0, a5
	beq.w invalid
	move.l a4, d3
	move.l a5, d4
	or.l d4, d3
	btst #0, d3
	bne.w invalid
	cmp.l Fields.RegionCount(a6), d1
	bhs.w invalid
	cmpi.l #REGION_LIMIT, d1
	bhs.w invalid
	mulu.w #REGION_BYTES, d1
	lea Fields.Regions(a6), a3
	adda.l d1, a3
	moveq #0, d7
	move.w Layout.HeaderBytes(a5), d7
	cmpi.l #4, d7
	blo.w invalid
	cmp.l Scan.Bytes(sp), d7
	bhi.w invalid
	move.l d7, d0
	or.l Scan.Bytes(sp), d0
	andi.l #3, d0
	bne.w invalid
	move.l d7, Scan.Header(sp)
	cmpi.w #WIDE_TOKEN_BYTES, Layout.TokenBytes(a5)
	bne.w invalid
	bsr.w validateLayout
	tst.l d0
	bne.w invalid
	moveq #0, d0
	move.w Layout.BytesOffset(a5), d0
	move.l 0(a4, d0.l), d1
	cmp.l Scan.Bytes(sp), d1
	bne.w invalid
	moveq #0, d0
	move.w Layout.CountOffset(a5), d0
	moveq #0, d1
	move.b 0(a4, d0.l), d1
	cmpi.l #TOKEN_LIMIT, d1
	bhi.w invalid
	move.l d1, Scan.Count(sp)
	moveq #0, d0
	move.w Layout.LexemesOffset(a5), d0
	move.l 0(a4, d0.l), Scan.Lexemes(sp)
	moveq #0, d0
	move.w Layout.PlansOffset(a5), d0
	move.l 0(a4, d0.l), Scan.Plans(sp)
	moveq #0, d0
	move.w Layout.SourceOffset(a5), d0
	move.l 0(a4, d0.l), Scan.Source(sp)
	; Exact nested alignment mirrors only the caller-supplied byte contract.
	move.l Scan.Count(sp), d0
	mulu.w #WIDE_TOKEN_BYTES, d0
	add.l Scan.Header(sp), d0
	bcs.w extentOverflow
	add.l Scan.Lexemes(sp), d0
	bcs.w extentOverflow
	addq.l #3, d0
	bcs.w extentOverflow
	andi.l #$fffffffc, d0
	add.l Scan.Plans(sp), d0
	bcs.w extentOverflow
	addq.l #3, d0
	bcs.w extentOverflow
	andi.l #$fffffffc, d0
	cmp.l Scan.Bytes(sp), d0
	bne.w invalid
	move.l RegionRow.OriginalUsed(a3), d0
	add.l Scan.Bytes(sp), d0
	bcs.w extentOverflow
	cmp.l Scan.Capacity(sp), d0
	bhi.w invalid
	move.l RegionRow.ActualCapacity(a3), d0
	cmp.l Scan.Capacity(sp), d0
	bhi.w invalid
	move.l Scan.Header(sp), d0
	tst.l Fields.RecordHeaderWidth(a6)
	beq.w width
	cmp.l Fields.RecordHeaderWidth(a6), d0
	bne.w invalid
width
	move.l d0, Fields.RecordHeaderWidth(a6)
	.LIFE_MAX Maxima.Count, Scan.Count(sp)
	.LIFE_MAX Maxima.SourceBytes, Scan.Source(sp)
	.LIFE_MAX Maxima.LexemeBytes, Scan.Lexemes(sp)
	movea.l a4, a2
	adda.l Scan.Header(sp), a2
	move.l Scan.Count(sp), d7
nextToken
	tst.l d7
	beq.w candidate
	.LIFE_TOKEN_WORD Layout.KindOffset, Maxima.Kind, 255
	; Reserved remains a full u16 in the hypothetical shape.
	.LIFE_TOKEN_WORD Layout.ReservedOffset, Maxima.Reserved, 65535
	.LIFE_TOKEN_LONG Layout.StartOffset, Maxima.Start
	.LIFE_TOKEN_LONG Layout.EndOffset, Maxima.End
	.LIFE_TOKEN_LONG Layout.OffsetOffset, Maxima.Offset
	.LIFE_TOKEN_LONG Layout.LengthOffset, Maxima.Length
	adda.w #WIDE_TOKEN_BYTES, a2
	subq.l #1, d7
	bra.w nextToken
candidate
	move.l Scan.Bytes(sp), d0
	tst.l Scan.Fallback(sp)
	bne.w compactReady
	move.l Scan.Count(sp), d1
	lsl.l #3, d1
	sub.l d1, d0
compactReady
	move.l d0, Scan.Compact(sp)
	move.l Scan.Capacity(sp), RegionRow.ActualCapacity(a3)
	.LIFE_ADD Fields.Records, #1
	.LIFE_ADD Fields.Tokens, Scan.Count(sp)
	.LIFE_ADD Fields.OriginalBytes, Scan.Bytes(sp)
	.LIFE_ADD Fields.CompactBytes, Scan.Compact(sp)
	.LIFE_ADD Fields.HeaderBytes, Scan.Header(sp)
	.LIFE_ADD Fields.LexemeBytes, Scan.Lexemes(sp)
	.LIFE_ADD Fields.PlanBytes, Scan.Plans(sp)
	moveq #1, d0
	lea RegionRow.Records(a3), a0
	bsr.w addScalar
	move.l Scan.Count(sp), d0
	lea RegionRow.Tokens(a3), a0
	bsr.w addScalar
	move.l Scan.Bytes(sp), d0
	lea RegionRow.OriginalUsed(a3), a0
	bsr.w addScalar
	move.l Scan.Compact(sp), d0
	lea RegionRow.CompactUsed(a3), a0
	bsr.w addScalar
	tst.l Scan.Fallback(sp)
	beq.w done
	.LIFE_ADD Fields.FallbackRecords, #1
	.LIFE_ADD Fields.FallbackTokens, Scan.Count(sp)
	moveq #1, d0
	lea RegionRow.FallbackRecords(a3), a0
	bsr.w addScalar
	move.l Scan.Count(sp), d0
	lea RegionRow.FallbackTokens(a3), a0
	bsr.w addScalar
	bra.w done
invalid
	ori.l #BOUNDS, Fields.Errors(a6)
	bra.w done
extentOverflow
	ori.l #OVERFLOW, Fields.Errors(a6)
done
	adda.w #SCAN_BYTES, sp
	.LIFE_RESTORE
	rts
	.bend  ; inspect
	.priv
; A0=maximum,D0=observed value; D0 preserved, CCR unspecified.
maxScalar	.block
	cmp.l (a0), d0
	bls.w done
	move.l d0, (a0)
done
	rts
	.bend  ; maxScalar
; A5=Layout,D7=header bytes. D0=status (zero valid), clobbers D1-D6/A0-A1.
; Header fields must not overlap and typed words/longs must be even aligned.
; Token fields partition all 20 bytes without depending on their field order.
validateLayout	.block
	lea Layout.CountOffset(a5), a0
	moveq #0, d6
headerField
	moveq #0, d2
	move.w 0(a0, d6.l), d2
	moveq #4, d3
	tst.l d6
	bne.w aligned
	moveq #1, d3
	bra.w extent
aligned
	btst #0, d2
	bne.w invalid
extent
	move.l d2, d4
	add.l d3, d4
	cmp.l d7, d4
	bhi.w invalid
	moveq #0, d5
prior
	cmp.l d6, d5
	bhs.w nextHeader
	moveq #0, d0
	move.w 0(a0, d5.l), d0
	moveq #4, d1
	tst.l d5
	bne.w priorWidth
	moveq #1, d1
priorWidth
	cmp.l d4, d0
	bhs.w nextPrior
	add.l d0, d1
	cmp.l d1, d2
	blo.w invalid
nextPrior
	addq.l #2, d5
	bra.w prior
nextHeader
	addq.l #2, d6
	cmpi.l #Layout.TokenBytes-Layout.CountOffset, d6
	blo.w headerField
	lea Layout.KindOffset(a5), a0
	moveq #0, d6
	moveq #0, d5
tokenField
	moveq #0, d0
	move.w 0(a0, d5.l), d0
	btst #0, d0
	bne.w invalid
	moveq #2, d1
	cmpi.l #Layout.StartOffset-Layout.KindOffset, d5
	blo.w tokenWidth
	moveq #4, d1
tokenWidth
	move.l d0, d2
	add.l d1, d2
	cmpi.l #WIDE_TOKEN_BYTES, d2
	bhi.w invalid
byte
	bset d0, d6
	bne.w invalid
	addq.l #1, d0
	subq.l #1, d1
	bne.w byte
	addq.l #2, d5
	cmpi.l #LAYOUT_BYTES-Layout.KindOffset, d5
	blo.w tokenField
	moveq #0, d0
	rts
invalid
	moveq #1, d0
	rts
	.bend  ; validateLayout
