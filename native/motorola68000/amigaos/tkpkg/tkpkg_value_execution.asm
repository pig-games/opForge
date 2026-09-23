; Stateless execution of package-owned VALUE_VM programs.
; @opforge-owner: tkpkg.amigaos.value_execution

	.module tkpkg.amigaos.value_execution
	.cpu 68020
	.include "telemetry_macros.i"

	.section code, kind=code
	.pub

; Direct Rust VALUE_VM v1/v2 port over the native signed-32 scalar transport.
; Inputs: D0 = opcode version; A1/D1 = program bytes; D3 = input zero.
; Outputs: D0 = 0 success, 1 malformed, or 2 constraint violation; D3 = value.
; Clobbers D1-D2/D4-D7/A0/A3; preserves A1-A2/A4-A6. CCR reflects D0.
execute	.block
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_VALUE
	movea.l a1, a0
	move.w d1, d7
	movea.l d0, a3
	cmpi.w #1, d0
	beq.s versionReady
	cmpi.w #2, d0
	bne.w fail
versionReady
	moveq #0, d6
loop
	tst.w d7
	beq.w fail
	moveq #0, d0
	move.b (a0)+, d0
	subq.w #1, d7
	cmpi.b #$FF, d0
	beq.w end
	cmpi.b #$01, d0
	beq.s pushLiteral
	cmpi.b #$02, d0
	beq.s pushInput
	cmpi.b #$03, d0
	beq.s normalize
	cmpi.b #$04, d0
	beq.w requireSignedBits
	cmpi.b #$05, d0
	beq.w requireUnsignedBits
	cmpi.b #$06, d0
	beq.w requireRange
	cmpi.b #$07, d0
	beq.w encodeUpperBoundAsZero
	bra.w fail

pushLiteral
	tst.b d6
	bne.w fail
	cmpi.w #8, d7
	bcs.w fail
	bsr.w readI64Le
	subi.w #8, d7
	moveq #0, d2
	tst.l d1
	bpl.w valueLiteralHighReady
	moveq #-1, d2
valueLiteralHighReady
	cmp.l d2, d0
	bne.w fail
	move.l d1, d3
	moveq #1, d6
	bra.w loop

pushInput
	tst.b d6
	bne.w fail
	tst.w d7
	beq.w fail
	tst.b (a0)+
	bne.w fail
	subq.w #1, d7
	moveq #1, d6
	bra.w loop
normalize
	tst.b d6
	beq.w fail
	tst.w d7
	beq.w fail
	moveq #0, d2
	move.b (a0)+, d2
	subq.w #1, d7
	tst.b d2
	beq.w fail
	cmpi.b #64, d2
	bhi.w fail
	cmpi.b #32, d2
	bhs.w loop
	moveq #1, d4
	subq.b #1, d2
	lsl.l d2, d4
	move.l d4, d5
	subq.l #1, d5
	cmp.l d5, d3
	ble.w loop
	addq.b #1, d2
	add.l d4, d4
	cmpi.b #31, d2
	beq.s normalizeApply
	cmp.l d4, d3
	bge.w loop
normalizeApply
	sub.l d4, d3
	bra.w loop

requireSignedBits
	tst.b d6
	beq.w fail
	tst.w d7
	beq.w fail
	moveq #0, d2
	move.b (a0)+, d2
	subq.w #1, d7
	tst.b d2
	beq.w fail
	cmpi.b #64, d2
	bhi.w fail
	cmpi.b #32, d2
	bhs.w loop
	moveq #1, d4
	subq.b #1, d2
	lsl.l d2, d4
	move.l d4, d0
	neg.l d4
	subq.l #1, d0
	cmp.l d4, d3
	blt.w constraintFail
	cmp.l d0, d3
	bgt.w constraintFail
	bra.w loop

requireUnsignedBits
	tst.b d6
	beq.w fail
	tst.w d7
	beq.w fail
	moveq #0, d2
	move.b (a0)+, d2
	subq.w #1, d7
	tst.b d2
	beq.w fail
	cmpi.b #64, d2
	bhi.w fail
	tst.l d3
	bmi.w constraintFail
	cmpi.b #31, d2
	bhs.w loop
	moveq #1, d4
	lsl.l d2, d4
	subq.l #1, d4
	cmp.l d4, d3
	bhi.w constraintFail
	bra.w loop

requireRange
	tst.b d6
	beq.w fail
	cmpi.w #16, d7
	bcs.w fail
	bsr.w readI64Le
	move.l d0, d4
	move.l d1, d5
	bsr.w readI64Le
	subi.w #16, d7
	; Reject a malformed minimum greater than the maximum, matching Rust's
	; inclusive-range program validation/execution contract.
	cmp.l d4, d0
	blt.w fail
	bgt.s rangeBoundsReady
	cmp.l d5, d1
	bcs.w fail

rangeBoundsReady
	; The native expression transport is one signed 32-bit scalar.  Compare its
	; sign-extended high/low pair against the package's signed i64 bounds.
	moveq #0, d2
	tst.l d3
	bpl.s rangeHighReady
	moveq #-1, d2

rangeHighReady
	cmp.l d4, d2
	blt.w constraintFail
	bgt.s rangeMinOk
	cmp.l d5, d3
	bcs.w constraintFail

rangeMinOk
	cmp.l d0, d2
	bgt.w constraintFail
	blt.w loop
	cmp.l d1, d3
	bhi.w constraintFail
	bra.w loop

encodeUpperBoundAsZero
	move.l a3, d1
	cmpi.w #2, d1
	bne.w fail
	tst.b d6
	beq.w fail
	tst.w d7
	beq.w fail
	moveq #0, d2
	move.b (a0)+, d2
	subq.w #1, d7
	tst.b d2
	beq.w fail
	cmpi.b #62, d2
	bhi.w fail
	tst.l d3
	ble.w constraintFail
	cmpi.b #31, d2
	bhs.w loop
	moveq #1, d4
	lsl.l d2, d4
	cmp.l d4, d3
	bhi.w constraintFail
	bne.w loop
	moveq #0, d3
	bra.w loop

end
	tst.w d7
	bne.w fail
	tst.b d6
	beq.w fail
	moveq #0, d0
	bra.s return
constraintFail
	moveq #2, d0
	bra.s return
fail
	moveq #1, d0
return
	.TELEMETRY_SERVICE_LEAVE
	tst.l d0
	rts
	.bend  ; execute

; Read one package i64 stored little-endian.  The split result preserves all
; bits for signed high-word plus unsigned low-word comparisons on 68020.
; Inputs: A0 = eight-byte value. Outputs: D0 = high 32, D1 = low 32, A0 += 8.
; Clobbers: CCR.
readI64Le	.block
	move.l (a0)+, d1
	ror.w #8, d1
	swap d1
	ror.w #8, d1
	move.l (a0)+, d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	rts
	.bend  ; readI64Le

	.endsection
	.endmodule
