; Parser request-envelope adaptation for the tkpkg service facade.

	.module tkpkg.amigaos.parse_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use prvm.amigaos.line_router

TKPKG_PARSE_ROUTE_FRAME_SIZE = 116

	.section code, kind=code

; Route one parser request frame through PRVM.
; Inputs: A0 = validated control block whose input window points at a route frame.
; Outputs: D0/D1 = PRVM status/result-count; D2 = 0 accepted, 1 malformed.
; Clobbers: D0-D2/A0-A1/CCR.
; CCR: reflects D2 on return.
parseLineV1	.block
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0
	moveq #0, d1
	move.b 17(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.W), a1
	moveq #0, d0
	move.b abi.CB_INPUT_LEN(a0), d0
	moveq #0, d1
	move.b 19(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #TKPKG_PARSE_ROUTE_FRAME_SIZE, d0
	bne.s badRequest
	movea.l a1, a0
	jsr line_router.prvmRouteLine68000
	moveq #0, d2
	rts

badRequest
	moveq #1, d2
	moveq #0, d0
	moveq #0, d1
	rts
	.bend  ; parseLineV1

	.endsection
	.endmodule
