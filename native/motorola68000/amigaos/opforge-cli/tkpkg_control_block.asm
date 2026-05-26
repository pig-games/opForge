; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.tkpkg_control_block
	.cpu 68020

	.use tkpkg.amigaos.abi

	.section code, kind=code
	.pub

; Write a CB-relative input window offset/length pair into control block A0.
opforgeNativeCliWriteInputWindow	.block
	move.b d0, abi.CB_INPUT_PTR(a0)
	lsr.w #8, d0
	move.b d0, 17(a0)
	move.b d1, abi.CB_INPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 19(a0)
	rts
	.bend  ; opforgeNativeCliWriteInputWindow

; Write a CB-relative extension window offset/length pair into control block A0.
opforgeNativeCliWriteExtensionWindow	.block
	move.b d0, abi.CB_EXTENSION_PTR(a0)
	lsr.w #8, d0
	move.b d0, 25(a0)
	move.b d1, abi.CB_EXTENSION_LEN(a0)
	lsr.w #8, d1
	move.b d1, 27(a0)
	rts
	.bend  ; opforgeNativeCliWriteExtensionWindow

; Read the tkpkg service status byte from control block A0.
opforgeNativeCliReadStatus	.block
	moveq #0, d0
	move.b abi.CB_STATUS_CODE(a0), d0
	rts
	.bend  ; opforgeNativeCliReadStatus

; Read the tkpkg service output length from control block A0.
opforgeNativeCliReadOutputLen	.block
	moveq #0, d0
	move.b abi.CB_OUTPUT_LEN(a0), d0
	moveq #0, d1
	move.b 23(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts
	.bend  ; opforgeNativeCliReadOutputLen

; Read the tkpkg service last-error length from control block A0.
opforgeNativeCliReadLastErrorLen	.block
	moveq #0, d0
	move.b abi.CB_LAST_ERROR_LEN(a0), d0
	moveq #0, d1
	move.b 31(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts
	.bend  ; opforgeNativeCliReadLastErrorLen

	.endsection
	.endmodule
