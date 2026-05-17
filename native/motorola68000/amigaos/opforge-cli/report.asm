; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.report
	.cpu 68020

	.use opforge.cli.state (NativeCliSourceLineNum, NativeCliCurrentPath)
	.use opforge.cli.state (NativeCliIncludeDepth)
	.use opforge.cli.dos (opforgeNativeCliPutStr)
	.use opforge.cli.copy (*)
	.use opforge.cli.text_output (opforgeNativeCliPutDecU16)
	.use opforge.cli.strings (IncludeLineText, SpaceText, NewlineText)

	.section code, kind=code
	.pub

opforgeNativeCliEmitIncludeLineRecord .block
	movem.l d0-d1, -(sp)
	move.l #IncludeLineText, d1
	jsr opforgeNativeCliPutStr
	move.w NativeCliIncludeDepth, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #SpaceText, d1
	jsr opforgeNativeCliPutStr
	move.l NativeCliSourceLineNum, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #SpaceText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliCurrentPath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d1
	rts
	.bend ; opforgeNativeCliEmitIncludeLineRecord

	.endsection
	.endmodule
