; Manifest-backed test entry for the shared compact binary-source engine.
	.module main
	.cpu 68020
	.use experimental.amigaos.binary_app as app
	.section entry, kind=code
	.pub
start	.block
	lea Config, a0
	move.l #InputPath, app.Frame.PackagePath(a0)
	clr.l app.Frame.SourcePath(a0)
	move.l #OutputPath, app.Frame.OutputPath(a0)
	clr.w app.Frame.Mode(a0)
	jsr app.execute
	rts
	.bend  ; start
	.endsection
	.section data, kind=data
InputPath	.byte "Work:input.bin", 0
OutputPath	.byte "Work:output.bin", 0
	.endsection
	.section bss, kind=bss
	.align 4
Config	.res byte, app.Frame.Mode+2
	.endsection
	.output "build/binary_source_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
