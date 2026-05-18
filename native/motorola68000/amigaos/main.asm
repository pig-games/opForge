; Native AmigaOS opForge CLI root composition.

	.module main
	.cpu 68020

	.use opforge.cli.entry (*)

	.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
	.endmodule
