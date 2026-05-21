; Native AmigaOS opForge CLI root composition.

	.module main
	.cpu 68020

	.use opforge.cli.entry (start)
	.use opasm.amigaos.assembly_driver as driver
	.use opasm.amigaos.tkpkg_bridge as tkpkg

	.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
	.endmodule
