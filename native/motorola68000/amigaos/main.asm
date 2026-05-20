; Native AmigaOS opForge CLI root composition.

	.module main
	.cpu 68020

	.use opforge.cli.entry (start)
	.use opasm.amigaos.tkpkg_bridge (opasmTkpkgBridgeDispatchEncodeSelectedV1)
	.use opasm.amigaos.tkpkg_bridge (opasmTkpkgBridgeDispatchEvaluateExpressionV1)

	.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
	.endmodule
