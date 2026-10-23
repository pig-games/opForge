; Native opForge product composition. Package assets are selected at build time.
	.module main
	.cpu 68020
	.use opforge.cli.entry as cli
	.section entry, kind=code
	.pub
; AmigaDOS executable entry. Tail-call the Shell adapter with the build catalog.
; D0=exit code on return; preserves D2-D7/A2-A6 through cli.start.
start	.block
	lea Catalog, a0
	jmp cli.start
	.bend  ; start
	.endsection
	.section data, kind=data
	.align 4
	.include "packages/catalog.i"
	.endsection
	.output "build/opforge", format=hunk, sections=entry, code, data, bss
	.endmodule
