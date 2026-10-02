; Shell configuration only; packed preparation and execution belong to app.
; @opforge-owner: experimental.amigaos.compact_cli
	.module main
	.cpu 68020
	.use experimental.amigaos.binary_app as app
	.use experimental.amigaos.cli_arguments as args
	.use experimental.amigaos.cli_input as input
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
GET_ARG_STR = -534
PUT_STR = -948
	.section entry, kind=code
	.pub
; Shell entry, D0=AmigaDOS exit code; preserves D2-D7/A2-A6.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	lea DosName, a1
	moveq #36, d0
	movea.l 4.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w unavailable
	move.l d0, DosBase
	movea.l d0, a6
	jsr GET_ARG_STR(a6)
	tst.l d0
	bne.w tail
	move.l #EmptyArgs, d0
tail
	movea.l d0, a0
	lea Arguments, a1
	jsr args.parse
	cmpi.l #args.HELP, d0
	beq.w help
	cmpi.l #args.VERSION, d0
	beq.w version
	tst.l d0
	bne.w badArguments
	; Preserve whether the CLI requested a derived output name before the
	; resolver fills State.Output from the input basename.
	tst.w args.State.OutputKind(a1)
	beq.s outputDefaultReady
	tst.b args.State.Output(a1)
	bne.s outputDefaultReady
	move.w #1, args.State.OutputDefault(a1)
outputDefaultReady
	lea Arguments, a0
	lea Root, a1
	jsr input.resolve
	bne.w badInput
	lea Arguments, a2
	lea Config, a0
	lea args.State.Input(a2), a1
	move.l a1, app.Frame.SourcePath(a0)
	lea args.State.Output(a2), a1
	move.l a1, app.Frame.OutputPath(a0)
	lea args.State.OutputBase(a2), a1
	move.l a1, app.Frame.OutputBase(a0)
	move.w args.State.OutputDefault(a2), app.Frame.OutputDefault(a0)
	move.w #2, app.Frame.Mode(a0)
	move.w args.State.OutputKind(a2), app.Frame.OutputKind(a0)
	bne.w outputConfigured
	move.w #3, app.Frame.OutputKind(a0)
outputConfigured
	move.w args.State.StartSet(a2), app.Frame.StartSet(a0)
	move.l args.State.Start(a2), app.Frame.Start(a0)
	lea args.State.ModulePaths(a2), a1
	move.l a1, app.Frame.ModuleRoots(a0)
	move.l args.State.ModuleCount(a2), app.Frame.ModuleCount(a0)
	lea args.State.IncludePaths(a2), a1
	move.l a1, app.Frame.IncludeRoots(a0)
	move.l args.State.IncludeCount(a2), app.Frame.IncludeCount(a0)
	move.l #Catalog, app.Frame.Catalog(a0)
	move.l Catalog, app.Frame.CatalogBytes(a0)
	tst.b args.State.RuntimePackage(a2)
	beq.w namedCpu
	lea args.State.RuntimePackage(a2), a1
	move.l a1, app.Frame.PackagePath(a0)
	bra.w optional
namedCpu
	tst.b args.State.Cpu(a2)
	beq.w missingTarget
	lea args.State.Cpu(a2), a1
	move.l a1, app.Frame.Cpu(a0)
optional
	tst.b args.State.Dialect(a2)
	beq.w packageRoot
	lea args.State.Dialect(a2), a1
	move.l a1, app.Frame.Dialect(a0)
packageRoot
	move.l #DefaultRoot, app.Frame.PackageRoot(a0)
	tst.b args.State.PackageRoot(a2)
	beq.w execute
	lea args.State.PackageRoot(a2), a1
	move.l a1, app.Frame.PackageRoot(a0)
execute
	bsr.w closeDos
	lea Config, a0
	jsr app.execute
	bra.w done
help
	move.l #UsageText, d1
	bra.w information
version
	move.l #VersionText, d1
information
	movea.l DosBase, a6
	jsr PUT_STR(a6)
	bsr.w closeDos
	moveq #0, d0
	bra.w done
badArguments
	move.l #ArgumentError, d1
	bra.w error
badInput
	move.l #InputError, d1
	bra.w error
missingTarget
	move.l #TargetError, d1
	bra.w error
error
	movea.l DosBase, a6
	jsr PUT_STR(a6)
	bsr.w closeDos
unavailable
	moveq #20, d0
done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.priv
closeDos	.block
	movea.l DosBase, a1
	movea.l 4.w, a6
	jsr CLOSE_LIBRARY(a6)
	rts
	.bend  ; closeDos
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
EmptyArgs	.byte 0
DefaultRoot	.byte "PROGDIR:packages", 0
UsageText	.byte "Usage: opforge_compact [OPTIONS] FILE|DIRECTORY", 10
	.byte "  -i, --infile FILE     Input (directory selects main.asm; default .)", 10
	.byte "      --cpu CPU         Initial target; embedded or external package", 10
	.byte "      --runtime-package FILE  Explicit BS16 runtime package", 10
	.byte "  -b, --bin [FILE]      Flat binary output", 10
	.byte "      --hunk [FILE]     Source-configured Hunk output", 10
	.byte "  -x, --hex [FILE]      Intel HEX output", 10
	.byte "  -s, --srec [FILE]     Motorola S-record output", 10
	.byte "  -g, --go ADDRESS      Record start address (4-8 hex digits)", 10
	.byte "  -M, --module-path DIR Additional module search root (repeatable)", 10
	.byte "  -I, --include-path DIR Additional include search root (repeatable)", 10
	.byte "  -P, --package-path DIR Package directory", 10
	.byte "  -d, --dialect NAME    Package dialect", 10
	.byte "  -h, --help           This help", 10
	.byte "  -V, --version        Build identity", 10
	.byte "Root-file directory is the default module/include search root.", 10
	.byte "Source .output filenames are literal; no request means validation only.", 10, 0
VersionText	.byte "opForge compact native | BS16 | experimental CLI-output checkpoint", 10, 0
ArgumentError	.byte "compact CLI: invalid or unsupported arguments (see --help)", 10, 0
InputError	.byte "compact CLI: invalid input; expected readable .asm file or directory with main.asm (bounded paths)", 10, 0
TargetError	.byte "compact CLI: initial target requires --cpu or --runtime-package", 10, 0
	.align 4
	.include "package_catalog.i"
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
Config	.res byte, app.FRAME_BYTES
Arguments	.res byte, args.STATE_BYTES
Root	.res byte, args.PATH_BYTES
	.endsection
	.output "build/opforge_compact", format=hunk, sections=entry, code, data, bss
	.endmodule
