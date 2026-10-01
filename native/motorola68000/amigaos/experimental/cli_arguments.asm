; Bounded Shell argument parsing only; path policy and IO belong to the caller.
; @opforge-owner: experimental.amigaos.cli_arguments
	.module experimental.amigaos.cli_arguments
	.cpu 68020
	.pub
PATH_BYTES = 256
TOKEN_BYTES = 512
MODULE_LIMIT = 8
INCLUDE_LIMIT = 15; one physical slot is reserved for the caller's default
OK = 0
HELP = 1
VERSION = 2
UNKNOWN = 3
UNSUPPORTED = 4
MISSING_VALUE = 5
DUPLICATE = 6
MIXED_INPUT = 7
EXTRA_INPUT = 8
MISSING_INPUT = 9
MALFORMED = 10
CAPACITY = 11
CONFLICT = 12
State	.struct
InputStyle	.word ?  ; 1 positional, 2 explicit -i/--infile
BinRequested	.word ?
OutputKind	.word ?  ; 0 none, 1 binary, 2 Hunk
Status	.word ?
Help	.word ?
Version	.word ?
EndOptions	.word ?
Reserved	.word ?
ModuleCount	.long ?
IncludeCount	.long ?
Input	.res PATH_BYTES
Output	.res PATH_BYTES
Cpu	.res PATH_BYTES
RuntimePackage	.res PATH_BYTES  ; BS14, never a Rust .opasm package
Dialect	.res PATH_BYTES
PackageRoot	.res PATH_BYTES
ModulePaths	.res MODULE_LIMIT*PATH_BYTES
IncludePaths	.res (INCLUDE_LIMIT+1)*PATH_BYTES
Token	.res TOKEN_BYTES
	.endstruct
STATE_BYTES = State.Token+TOKEN_BYTES
	.priv
K_INPUT = 1
K_VALUE = 2
K_MODULE = 3
K_INCLUDE = 4
K_BIN = 5
K_HUNK = 6
K_HELP = 7
K_VERSION = 8
K_UNSUPPORTED = 9
Record	.struct
Short	.word ?
Kind	.word ?
Dest	.word ?
Reserved	.word ?
Name	.long ?
	.endstruct
RECORD_BYTES = Record.Name+4
	.pub
	.section code, kind=code

; Parse a NUL-terminated AmigaDOS argument tail into caller-owned bounded state.
; A0=tail, A1=STATE_BYTES writable bytes (cleared here). D0/CCR=Status enum;
; OK=0, HELP/VERSION are distinct non-error outcomes; errors start at UNKNOWN.
; Other registers preserved. State.Token retains the last decoded token.
; Optional output consumes a following non-option token, as with the Rust CLI.
; Quotes group whitespace; *" and ** decode within quotes. Backslash is literal.
parse	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	move.w #STATE_BYTES/2-1, d1
clear
	clr.w (a1)+
	dbra d1, clear
next
	lea State.Token(a4), a1
	bsr.w token
	cmpi.l #-1, d0
	beq.w complete
	tst.l d0
	bne.w done
	lea State.Token(a4), a0
	tst.w State.EndOptions(a4)
	bne.w positional
	cmpi.b #'-', (a0)
	bne.w positional
	tst.b 1(a0)
	beq.w positional
	suba.l a2, a2  ; null means there is no attached value
	cmpi.b #'-', 1(a0)
	bne.w short
	tst.b 2(a0)
	bne.w long
	move.w #1, State.EndOptions(a4)
	bra.w next
long
	addq.l #2, a0
	movea.l a0, a1
split
	move.b (a1)+, d0
	beq.w findLong
	cmpi.b #'=', d0
	bne.w split
	clr.b -1(a1)
	movea.l a1, a2
findLong
	lea Options, a5
longRow
	movea.l Record.Name(a5), a1
	move.l a1, d0
	beq.w rejectUnknown
	movea.l a0, a6
compare
	move.b (a6)+, d0
	cmp.b (a1)+, d0
	bne.w longNext
	tst.b d0
	bne.w compare
	bra.w selected
longNext
	adda.w #RECORD_BYTES, a5
	bra.w longRow
short
	moveq #0, d0
	move.b 1(a0), d0
	lea Options, a5
shortRow
	tst.l Record.Name(a5)
	beq.w rejectUnknown
	cmp.w Record.Short(a5), d0
	beq.w shortValue
	adda.w #RECORD_BYTES, a5
	bra.w shortRow
shortValue
	tst.b 2(a0)
	beq.w selected
	lea 2(a0), a2
	cmpi.b #'=', (a2)
	bne.w selected
	addq.l #1, a2
selected
	move.w Record.Kind(a5), d2
	cmpi.w #K_UNSUPPORTED, d2
	beq.w rejectUnsupported
	cmpi.w #K_HELP, d2
	beq.w showHelp
	cmpi.w #K_VERSION, d2
	beq.w showVersion
	cmpi.w #K_INPUT, d2
	beq.w infile
	cmpi.w #K_MODULE, d2
	beq.w module
	cmpi.w #K_INCLUDE, d2
	beq.w include
	cmpi.w #K_BIN, d2
	beq.w output
	cmpi.w #K_HUNK, d2
	beq.w output
	movea.l a4, a6
	adda.w Record.Dest(a5), a6
	tst.b (a6)
	bne.w rejectDuplicate
	bsr.w required
	bne.w done
	bra.w next
infile
	cmpi.w #1, State.InputStyle(a4)
	beq.w rejectMixed
	tst.w State.InputStyle(a4)
	bne.w rejectUnsupported  ; repeated -i needs multi-input engine orchestration
	lea State.Input(a4), a6
	bsr.w required
	bne.w done
	move.w #2, State.InputStyle(a4)
	bra.w next
module
	move.l State.ModuleCount(a4), d5
	cmpi.l #MODULE_LIMIT, d5
	bhs.w rejectCapacity
	lea State.ModulePaths(a4), a6
	bra.w root
include
	move.l State.IncludeCount(a4), d5
	cmpi.l #INCLUDE_LIMIT, d5
	bhs.w rejectCapacity
	lea State.IncludePaths(a4), a6
root
	lsl.l #8, d5
	adda.l d5, a6
	bsr.w required
	bne.w done
	cmpi.w #K_MODULE, d2
	bne.w includeAdded
	addq.l #1, State.ModuleCount(a4)
	bra.w next
includeAdded
	addq.l #1, State.IncludeCount(a4)
	bra.w next
output
	moveq #1, d5
	cmpi.w #K_BIN, d2
	beq.w outputKind
	moveq #2, d5
outputKind
	tst.w State.OutputKind(a4)
	beq.w firstOutput
	cmp.w State.OutputKind(a4), d5
	bne.w rejectConflict
	bra.w rejectUnsupported  ; repeatable binary artifacts are outside this slice
firstOutput
	move.w d5, State.OutputKind(a4)
	cmpi.w #1, d5
	bne.w outputValue
	move.w #1, State.BinRequested(a4)
outputValue
	lea State.Output(a4), a6
	move.l a2, d0
	bne.w copyOutput
	bsr.w space
	tst.b (a3)
	beq.w next
	cmpi.b #'-', (a3)
	beq.w next
	lea State.Token(a4), a1
	bsr.w token
	bne.w done
	lea State.Token(a4), a2
copyOutput
	cmpi.w #K_BIN, d2
	bne.w copyPath
	bsr.w range
	bne.w done
copyPath
	bsr.w copy
	bne.w done
	bra.w next
positional
	cmpi.w #2, State.InputStyle(a4)
	beq.w rejectMixed
	tst.w State.InputStyle(a4)
	bne.w rejectExtra
	lea State.Input(a4), a6
	movea.l a0, a2
	tst.b (a2)
	beq.w rejectMissing
	bsr.w copy
	bne.w done
	move.w #1, State.InputStyle(a4)
	bra.w next
showHelp
	move.l a2, d0
	bne.w rejectMalformed
	move.w #1, State.Help(a4)
	moveq #HELP, d0
	bra.w done
showVersion
	move.l a2, d0
	bne.w rejectMalformed
	move.w #1, State.Version(a4)
	moveq #VERSION, d0
	bra.w done
complete
	tst.w State.InputStyle(a4)
	bne.w inputReady
	move.w #1, State.InputStyle(a4)
	move.b #'.', State.Input(a4)
inputReady
	tst.b State.RuntimePackage(a4)
	beq.w configured
	tst.b State.Cpu(a4)
	bne.w rejectConflict
	tst.b State.Dialect(a4)
	bne.w rejectConflict
	tst.b State.PackageRoot(a4)
	bne.w rejectConflict
configured
	moveq #OK, d0
	bra.w done
rejectUnknown
	moveq #UNKNOWN, d0
	bra.w done
rejectUnsupported
	moveq #UNSUPPORTED, d0
	bra.w done
rejectDuplicate
	moveq #DUPLICATE, d0
	bra.w done
rejectMixed
	moveq #MIXED_INPUT, d0
	bra.w done
rejectExtra
	moveq #EXTRA_INPUT, d0
	bra.w done
rejectMissing
	moveq #MISSING_VALUE, d0
	bra.w done
rejectMalformed
	moveq #MALFORMED, d0
	bra.w done
rejectCapacity
	moveq #CAPACITY, d0
	bra.w done
rejectConflict
	moveq #CONFLICT, d0
done
	move.w d0, State.Status(a4)
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; parse
	.priv

; A2=attached value or null, A3=tail, A6=path destination, A4=state.
; D0/CCR=status. Advances A2/A3/A6; clobbers D1/D3/D4/A1.
required	.block
	move.l a2, d0
	bne.w have
	bsr.w space
	tst.b (a3)
	beq.w rejectMissing
	cmpi.b #'-', (a3)
	beq.w rejectMissing
	lea State.Token(a4), a1
	bsr.w token
	bne.w done
	lea State.Token(a4), a2
have
	tst.b (a2)
	beq.w rejectMissing
	bsr.w copy
	rts
rejectMissing
	moveq #MISSING_VALUE, d0
done
	rts
	.bend  ; required

; Reject the Rust binary-range suffix, not ordinary Amiga volume colons.
; A2=NUL output value. D0/CCR=OK or UNSUPPORTED; clobbers D1/D4/A0/A1/A5.
; A range has two final 4..8-digit hexadecimal groups, optionally after FILE:.
range	.block
	movea.l a2, a0  ; start of the penultimate colon-delimited group
	suba.l a1, a1  ; start of the final group, null until a colon is seen
	movea.l a2, a5
scan
	move.b (a5)+, d0
	beq.w scanned
	cmpi.b #':', d0
	bne.w scan
	move.l a1, d0
	beq.w first
	movea.l a1, a0
first
	movea.l a5, a1
	bra.w scan
scanned
	move.l a1, d0
	beq.w plain
	movea.l a0, a5
	moveq #':', d4
	bsr.w hexGroup
	bne.w plain
	movea.l a1, a5
	moveq #0, d4
	bsr.w hexGroup
	bne.w plain
	moveq #UNSUPPORTED, d0
	rts
plain
	moveq #OK, d0
	rts
	.bend  ; range

; A5=group,D4=required delimiter (colon or NUL). D0/CCR=0 iff 4..8 hex
; digits precede that delimiter. Clobbers D1/A5; the delimiter is consumed.
hexGroup	.block
	moveq #0, d1
next
	move.b (a5)+, d0
	cmp.b d4, d0
	beq.w end
	cmpi.b #'0', d0
	blo.w bad
	cmpi.b #'9', d0
	bls.w digit
	cmpi.b #'A', d0
	blo.w bad
	cmpi.b #'F', d0
	bls.w digit
	cmpi.b #'a', d0
	blo.w bad
	cmpi.b #'f', d0
	bhi.w bad
digit
	addq.w #1, d1
	cmpi.w #8, d1
	bhi.w bad
	bra.w next
end
	cmpi.w #4, d1
	blo.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; hexGroup

; A2=NUL string,A6=256-byte path. D0/CCR=status; D1/A2/A6 advanced.
copy	.block
	move.w #PATH_BYTES-1, d1
loop
	move.b (a2)+, d0
	beq.w end
	tst.w d1
	beq.w overflow
	move.b d0, (a6)+
	subq.w #1, d1
	bra.w loop
end
	clr.b (a6)
	moveq #OK, d0
	rts
overflow
	clr.b (a6)
	moveq #CAPACITY, d0
	rts
	.bend  ; copy

; Decode one bounded AmigaDOS token. A3=tail,A1=TOKEN_BYTES destination.
; D0/CCR=OK, -1=end, MALFORMED=unclosed quote, CAPACITY=overflow.
; Advances A1/A3; clobbers D1/D3/D4. Empty quoted tokens remain valid tokens.
token	.block
	bsr.w space
	tst.b (a3)
	beq.w empty
	move.w #TOKEN_BYTES-1, d1
	moveq #0, d3  ; quote state
loop
	moveq #0, d0
	move.b (a3), d0
	beq.w end
	cmpi.b #'"', d0
	beq.w quote
	tst.w d3
	beq.w outside
	cmpi.b #'*', d0
	bne.w append
	move.b 1(a3), d4
	cmpi.b #'"', d4
	beq.w escaped
	cmpi.b #'*', d4
	bne.w append
escaped
	addq.l #1, a3
	move.b d4, d0
	bra.w append
outside
	cmpi.b #' ', d0
	beq.w end
	cmpi.b #9, d0
	beq.w end
	cmpi.b #10, d0
	beq.w end
	cmpi.b #13, d0
	beq.w end
append
	tst.w d1
	beq.w overflow
	move.b d0, (a1)+
	addq.l #1, a3
	subq.w #1, d1
	bra.w loop
quote
	eori.w #1, d3
	addq.l #1, a3
	bra.w loop
end
	clr.b (a1)
	tst.w d3
	bne.w rejectMalformed
	moveq #OK, d0
	rts
empty
	clr.b (a1)
	moveq #-1, d0
	rts
rejectMalformed
	moveq #MALFORMED, d0
	rts
overflow
	clr.b (a1)
	moveq #CAPACITY, d0
	rts
	.bend  ; token

; A3=tail cursor; advance over AmigaDOS whitespace. D0/CCR clobbered.
space	.block
loop
	move.b (a3), d0
	cmpi.b #' ', d0
	beq.w advance
	cmpi.b #9, d0
	beq.w advance
	cmpi.b #10, d0
	beq.w advance
	cmpi.b #13, d0
	bne.w done
advance
	addq.l #1, a3
	bra.w loop
done
	rts
	.bend  ; space
	.endsection
	.section data, kind=data
; Declarative option records share long and attached-short dispatch.
Options
	.word 'i', K_INPUT, State.Input, 0
	.long Name0
	.word 0, K_VALUE, State.Cpu, 0
	.long Name1
	.word 'I', K_INCLUDE, 0, 0
	.long Name2
	.word 'M', K_MODULE, 0, 0
	.long Name3
	.word 'b', K_BIN, 0, 0
	.long Name4
	.word 0, K_HUNK, 0, 0
	.long Name5
	.word 'h', K_HELP, 0, 0
	.long Name6
	.word 'V', K_VERSION, 0, 0
	.long Name7
	.word 0, K_VALUE, State.RuntimePackage, 0
	.long Name8
	.word 'P', K_VALUE, State.PackageRoot, 0
	.long Name9
	.word 'd', K_VALUE, State.Dialect, 0
	.long Name10
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name11
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name12
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name13
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name14
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name15
	.word 'q', K_UNSUPPORTED, 0, 0
	.long Name16
	.word 'E', K_UNSUPPORTED, 0, 0
	.long Name17
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name18
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name19
	.word 'w', K_UNSUPPORTED, 0, 0
	.long Name20
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name21
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name22
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name23
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name24
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name25
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name26
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name27
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name28
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name29
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name30
	.word 'l', K_UNSUPPORTED, 0, 0
	.long Name31
	.word 'x', K_UNSUPPORTED, 0, 0
	.long Name32
	.word 's', K_UNSUPPORTED, 0, 0
	.long Name33
	.word 'o', K_UNSUPPORTED, 0, 0
	.long Name34
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name35
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name36
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name37
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name38
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name39
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name40
	.word 'f', K_UNSUPPORTED, 0, 0
	.long Name41
	.word 'g', K_UNSUPPORTED, 0, 0
	.long Name42
	.word 'c', K_UNSUPPORTED, 0, 0
	.long Name43
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name44
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name45
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name46
	.word 'D', K_UNSUPPORTED, 0, 0
	.long Name47
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name48
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name49
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name50
	.word 0, K_UNSUPPORTED, 0, 0
	.long Name51
	.word 0, 0, 0, 0
	.long 0
Name0	.byte "infile", 0
Name1	.byte "cpu", 0
Name2	.byte "include-path", 0
Name3	.byte "module-path", 0
Name4	.byte "bin", 0
Name5	.byte "hunk", 0
Name6	.byte "help", 0
Name7	.byte "version", 0
Name8	.byte "runtime-package", 0
Name9	.byte "package-path", 0
Name10	.byte "dialect", 0
Name11	.byte "format", 0
Name12	.byte "diagnostics-style", 0
Name13	.byte "fixits-dry-run", 0
Name14	.byte "apply-fixits", 0
Name15	.byte "fixits-output", 0
Name16	.byte "quiet", 0
Name17	.byte "error", 0
Name18	.byte "error-append", 0
Name19	.byte "no-error", 0
Name20	.byte "no-warn", 0
Name21	.byte "Wall", 0
Name22	.byte "Werror", 0
Name23	.byte "print-capabilities", 0
Name24	.byte "print-cpusupport", 0
Name25	.byte "fmt", 0
Name26	.byte "fmt-check", 0
Name27	.byte "fmt-write", 0
Name28	.byte "fmt-stdout", 0
Name29	.byte "fmt-config", 0
Name30	.byte "opasm-package", 0
Name31	.byte "list", 0
Name32	.byte "hex", 0
Name33	.byte "srec", 0
Name34	.byte "outfile", 0
Name35	.byte "dependencies", 0
Name36	.byte "labels", 0
Name37	.byte "vice-labels", 0
Name38	.byte "ctags-labels", 0
Name39	.byte "dependencies-append", 0
Name40	.byte "make-phony", 0
Name41	.byte "fill", 0
Name42	.byte "go", 0
Name43	.byte "cond-debug", 0
Name44	.byte "line-numbers", 0
Name45	.byte "tab-size", 0
Name46	.byte "verbose-list", 0
Name47	.byte "define", 0
Name48	.byte "pp-macro-depth", 0
Name49	.byte "max-loop-iterations", 0
Name50	.byte "input-asm-ext", 0
Name51	.byte "input-inc-ext", 0
	.endsection
	.endmodule
