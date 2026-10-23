; Scope storage component observation; no source parser or package is involved.
	.module opforge.tests.assembler.scope_arena_harness
	.cpu 68020
	.use opforge.assembler.symbols.scopes as scopes
	.use opforge.assembler.symbols.scope_layout as layout
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
SOURCE_BASE = 1000
NAME_BYTES = 128
NAME_COUNT = 10000
IDENTITY_MISMATCH = 2
OBS_STATUS = 0
OBS_COUNT = 4
OBS_ARENA_USED = 8
OBS_BYTES = 12
	.section entry, kind=code
	.pub
; Shell entry: D0=exit status; preserves D2-D7/A2-A6. CCR reflects D0.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	lea ScopeState, a0
	move.l #SOURCE_BASE, d0
	moveq #0, d1
	jsr scopes.begin
	tst.l d0
	bne.w observed
	moveq #0, d6
bindNext
	bsr.w spell
	lea Name, a0
	move.l #NAME_BYTES, d0
	lea ScopeState, a1
	jsr scopes.bindOrigin
	tst.l d0
	bne.w observed
	move.l d6, d2
	add.l #SOURCE_BASE, d2
	cmp.l d2, d1
	bne.w identityBad
	addq.l #1, d6
	move.l d6, Observation+OBS_COUNT
	cmpi.l #NAME_COUNT, d6
	bcs.w bindNext
	; Both ends must retain their original IDs after arena/table growth.
	moveq #0, d6
	bsr.w checkIdentity
	tst.l d0
	bne.w observed
	move.l #NAME_COUNT-1, d6
	bsr.w checkIdentity
	bra.w observed
identityBad
	moveq #IDENTITY_MISMATCH, d0
observed
	move.l d0, Observation+OBS_STATUS
	lea ScopeState, a0
	move.l layout.State.ArenaUsed(a0), Observation+OBS_ARENA_USED
	jsr scopes.release
	lea DosName, a1
	moveq #36, d0
	movea.l 4.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	tst.l d0
	beq.w unavailable
	movea.l d0, a5
	movea.l a5, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	tst.l d0
	beq.w failed
	move.l d0, d4
	move.l d4, d1
	move.l #Observation, d2
	move.l #OBS_BYTES, d3
	jsr DOS_WRITE(a6)
	move.l d0, d5
	move.l d4, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	beq.w failed
	cmpi.l #OBS_BYTES, d5
	bne.w failed
	moveq #0, d7
	bra.w closeLibrary
failed
	moveq #20, d7
closeLibrary
	movea.l a5, a1
	movea.l 4.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
	move.l d7, d0
	bra.w done
unavailable
	moveq #20, d0
done
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; start
	.priv
; D6=index, D0=status; D1/D2/A0/A1 clobbered; CCR reflects D0.
checkIdentity	.block
	bsr.w spell
	lea Name, a0
	move.l #NAME_BYTES, d0
	lea ScopeState, a1
	jsr scopes.bindOrigin
	tst.l d0
	bne.w done
	move.l d6, d2
	add.l #SOURCE_BASE, d2
	cmp.l d2, d1
	beq.w done
	moveq #IDENTITY_MISMATCH, d0
done
	tst.l d0
	rts
	.bend  ; checkIdentity
; D6=index; writes four lowercase hex digits, clobbers D0-D2/A0/A1.
spell	.block
	move.l d6, d0
	lea Name+NAME_BYTES, a0
	lea HexDigits, a1
	moveq #3, d2
nextDigit
	move.l d0, d1
	andi.l #15, d1
	move.b 0(a1, d1.w), -(a0)
	lsr.l #4, d0
	dbra d2, nextDigit
	rts
	.bend  ; spell
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:scope-arena.bin", 0
HexDigits	.byte "0123456789abcdef"
Name	.fill byte, NAME_BYTES, $61
	.endsection
	.section bss, kind=bss
	.align 4
Observation	.res byte, OBS_BYTES
ScopeState	.res byte, scopes.SCRATCH_BYTES
	.endsection
	.output "scope-arena-probe.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
	.end
