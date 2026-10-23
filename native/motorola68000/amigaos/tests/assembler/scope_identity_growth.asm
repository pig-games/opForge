; Actual binding grows identity tables through the final word-sized source ID.
; @opforge-owner: opforge.tests.assembler.scope_identity_growth
.module opforge.tests.assembler.scope_identity_growth
.cpu 68020
.use opforge.assembler.symbols.scopes as scopes
.use opforge.assembler.symbols.lexical_store as lexical
.use opforge.assembler.symbols.scope_layout as layout
.use opforge.platform.memory as memory
.priv
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
.pub
.section entry, kind=code
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	jsr cases
	move.l d0, -(sp)
	lea Scope, a0
	jsr scopes.release
	move.l (sp)+, d0
	tst.l d0
	bne.w done
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w done
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	move.l d0, d1
	move.l #Result, d2
	moveq #1, d3
	jsr DOS_WRITE(a6)
	cmpi.l #1, d0
	bne.w closeFile
	clr.l ReturnCode
closeFile
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
.bend  ; start
.endsection
.priv
.section code, kind=code
BASE = 2688
NAMES = 65536-BASE
; D0/CCR=status. Generated names use the ordinary source-binding callback.
cases	.block
	lea Scope, a0
	move.l #BASE, d0
	moveq #0, d1
	jsr scopes.begin
	bne.w bad
	moveq #0, d6
next
	bsr.w bindIndex
	bne.w bad
	move.l d6, d3
	add.l #BASE, d3
	cmp.l d3, d1
	bne.w bad
	addq.l #1, d6
	cmpi.l #NAMES, d6
	blo.w next
	lea Scope, a0
	jsr scopes.count
	cmpi.l #65536, d0
	bne.w bad
	; Preserve both allocation descriptors, published extent and final entry.
	lea Snapshot, a0
	.for offset in {scopes.ENTRIES+memory.Block.Pointer, scopes.ENTRIES+memory.Block.Capacity, scopes.ENTRIES+memory.Block.Used, scopes.ARENA+memory.Block.Pointer, scopes.ARENA+memory.Block.Capacity, scopes.ARENA+memory.Block.Used, layout.State.ArenaUsed}
	move.l Scope+offset, (a0)+
	.endfor
	lea Scope+layout.LEXICAL_STATE, a0
	lea EntryCopy, a1
	move.l #NAMES-1, d0
	jsr lexical.readEntry
	bne.w bad
	lea EntryCopy, a4
	.for i in 0..scopes.ENTRY_BYTES/4
	move.l i*4(a4), LastEntry+i*4
	.endfor
	bsr.w bindIndex
	beq.w bad
	lea Scope, a0
	jsr scopes.count
	cmpi.l #65536, d0
	bne.w bad
	lea Snapshot, a0
	.for offset in {scopes.ENTRIES+memory.Block.Pointer, scopes.ENTRIES+memory.Block.Capacity, scopes.ENTRIES+memory.Block.Used, scopes.ARENA+memory.Block.Pointer, scopes.ARENA+memory.Block.Capacity, scopes.ARENA+memory.Block.Used, layout.State.ArenaUsed}
	move.l Scope+offset, d0
	cmp.l (a0)+, d0
	bne.w bad
	.endfor
	lea Scope+layout.LEXICAL_STATE, a0
	lea EntryCopy, a1
	move.l #NAMES-1, d0
	jsr lexical.readEntry
	bne.w bad
	.for i in 0..scopes.ENTRY_BYTES/4
	move.l i*4(a4), d0
	cmp.l LastEntry+i*4, d0
	bne.w bad
	.endfor
	moveq #0, d6
	bsr.w bindIndex
	bne.w bad
	cmpi.l #BASE, d1
	bne.w bad
	move.l #NAMES-1, d6
	bsr.w bindIndex
	bne.w bad
	cmpi.l #65535, d1
	bne.w bad
	lea Scope, a0
	jsr scopes.count
	cmpi.l #65536, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
.bend  ; cases
; D6=index. D0/CCR=status,D1=ID. Clobbers D2-D3/A0-A1; keeps D4-D7/A2-A6.
bindIndex	.block
	move.w d6, d2
	lea Name+1, a0
	lea Hex, a1
	moveq #3, d3
hexDigit
	rol.w #4, d2
	move.w d2, d0
	andi.w #15, d0
	move.b 0(a1, d0.w), (a0)+
	dbra d3, hexDigit
	lea Name, a0
	lea Scope, a1
	moveq #5, d0
	jsr scopes.bind
	rts
.bend  ; bindIndex
.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:scope-identity-growth.bin", 0
Result	.byte $5a
Name	.byte "n0000"
Hex	.byte "0123456789ABCDEF"
.endsection
.section bss, kind=bss
.align 4
Scope	.res byte, scopes.SCRATCH_BYTES
Snapshot	.res long, 7
LastEntry	.res byte, scopes.ENTRY_BYTES
EntryCopy	.res byte, scopes.ENTRY_BYTES
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
.endsection
.output "scope-identity-growth.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
