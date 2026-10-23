; Adapter proof starts at owned TKVM records, not tokenizer source scanning.
	.module main
	.cpu 68020
	.use opforge.assembler.language.statement_facets as adapter
	.use opforge.vm.prvm.statement_abi as abi
	.priv
CaseHeader	.struct
Fault	.long ?
Budget	.long ?
ViewCapacity	.long ?
NameCapacity	.long ?
ArenaCapacity	.long ?
WorkspaceCapacity	.long ?
Count	.long ?
TokenBytes	.long ?  ; declared input capacity; fixture still carries Count rows
LexemeBytes	.long ?
IdsBytes	.long ?
	.endstruct
CASE_HEADER_BYTES = CaseHeader.IdsBytes+4
Result	.struct
Status	.long ?
Views	.long ?
Arena	.long ?
	.endstruct
RESULT_BYTES = Result.Arena+4
TOKEN_BYTES = 20
VIEW_BYTES = 64*abi.TOKEN_BYTES
NAME_BYTES = 64*2
ARENA_BYTES = 8192
WORKSPACE_BYTES = 16384
SENTINEL = $a5a5a5a5
FAULT_VIEW_WORKSPACE = 1
FAULT_ARENA_INPUT = 2
FAULT_ODD_VIEW = 3
FAULT_NAME_VIEW = 4
FAULT_FRAME_VIEW = 5
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
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	jsr cases
	move.l d0, ReturnCode
	movea.l DosBase, a6
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
cases	.block
	move.l #Fixture, Cursor
	moveq #4, d0
	bsr.w take
	bne.w fail
	move.l (a0), d7
	bmi.w fail
	moveq #4, d0
	bsr.w take
	bne.w fail
	move.l (a0), d0
	bsr.w padded
	bne.w fail
	move.l a0, PackagePtr
	bsr.w checksum
	move.l d0, InputChecksum
next
	tst.l d7
	beq.w exhausted
	moveq #CASE_HEADER_BYTES, d0
	bsr.w take
	bne.w fail
	movea.l a0, a4
	move.l CaseHeader.Count(a4), d0
	cmpi.l #65, d0
	bhi.w fail
	mulu.w #TOKEN_BYTES, d0
	bsr.w take
	bne.w fail
	move.l a0, TokenPtr
	move.l CaseHeader.LexemeBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, LexemePtr
	move.l CaseHeader.Count(a4), d0
	add.l d0, d0
	bsr.w take
	bne.w fail
	move.l a0, IdsPtr
	cmpi.l #VIEW_BYTES, CaseHeader.ViewCapacity(a4)
	bhi.w fail
	cmpi.l #NAME_BYTES, CaseHeader.NameCapacity(a4)
	bhi.w fail
	cmpi.l #ARENA_BYTES, CaseHeader.ArenaCapacity(a4)
	bhi.w fail
	cmpi.l #WORKSPACE_BYTES, CaseHeader.WorkspaceCapacity(a4)
	bhi.w fail
	lea Views, a0
	move.l #VIEW_BYTES+NAME_BYTES+ARENA_BYTES, d0
	bsr.w fill
	bsr.w invoke
	move.l d0, ResultData+Result.Status
	move.l Request+adapter.Frame.ViewCount, ResultData+Result.Views
	move.l Request+adapter.Frame.ArenaUsed, ResultData+Result.Arena
	moveq #0, d4
	moveq #0, d5
	moveq #0, d6
	tst.l d0
	bne.w rejected
	move.l Request+adapter.Frame.ViewCount, d4
	cmp.l CaseHeader.Count(a4), d4
	bne.w fail
	move.l d4, d5
	add.l d5, d5
	lsl.l #5, d4
	move.l Request+adapter.Frame.ArenaUsed, d6
	cmp.l CaseHeader.ArenaCapacity(a4), d6
	bhi.w fail
	bra.w guarded
rejected
	cmpi.l #SENTINEL, Request+adapter.Frame.ViewCount
	bne.w fail
	cmpi.l #SENTINEL, Request+adapter.Frame.ArenaUsed
	bne.w fail
guarded
	lea Views, a0
	adda.l d4, a0
	move.l #VIEW_BYTES, d0
	sub.l d4, d0
	bsr.w guard
	bne.w fail
	lea Names, a0
	adda.l d5, a0
	move.l #NAME_BYTES, d0
	sub.l d5, d0
	bsr.w guard
	bne.w fail
	lea Arena, a0
	adda.l d6, a0
	move.l #ARENA_BYTES, d0
	sub.l d6, d0
	bsr.w guard
	bne.w fail
	lea ResultData, a0
	moveq #RESULT_BYTES, d0
	bsr.w writeSpan
	bne.w fail
	lea Views, a0
	move.l d4, d0
	bsr.w writeSpan
	bne.w fail
	lea Names, a0
	move.l d5, d0
	bsr.w writeSpan
	bne.w fail
	lea Arena, a0
	move.l d6, d0
	bsr.w writeSpan
	bne.w fail
	subq.l #1, d7
	bra.w next
exhausted
	; The immutable fixture is checked once after all cases, avoiding repeated
	; scans of the generated capsule. Every output tail is checked per case.
	bsr.w checksum
	cmp.l InputChecksum, d0
	bne.w fail
	movea.l Cursor, a0
	lea FixtureEnd, a1
	cmpa.l a1, a0
	bne.w fail
	moveq #0, d0
	rts
fail
	moveq #20, d0
	rts
	.bend  ; cases

; A4=fixture header. D0=adapter status; other registers preserved.
invoke	.block
	movem.l a0, -(sp)
	lea Request, a0
	move.l PackagePtr, adapter.Frame.Package(a0)
	move.l TokenPtr, adapter.Frame.Tokens(a0)
	move.l CaseHeader.TokenBytes(a4), adapter.Frame.TokenBytes(a0)
	move.l CaseHeader.Count(a4), adapter.Frame.Count(a0)
	move.l LexemePtr, adapter.Frame.Lexemes(a0)
	move.l CaseHeader.LexemeBytes(a4), adapter.Frame.LexemeBytes(a0)
	move.l IdsPtr, adapter.Frame.Ids(a0)
	move.l CaseHeader.IdsBytes(a4), adapter.Frame.IdsBytes(a0)
	move.l #Views, adapter.Frame.Views(a0)
	move.l CaseHeader.ViewCapacity(a4), adapter.Frame.ViewCapacity(a0)
	move.l #Names, adapter.Frame.Names(a0)
	move.l CaseHeader.NameCapacity(a4), adapter.Frame.NameCapacity(a0)
	move.l #Arena, adapter.Frame.Arena(a0)
	move.l CaseHeader.ArenaCapacity(a4), adapter.Frame.ArenaCapacity(a0)
	move.l #Workspace, adapter.Frame.Workspace(a0)
	move.l CaseHeader.WorkspaceCapacity(a4), adapter.Frame.WorkspaceBytes(a0)
	move.l #Budget, adapter.Frame.Budget(a0)
	move.l CaseHeader.Budget(a4), Budget
	move.l #SENTINEL, adapter.Frame.ViewCount(a0)
	move.l #SENTINEL, adapter.Frame.ArenaUsed(a0)
	cmpi.l #FAULT_VIEW_WORKSPACE, CaseHeader.Fault(a4)
	bne.w arenaFault
	move.l #Workspace, adapter.Frame.Views(a0)
arenaFault
	cmpi.l #FAULT_ARENA_INPUT, CaseHeader.Fault(a4)
	bne.w oddFault
	move.l LexemePtr, adapter.Frame.Arena(a0)
oddFault
	cmpi.l #FAULT_ODD_VIEW, CaseHeader.Fault(a4)
	bne.w nameFault
	move.l #Views, adapter.Frame.Views(a0)
	addq.l #1, adapter.Frame.Views(a0)
nameFault
	cmpi.l #FAULT_NAME_VIEW, CaseHeader.Fault(a4)
	bne.w frameFault
	move.l #Views, adapter.Frame.Names(a0)
frameFault
	cmpi.l #FAULT_FRAME_VIEW, CaseHeader.Fault(a4)
	bne.w ready
	move.l #Request, adapter.Frame.Views(a0)
ready
	jsr adapter.prepare
	movem.l (sp)+, a0
	rts
	.bend  ; invoke

; A0/D0=buffer/bytes. Clobbers A0/D0/CCR.
fill	.block
next
	move.b #$a5, (a0)+
	subq.l #1, d0
	bne.w next
	rts
	.bend  ; fill
; A0/D0=unused buffer tail. D0/status/CCR; preserves other registers.
guard	.block
	movem.l a0, -(sp)
next
	tst.l d0
	beq.w valid
	cmpi.b #$a5, (a0)+
	bne.w fail
	subq.l #1, d0
	bra.w next
valid
	moveq #0, d0
	bra.w done
fail
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, a0
	rts
	.bend  ; guard
; Detect fixture mutation in overlap failures; input is never intentionally written.
; D0=folded checksum; other registers preserved.
checksum	.block
	movem.l d1-d2/a0, -(sp)
	lea Fixture, a0
	move.l #FixtureEnd-Fixture, d2
	moveq #0, d0
next
	rol.l #5, d0
	moveq #0, d1
	move.b (a0)+, d1
	eor.l d1, d0
	subq.l #1, d2
	bne.w next
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; checksum
; D0=nonnegative bytes. A0=bounded fixture span,D0=status/CCR; others preserved.
take	.block
	movem.l d1, -(sp)
	tst.l d0
	bmi.w invalid
	move.l #FixtureEnd, d1
	sub.l Cursor, d1
	cmp.l d1, d0
	bhi.w invalid
	movea.l Cursor, a0
	add.l d0, Cursor
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1
	rts
	.bend  ; take
; D0=raw bytes; consume its even-byte-padded fixture block. Same outputs as take.
padded	.block
	tst.l d0
	bmi.w invalid
	btst #0, d0
	beq.w ready
	addq.l #1, d0
ready
	bsr.w take
	rts
invalid
	moveq #1, d0
	rts
	.bend  ; padded
; A0/D0=output span. D0=status/CCR; others preserved. Exact partial-write loop.
writeSpan	.block
	movem.l d1-d5/a0-a6, -(sp)
	movea.l a0, a3
	move.l d0, d5
next
	tst.l d5
	beq.w valid
	movea.l DosBase, a6
	move.l OutputHandle, d1
	move.l a3, d2
	move.l d5, d3
	jsr DOS_WRITE(a6)
	tst.l d0
	ble.w fail
	cmp.l d5, d0
	bhi.w fail
	adda.l d0, a3
	sub.l d0, d5
	bra.w next
valid
	moveq #0, d0
	bra.w done
fail
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1-d5/a0-a6
	rts
	.bend  ; writeSpan
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:statement-facets-results.bin", 0
	.align 4
Fixture	.incbin "statement-facets-cases.bin"
FixtureEnd
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
Cursor	.res long, 1
PackagePtr	.res long, 1
TokenPtr	.res long, 1
LexemePtr	.res long, 1
IdsPtr	.res long, 1
InputChecksum	.res long, 1
Budget	.res long, 1
Request	.res byte, adapter.FRAME_BYTES
ResultData	.res byte, RESULT_BYTES
Views	.res byte, VIEW_BYTES
Names	.res byte, NAME_BYTES
Arena	.res byte, ARENA_BYTES
Workspace	.res byte, WORKSPACE_BYTES
	.endsection
	.output "statement-facets.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
