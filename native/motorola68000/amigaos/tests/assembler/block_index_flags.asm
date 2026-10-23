; Current prepared flags preserve named block topology and marker validation.
; @opforge-owner: opforge.tests.assembler.block_index_flags
.module opforge.tests.assembler.block_index_flags
.cpu 68020
.include "binding_telemetry.i"
.use opforge.assembler.layout.block_index as blocks
.use opforge.platform.memory as memory
.use opforge.assembler.source.records as source
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
	move.l d0, CaseStatus
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w done
	.BINDING_DIAGNOSTIC_REPORT DosBase
	tst.l CaseStatus
	bne.w closeLibrary
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
cases	.block
	lea Valid, a0
	move.l #ValidEnd-Valid, d0
	bsr.w index
	bne.w bad
	cmpi.l #1, d1
	bne.w bad
	movea.l Scratch+memory.Block.Pointer, a0
	cmpi.l #1, blocks.Scratch.Count(a0)
	bne.w bad
	cmpi.l #0, blocks.HEADER_BYTES+blocks.Span.Start(a0)
	bne.w bad
	cmpi.l #ValidEnd-Valid, blocks.HEADER_BYTES+blocks.Span.End(a0)
	bne.w bad
	cmpi.w #$8000, blocks.HEADER_BYTES+blocks.Span.Entry(a0)
	bne.w bad
	lea Unknown, a0
	move.l #UnknownEnd-Unknown, d0
	bsr.w index
	beq.w bad
	lea Mixed, a0
	move.l #MixedEnd-Mixed, d0
	bsr.w index
	beq.w bad
	lea WrongMarker, a0
	move.l #WrongMarkerEnd-WrongMarker, d0
	bsr.w index
	beq.w bad
	lea ShortOpen, a0
	move.l #ShortOpenEnd-ShortOpen, d0
	bsr.w index
	beq.w bad
	lea ClosePayload, a0
	move.l #ClosePayloadEnd-ClosePayload, d0
	bsr.w index
	beq.w bad
	lea Unclosed, a0
	move.l #UnclosedEnd-Unclosed, d0
	bsr.w index
	beq.w bad
	lea TrailerOpen, a0
	move.l #TrailerOpenEnd-TrailerOpen, d0
	bsr.w index
	beq.w bad
	moveq #0, d0
	bra.w finish
bad
	moveq #1, d0
finish
	move.l d0, -(sp)
	lea Scratch, a0
	jsr memory.release
	move.l (sp)+, d0
	tst.l d0
	rts
.bend
index	.block
	lea Scratch, a1
	jsr blocks.index
	rts
.bend
.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:block-index-flags.bin", 0
Result	.byte $5a
Valid
	.byte 8, source.FLAG_ALLOWED-source.FLAG_BLOCK_CLOSE
	.word 1
	.byte 0, $80, 0, 0, source.TOKEN_LABEL_DECLARATION
	.byte 9, source.FLAG_PLAN
	.word 2
	.byte source.MACRO_PLAN
	.long $00ffff00
	.byte source.PLAN_TOKEN_BYTES
	.byte 9, source.LINE_PLAN_FLAG
	.word 3
	.byte source.LINE_PLAN
	.long $00ffff00
	.byte source.PLAN_TOKEN_BYTES
	.byte 3, source.FLAG_MUTABLE_SNAPSHOT+source.FLAG_OMIT
	.word 4
	.byte 3, source.FLAG_BLOCK_CLOSE+source.FLAG_OMIT+source.FLAG_LAYOUT
	.word 5
ValidEnd
Unknown	.byte 3, $80
	.word 1
UnknownEnd
Mixed	.byte 8, source.FLAG_BLOCK_OPEN+source.FLAG_BLOCK_CLOSE
	.word 1
	.byte 0, $80, 0, 0, source.TOKEN_LABEL_DECLARATION
MixedEnd
WrongMarker	.byte 8, source.FLAG_BLOCK_OPEN
	.word 1
	.byte 0, $80, 0, 0, source.TOKEN_MUTABLE_DECLARATION
WrongMarkerEnd
ShortOpen	.byte 3, source.FLAG_BLOCK_OPEN
	.word 1
ShortOpenEnd
ClosePayload	.byte 4, source.FLAG_BLOCK_CLOSE
	.word 1
	.byte 7
ClosePayloadEnd
Unclosed	.byte 8, source.FLAG_BLOCK_OPEN
	.word 1
	.byte 0, $80, 0, 0, source.TOKEN_LABEL_DECLARATION
UnclosedEnd
TrailerOpen	.byte 14, source.FLAG_BLOCK_OPEN+source.FLAG_PLAN
	.word 1
	.byte 0, $80, 0, 0, source.TOKEN_LABEL_DECLARATION, source.MACRO_PLAN
	.long 1
	.byte source.PLAN_TOKEN_BYTES
TrailerOpenEnd
.endsection
.section bss, kind=bss
.align 4
Scratch	.res byte, memory.Block.Used+4
DosBase	.res long, 1
OutputHandle	.res long, 1
CaseStatus	.res long, 1
ReturnCode	.res long, 1
.endsection
.output "block-index-flags.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
