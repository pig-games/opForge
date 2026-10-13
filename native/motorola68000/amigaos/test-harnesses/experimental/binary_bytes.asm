; Canonical binary payloads are opaque and reject truncated views.
; @opforge-owner: test.amigaos.binary_bytes
.module test.amigaos.binary_bytes
.cpu 68020
.use experimental.amigaos.binary_binding_records as records
.use experimental.amigaos.binary_payload_bounds as payloads
.use experimental.amigaos.binary_source as source

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
cases	.block
	move.w #$abcd, Entries+records.Entry.Target
	lea Record, a0
	move.l #RecordEnd-Record, d0
	lea Entries, a1
	move.l #$8000, d1
	moveq #1, d2
	jsr records.remap
	bne.w bad
	cmpi.b #$ab, Record+6
	bne.w bad
	cmpi.b #$cd, Record+7
	bne.w bad
	cmpi.l #$00ffff00, Raw+source.BinaryBytesToken.Payload
	bne.w bad
	cmpi.b #6, RawEnd-1
	bne.w bad
	lea Raw, a0
	lea RawEnd, a1
	jsr payloads.binary
	bne.w bad
	cmpi.l #5, d1
	bne.w bad
	cmpi.l #source.BINARY_BYTES_HEADER_BYTES+5, d2
	bne.w bad
	lea Short, a0
	lea ShortEnd, a1
	jsr payloads.binary
	beq.w bad
	lea Short, a2
	cmpa.l a2, a0
	bne.w bad
	lea Long, a0
	lea LongEnd, a1
	jsr payloads.binary
	beq.w bad
	lea Long, a2
	cmpa.l a2, a0
	bne.w bad
	lea Wrong, a0
	lea WrongEnd, a1
	jsr payloads.binary
	beq.w bad
	lea Wrong, a2
	cmpa.l a2, a0
	bne.w bad
	lea ShortRecord, a0
	move.l #ShortRecordEnd-ShortRecord, d0
	lea Entries, a1
	move.l #$8000, d1
	moveq #1, d2
	jsr records.remap
	beq.w bad
	lea LongRecord, a0
	move.l #LongRecordEnd-LongRecord, d0
	lea Entries, a1
	move.l #$8000, d1
	moveq #1, d2
	jsr records.remap
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
.bend  ; cases
.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:binary-bytes.bin", 0
Result	.byte $5a
Record
	.byte RecordEnd-Record-1, 0
	.word 1
	.byte 7, 0, $80, 0, 0
Raw
	.byte source.TOKEN_BINARY_BYTES, 5
	.byte 0, $ff, $ff, 0, 6
RawEnd
RecordEnd
Short	.byte source.TOKEN_BINARY_BYTES
ShortEnd
Long	.byte source.TOKEN_BINARY_BYTES, 255, 0
LongEnd
Wrong	.byte 3, 0
WrongEnd
ShortRecord
	.byte ShortRecordEnd-ShortRecord-1, 0
	.word 1
	.byte 7, 0, 0, 0, 0, source.TOKEN_BINARY_BYTES
ShortRecordEnd
LongRecord
	.byte LongRecordEnd-LongRecord-1, 0
	.word 1
	.byte 7, 0, 0, 0, 0, source.TOKEN_BINARY_BYTES, 255, 0
LongRecordEnd
.endsection
.section bss, kind=bss
.align 4
Entries	.res byte, records.ENTRY_BYTES
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
.endsection
.output "binary-bytes.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
