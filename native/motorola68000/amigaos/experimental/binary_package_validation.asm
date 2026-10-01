; Shared structural boundary for embedded and external BS13 packages.
; This checks identity, regions and table records, not VM opcode semantics;
; execution engines retain their independent operand/opcode and step bounds.
	.module experimental.amigaos.binary_package_validation
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_memory as memory
	.pub
SUCCESS = 0
INVALID = 1
TARGET_LIMIT = 26
COUNT_LIMIT = 65535
ROW_BYTES = 32
REGISTER_BYTES = 6
PROGRAM_BYTES = 12
TOKENIZER_MIN_BYTES = 16
TOKENIZER_VERSION = 1
MACRO_VERSION = 2
	.section code, kind=code
; A0=BS13 bytes,D0=readable length,A1=optional expected canonical NUL key.
; D0/CCR=status. Preserves all other registers; no allocation or mutation.
; Readable length is trusted; every package read stays inside that span.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	movea.l a1, a5
	move.l d0, d7
	cmpi.l #package.HEADER_BYTES, d7
	blo.w bad
	cmpi.l #memory.LIMIT, d7
	bhi.w bad
	move.l a4, d0
	btst #0, d0
	bne.w bad
	add.l d7, d0
	bcs.w bad
	cmpi.l #package.MAGIC, package.Header.Magic(a4)
	bne.w bad
	cmp.l package.Header.Bytes(a4), d7
	bne.w bad
	move.l package.Header.RuntimeBytes(a4), d4
	cmpi.l #package.HEADER_BYTES, d4
	blo.w bad
	cmp.l d7, d4
	bhi.w bad
	btst #0, d4
	bne.w bad
	move.l #package.HEADER_BYTES, d3
	move.l package.Header.TargetOffset(a4), d0
	moveq #0, d1
	move.w package.Header.TargetBytes(a4), d1
	beq.w bad
	cmpi.w #TARGET_LIMIT, d1
	bhi.w bad
	tst.w package.Header.TargetReserved(a4)
	bne.w bad
	bsr.w span
	bne.w bad
	moveq #0, d2
keyByte
	move.b 0(a3, d2.l), d0
	cmpi.b #'_', d0
	beq.w keyNext
	cmpi.b #'-', d0
	beq.w keyNext
	cmpi.b #'0', d0
	blo.w bad
	cmpi.b #'9', d0
	bls.w keyNext
	cmpi.b #'A', d0
	blo.w bad
	cmpi.b #'Z', d0
	bls.w keyNext
	cmpi.b #'a', d0
	blo.w bad
	cmpi.b #'z', d0
	bhi.w bad
keyNext
	addq.l #1, d2
	cmp.l d1, d2
	blo.w keyByte
	move.l a5, d0
	beq.w tables
	moveq #0, d2
identity
	move.b 0(a5, d2.l), d0
	beq.w bad
	cmp.b 0(a3, d2.l), d0
	bne.w bad
	addq.l #1, d2
	cmp.l d1, d2
	blo.w identity
	tst.b 0(a5, d2.l)
	bne.w bad
tables
	move.l package.Header.Rows(a4), d0
	move.l package.Header.RowCount(a4), d2
	moveq #ROW_BYTES, d1
	bsr.w table
	bne.w bad
	move.l package.Header.RegisterRows(a4), d0
	move.l package.Header.RegisterCount(a4), d2
	moveq #REGISTER_BYTES, d1
	bsr.w table
	bne.w bad
	move.l package.Header.Programs(a4), d0
	move.l package.Header.ProgramCount(a4), d2
	moveq #PROGRAM_BYTES, d1
	bsr.w table
	bne.w bad
	movea.l a3, a2
	move.l package.Header.ProgramCount(a4), d6
programLoop
	tst.l d6
	beq.w preparation
	tst.w package.Program.Kind(a2)
	beq.w bad
	tst.w package.Program.Version(a2)
	beq.w bad
	move.l package.Program.Offset(a2), d0
	btst #0, d0
	bne.w bad
	move.l package.Program.Bytes(a2), d1
	beq.w bad
	bsr.w span
	bne.w bad
	adda.w #PROGRAM_BYTES, a2
	subq.l #1, d6
	bra.w programLoop
preparation
	move.l d4, d3
	move.l d7, d4
	move.l package.Header.DictionaryCount(a4), d6
	cmpi.l #COUNT_LIMIT, d6
	bhi.w bad
	move.l package.Header.Dictionary(a4), d5
	btst #0, d5
	bne.w bad
dictionaryLoop
	tst.l d6
	beq.w tokenizer
	move.l d5, d0
	moveq #package.DICTIONARY_ENTRY_BYTES, d1
	bsr.w span
	bne.w bad
	moveq #0, d1
	move.w package.DictionaryEntry.Length(a3), d1
	beq.w bad
	addi.l #package.DICTIONARY_ENTRY_BYTES+1, d1
	andi.l #$fffffffe, d1
	move.l d5, d0
	bsr.w span
	bne.w bad
	add.l d1, d5
	subq.l #1, d6
	bra.w dictionaryLoop
tokenizer
	; Validate even an empty dictionary's start offset.
	move.l package.Header.Dictionary(a4), d0
	moveq #0, d1
	bsr.w span
	bne.w bad
	move.l package.Header.Tokenizer(a4), d0
	btst #0, d0
	bne.w bad
	move.l package.Header.TokenizerBytes(a4), d1
	cmpi.l #TOKENIZER_MIN_BYTES, d1
	blo.w bad
	bsr.w span
	bne.w bad
	cmpi.w #TOKENIZER_VERSION, (a3)
	bne.w bad
	moveq #0, d2
	move.w 4(a3), d2
	beq.w bad
	moveq #0, d0
	move.w 2(a3), d0
	cmp.l d2, d0
	bhs.w bad
	lsl.l #2, d2
	addi.l #12, d2
	cmp.l d1, d2
	bhs.w bad
	cmpi.w #MACRO_VERSION, package.Header.MacroVersion(a4)
	bne.w bad
	lea package.Header.MacroCall(a4), a2
	bsr.w macro
	bne.w bad
	lea package.Header.MacroHeader(a4), a2
	bsr.w macro
	bne.w bad
	lea package.Header.MacroPacked(a4), a2
	bsr.w macro
	bne.w bad
	lea package.Header.MacroSpelling(a4), a2
	bsr.w macro
	bne.w bad
	lea package.Header.MacroFragments(a4), a2
	bsr.w macro
	bne.w bad
	lea package.Header.FilePlan(a4), a2
	bsr.w macro
	bne.w bad
	moveq #SUCCESS, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validate
	.priv
; D0=table offset,D2=count,D1=record bytes. D0/CCR=status,A3=table.
table	.block
	cmpi.l #COUNT_LIMIT, d2
	bhi.w bad
	btst #0, d0
	bne.w bad
	mulu.w d2, d1
	bra.w span
bad
	moveq #INVALID, d0
	rts
	.bend  ; table
; A2=offset/length fields; D3/D4=preparation bounds.
macro	.block
	move.l (a2), d0
	move.l 4(a2), d1
	beq.w bad
	bra.w span
bad
	moveq #INVALID, d0
	rts
	.bend  ; macro
; D0=offset,D1=bytes,D3=lower bound,D4=upper bound. A3=checked span.
; Subtraction avoids overflow in offset+length. D5-D7 and D1 preserved.
span	.block
	cmp.l d3, d0
	blo.w bad
	cmp.l d4, d0
	bhi.w bad
	move.l d4, d2
	sub.l d0, d2
	cmp.l d2, d1
	bhi.w bad
	lea 0(a4, d0.l), a3
	moveq #SUCCESS, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; span
	.endsection
	.endmodule
