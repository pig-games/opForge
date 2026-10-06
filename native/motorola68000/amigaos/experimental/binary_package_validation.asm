; @opforge-owner: experimental.amigaos.binary_package_validation
; Shared structural boundary for embedded and external BS28 packages.
; This checks identity, regions and table records, not VM opcode semantics;
; execution engines retain their independent operand/opcode and step bounds.
	.module experimental.amigaos.binary_package_validation
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_state as state
	.use experimental.amigaos.binary_memory as memory
	.pub
SUCCESS = 0
INVALID = 1
TARGET_LIMIT = 26
COUNT_LIMIT = 65535
ROW_BYTES = package.ROW_BYTES
REGISTER_BYTES = 6
PROGRAM_BYTES = 12
TOKENIZER_MIN_BYTES = 16
TOKENIZER_VERSION = 1
MACRO_VERSION = 2
	.section code, kind=code
; A0=BS28 bytes,D0=readable length,A1=optional expected canonical NUL key.
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
	move.w package.Header.TargetFlags(a4), d2
	andi.w #$ffff-package.TARGET_PRESERVE_WRAPPERS-package.TARGET_NESTED_PATHS, d2
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
	move.l package.Header.MemberBindings(a4), d0
	move.l package.Header.MemberBindingCount(a4), d2
	moveq #package.MEMBER_BINDING_BYTES, d1
	bsr.w table
	bne.w bad
	move.l package.Header.MemberBindingCount(a4), d6
memberLoop
	tst.l d6
	beq.w candidateTables
	move.w package.MemberBinding.Name(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	move.w package.MemberBinding.Field(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	tst.w package.MemberBinding.Reserved(a3)
	bne.w bad
	adda.w #package.MEMBER_BINDING_BYTES, a3
	subq.l #1, d6
	bra.w memberLoop
candidateTables
	move.l package.Header.Rows(a4), d0
	move.l package.Header.RowCount(a4), d2
	moveq #ROW_BYTES, d1
	bsr.w table
	bne.w bad
	move.l package.Header.RowCount(a4), d6
candidateLoop
	tst.l d6
	beq.w registerTable
	cmpi.b #11, package.Row.RequiredForm2(a3)
	bhi.w bad
	tst.w package.Row.Reserved(a3)
	bne.w bad
	adda.w #ROW_BYTES, a3
	subq.l #1, d6
	bra.w candidateLoop
registerTable
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
	movea.l a4, a2
	jsr state.validate
	bne.w bad
	cmpi.w #2, package.Header.DeclarationPlanVersion(a4)
	bne.w bad
	tst.w package.Header.DeclarationReserved(a4)
	bne.w bad
	move.l package.Header.DeclarationPlan(a4), d0
	btst #0, d0
	bne.w bad
	move.l package.Header.DeclarationPlanBytes(a4), d1
	cmpi.l #17, d1
	bne.w bad
	bsr.w span
	bne.w bad
	cmpi.b #$98, (a3)
	bne.w bad
	cmpi.b #3, 1(a3)
	bne.w bad
	; Exact latest policy: one mutable colon/equal operator row.
	cmpi.l #$01020522, 11(a3)
	bne.w bad
	cmpi.b #$83, 15(a3)
	bne.w bad
	tst.b 16(a3)
	bne.w bad
	addq.l #2, a3
	moveq #2, d6
declarationRow
	moveq #0, d0
	move.b (a3), d0
	lsl.w #8, d0
	move.b 1(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	cmpi.b #1, 2(a3)
	blo.w bad
	cmpi.b #2, 2(a3)
	bhi.w bad
	addq.l #3, a3
	dbra d6, declarationRow
	cmpi.w #2, package.Header.HeadPolicyVersion(a4)
	bne.w bad
	tst.w package.Header.Reserved(a4)
	bne.w bad
	move.l package.Header.HeadPolicy(a4), d0
	btst #0, d0
	bne.w bad
	move.l package.Header.HeadPolicyBytes(a4), d1
	cmpi.l #4, d1
	bne.w bad
	bsr.w span
	bne.w bad
	moveq #0, d0
	move.w package.Header.EmitDirective(a4), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	tst.w package.Header.WordBytes(a4)
	beq.w bad
	move.l package.Header.DataPlan(a4), d0
	btst #0, d0
	bne.w bad
	move.l package.Header.DataPlanBytes(a4), d1
	cmpi.l #16, d1
	bne.w bad
	bsr.w span
	bne.w bad
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
	lea package.Header.MetadataPlan(a4), a2
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
