; @opforge-owner: opforge.packages.validation
; Shared structural boundary for embedded and external BS38 packages.
; Checks identity, regions and table records. Preparation policy validation is
; delegated to its shared VM owner; runtime engines retain execution bounds.
	.module opforge.packages.validation
	.cpu 68020
	.use opforge.packages.format as package
	.use opforge.assembler.layout.state as state
	.use opforge.platform.memory as memory
	.use opforge.vm.prvm.abi as parser_abi
	.use opforge.vm.prvm.packed_branch as branches
	.use opforge.vm.prvm.packed_declaration as declarations
	.use opforge.vm.prvm.packed_text as text
	.use opforge.vm.prvm.statement_abi as statement_abi
	.use opforge.vm.prvm.statement_compile as statement_compile
	.use opforge.vm.prvm.statement_match as statement_match
	.use opforge.vm.prvm.statement_head as statement_head
	.pub
SUCCESS = 0
INVALID = 1
TARGET_LIMIT = 26
COUNT_LIMIT = 65535
ROW_BYTES = package.ROW_BYTES
REGISTER_BYTES = 6
PROGRAM_BYTES = 12
ROLE_BYTES = 12
ROLE_LIMIT = 16
TOKENIZER_MIN_BYTES = 16
TOKENIZER_VERSION = 1
MACRO_VERSION = 2
	.section code, kind=code
; A0=BS38 bytes,D0=readable length,A1=optional expected canonical NUL key.
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
	tst.w package.Header.BuiltinReserved(a4)
	bne.w bad
	move.w package.Header.BuiltinLenName(a4), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	move.w package.Header.DsDirective(a4), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
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
	tst.w package.Row.RoleReserved(a3)
	bne.w bad
	moveq #0, d1
	move.w package.Row.RoleCount(a3), d1
	cmpi.w #ROLE_LIMIT, d1
	bhi.w bad
	move.l package.Row.RoleInputs(a3), d0
	tst.w d1
	bne.w roleSpan
	tst.l d0
	bne.w bad
	bra.w nextCandidate
roleSpan
	btst #0, d0
	bne.w bad
	mulu.w #ROLE_BYTES, d1
	move.l a3, -(sp)
	bsr.w span
	movea.l (sp)+, a3
	bne.w bad
nextCandidate
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
	move.l package.Header.LoopPlan(a4), d0
	btst #0, d0
	bne.w bad
	move.l package.Header.LoopPlanBytes(a4), d1
	cmpi.l #parser_abi.LOOP_PROGRAM_BYTES, d1
	bne.w bad
	bsr.w span
	bne.w bad
	cmpi.b #parser_abi.OP_LOOP_HEADER, (a3)
	bne.w bad
	cmpi.b #parser_abi.OP_PUBLISH, parser_abi.LoopProgram.Publish(a3)
	bne.w bad
	tst.b parser_abi.LoopProgram.End(a3)
	bne.w bad
	move.w parser_abi.LoopProgram.ForHigh(a3), d0
	cmp.w package.Header.ForDirective(a4), d0
	bne.w bad
	moveq #0, d0
	move.b parser_abi.LoopProgram.InHigh(a3), d0
	lsl.w #8, d0
	move.b parser_abi.LoopProgram.InLow(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	move.b parser_abi.LoopProgram.EndforHigh(a3), d0
	lsl.w #8, d0
	move.b parser_abi.LoopProgram.EndforLow(a3), d0
	cmp.w package.Header.EndforDirective(a4), d0
	bne.w bad
	move.b parser_abi.LoopProgram.WhileHigh(a3), d0
	lsl.w #8, d0
	move.b parser_abi.LoopProgram.WhileLow(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	move.b parser_abi.LoopProgram.EndwhileHigh(a3), d0
	lsl.w #8, d0
	move.b parser_abi.LoopProgram.EndwhileLow(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	moveq #0, d0
	move.w parser_abi.LoopProgram.BforHigh(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	moveq #0, d0
	move.w parser_abi.LoopProgram.BwhileHigh(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	move.l package.Header.BranchPlan(a4), d0
	btst #0, d0
	bne.w bad
	move.l package.Header.BranchPlanBytes(a4), d1
	cmpi.l #branches.PROGRAM_BYTES, d1
	bne.w bad
	bsr.w span
	bne.w bad
	cmpi.b #branches.OPCODE, branches.Program.Opcode(a3)
	bne.w bad
	cmpi.b #branches.HEAD_COUNT, branches.Program.Count(a3)
	bne.w bad
	cmpi.b #parser_abi.OP_PUBLISH, branches.Program.Publish(a3)
	bne.w bad
	tst.b branches.Program.End(a3)
	bne.w bad
	lea branches.Program.Rows(a3), a2
	moveq #1, d6
branchRows
	move.w (a2), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	cmp.b branches.Row.Role(a2), d6
	bne.w bad
	moveq #0, d0
	cmpi.w #branches.MATCH, d6
	beq.w branchExpression
	cmpi.w #branches.CASE, d6
	beq.w branchExpression
	cmpi.w #branches.ELSEIF, d6
	bhi.w branchPolicy
branchExpression
	moveq #1, d0
branchPolicy
	cmp.b branches.Row.Expression(a2), d0
	bne.w bad
	addq.w #1, d6
	adda.w #branches.ROW_BYTES, a2
	cmpi.w #branches.HEAD_COUNT+1, d6
	blo.w branchRows
	move.l package.Header.TextPlan(a4), d0
	btst #0, d0
	bne.w bad
	move.l package.Header.TextPlanBytes(a4), d1
	cmpi.l #text.PROGRAM_BYTES, d1
	bne.w bad
	bsr.w span
	bne.w bad
	movem.l d2-d7/a0-a2, -(sp)
	movea.l a3, a1
	moveq #0, d0
	suba.l a0, a0
	jsr text.nameRole
	movem.l (sp)+, d2-d7/a0-a2
	tst.l d0
	bne.w bad
	lea text.PROGRAM_HEADER(a3), a2
	moveq #text.HEAD_COUNT-1, d6
textNames
	moveq #0, d0
	move.b text.DIRECTIVE_ID(a2), d0
	lsl.w #8, d0
	move.b text.DIRECTIVE_ID+1(a2), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	adda.w #text.DIRECTIVE_BYTES, a2
	dbra d6, textNames
	move.l package.Header.ExpressionPlan(a4), d0
	btst #0, d0
	bne.w bad
	move.l package.Header.ExpressionPlanBytes(a4), d1
	beq.w bad
	cmpi.l #65535, d1
	bhi.w bad
	bsr.w span
	bne.w bad
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
	bsr.w span
	bne.w bad
	movea.l a3, a0
	move.l package.Header.DeclarationPlanBytes(a4), d0
	jsr declarations.validateProgram
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
	moveq #0, d0
	move.b (a3), d0
	lsl.w #8, d0
	move.b 1(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
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
	suba.l a5, a5
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
	cmpi.b #package.DICTIONARY_ROLE_ALLOWED, package.DictionaryEntry.Roles(a3)
	bhi.w bad
	move.w package.DictionaryEntry.Name(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	btst #package.DICTIONARY_BUILTIN_BIT, package.DictionaryEntry.Roles(a3)
	beq.w dictionaryNext
	cmp.w package.Header.BuiltinLenName(a4), d0
	bne.w bad
	tst.b package.DictionaryEntry.Qualifier(a3)
	bne.w bad
	addq.l #1, a5
dictionaryNext
	add.l d1, d5
	subq.l #1, d6
	bra.w dictionaryLoop
tokenizer
	move.l a5, d0
	cmpi.l #1, d0
	bne.w bad
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
	lea package.Header.StatementCompile(a4), a2
	bsr.w preparationProgram
	bne.w bad
	move.l d1, d0
	jsr statement_compile.validateProgram
	bne.w bad
	lea package.Header.StatementMatch(a4), a2
	bsr.w preparationProgram
	bne.w bad
	move.l d1, d0
	jsr statement_match.validateProgram
	bne.w bad
	lea package.Header.StatementHead(a4), a2
	bsr.w preparationProgram
	bne.w bad
	move.l d1, d0
	jsr statement_head.validateProgram
	bne.w bad
	move.w statement_abi.HeadProgram.Definition(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	move.w statement_abi.HeadProgram.EndMarker(a3), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
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
; A2=program offset/length pair; D3/D4=preparation bounds.
; D0/CCR=status; A0/D1=checked program pointer/length for its VM validator.
; A3 is the checked span; other span-helper clobbers are D1/D2.
preparationProgram	.block
	move.l (a2), d0  ; package.PreparationProgram.Offset (zero offset)
	btst #0, d0
	bne.w bad
	move.l package.PreparationProgram.Bytes(a2), d1
	beq.w bad
	bsr.w span
	bne.w bad
	movea.l a3, a0
	moveq #SUCCESS, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; preparationProgram
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
