; Owned macro plans clone all offsets without retaining source storage.
; @opforge-owner: opforge.tests.assembler.macro_plan_clone
.module opforge.tests.assembler.macro_plan_clone
.cpu 68020
.use opforge.assembler.language.macro_plans as plans
.use opforge.platform.memory as memory
.use opforge.vm.prvm.abi as abi
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
; Constructor-generated plans provide repeated roots with nonzero offsets.
; D0/CCR=status. Rejection must preserve destination Used and the public ABI.
cases	.block
	move.l #31, ReturnCode  ; construct recipe-free source root
	lea Source, a0
	jsr plans.begin
	lea Destination, a0
	jsr plans.begin
	lea Generated, a0
	jsr plans.createGenerated
	bne.w bad
	cmpi.l #1, d1
	bne.w bad
	move.l #32, ReturnCode  ; construct recipe-bearing source root
	move.l #Recipes, Generated+plans.GeneratedFrame.RecipeEvents
	move.l #RECIPE_ROWS, Generated+plans.GeneratedFrame.RecipeCount
	lea Generated, a0
	jsr plans.createGenerated
	bne.w bad
	cmpi.l #1, d1
	bls.w bad
	move.l d1, SourceHandle
	move.l #60, ReturnCode  ; independently check constructor-owned spelling
	bsr.w checkSourceSpelling
	bne.w bad
	move.l #33, ReturnCode  ; establish nonzero destination prefix
	lea Destination, a0
	moveq #PREFIX_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	move.l #PREFIX_BYTES, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	moveq #PREFIX_BYTES/4-1, d0
prefix
	move.l #$a5a5a5a5, (a1)+
	dbra d0, prefix
	move.l #34, ReturnCode  ; first clone and its caller ABI
	lea Source, a0
	lea Destination, a1
	move.l SourceHandle, d1
	bsr.w cloneAbi
	bne.w bad
	cmpi.l #PREFIX_BYTES+1, d1
	bne.w bad
	move.l d1, FirstHandle
	move.l #35, ReturnCode  ; first clone geometry and recipe rebasing
	bsr.w checkClone
	bne.w bad
	move.l #36, ReturnCode  ; repeat clone after destination growth
	lea Source, a0
	lea Destination, a1
	move.l SourceHandle, d1
	bsr.w cloneAbi
	bne.w bad
	cmpi.l #PREFIX_BYTES+CLONE_BYTES+1, d1
	bne.w bad
	move.l d1, SecondHandle
	move.l #37, ReturnCode  ; repeated clone geometry
	bsr.w checkClone
	bne.w bad
	move.l #38, ReturnCode  ; recipe-free clone and geometry
	; Header/default plans may have no recipes; preserve that separate form.
	lea Source, a0
	lea Destination, a1
	moveq #1, d1
	bsr.w cloneAbi
	bne.w bad
	move.l d1, PlainHandle
	bsr.w checkPlainClone
	bne.w bad
	move.l #39, ReturnCode  ; transactional malformed-plan rejections
	bsr.w rejectCases
	bne.w bad
	move.l #40, ReturnCode  ; source retirement and replacement allocation
	; Both destination plans must survive poisoned release and reuse of source.
	movea.l Source+memory.Block.Pointer, a0
	move.l Source+memory.Block.Used, d0
poison
	move.b #$cc, (a0)+
	subq.l #1, d0
	bne.w poison
	lea Source, a0
	jsr plans.finish
	move.l #512, d0
	jsr memory.reserveExact
	bne.w bad
	movea.l Source+memory.Block.Pointer, a0
	moveq #127, d0
reuse
	move.l #$deadbeef, (a0)+
	dbra d0, reuse
	move.l #41, ReturnCode  ; clones survive source retirement/reuse
	move.l FirstHandle, d1
	bsr.w checkClone
	bne.w bad
	move.l SecondHandle, d1
	bsr.w checkClone
	bne.w bad
	move.l PlainHandle, d1
	bsr.w checkPlainClone
	bne.w bad
	move.l #42, ReturnCode  ; preexisting destination prefix remains exact
	movea.l Destination+memory.Block.Pointer, a0
	moveq #PREFIX_BYTES/4-1, d1
sentinel
	cmpi.l #$a5a5a5a5, (a0)+
	bne.w bad
	dbra d1, sentinel
	moveq #0, d7
	bra.w done
bad
	moveq #1, d7
done
	lea Source, a0
	jsr plans.finish
	lea Destination, a0
	jsr plans.finish
	move.l d7, d0
	tst.l d0
	rts
	.bend  ; cases

; Verify the constructor before blaming clone copying or offset rebasing.
checkSourceSpelling	.block
	lea Source, a0
	move.l SourceHandle, d1
	jsr plans.resolve
	bne.w bad
	movea.l Source+memory.Block.Pointer, a0
	adda.l plans.HEADER_BYTES+plans.Row.SpellingStart(a1), a0
	lea Spelling, a1
	moveq #SPELLING_BYTES-1, d0
compare
	cmpm.b (a0)+, (a1)+
	bne.w bad
	dbra d0, compare
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; checkSourceSpelling

; Exercise the documented preservation set around a real clone.
cloneAbi	.block
	movem.l d2-d7/a0-a6, Before
	jsr plans.clone
	move.l d0, CloneStatus
	move.l d1, CloneHandle
	movem.l d2-d7/a0-a6, After
	lea Before, a0
	lea After, a1
	moveq #12, d0
compare
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w bad
	dbra d0, compare
	move.l CloneStatus, d0
	move.l CloneHandle, d1
	tst.l d0
	rts
bad
	moveq #-1, d1  ; distinguish ABI failure from a valid rejected clone
	moveq #1, d0
	rts
	.bend  ; cloneAbi

; D1=destination handle. Check every copied row and every spelling-dependent
; recipe field against independent normalized input rows, not the source owner.
checkClone	.block
	move.l #51, ReturnCode  ; cloned plan header extent/count
	lea Destination, a0
	jsr plans.resolve
	bne.w bad
	move.l d1, d4
	subq.l #1, d4
	move.l d4, d5
	addi.l #CLONE_BYTES, d5
	cmp.l plans.Plan.Bytes(a1), d5
	bne.w bad
	cmpi.l #MAIN_ROWS, plans.Plan.Count(a1)
	bne.w bad
	move.l #52, ReturnCode  ; root spelling offsets
	move.l d4, d6
	addi.l #SPELLING_OFFSET, d6
	cmp.l plans.HEADER_BYTES+plans.Row.SpellingStart(a1), d6
	bne.w bad
	move.l d6, d0
	addi.l #SPELLING_BYTES, d0
	cmp.l plans.HEADER_BYTES+plans.Row.SpellingEnd(a1), d0
	bne.w bad
	move.l #53, ReturnCode  ; formal spelling offsets
	move.l d6, d0
	addi.l #10, d0
	cmp.l plans.HEADER_BYTES+plans.ROW_BYTES+plans.Row.SpellingStart(a1), d0
	bne.w bad
	move.l d6, d0
	addi.l #14, d0
	cmp.l plans.HEADER_BYTES+plans.ROW_BYTES+plans.Row.SpellingEnd(a1), d0
	bne.w bad
	move.l #54, ReturnCode  ; packed auxiliary sentinels
	cmpi.l #-1, plans.HEADER_BYTES+plans.Row.Aux2(a1)
	bne.w bad
	cmpi.l #-1, plans.HEADER_BYTES+plans.ROW_BYTES+plans.Row.Aux0(a1)
	bne.w bad
	move.l #55, ReturnCode  ; recipe view/count/owned spelling base
	jsr plans.recipeView
	cmpi.l #plans.RECIPE_READY, d0
	bne.w bad
	cmpi.l #RECIPE_ROWS, d1
	bne.w bad
	cmp.l d6, d2
	bne.w bad
	lea Recipes, a2
	moveq #RECIPE_ROWS-1, d7
recipe
	move.l #56, ReturnCode  ; copied recipe kind and packed offsets
	move.l (a2), d0
	cmp.l (a1), d0
	bne.w bad
	move.l plans.Row.PackedStart(a2), d0
	cmp.l plans.Row.PackedStart(a1), d0
	bne.w bad
	move.l plans.Row.PackedEnd(a2), d0
	cmp.l plans.Row.PackedEnd(a1), d0
	bne.w bad
	move.l #57, ReturnCode  ; recipe spelling offset rebasing
	move.l plans.Row.SpellingStart(a2), d0
	add.l d6, d0
	cmp.l plans.Row.SpellingStart(a1), d0
	bne.w bad
	move.l plans.Row.SpellingEnd(a2), d0
	add.l d6, d0
	cmp.l plans.Row.SpellingEnd(a1), d0
	bne.w bad
	move.l #58, ReturnCode  ; recipe-specific auxiliaries
	move.l plans.Row.Aux0(a2), d0
	move.l plans.Row.Aux1(a2), d3
	cmpi.w #abi.PRVM_RESULT_MACRO_NAMED, plans.Row.Kind(a2)
	bne.w auxiliary
	add.l d6, d0
	add.l d6, d3
auxiliary
	cmp.l plans.Row.Aux0(a1), d0
	bne.w bad
	cmp.l plans.Row.Aux1(a1), d3
	bne.w bad
	move.l plans.Row.Aux2(a2), d0
	cmp.l plans.Row.Aux2(a1), d0
	bne.w bad
	adda.w #plans.ROW_BYTES, a1
	adda.w #plans.ROW_BYTES, a2
	dbra d7, recipe
	move.l #59, ReturnCode  ; complete cloned spelling bytes
	movea.l Destination+memory.Block.Pointer, a0
	adda.l d6, a0
	lea Spelling, a1
	moveq #SPELLING_BYTES-1, d0
compareSpelling
	cmpm.b (a0)+, (a1)+
	bne.w spellingMismatch
	dbra d0, compareSpelling
	moveq #0, d0
	rts
spellingMismatch
	moveq #SPELLING_BYTES-1, d1
	sub.l d0, d1
	addi.l #80, d1  ; exit80..96 identifies the first copied spelling mismatch
	move.l d1, ReturnCode
bad
	moveq #1, d0
	rts
	.bend  ; checkClone

; D1=recipe-free cloned root. Its complete owned spelling remains independent.
checkPlainClone	.block
	lea Destination, a0
	jsr plans.resolve
	bne.w bad
	move.l d1, d4
	subq.l #1, d4
	move.l d4, d0
	addi.l #PLAIN_BYTES, d0
	cmp.l plans.Plan.Bytes(a1), d0
	bne.w bad
	tst.l plans.Plan.Recipes(a1)
	bne.w bad
	addi.l #plans.HEADER_BYTES+MAIN_ROWS*plans.ROW_BYTES, d4
	cmp.l plans.HEADER_BYTES+plans.Row.SpellingStart(a1), d4
	bne.w bad
	jsr plans.recipeView
	cmpi.l #plans.RECIPE_ABSENT, d0
	bne.w bad
	movea.l Destination+memory.Block.Pointer, a0
	adda.l d4, a0
	lea Spelling, a1
	moveq #SPELLING_BYTES-1, d0
compare
	cmpm.b (a0)+, (a1)+
	bne.w bad
	dbra d0, compare
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; checkPlainClone

rejectCases	.block
	lea Source, a0
	move.l SourceHandle, d1
	jsr plans.resolve
	bne.w bad
	movea.l a1, a4
	subq.l #1, plans.HEADER_BYTES+plans.Row.SpellingStart(a4)
	bsr.w rejectClone
	addq.l #1, plans.HEADER_BYTES+plans.Row.SpellingStart(a4)
	tst.l d0
	bne.w bad
	addq.l #2, plans.Plan.Recipes(a4)
	bsr.w rejectClone
	subq.l #2, plans.Plan.Recipes(a4)
	tst.l d0
	bne.w bad
	movea.l Source+memory.Block.Pointer, a3
	adda.l plans.Plan.Recipes(a4), a3
	move.l #plans.MAX_ROWS+1, plans.RecipeSet.Count(a3)
	bsr.w rejectClone
	move.l #RECIPE_ROWS, plans.RecipeSet.Count(a3)
	tst.l d0
	bne.w bad
	lea plans.RecipeSet.Rows+3*plans.ROW_BYTES(a3), a3
	addq.l #1, plans.Row.Aux1(a3)
	bsr.w rejectClone
	subq.l #1, plans.Row.Aux1(a3)
	tst.l d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; rejectCases
; All rejected clones must preserve publication and the complete caller ABI.
rejectClone	.block
	move.l Destination+memory.Block.Used, SavedUsed
	lea Source, a0
	lea Destination, a1
	move.l SourceHandle, d1
	bsr.w cloneAbi
	tst.l d0
	beq.w bad
	tst.l d1
	bne.w bad
	move.l SavedUsed, d0
	cmp.l Destination+memory.Block.Used, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; rejectClone
.endsection
.section data, kind=data
.align 4
MAIN_ROWS = 2
RECIPE_ROWS = 6
PREFIX_BYTES = 16
SPELLING_BYTES = 17
SPELLING_OFFSET = plans.HEADER_BYTES+MAIN_ROWS*plans.ROW_BYTES+plans.RecipeSet.Rows+RECIPE_ROWS*plans.ROW_BYTES
CLONE_BYTES = (SPELLING_OFFSET+SPELLING_BYTES+1)&$fffffffe
PLAIN_BYTES = (plans.HEADER_BYTES+MAIN_ROWS*plans.ROW_BYTES+SPELLING_BYTES+1)&$fffffffe
Generated
	.long Source, Packed, MAIN_ROWS, Packed, MAIN_ROWS, Spelling, SPELLING_BYTES, 0, 0, 0
Packed
	.word abi.PRVM_RESULT_MACRO_LINE, 0
	.long 0, 0, 0, SPELLING_BYTES, 0, 0, -1
	.word abi.PRVM_RESULT_MACRO_FORMAL, 0
	.long 0, 0, 10, 14, -1, 0, 0
Recipes
	.word abi.PRVM_RESULT_MACRO_LITERAL, 0
	.long 0, 0, 0, 6, 0, 0, 0
	.word abi.PRVM_RESULT_MACRO_POSITIONAL, 0
	.long 0, 0, 6, 8, 1, 0, 0
	.word abi.PRVM_RESULT_MACRO_LITERAL, 0
	.long 0, 0, 8, 9, 0, 0, 0
	.word abi.PRVM_RESULT_MACRO_NAMED, 0
	.long 0, 0, 9, 14, 10, 14, 0
	.word abi.PRVM_RESULT_MACRO_LITERAL, 0
	.long 0, 0, 14, 15, 0, 0, 0
	.word abi.PRVM_RESULT_MACRO_SUPPLIED_LIST, 0
	.long 0, 0, 15, 17, 0, 0, 0
Spelling	.byte ".byte .1,.name,.@"
Result	.byte $5a
DosName	.byte "dos.library", 0
OutputName	.byte "Work:macro-plan-clone.bin", 0
.endsection
.section bss, kind=bss
.align 4
Source	.res byte, memory.Block.Used+4
Destination	.res byte, memory.Block.Used+4
SourceHandle	.res long, 1
FirstHandle	.res long, 1
SecondHandle	.res long, 1
PlainHandle	.res long, 1
CloneStatus	.res long, 1
CloneHandle	.res long, 1
SavedUsed	.res long, 1
Before	.res long, 13
After	.res long, 13
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
.endsection
.output "macro-plan-clone.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
