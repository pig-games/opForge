; Shared request construction for explicitly physical or generated spelling.
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a4
	movea.l macro_plans.Frame.Events(a4), a1
	move.l macro_plans.Row.SpellingStart(a1), d1
	move.l macro_plans.Row.SpellingEnd(a1), d2
	sub.l d1, d2
	movea.l macro_plans.Frame.Source(a4), a2
	adda.l d1, a2
	lea MACRO_REQUEST(a6), a0
	movea.l a0, a1
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clear
	clr.l (a1)+
	dbra d0, clear
	move.l #parser_abi.PRVM_MAGIC_OPRP, parser_abi.PRVM_FRAME_MAGIC(a0)
	move.w #parser_abi.PRVM_ABI_VERSION_V1, parser_abi.PRVM_FRAME_ABI_VERSION(a0)
	move.w #parser_abi.PRVM_REQUEST_FRAME_SIZE, parser_abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #parser_abi.PRVM_ENTRY_KIND_MACRO_FRAGMENTS, parser_abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l a2, parser_abi.PRVM_FRAME_SOURCE_PTR(a0)
	move.l d2, parser_abi.PRVM_FRAME_SOURCE_LEN(a0)
	movea.l Frame.Package(a5), a1
	move.l package.Header.MacroFragmentsBytes(a1), parser_abi.PRVM_FRAME_PROGRAM_LEN(a0)
	adda.l package.Header.MacroFragments(a1), a1
	move.l a1, parser_abi.PRVM_FRAME_PROGRAM_PTR(a0)
	movea.l a6, a1
	adda.l #MACRO_SPELL_SCRATCH, a1
	move.l a1, parser_abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #64*parser_abi.PRVM_RESULT_RECORD_SIZE, parser_abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #parser_abi.PRVM_PARSER_CONTRACT_VERSION_V2, parser_abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a0)
	move.l #65536, parser_abi.PRVM_FRAME_STEP_BUDGET(a0)
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr macro_runtime.run
	bne.w done
	move.l d1, macro_plans.Frame.RecipeCount(a4)
	movea.l a6, a1
	adda.l #MACRO_SPELL_SCRATCH, a1
	move.l a1, macro_plans.Frame.RecipeEvents(a4)
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
