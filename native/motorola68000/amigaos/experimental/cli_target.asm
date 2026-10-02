; Root preamble IO only; canonical tokenizer and shared PRVM own its grammar.
; @opforge-owner: experimental.amigaos.cli_target
	.module experimental.amigaos.cli_target
	.cpu 68020
	.use experimental.amigaos.cli_arguments as args
	.use experimental.amigaos.binary_line_input as lines
	.use tkvm.amigaos.runtime as tokenizer
	.use tkvm.amigaos.state as tkstate
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_runtime as parser
	.pub
OK = 0
ERROR = 1
DOS_OPEN = -30
DOS_CLOSE = -36
MODE_OLDFILE = 1005
LINE_BYTES = 1024
PREAMBLE_LINES = 4096
	.section code, kind=code
; Fill omitted initial CPU from the unconditional root-file preamble/default.
; Inputs A0=args.State,A6=DOS. Explicit CPU/package bypass all source IO.
; Outputs D0/CCR=OK or ERROR. Other registers preserved. TKVM controls restored.
select	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	tst.b args.State.Cpu(a4)
	bne.w bypass
	tst.b args.State.RuntimePackage(a4)
	bne.w bypass
	move.l tkstate.TkvmStepBudget, SavedBudget
	move.l tkstate.TkvmProgramStateTablePtr, SavedTable
	move.l tkstate.TkvmProgramStateCount, SavedCount
	move.w tkstate.TkvmProgramStartState.l, d0
	move.w d0, SavedStart.l
	move.w tkstate.TkvmLastFailureKind.l, d0
	move.w d0, SavedFailure.l
	move.w tkstate.TkvmLastFailureOperand.l, d0
	move.w d0, SavedOperand.l
	move.l #65536, tkstate.TkvmStepBudget
	move.l #StateOffsets, tkstate.TkvmProgramStateTablePtr
	move.l #1, tkstate.TkvmProgramStateCount
	clr.w tkstate.TkvmProgramStartState.l
	lea LineState, a0
	moveq #lines.SCRATCH_BYTES/4-1, d0
clearLineState
	clr.l (a0)+
	dbra d0, clearLineState
	move.l #ReadBuffer, LineState+lines.State.Buffer
	move.l #LINE_BYTES, LineState+lines.State.Capacity
	move.l a6, LineState+lines.State.Dos
	lea args.State.Input(a4), a0
	move.l a0, d1
	move.l #MODE_OLDFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, LineState+lines.State.Handle
	beq.w failed
	lea Request, a0
	moveq #abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clearRequest
	clr.l (a0)+
	dbra d0, clearRequest
	lea Request, a0
	move.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a0)
	move.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a0)
	move.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #abi.PRVM_ENTRY_KIND_TARGET_BOOTSTRAP, abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l #Lexemes, Request+abi.PRVM_FRAME_SOURCE_PTR
	move.l #Tokens, Request+abi.PRVM_FRAME_TOKEN_PTR
	move.l #abi.PRVM_TOKEN_RECORD_SIZE, Request+abi.PRVM_FRAME_TOKEN_RECORD_SIZE
	move.l #BootstrapParser, Request+abi.PRVM_FRAME_PROGRAM_PTR
	move.l #BootstrapParserEnd-BootstrapParser, Request+abi.PRVM_FRAME_PROGRAM_LEN
	move.l #Result, Request+abi.PRVM_FRAME_RESULT_PTR
	move.l #1, Request+abi.PRVM_FRAME_RESULT_CAPACITY
	move.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, Request+abi.PRVM_FRAME_PARSER_CONTRACT_VERSION
	move.l #256, Request+abi.PRVM_FRAME_STEP_BUDGET
	move.l #PREAMBLE_LINES, d5
nextLine
	subq.l #1, d5
	bmi.w failed
	lea LineState, a0
	lea LineBuffer, a1
	move.l #LINE_BYTES, d0
	jsr lines.next
	cmpi.l #lines.EOF, d0
	beq.w endOfFile
	tst.l d0
	bne.w failed
	bra.w tokenize
endOfFile
	tst.l d1
	beq.w defaultTarget
tokenize
	; DOS physical lines may retain a CR from CRLF input.
	tst.l d1
	beq.w lineReady
	lea LineBuffer, a0
	cmpi.b #13, -1(a0, d1.l)
	bne.w lineReady
	subq.l #1, d1
lineReady
	move.l d1, d0
	lea LineBuffer, a0
	lea Tokens, a1
	moveq #64, d1
	lea Lexemes, a2
	move.l #LINE_BYTES, d2
	lea BootstrapTokenizer, a3
	move.l #BootstrapTokenizerEnd-BootstrapTokenizer, d3
	jsr tokenizer.tkvmRun68000
	bne.w failed
	move.l d1, Request+abi.PRVM_FRAME_TOKEN_COUNT
	move.l d3, Request+abi.PRVM_FRAME_SOURCE_LEN
	lea Request, a0
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr parser.run
	bne.w failed
	tst.l d1
	beq.w nextLine
	lea Result, a0
	cmpi.w #abi.PRVM_RESULT_TARGET_BOOTSTRAP, (a0)
	bne.w failed
	cmpi.w #2, 2(a0)
	beq.w defaultTarget
	cmpi.w #1, 2(a0)
	bne.w failed
	lea Lexemes, a0
	adda.l Result+4, a0
	move.l Result+8, d1
	bra.w copyTarget
defaultTarget
	lea BootstrapDefault, a0
	movea.l a0, a1
	moveq #0, d1
measureDefault
	tst.b (a1)+
	beq.w defaultReady
	addq.l #1, d1
	cmpi.l #args.PATH_BYTES-1, d1
	bls.w measureDefault
	bra.w failed
defaultReady
	tst.l d1
	beq.w failed
copyTarget
	lea args.State.Cpu(a4), a1
	subq.l #1, d1
copyByte
	move.b (a0)+, (a1)+
	dbra d1, copyByte
	clr.b (a1)
	moveq #OK, d6
	bra.w cleanup
failed
	moveq #ERROR, d6
cleanup
	move.l LineState+lines.State.Handle, d1
	beq.w restore
	movea.l LineState+lines.State.Dos, a6
	jsr DOS_CLOSE(a6)
	clr.l LineState+lines.State.Handle
restore
	move.l SavedBudget, tkstate.TkvmStepBudget
	move.l SavedTable, tkstate.TkvmProgramStateTablePtr
	move.l SavedCount, tkstate.TkvmProgramStateCount
	move.w SavedStart.l, d0
	move.w d0, tkstate.TkvmProgramStartState.l
	move.w SavedFailure.l, d0
	move.w d0, tkstate.TkvmLastFailureKind.l
	move.w SavedOperand.l, d0
	move.w d0, tkstate.TkvmLastFailureOperand.l
	move.l d6, d0
	bra.w done
bypass
	moveq #OK, d0
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; select
	.endsection
	.section data, kind=data
	.align 4
StateOffsets	.long 0
	.include "target_bootstrap.i"
	.endsection
	.section bss, kind=bss
	.align 4
SavedBudget	.res long, 1
SavedTable	.res long, 1
SavedCount	.res long, 1
SavedStart	.res word, 1
SavedFailure	.res word, 1
SavedOperand	.res word, 1
	.align 4
LineState	.res byte, lines.SCRATCH_BYTES
Request	.res byte, abi.PRVM_REQUEST_FRAME_SIZE
Result	.res byte, abi.PRVM_RESULT_RECORD_SIZE
Tokens	.res byte, 64*abi.PRVM_TOKEN_RECORD_SIZE
ReadBuffer	.res byte, LINE_BYTES
LineBuffer	.res byte, LINE_BYTES
Lexemes	.res byte, LINE_BYTES
	.endsection
	.endmodule
