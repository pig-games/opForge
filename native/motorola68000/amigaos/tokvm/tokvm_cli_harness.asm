; Host-facing CLI harness module.
;
; Owns the AmigaOS Shell bridge, report formatting surface, and caller-owned
; buffers. Imports the tokenizer VM contract from tokvm.amigaos.tokenizer_vm.

	.module tokvm.amigaos.cli_harness
	.cpu 68020
	.pub
	.use tokvm.amigaos.tokenizer_vm (tokvmRun68000, tokvmSetStepBudget68000)
	.use tokvm.amigaos.tokenizer_vm (DemoProgram, DemoProgramLen, TOKVM_DEFAULT_MAX_STEPS_PER_LINE)
	.use tokvm.amigaos.tokenizer_vm (TOKEN_BUFFER_CAPACITY, TOKEN_RECORD_SIZE)
	.use tokvm.amigaos.tokenizer_vm (SOURCE_BUFFER_CAPACITY, SCRATCH_BUFFER_CAPACITY)
	.use tokvm.amigaos.tokenizer_vm (TK_STATUS_VM_FAILURE, TK_STATUS_INVALID_PROGRAM, TK_KIND_OP_LT)

; ---------------------------------------------------------------------------
; AmigaOS Exec/DOS offsets used by the CLI harness layer.
; ---------------------------------------------------------------------------

SYS_BASE                        = 4

PR_CLI                          = 172
PR_MSG_PORT                     = 92

OPEN_LIBRARY                    = -552
CLOSE_LIBRARY                   = -414
FIND_TASK                       = -294
WAIT_PORT                       = -384
GET_MSG                         = -372
REPLY_MSG                       = -378
FORBID                          = -132

OPEN                            = -30
CLOSE                           = -36
READ                            = -42
WRITE                           = -48
OUTPUT                          = -60
IO_ERR                          = -132
GET_ARG_STR                     = -534

MODE_OLDFILE                    = 1005
MODE_NEWFILE                    = 1006

; Process exit codes returned from the Amiga entry path.
RETURN_OK                       = 0
RETURN_USAGE                    = 20
RETURN_FILE_FAILURE             = 21
RETURN_INPUT_TOO_LARGE          = 22
RETURN_VM_FAILURE               = 23
RETURN_OUTPUT_FAILURE           = 24
RETURN_WORKBENCH_UNSUPPORTED    = 25

; Negative harness statuses are report-visible host failures that occur before
; or around VM execution.
HARNESS_STATUS_USAGE            = -100
HARNESS_STATUS_QUOTED_PATH      = -101
HARNESS_STATUS_INPUT_OPEN       = -102
HARNESS_STATUS_INPUT_READ       = -103
HARNESS_STATUS_INPUT_TOO_LARGE  = -104
HARNESS_STATUS_OUTPUT_OPEN      = -105

PATH_BUFFER_CAPACITY            = 256

; globals is a tiny process-local host state block shared across the harness.
; Offsets are intentionally fixed because A4 stays pinned to this base while the
; report writer and DOS wrappers call each other.
GLOBALS_DOS_BASE                = 0
GLOBALS_STDOUT_HANDLE           = 4
GLOBALS_LAST_IOERR              = 8
GLOBALS_OUTPUT_HANDLE           = 12
GLOBALS_SIZE                    = 16

	.section code, kind=code

	.pub

; ---------------------------------------------------------------------------
; Host-facing harness.
;
; This is the native equivalent of the Rust tokenizer bridge setup:
; 1. initialize DOS access
; 2. obtain and parse CLI arguments
; 3. read exactly one single-line source buffer plus a one-byte overflow probe
; 4. invoke tokvm_run_68000 with caller-owned buffers and demoProgram
; 5. render the OPFORGE-TOKVM 1 report from the native token buffer
; ---------------------------------------------------------------------------

tokvmAmigaosCliHarnessRun	.block
	movem.l d2-d7/a2-a6, -(sp)
	lea Globals, a4  ; shared host state block: DOS base, stdout, output handle, last IoErr
	moveq #RETURN_OK, d7  ; optimistic Shell return until a host or VM failure overrides it

	bsr.w amigaosCliFileioInit  ; mirrors Rust-side host bootstrap: open DOS and discover stdout
	tst.l d0
	bne.w cleanup

	bsr.w amigaosCliFileioGetArgStr  ; DOS GetArgStr provides the raw Shell argument tail
	bsr.w parseArgs  ; native spec is intentionally fixed: <input-path> <output-path>
	tst.l d0
	beq.w argsParsed
	moveq #RETURN_USAGE, d7
	move.l GLOBALS_STDOUT_HANDLE(a4), d1
	cmpi.l #HARNESS_STATUS_QUOTED_PATH, d0
	bne.s usage
	lea QuotedPathMessage, a0
	bsr.w amigaosCliFileioWriteCstr
	bra.w cleanup

usage
	lea UsageMessage, a0
	bsr.w amigaosCliFileioWriteCstr
	bra.w cleanup

argsParsed
	lea OutputPathBuffer, a0  ; open the report target first so later failure paths can still emit report text
	bsr.w amigaosCliFileioOpenOutput
	tst.l d0
	bne.s outputOpened
	moveq #RETURN_OUTPUT_FAILURE, d7
	move.l GLOBALS_STDOUT_HANDLE(a4), d1
	lea OutputOpenMessage, a0
	bsr.w amigaosCliFileioWriteCstr
	bra.w cleanup

outputOpened
	move.l d0, GLOBALS_OUTPUT_HANDLE(a4)
	move.l d0, d6  ; keep the output handle live for report writes and final cleanup
	lea InputPathBuffer, a0
	bsr.w amigaosCliFileioOpenInput
	tst.l d0
	bne.s inputOpened
	moveq #RETURN_FILE_FAILURE, d7
	move.l #HARNESS_STATUS_INPUT_OPEN, d0
	bsr.w writeFailureReport
	tst.l d0
	beq.w cleanup
	moveq #RETURN_OUTPUT_FAILURE, d7
	bra.w cleanup

inputOpened
	move.l d0, d5  ; input handle lives only until the bounded single-line read completes
	lea SourceBuffer, a0
	move.l #SOURCE_BUFFER_CAPACITY, d0  ; tokvm native ABI takes a caller-owned contiguous source slice
	move.l d5, d1
	bsr.w amigaosCliFileioRead
	cmp.l #-1, d0
	bne.s readOk
	move.l d5, d1
	bsr.w amigaosCliFileioClose
	moveq #RETURN_FILE_FAILURE, d7
	move.l #HARNESS_STATUS_INPUT_READ, d0
	bsr.w writeFailureReport
	tst.l d0
	beq.w cleanup
	moveq #RETURN_OUTPUT_FAILURE, d7
	bra.w cleanup

readOk
	move.l d0, d4  ; D4 becomes source byte length, matching tokvm_run_68000 input ABI
	lea InputProbeByte, a0
	moveq #1, d0  ; one-byte overflow probe enforces the spec's bounded single-line input rule
	move.l d5, d1
	bsr.w amigaosCliFileioRead
	move.l d0, d3
	cmp.l #-1, d0
	bne.s probeOk
	move.l d5, d1
	bsr.w amigaosCliFileioClose
	moveq #RETURN_FILE_FAILURE, d7
	move.l #HARNESS_STATUS_INPUT_READ, d0
	bsr.w writeFailureReport
	tst.l d0
	beq.w cleanup
	moveq #RETURN_OUTPUT_FAILURE, d7
	bra.w cleanup

probeOk
	move.l d5, d1
	bsr.w amigaosCliFileioClose
	tst.l d0
	beq.s probeClosed
	moveq #RETURN_FILE_FAILURE, d7
	move.l #HARNESS_STATUS_INPUT_READ, d0
	bsr.w writeFailureReport
	tst.l d0
	beq.w cleanup
	moveq #RETURN_OUTPUT_FAILURE, d7
	bra.w cleanup

probeClosed
	tst.l d3  ; any extra byte means the caller exceeded SOURCE_BUFFER_CAPACITY
	beq.s invokeVm
	moveq #RETURN_INPUT_TOO_LARGE, d7
	move.l #HARNESS_STATUS_INPUT_TOO_LARGE, d0
	bsr.w writeFailureReport
	tst.l d0
	beq.w cleanup
	moveq #RETURN_OUTPUT_FAILURE, d7
	bra.w cleanup

invokeVm
	; Register ABI for tokvm_run_68000:
	; A0/D0 source slice, A1/D1 token buffer+capacity, A2/D2 scratch buffer+capacity,
	; A3/D3 demo bytecode pointer+length. This mirrors the native contract documented
	; in tokvm_tokenizer_vm.asm and used by the Rust-side bridge tests.
	move.l #TOKVM_DEFAULT_MAX_STEPS_PER_LINE, d0
	jsr tokvmSetStepBudget68000
	lea SourceBuffer, a0
	move.l d4, d0
	lea TokenBuffer, a1
	move.l #TOKEN_BUFFER_CAPACITY, d1
	lea LexemeScratch, a2
	move.l #SCRATCH_BUFFER_CAPACITY, d2
	lea DemoProgram, a3
	move.l DemoProgramLen, d3
	jsr tokvmRun68000

	move.l d0, d4  ; status
	move.l d1, d5  ; emitted token count
	move.l d2, d6  ; final source cursor / column end
	tst.l d4
	beq.s writeVmReport
	moveq #RETURN_VM_FAILURE, d7

writeVmReport
	move.l d4, d0
	move.l d5, d1
	move.l d6, d2
	bsr.w writeReport  ; render the OPFORGE-TOKVM 1 report consumed by asm regression tests
	tst.l d0
	beq.s cleanup
	moveq #RETURN_OUTPUT_FAILURE, d7

cleanup
	move.l GLOBALS_OUTPUT_HANDLE(a4), d1  ; close the report handle even after VM or formatting failures
	beq.s shutdown
	bsr.w amigaosCliFileioClose
	clr.l GLOBALS_OUTPUT_HANDLE(a4)
	tst.l d0
	beq.s shutdown
	moveq #RETURN_OUTPUT_FAILURE, d7

shutdown
	bsr.w amigaosCliFileioShutdown
	move.l d7, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; tokvmAmigaosCliHarnessRun

; Parse exactly two unquoted Shell paths from DOS GetArgStr output.
; This stays intentionally narrower than a full command-line parser because the
; harness spec defines a fixed tokvm <input-path> <output-path> contract.
parseArgs .block
	movem.l d2-d7/a2-a6, -(sp)
	movea.l a0, a3  ; A3 walks the raw DOS argument tail in-place
	bsr.s skipWhitespace  ; align with the Rust test helper that trims surrounding Shell whitespace
	tst.b (a3)
	beq.s argsMissing
	cmpi.b #'"', (a3)  ; quoted paths stay unsupported in this narrow first-shell contract
	beq.s argsQuoted
	lea InputPathBuffer, a1
	bsr.s copyToken  ; copy token 0 => input path
	tst.l d0
	bne.s argsDone
	bsr.s skipWhitespace
	tst.b (a3)
	beq.s argsMissing
	cmpi.b #'"', (a3)
	beq.s argsQuoted
	lea OutputPathBuffer, a1
	bsr.s copyToken  ; copy token 1 => output path
	tst.l d0
	bne.s argsDone
	bsr.s skipWhitespace
	tst.b (a3)
	bne.s argsMissing
	moveq #0, d0
	bra.s argsDone

argsMissing
	move.l #HARNESS_STATUS_USAGE, d0
	bra.s argsDone

argsQuoted
	move.l #HARNESS_STATUS_QUOTED_PATH, d0

argsDone
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; parseArgs

skipWhitespace .block
	cmpi.b #' ', (a3)
	beq.s skipOne
	cmpi.b #9, (a3)
	beq.s skipOne
	cmpi.b #10, (a3)
	beq.s skipOne
	cmpi.b #13, (a3)
	bne.s skipDone
skipOne
	addq.l #1, a3
	bra.s skipWhitespace

skipDone
	rts
	.bend ; skipWhitespace

; Copy one CLI path token into the caller-selected buffer.
; This intentionally implements a narrow Shell token grammar rather than full
; quote/escape handling because the tokvm host spec only accepts raw paths.
copyToken .block
	move.l #PATH_BUFFER_CAPACITY - 1, d6  ; reserve space for the trailing NUL DOS expects
copyLoop
	moveq #0, d0
	move.b (a3), d0
	beq.s copyFinish
	cmpi.b #' ', d0
	beq.s copyFinish
	cmpi.b #9, d0
	beq.s copyFinish
	cmpi.b #10, d0
	beq.s copyFinish
	cmpi.b #13, d0
	beq.s copyFinish
	cmpi.b #'"', d0
	beq.s copyQuoted
	tst.l d6
	beq.s copyOverflow
	move.b d0, (a1)+  ; preserve the raw path byte; no shell-style unescaping in this slice
	addq.l #1, a3
	subq.l #1, d6
	bra.s copyLoop

copyFinish
	clr.b (a1)
	moveq #0, d0
	rts

copyQuoted
	move.l #HARNESS_STATUS_QUOTED_PATH, d0
	rts

copyOverflow
	move.l #HARNESS_STATUS_USAGE, d0
	rts
	.bend ; copyToken

; Failure reports reuse the same report writer as success reports so the file
; format stays identical across success and host/VM failure paths.
writeFailureReport .block
	clr.l d1
	clr.l d2
	clr.l d3
	bra.w writeReport
	.bend ; writeFailureReport

writeReport .block
	movem.l d2-d7/a2-a6, -(sp)
	move.l d0, d4
	move.l d1, d5
	move.l d2, d6
	; Reject malformed native buffers before formatting token lines so bad VM state
	; degrades to TK_STATUS_VM_FAILURE instead of emitting invalid report metadata.
	move.l d3, d2
	bsr.w validateVmResult
	tst.l d0
	beq.s reportValidated
	moveq #TK_STATUS_VM_FAILURE, d4
	moveq #0, d5
	moveq #0, d6
	clr.l d2
reportValidated
	move.l GLOBALS_OUTPUT_HANDLE(a4), d7  ; all report lines flow through the already-open DOS output handle

	; The report header and scalar fields intentionally match the golden
	; OPFORGE-TOKVM 1 text format exercised in crates/opforge-asm/src/tests.rs.
	move.l d7, d1
	lea ReportHeader, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea ReportStatusPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	move.l d4, d0
	bsr.w writeI32
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea NewlineString, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea ReportTokensPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	move.l d5, d0
	bsr.w writeU32
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea NewlineString, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea ReportCursorPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	move.l d6, d0
	bsr.w writeU32
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea NewlineString, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	; Walk the native 20-byte token records and re-expand them into the
	; stable text report surface consumed by the asm reference tests.
	lea TokenBuffer, a5  ; native 20-byte token records that the asm tests decode back into report lines
	moveq #0, d3
reportTokenLoop
	cmp.l d5, d3
	bcc.w reportEnd

	move.l d7, d1
	lea ReportTokenPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	move.l d3, d0
	bsr.w writeU32
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea ReportKindPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	moveq #0, d0
	move.w (a5), d0
	bsr.w kindName  ; record kind code -> PortableTokenKind/report name mapping
	move.l d7, d1
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea ReportStartPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	move.l 4(a5), d0
	bsr.w writeU32
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea ReportEndPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	move.l 8(a5), d0
	bsr.w writeU32
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea ReportLenPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	move.l 16(a5), d0
	bsr.w writeU32
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea ReportLexhexPrefix, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	movea.l a5, a6  ; A5 points at the current record, A6 is a temporary for offseted fields
	move.l 12(a6), d0
	lea LexemeScratch, a0
	adda.l d0, a0  ; lexemeScratch + offset => the exact byte slice emitted by the native scanner
	move.l 16(a6), d0
	move.l d7, d1
	bsr.w writeHexBytes
	tst.l d0
	bne.w reportFail

	move.l d7, d1
	lea NewlineString, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail

	adda.l #TOKEN_RECORD_SIZE, a5
	addq.l #1, d3
	bra.w reportTokenLoop

reportEnd
	; END terminates both success and failure reports so the file format is
	; self-delimiting even when token count is zero.
	move.l d7, d1
	lea ReportEndLine, a0
	bsr.w amigaosCliFileioWriteCstr
	tst.l d0
	bne.w reportFail
	moveq #0, d0
	bra.w reportDone

reportFail
	moveq #1, d0

reportDone
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; writeReport

; Defensive validation in front of report formatting.
; The Rust runtime validates token shapes before exposing PortableToken output;
; this native harness mirrors that expectation so malformed native state falls
; back to TK_STATUS_VM_FAILURE instead of emitting invalid token metadata.
validateVmResult .block
	tst.l d4  ; non-negative statuses are TK_STATUS_* VM results, negative are harness failures
	bmi validateNegativeStatus
	cmpi.l #TK_STATUS_INVALID_PROGRAM, d4
	bgt validateInvalid
	cmp.l #TOKEN_BUFFER_CAPACITY, d5
	bhi validateInvalid
	cmp.l #SOURCE_BUFFER_CAPACITY, d6
	bhi validateInvalid
	cmp.l #SCRATCH_BUFFER_CAPACITY, d2
	bhi validateInvalid

	lea TokenBuffer, a0
	moveq #0, d1
validateTokenLoop
	cmp.l d5, d1
	bcc.s validateOk

	moveq #0, d0
	move.w (a0), d0
	cmpi.l #TK_KIND_OP_LT, d0  ; last valid kind in the native PortableTokenKind mirror table
	bgt validateInvalid

	move.l 4(a0), d0
	tst.l d0
	beq validateInvalid
	cmpi.l #SOURCE_BUFFER_CAPACITY + 1, d0
	bhi validateInvalid

	move.l 8(a0), d0
	tst.l d0
	beq validateInvalid
	cmpi.l #SOURCE_BUFFER_CAPACITY + 1, d0
	bhi validateInvalid
	cmp.l 4(a0), d0
	blt validateInvalid

	move.l 12(a0), d0  ; lexeme offset must stay inside the committed scratch prefix
	cmp.l d2, d0
	bhi validateInvalid
	move.l 16(a0), d3
	move.l d0, d7
	add.l d3, d7
	cmp.l d2, d7
	bhi validateInvalid

	adda.l #TOKEN_RECORD_SIZE, a0
	addq.l #1, d1
	bra.s validateTokenLoop

validateNegativeStatus
	; Negative statuses are host-side harness failures. When one of those is
	; reported, no VM-owned token or scratch state is allowed to leak out.
	cmpi.l #HARNESS_STATUS_USAGE, d4
	bgt.s validateInvalid
	cmpi.l #HARNESS_STATUS_OUTPUT_OPEN, d4
	blt.s validateInvalid
	tst.l d5
	bne.s validateInvalid
	tst.l d6
	bne.s validateInvalid
	tst.l d2
	bne.s validateInvalid

validateOk
	moveq #0, d0
	rts

validateInvalid
	moveq #1, d0
	rts
	.bend ; validateVmResult

; Signed report writer used for STATUS fields so host failures can emit their
; negative HARNESS_STATUS_* values without special formatting logic.
writeI32 .block
	tst.l d0
	bpl.s writeI32Unsigned
	move.l d2, -(sp)
	move.l d0, d2
	move.l d1, -(sp)
	lea MinusString, a0
	bsr.w amigaosCliFileioWriteCstr
	move.l (sp)+, d1
	tst.l d0
	bne.s writeI32NegativeDone
	move.l d2, d0
	neg.l d0
	bsr.w writeU32
writeI32NegativeDone
	move.l (sp)+, d2
	rts
writeI32Unsigned
	bsr.w writeU32
writeI32Done
	rts
	.bend ; writeI32

; Minimal decimal formatter. Values are accumulated right-to-left in a scratch
; buffer and then flushed in one exact DOS write.
writeU32 .block
	movem.l d2-d7/a2-a6, -(sp)
	lea DecimalBufferEnd, a0  ; emit backwards into scratch, then flush the exact decimal slice once
	moveq #0, d2
	tst.l d0
	bne.s loop
	move.b #'0', -(a0)
	moveq #1, d2
	bra.s emit

loop
	divu.w #10, d0
	move.w d0, d3
	swap d0
	addi.w #'0', d0
	move.b d0, -(a0)
	addq.l #1, d2
	moveq #0, d0
	move.w d3, d0
	tst.l d0
	bne.s loop

emit
	move.l d2, d0
	bsr.w amigaosCliFileioWriteExact
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; writeU32

; Render a lexeme payload as uppercase hexadecimal bytes. This matches the
; LEXHEX field expected by the native report rendering tests.
writeHexBytes .block
	movem.l d2-d7/a2-a6, -(sp)
	movea.l a0, a6
	move.l d0, d5  ; remaining byte count for the report's LEXHEX field
	lea HexDigits, a5
loop
	tst.l d5
	beq.s done
	moveq #0, d2
	move.b (a6)+, d2
	move.l d2, d3
	lsr.b #4, d3
	andi.w #$000F, d3
	andi.w #$000F, d2
	move.b 0(a5, d3.W), HexPairBuffer
	moveq #0, d4
	move.b 0(a5, d2.W), d4
	lea HexPairBuffer, a1
	move.b d4, 1(a1)
	lea HexPairBuffer, a0
	moveq #2, d0
	bsr.w amigaosCliFileioWriteExact
	tst.l d0
	bne.s done
	subq.l #1, d5
	bra.s loop

done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; writeHexBytes

; Token kind name lookup for report emission.
; The table order is locked to the TK_KIND_* numeric encoding exported by the
; native tokenizer VM module.
kindName .block
	cmp.l #TK_KIND_OP_LT, d0
	bhi.s kindUnknown
	add.l d0, d0
	add.l d0, d0
	lea KindNamePtrs, a1
	movea.l 0(a1, d0.l), a0
	rts

kindUnknown
	lea KindNameUnknown, a0
	rts
	.bend ; kindName

; ---------------------------------------------------------------------------
; Minimal DOS helper layer used by the harness.
;
; These routines intentionally stay thin: all tokenizer semantics live in the
; VM below, while this layer only handles DOS open/read/write/close concerns.
; ---------------------------------------------------------------------------

amigaosCliFileioInit .block
	movem.l d2-d7/a2-a6, -(sp)
	; Reset host globals first so every early-exit path can safely call the
	; shared shutdown/cleanup logic.
	clr.l GLOBALS_DOS_BASE(a4)
	clr.l GLOBALS_STDOUT_HANDLE(a4)
	clr.l GLOBALS_LAST_IOERR(a4)
	clr.l GLOBALS_OUTPUT_HANDLE(a4)
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.W, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.s fail
	move.l d0, GLOBALS_DOS_BASE(a4)
	movea.l d0, a6
	jsr OUTPUT(a6)
	move.l d0, GLOBALS_STDOUT_HANDLE(a4)
	moveq #0, d0
	bra.w done

fail
	moveq #1, d0

done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; amigaosCliFileioInit

; Release DOS library state once the harness is finished with all host I/O.
amigaosCliFileioShutdown .block
	movem.l d2-d7/a2-a6, -(sp)
	move.l GLOBALS_DOS_BASE(a4), d0
	beq.s done
	movea.l d0, a1
	movea.l SYS_BASE.W, a6
	jsr CLOSE_LIBRARY(a6)
	clr.l GLOBALS_DOS_BASE(a4)

done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; amigaosCliFileioShutdown

; DOS GetArgStr returns the raw argument tail for the current Shell process.
amigaosCliFileioGetArgStr .block
	movem.l d2-d7/a2-a6, -(sp)
	movea.l GLOBALS_DOS_BASE(a4), a6
	jsr GET_ARG_STR(a6)
	movea.l d0, a0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; amigaosCliFileioGetArgStr

amigaosCliFileioOpenInput .block	
	movem.l d2-d7/a2-a6, -(sp)
	; Open the source file in read-only mode and cache IoErr on failure so
	; later diagnostics or debugging can inspect the DOS reason code.
	move.l a0, d1
	move.l #MODE_OLDFILE, d2
	movea.l GLOBALS_DOS_BASE(a4), a6
	jsr OPEN(a6)
	tst.l d0
	bne.s done
	bsr.w amigaosCliFileioCaptureIoerr

done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; amigaosCliFileioOpenInput

; Create/truncate the report target before any input work happens so all later
; failure paths can still emit a report file instead of only returning a code.
amigaosCliFileioOpenOutput .block
	movem.l d2-d7/a2-a6, -(sp)
	move.l a0, d1
	move.l #MODE_NEWFILE, d2
	movea.l GLOBALS_DOS_BASE(a4), a6
	jsr OPEN(a6)
	tst.l d0
	bne.s done
	bsr.w amigaosCliFileioCaptureIoerr

done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; amigaosCliFileioOpenOutput

; Thin DOS Read wrapper. The harness uses it both for the bounded source read
; and the one-byte overflow probe that enforces the single-line capacity rule.
amigaosCliFileioRead .block
	movem.l d2-d7/a2-a6, -(sp)
	move.l a0, d2  ; DOS Read uses D2=buffer, D3=len in the helper's stable calling convention
	move.l d0, d3
	movea.l GLOBALS_DOS_BASE(a4), a6
	jsr READ(a6)
	cmp.l #-1, d0
	bne.s done
	bsr.w amigaosCliFileioCaptureIoerr

done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; amigaosCliFileioRead

; Count a NUL-terminated string and forward it to the exact-byte write helper.
amigaosCliFileioWriteCstr .block
	movem.l d2-d7/a2-a6, -(sp)
	movea.l a0, a2
	moveq #0, d0
loop
	tst.b (a0)+
	beq.s emit
	addq.l #1, d0
	bra.s loop

emit
	movea.l a2, a0
	bsr.w amigaosCliFileioWriteExact
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; amigaosCliFileioWriteCstr

; Preserve D1 across DOS Write calls because the report writer keeps the output handle
; in D1 while emitting decimal fields and repeated LEXHEX byte pairs.
amigaosCliFileioWriteExact .block
	movem.l d1-d7/a2-a6, -(sp)
	move.l d0, d3
	move.l a0, d2
	movea.l GLOBALS_DOS_BASE(a4), a6
	jsr WRITE(a6)
	cmp.l #-1, d0
	beq.s fail
	cmp.l d3, d0
	bne.w short
	moveq #0, d0
	bra.w done

fail
	bsr.w amigaosCliFileioCaptureIoerr
	moveq #1, d0
	bra.w done

short
	clr.l GLOBALS_LAST_IOERR(a4)
	moveq #1, d0

done
	movem.l (sp)+, d1-d7/a2-a6
	rts
	.bend ; amigaosCliFileioWriteExact

; Close a DOS file handle and preserve IoErr if the close itself fails.
amigaosCliFileioClose .block
	movem.l d2-d7/a2-a6, -(sp)
	movea.l GLOBALS_DOS_BASE(a4), a6
	jsr CLOSE(a6)
	tst.l d0
	bne.w ok
	bsr.w amigaosCliFileioCaptureIoerr
	moveq #1, d0
	bra.w done

ok
	moveq #0, d0

done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend ; amigaosCliFileioClose

; Cache the last DOS IoErr in globals so callers do not need to immediately
; inspect D0 after every failed host I/O operation.
amigaosCliFileioCaptureIoerr .block
	movea.l GLOBALS_DOS_BASE(a4), a6
	jsr IO_ERR(a6)
	move.l d0, GLOBALS_LAST_IOERR(a4)
	rts
	.bend ; amigaosCliFileioCaptureIoerr

	.endsection
	.section data, kind=data

; Static strings and lookup tables for the host/report layer.
DosName
	.byte "dos.library", 0

UsageMessage
	.byte "Usage: tokvm <input-path> <output-path>", 10, 0
QuotedPathMessage
	.byte "tokvm: quoted paths are not supported", 10, 0
OutputOpenMessage
	.byte "tokvm: failed to open output file", 10, 0

ReportHeader
	.byte "OPFORGE-TOKVM 1", 10, 0
ReportStatusPrefix
	.byte "STATUS ", 0
ReportTokensPrefix
	.byte "TOKENS ", 0
ReportCursorPrefix
	.byte "CURSOR ", 0
ReportTokenPrefix
	.byte "TOKEN ", 0
ReportKindPrefix
	.byte " KIND ", 0
ReportStartPrefix
	.byte " START ", 0
ReportEndPrefix
	.byte " END ", 0
ReportLenPrefix
	.byte " LEN ", 0
ReportLexhexPrefix
	.byte " LEXHEX ", 0
ReportEndLine
	.byte "END", 10, 0
NewlineString
	.byte 10, 0
MinusString
	.byte "-", 0
HexDigits
	.byte "0123456789ABCDEF"

KindNameIdentifier
	.byte "identifier", 0
KindNameRegister
	.byte "register", 0
KindNameNumber
	.byte "number", 0
KindNameString
	.byte "string", 0
KindNameComma
	.byte "comma", 0
KindNameColon
	.byte "colon", 0
KindNameDollar
	.byte "dollar", 0
KindNameDot
	.byte "dot", 0
KindNameHash
	.byte "hash", 0
KindNameQuestion
	.byte "question", 0
KindNameOpenBracket
	.byte "open_bracket", 0
KindNameCloseBracket
	.byte "close_bracket", 0
KindNameOpenBrace
	.byte "open_brace", 0
KindNameCloseBrace
	.byte "close_brace", 0
KindNameOpenParen
	.byte "open_paren", 0
KindNameCloseParen
	.byte "close_paren", 0
KindNameOpRange
	.byte "op_range", 0
KindNameOpRangeInclusive
	.byte "op_range_inclusive", 0
KindNameOpPlus
	.byte "op_plus", 0
KindNameOpMinus
	.byte "op_minus", 0
KindNameOpMultiply
	.byte "op_multiply", 0
KindNameOpPower
	.byte "op_power", 0
KindNameOpDivide
	.byte "op_divide", 0
KindNameOpMod
	.byte "op_mod", 0
KindNameOpShl
	.byte "op_shl", 0
KindNameOpShr
	.byte "op_shr", 0
KindNameOpBitNot
	.byte "op_bit_not", 0
KindNameOpLogicNot
	.byte "op_logic_not", 0
KindNameOpBitAnd
	.byte "op_bit_and", 0
KindNameOpBitOr
	.byte "op_bit_or", 0
KindNameOpBitXor
	.byte "op_bit_xor", 0
KindNameOpLogicAnd
	.byte "op_logic_and", 0
KindNameOpLogicOr
	.byte "op_logic_or", 0
KindNameOpLogicXor
	.byte "op_logic_xor", 0
KindNameOpEq
	.byte "op_eq", 0
KindNameOpNe
	.byte "op_ne", 0
KindNameOpGe
	.byte "op_ge", 0
KindNameOpGt
	.byte "op_gt", 0
KindNameOpLe
	.byte "op_le", 0
KindNameOpLt
	.byte "op_lt", 0
KindNameUnknown
	.byte "unknown", 0

	.align 4
; kindNamePtrs is indexed directly by the TK_KIND_* numeric code.
; Each entry expands the compact native token record into the report text name
; that matches PortableTokenKind-oriented test expectations.
KindNamePtrs
	.long KindNameIdentifier
	.long KindNameRegister
	.long KindNameNumber
	.long KindNameString
	.long KindNameComma
	.long KindNameColon
	.long KindNameDollar
	.long KindNameDot
	.long KindNameHash
	.long KindNameQuestion
	.long KindNameOpenBracket
	.long KindNameCloseBracket
	.long KindNameOpenBrace
	.long KindNameCloseBrace
	.long KindNameOpenParen
	.long KindNameCloseParen
	.long KindNameOpRange
	.long KindNameOpRangeInclusive
	.long KindNameOpPlus
	.long KindNameOpMinus
	.long KindNameOpMultiply
	.long KindNameOpPower
	.long KindNameOpDivide
	.long KindNameOpMod
	.long KindNameOpShl
	.long KindNameOpShr
	.long KindNameOpBitNot
	.long KindNameOpLogicNot
	.long KindNameOpBitAnd
	.long KindNameOpBitOr
	.long KindNameOpBitXor
	.long KindNameOpLogicAnd
	.long KindNameOpLogicOr
	.long KindNameOpLogicXor
	.long KindNameOpEq
	.long KindNameOpNe
	.long KindNameOpGe
	.long KindNameOpGt
	.long KindNameOpLe
	.long KindNameOpLt

	.endsection
	.section bss, kind=bss

; Caller-owned and harness-owned buffers.
; tokenBuffer and lexemeScratch together form the native token ABI surface that
; tokvm_amigaos_cli_harness_write_report rehydrates into OPFORGE-TOKVM 1 lines.
	.align 4
Globals
	.res byte, GLOBALS_SIZE
InputPathBuffer
	.res byte, PATH_BUFFER_CAPACITY
OutputPathBuffer
	.res byte, PATH_BUFFER_CAPACITY
SourceBuffer
	.res byte, SOURCE_BUFFER_CAPACITY
TokenBuffer
	.res byte, TOKEN_RECORD_SIZE * TOKEN_BUFFER_CAPACITY
LexemeScratch
	.res byte, SCRATCH_BUFFER_CAPACITY
DecimalBuffer
	.res byte, 16
DecimalBufferEnd
HexPairBuffer
	.res byte, 2
InputProbeByte
	.res byte, 1

	.endsection
	.endmodule
