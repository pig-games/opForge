; Host-facing CLI harness module.
;
; Owns the AmigaOS Shell bridge, report formatting surface, and caller-owned
; buffers. Imports the tokenizer VM contract from tokvm.amigaos.tokenizer_vm.

        .module tokvm.amigaos.cli_harness
        .cpu 68020
        .pub
        .use tokvm.amigaos.tokenizer_vm (tokvm_run_68000, tokvm_set_step_budget_68000)
        .use tokvm.amigaos.tokenizer_vm (demoProgram, demoProgramLen, TOKVM_DEFAULT_MAX_STEPS_PER_LINE)
        .use tokvm.amigaos.tokenizer_vm (TOKEN_BUFFER_CAPACITY, TOKEN_RECORD_SIZE)
        .use tokvm.amigaos.tokenizer_vm (SOURCE_BUFFER_CAPACITY, SCRATCH_BUFFER_CAPACITY)
        .use tokvm.amigaos.tokenizer_vm (TK_STATUS_VM_FAILURE, TK_STATUS_INVALID_PROGRAM, TK_KIND_OP_LT)

; ---------------------------------------------------------------------------
; AmigaOS Exec/DOS offsets used by the CLI harness layer.
; ---------------------------------------------------------------------------

SysBase                         = 4

pr_CLI                          = 172
pr_MsgPort                      = 92

OpenLibrary                     = -552
CloseLibrary                    = -414
FindTask                        = -294
WaitPort                        = -384
GetMsg                          = -372
ReplyMsg                        = -378
Forbid                          = -132

Open                            = -30
Close                           = -36
Read                            = -42
Write                           = -48
Output                          = -60
IoErr                           = -132
GetArgStr                       = -534

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

tokvm_amigaos_cli_harness_run:
        MOVEM.L D2-D7/A2-A6, -(SP)
        LEA globals, A4  ; shared host state block: DOS base, stdout, output handle, last IoErr
        MOVEQ #RETURN_OK, D7  ; optimistic Shell return until a host or VM failure overrides it

        BSR.W amigaos_cli_fileio_init  ; mirrors Rust-side host bootstrap: open DOS and discover stdout
        TST.L D0
        BNE.W tokvmHarnessCleanup

        BSR.W amigaos_cli_fileio_get_arg_str  ; DOS GetArgStr provides the raw Shell argument tail
        BSR.W tokvm_amigaos_cli_harness_parse_args  ; native spec is intentionally fixed: <input-path> <output-path>
        TST.L D0
        BEQ.W tokvmHarnessArgsParsed
        MOVEQ #RETURN_USAGE, D7
        MOVE.L GLOBALS_STDOUT_HANDLE(A4), D1
        CMPI.L #HARNESS_STATUS_QUOTED_PATH, D0
        BNE.S tokvmHarnessUsage
        LEA quotedPathMessage, A0
        BSR.W amigaos_cli_fileio_write_cstr
        BRA.W tokvmHarnessCleanup

tokvmHarnessUsage:
        LEA usageMessage, A0
        BSR.W amigaos_cli_fileio_write_cstr
        BRA.W tokvmHarnessCleanup

tokvmHarnessArgsParsed:
        LEA outputPathBuffer, A0  ; open the report target first so later failure paths can still emit report text
        BSR.W amigaos_cli_fileio_open_output
        TST.L D0
        BNE.S tokvmHarnessOutputOpened
        MOVEQ #RETURN_OUTPUT_FAILURE, D7
        MOVE.L GLOBALS_STDOUT_HANDLE(A4), D1
        LEA outputOpenMessage, A0
        BSR.W amigaos_cli_fileio_write_cstr
        BRA.W tokvmHarnessCleanup

tokvmHarnessOutputOpened:
        MOVE.L D0, GLOBALS_OUTPUT_HANDLE(A4)
        MOVE.L D0, D6  ; keep the output handle live for report writes and final cleanup
        LEA inputPathBuffer, A0
        BSR.W amigaos_cli_fileio_open_input
        TST.L D0
        BNE.S tokvmHarnessInputOpened
        MOVEQ #RETURN_FILE_FAILURE, D7
        MOVE.L #HARNESS_STATUS_INPUT_OPEN, D0
        BSR.W tokvm_amigaos_cli_harness_write_failure_report
        TST.L D0
        BEQ.W tokvmHarnessCleanup
        MOVEQ #RETURN_OUTPUT_FAILURE, D7
        BRA.W tokvmHarnessCleanup

tokvmHarnessInputOpened:
        MOVE.L D0, D5  ; input handle lives only until the bounded single-line read completes
        LEA sourceBuffer, A0
        MOVE.L #SOURCE_BUFFER_CAPACITY, D0  ; tokvm native ABI takes a caller-owned contiguous source slice
        MOVE.L D5, D1
        BSR.W amigaos_cli_fileio_read
        CMP.L #-1, D0
        BNE.S tokvmHarnessReadOk
        MOVE.L D5, D1
        BSR.W amigaos_cli_fileio_close
        MOVEQ #RETURN_FILE_FAILURE, D7
        MOVE.L #HARNESS_STATUS_INPUT_READ, D0
        BSR.W tokvm_amigaos_cli_harness_write_failure_report
        TST.L D0
        BEQ.W tokvmHarnessCleanup
        MOVEQ #RETURN_OUTPUT_FAILURE, D7
        BRA.W tokvmHarnessCleanup

tokvmHarnessReadOk:
        MOVE.L D0, D4  ; D4 becomes source byte length, matching tokvm_run_68000 input ABI
        LEA inputProbeByte, A0
        MOVEQ #1, D0  ; one-byte overflow probe enforces the spec's bounded single-line input rule
        MOVE.L D5, D1
        BSR.W amigaos_cli_fileio_read
        MOVE.L D0, D3
        CMP.L #-1, D0
        BNE.S tokvmHarnessProbeOk
        MOVE.L D5, D1
        BSR.W amigaos_cli_fileio_close
        MOVEQ #RETURN_FILE_FAILURE, D7
        MOVE.L #HARNESS_STATUS_INPUT_READ, D0
        BSR.W tokvm_amigaos_cli_harness_write_failure_report
        TST.L D0
        BEQ.W tokvmHarnessCleanup
        MOVEQ #RETURN_OUTPUT_FAILURE, D7
        BRA.W tokvmHarnessCleanup

tokvmHarnessProbeOk:
        MOVE.L D5, D1
        BSR.W amigaos_cli_fileio_close
        TST.L D0
        BEQ.S tokvmHarnessProbeClosed
        MOVEQ #RETURN_FILE_FAILURE, D7
        MOVE.L #HARNESS_STATUS_INPUT_READ, D0
        BSR.W tokvm_amigaos_cli_harness_write_failure_report
        TST.L D0
        BEQ.W tokvmHarnessCleanup
        MOVEQ #RETURN_OUTPUT_FAILURE, D7
        BRA.W tokvmHarnessCleanup

tokvmHarnessProbeClosed:
        TST.L D3  ; any extra byte means the caller exceeded SOURCE_BUFFER_CAPACITY
        BEQ.S tokvmHarnessInvokeVm
        MOVEQ #RETURN_INPUT_TOO_LARGE, D7
        MOVE.L #HARNESS_STATUS_INPUT_TOO_LARGE, D0
        BSR.W tokvm_amigaos_cli_harness_write_failure_report
        TST.L D0
        BEQ.W tokvmHarnessCleanup
        MOVEQ #RETURN_OUTPUT_FAILURE, D7
        BRA.W tokvmHarnessCleanup

tokvmHarnessInvokeVm:
        ; Register ABI for tokvm_run_68000:
        ; A0/D0 source slice, A1/D1 token buffer+capacity, A2/D2 scratch buffer+capacity,
        ; A3/D3 demo bytecode pointer+length. This mirrors the native contract documented
        ; in tokvm_tokenizer_vm.asm and used by the Rust-side bridge tests.
        MOVE.L #TOKVM_DEFAULT_MAX_STEPS_PER_LINE, D0
        JSR tokvm_set_step_budget_68000
        LEA sourceBuffer, A0
        MOVE.L D4, D0
        LEA tokenBuffer, A1
        MOVE.L #TOKEN_BUFFER_CAPACITY, D1
        LEA lexemeScratch, A2
        MOVE.L #SCRATCH_BUFFER_CAPACITY, D2
        LEA demoProgram, A3
        MOVE.L demoProgramLen, D3
        JSR tokvm_run_68000

        MOVE.L D0, D4  ; status
        MOVE.L D1, D5  ; emitted token count
        MOVE.L D2, D6  ; final source cursor / column end
        TST.L D4
        BEQ.S tokvmHarnessWriteVmReport
        MOVEQ #RETURN_VM_FAILURE, D7

tokvmHarnessWriteVmReport:
        MOVE.L D4, D0
        MOVE.L D5, D1
        MOVE.L D6, D2
        BSR.W tokvm_amigaos_cli_harness_write_report  ; render the OPFORGE-TOKVM 1 report consumed by asm regression tests
        TST.L D0
        BEQ.S tokvmHarnessCleanup
        MOVEQ #RETURN_OUTPUT_FAILURE, D7

tokvmHarnessCleanup:
        MOVE.L GLOBALS_OUTPUT_HANDLE(A4), D1  ; close the report handle even after VM or formatting failures
        BEQ.S tokvmHarnessShutdown
        BSR.W amigaos_cli_fileio_close
        CLR.L GLOBALS_OUTPUT_HANDLE(A4)
        TST.L D0
        BEQ.S tokvmHarnessShutdown
        MOVEQ #RETURN_OUTPUT_FAILURE, D7

tokvmHarnessShutdown:
        BSR.W amigaos_cli_fileio_shutdown
        MOVE.L D7, D0
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Parse exactly two unquoted Shell paths from DOS GetArgStr output.
; This stays intentionally narrower than a full command-line parser because the
; harness spec defines a fixed tokvm <input-path> <output-path> contract.
tokvm_amigaos_cli_harness_parse_args:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEA.L A0, A3  ; A3 walks the raw DOS argument tail in-place
        BSR.S tokvmHarnessSkipWhitespace  ; align with the Rust test helper that trims surrounding Shell whitespace
        TST.B (A3)
        BEQ.S tokvmHarnessArgsMissing
        CMPI.B #'"', (A3)  ; quoted paths stay unsupported in this narrow first-shell contract
        BEQ.S tokvmHarnessArgsQuoted
        LEA inputPathBuffer, A1
        BSR.S tokvmHarnessCopyToken  ; copy token 0 => input path
        TST.L D0
        BNE.S tokvmHarnessArgsDone
        BSR.S tokvmHarnessSkipWhitespace
        TST.B (A3)
        BEQ.S tokvmHarnessArgsMissing
        CMPI.B #'"', (A3)
        BEQ.S tokvmHarnessArgsQuoted
        LEA outputPathBuffer, A1
        BSR.S tokvmHarnessCopyToken  ; copy token 1 => output path
        TST.L D0
        BNE.S tokvmHarnessArgsDone
        BSR.S tokvmHarnessSkipWhitespace
        TST.B (A3)
        BNE.S tokvmHarnessArgsMissing
        MOVEQ #0, D0
        BRA.S tokvmHarnessArgsDone

tokvmHarnessArgsMissing:
        MOVE.L #HARNESS_STATUS_USAGE, D0
        BRA.S tokvmHarnessArgsDone

tokvmHarnessArgsQuoted:
        MOVE.L #HARNESS_STATUS_QUOTED_PATH, D0

tokvmHarnessArgsDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tokvmHarnessSkipWhitespace:
        CMPI.B #' ', (A3)
        BEQ.S tokvmHarnessSkipOne
        CMPI.B #9, (A3)
        BEQ.S tokvmHarnessSkipOne
        CMPI.B #10, (A3)
        BEQ.S tokvmHarnessSkipOne
        CMPI.B #13, (A3)
        BNE.S tokvmHarnessSkipDone
tokvmHarnessSkipOne:
        ADDQ.L #1, A3
        BRA.S tokvmHarnessSkipWhitespace

tokvmHarnessSkipDone:
        RTS

; Copy one CLI path token into the caller-selected buffer.
; This intentionally implements a narrow Shell token grammar rather than full
; quote/escape handling because the tokvm host spec only accepts raw paths.
tokvmHarnessCopyToken:
        MOVE.L #PATH_BUFFER_CAPACITY - 1, D6  ; reserve space for the trailing NUL DOS expects
tokvmHarnessCopyLoop:
        MOVEQ #0, D0
        MOVE.B (A3), D0
        BEQ.S tokvmHarnessCopyFinish
        CMPI.B #' ', D0
        BEQ.S tokvmHarnessCopyFinish
        CMPI.B #9, D0
        BEQ.S tokvmHarnessCopyFinish
        CMPI.B #10, D0
        BEQ.S tokvmHarnessCopyFinish
        CMPI.B #13, D0
        BEQ.S tokvmHarnessCopyFinish
        CMPI.B #'"', D0
        BEQ.S tokvmHarnessCopyQuoted
        TST.L D6
        BEQ.S tokvmHarnessCopyOverflow
        MOVE.B D0, (A1)+  ; preserve the raw path byte; no shell-style unescaping in this slice
        ADDQ.L #1, A3
        SUBQ.L #1, D6
        BRA.S tokvmHarnessCopyLoop

tokvmHarnessCopyFinish:
        CLR.B (A1)
        MOVEQ #0, D0
        RTS

tokvmHarnessCopyQuoted:
        MOVE.L #HARNESS_STATUS_QUOTED_PATH, D0
        RTS

tokvmHarnessCopyOverflow:
        MOVE.L #HARNESS_STATUS_USAGE, D0
        RTS

; Failure reports reuse the same report writer as success reports so the file
; format stays identical across success and host/VM failure paths.
tokvm_amigaos_cli_harness_write_failure_report:
        CLR.L D1
        CLR.L D2
        CLR.L D3
        BRA.W tokvm_amigaos_cli_harness_write_report

tokvm_amigaos_cli_harness_write_report:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVE.L D0, D4
        MOVE.L D1, D5
        MOVE.L D2, D6
        ; Reject malformed native buffers before formatting token lines so bad VM state
        ; degrades to TK_STATUS_VM_FAILURE instead of emitting invalid report metadata.
        MOVE.L D3, D2
        BSR.W tokvm_amigaos_cli_harness_validate_vm_result
        TST.L D0
        BEQ.S tokvmHarnessReportValidated
        MOVEQ #TK_STATUS_VM_FAILURE, D4
        MOVEQ #0, D5
        MOVEQ #0, D6
        CLR.L D2
tokvmHarnessReportValidated:
        MOVE.L GLOBALS_OUTPUT_HANDLE(A4), D7  ; all report lines flow through the already-open DOS output handle

        ; The report header and scalar fields intentionally match the golden
        ; OPFORGE-TOKVM 1 text format exercised in crates/opforge-asm/src/tests.rs.
        MOVE.L D7, D1
        LEA reportHeader, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA reportStatusPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        MOVE.L D4, D0
        BSR.W tokvm_amigaos_cli_harness_write_i32
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA newlineString, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA reportTokensPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        MOVE.L D5, D0
        BSR.W tokvm_amigaos_cli_harness_write_u32
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA newlineString, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA reportCursorPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        MOVE.L D6, D0
        BSR.W tokvm_amigaos_cli_harness_write_u32
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA newlineString, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        ; Walk the native 20-byte token records and re-expand them into the
        ; stable text report surface consumed by the asm reference tests.
        LEA tokenBuffer, A5  ; native 20-byte token records that the asm tests decode back into report lines
        MOVEQ #0, D3
tokvmHarnessReportTokenLoop:
        CMP.L D5, D3
        BCC.W tokvmHarnessReportEnd

        MOVE.L D7, D1
        LEA reportTokenPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        MOVE.L D3, D0
        BSR.W tokvm_amigaos_cli_harness_write_u32
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA reportKindPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVEQ #0, D0
        MOVE.W (A5), D0
        BSR.W tokvm_amigaos_cli_harness_kind_name  ; record kind code -> PortableTokenKind/report name mapping
        MOVE.L D7, D1
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA reportStartPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        MOVE.L 4(A5), D0
        BSR.W tokvm_amigaos_cli_harness_write_u32
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA reportEndPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        MOVE.L 8(A5), D0
        BSR.W tokvm_amigaos_cli_harness_write_u32
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA reportLenPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        MOVE.L 16(A5), D0
        BSR.W tokvm_amigaos_cli_harness_write_u32
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA reportLexhexPrefix, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVEA.L A5, A6  ; A5 points at the current record, A6 is a temporary for offseted fields
        MOVE.L 12(A6), D0
        LEA lexemeScratch, A0
        ADDA.L D0, A0  ; lexemeScratch + offset => the exact byte slice emitted by the native scanner
        MOVE.L 16(A6), D0
        MOVE.L D7, D1
        BSR.W tokvm_amigaos_cli_harness_write_hex_bytes
        TST.L D0
        BNE.W tokvmHarnessReportFail

        MOVE.L D7, D1
        LEA newlineString, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail

        ADDA.L #TOKEN_RECORD_SIZE, A5
        ADDQ.L #1, D3
        BRA.W tokvmHarnessReportTokenLoop

tokvmHarnessReportEnd:
        ; END terminates both success and failure reports so the file format is
        ; self-delimiting even when token count is zero.
        MOVE.L D7, D1
        LEA reportEndLine, A0
        BSR.W amigaos_cli_fileio_write_cstr
        TST.L D0
        BNE.W tokvmHarnessReportFail
        MOVEQ #0, D0
        BRA.W tokvmHarnessReportDone

tokvmHarnessReportFail:
        MOVEQ #1, D0

tokvmHarnessReportDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Defensive validation in front of report formatting.
; The Rust runtime validates token shapes before exposing PortableToken output;
; this native harness mirrors that expectation so malformed native state falls
; back to TK_STATUS_VM_FAILURE instead of emitting invalid token metadata.
tokvm_amigaos_cli_harness_validate_vm_result:
        TST.L D4  ; non-negative statuses are TK_STATUS_* VM results, negative are harness failures
        BMI tokvmHarnessValidateNegativeStatus
        CMPI.L #TK_STATUS_INVALID_PROGRAM, D4
        BGT tokvmHarnessValidateInvalid
        CMP.L #TOKEN_BUFFER_CAPACITY, D5
        BHI tokvmHarnessValidateInvalid
        CMP.L #SOURCE_BUFFER_CAPACITY, D6
        BHI tokvmHarnessValidateInvalid
        CMP.L #SCRATCH_BUFFER_CAPACITY, D2
        BHI tokvmHarnessValidateInvalid

        LEA tokenBuffer, A0
        MOVEQ #0, D1
tokvmHarnessValidateTokenLoop:
        CMP.L D5, D1
        BCC.S tokvmHarnessValidateOk

        MOVEQ #0, D0
        MOVE.W (A0), D0
        CMPI.L #TK_KIND_OP_LT, D0  ; last valid kind in the native PortableTokenKind mirror table
        BGT tokvmHarnessValidateInvalid

        MOVE.L 4(A0), D0
        TST.L D0
        BEQ tokvmHarnessValidateInvalid
        CMPI.L #SOURCE_BUFFER_CAPACITY + 1, D0
        BHI tokvmHarnessValidateInvalid

        MOVE.L 8(A0), D0
        TST.L D0
        BEQ tokvmHarnessValidateInvalid
        CMPI.L #SOURCE_BUFFER_CAPACITY + 1, D0
        BHI tokvmHarnessValidateInvalid
        CMP.L 4(A0), D0
        BLT tokvmHarnessValidateInvalid

        MOVE.L 12(A0), D0  ; lexeme offset must stay inside the committed scratch prefix
        CMP.L D2, D0
        BHI tokvmHarnessValidateInvalid
        MOVE.L 16(A0), D3
        MOVE.L D0, D7
        ADD.L D3, D7
        CMP.L D2, D7
        BHI tokvmHarnessValidateInvalid

        ADDA.L #TOKEN_RECORD_SIZE, A0
        ADDQ.L #1, D1
        BRA.S tokvmHarnessValidateTokenLoop

tokvmHarnessValidateNegativeStatus:
        ; Negative statuses are host-side harness failures. When one of those is
        ; reported, no VM-owned token or scratch state is allowed to leak out.
        CMPI.L #HARNESS_STATUS_USAGE, D4
        BGT.S tokvmHarnessValidateInvalid
        CMPI.L #HARNESS_STATUS_OUTPUT_OPEN, D4
        BLT.S tokvmHarnessValidateInvalid
        TST.L D5
        BNE.S tokvmHarnessValidateInvalid
        TST.L D6
        BNE.S tokvmHarnessValidateInvalid
        TST.L D2
        BNE.S tokvmHarnessValidateInvalid

tokvmHarnessValidateOk:
        MOVEQ #0, D0
        RTS

tokvmHarnessValidateInvalid:
        MOVEQ #1, D0
        RTS

; Signed report writer used for STATUS fields so host failures can emit their
; negative HARNESS_STATUS_* values without special formatting logic.
tokvm_amigaos_cli_harness_write_i32:
        TST.L D0
        BPL.S tokvmHarnessWriteI32Unsigned
        MOVE.L D2, -(SP)
        MOVE.L D0, D2
        MOVE.L D1, -(SP)
        LEA minusString, A0
        BSR.W amigaos_cli_fileio_write_cstr
        MOVE.L (SP)+, D1
        TST.L D0
        BNE.S tokvmHarnessWriteI32NegativeDone
        MOVE.L D2, D0
        NEG.L D0
        BSR.W tokvm_amigaos_cli_harness_write_u32
tokvmHarnessWriteI32NegativeDone:
        MOVE.L (SP)+, D2
        RTS
tokvmHarnessWriteI32Unsigned:
        BSR.W tokvm_amigaos_cli_harness_write_u32
tokvmHarnessWriteI32Done:
        RTS

; Minimal decimal formatter. Values are accumulated right-to-left in a scratch
; buffer and then flushed in one exact DOS write.
tokvm_amigaos_cli_harness_write_u32:
        MOVEM.L D2-D7/A2-A6, -(SP)
        LEA decimalBufferEnd, A0  ; emit backwards into scratch, then flush the exact decimal slice once
        MOVEQ #0, D2
        TST.L D0
        BNE.S tokvmHarnessWriteU32Loop
        MOVE.B #'0', -(A0)
        MOVEQ #1, D2
        BRA.S tokvmHarnessWriteU32Emit

tokvmHarnessWriteU32Loop:
        DIVU.W #10, D0
        MOVE.W D0, D3
        SWAP D0
        ADDI.W #'0', D0
        MOVE.B D0, -(A0)
        ADDQ.L #1, D2
        MOVEQ #0, D0
        MOVE.W D3, D0
        TST.L D0
        BNE.S tokvmHarnessWriteU32Loop

tokvmHarnessWriteU32Emit:
        MOVE.L D2, D0
        BSR.W amigaos_cli_fileio_write_exact
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Render a lexeme payload as uppercase hexadecimal bytes. This matches the
; LEXHEX field expected by the native report rendering tests.
tokvm_amigaos_cli_harness_write_hex_bytes:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEA.L A0, A6
        MOVE.L D0, D5  ; remaining byte count for the report's LEXHEX field
        LEA hexDigits, A5
tokvmHarnessWriteHexLoop:
        TST.L D5
        BEQ.S tokvmHarnessWriteHexDone
        MOVEQ #0, D2
        MOVE.B (A6)+, D2
        MOVE.L D2, D3
        LSR.B #4, D3
        ANDI.W #$000F, D3
        ANDI.W #$000F, D2
        MOVE.B 0(A5, D3.W), hexPairBuffer
        MOVEQ #0, D4
        MOVE.B 0(A5, D2.W), D4
        LEA hexPairBuffer, A1
        MOVE.B D4, 1(A1)
        LEA hexPairBuffer, A0
        MOVEQ #2, D0
        BSR.W amigaos_cli_fileio_write_exact
        TST.L D0
        BNE.S tokvmHarnessWriteHexDone
        SUBQ.L #1, D5
        BRA.S tokvmHarnessWriteHexLoop

tokvmHarnessWriteHexDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Token kind name lookup for report emission.
; The table order is locked to the TK_KIND_* numeric encoding exported by the
; native tokenizer VM module.
tokvm_amigaos_cli_harness_kind_name:
        CMP.L #TK_KIND_OP_LT, D0
        BHI.S tokvmHarnessKindUnknown
        ADD.L D0, D0
        ADD.L D0, D0
        LEA kindNamePtrs, A1
        MOVEA.L 0(A1, D0.L), A0
        RTS

tokvmHarnessKindUnknown:
        LEA kindNameUnknown, A0
        RTS

; ---------------------------------------------------------------------------
; Minimal DOS helper layer used by the harness.
;
; These routines intentionally stay thin: all tokenizer semantics live in the
; VM below, while this layer only handles DOS open/read/write/close concerns.
; ---------------------------------------------------------------------------

amigaos_cli_fileio_init:
        MOVEM.L D2-D7/A2-A6, -(SP)
        ; Reset host globals first so every early-exit path can safely call the
        ; shared shutdown/cleanup logic.
        CLR.L GLOBALS_DOS_BASE(A4)
        CLR.L GLOBALS_STDOUT_HANDLE(A4)
        CLR.L GLOBALS_LAST_IOERR(A4)
        CLR.L GLOBALS_OUTPUT_HANDLE(A4)
        LEA dosName, A1
        MOVEQ #36, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)
        TST.L D0
        BEQ.S amigaosCliFileIoInitFail
        MOVE.L D0, GLOBALS_DOS_BASE(A4)
        MOVEA.L D0, A6
        JSR Output(A6)
        MOVE.L D0, GLOBALS_STDOUT_HANDLE(A4)
        MOVEQ #0, D0
        BRA.W amigaosCliFileIoInitDone

amigaosCliFileIoInitFail:
        MOVEQ #1, D0

amigaosCliFileIoInitDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Release DOS library state once the harness is finished with all host I/O.
amigaos_cli_fileio_shutdown:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVE.L GLOBALS_DOS_BASE(A4), D0
        BEQ.S amigaosCliFileIoShutdownDone
        MOVEA.L D0, A1
        MOVEA.L SysBase.W, A6
        JSR CloseLibrary(A6)
        CLR.L GLOBALS_DOS_BASE(A4)

amigaosCliFileIoShutdownDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; DOS GetArgStr returns the raw argument tail for the current Shell process.
amigaos_cli_fileio_get_arg_str:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEA.L GLOBALS_DOS_BASE(A4), A6
        JSR GetArgStr(A6)
        MOVEA.L D0, A0
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

amigaos_cli_fileio_open_input:
        MOVEM.L D2-D7/A2-A6, -(SP)
        ; Open the source file in read-only mode and cache IoErr on failure so
        ; later diagnostics or debugging can inspect the DOS reason code.
        MOVE.L A0, D1
        MOVE.L #MODE_OLDFILE, D2
        MOVEA.L GLOBALS_DOS_BASE(A4), A6
        JSR Open(A6)
        TST.L D0
        BNE.S amigaosCliFileIoOpenDone
        BSR.W amigaos_cli_fileio_capture_ioerr

amigaosCliFileIoOpenDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Create/truncate the report target before any input work happens so all later
; failure paths can still emit a report file instead of only returning a code.
amigaos_cli_fileio_open_output:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVE.L A0, D1
        MOVE.L #MODE_NEWFILE, D2
        MOVEA.L GLOBALS_DOS_BASE(A4), A6
        JSR Open(A6)
        TST.L D0
        BNE.S amigaosCliFileIoCreateDone
        BSR.W amigaos_cli_fileio_capture_ioerr

amigaosCliFileIoCreateDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Thin DOS Read wrapper. The harness uses it both for the bounded source read
; and the one-byte overflow probe that enforces the single-line capacity rule.
amigaos_cli_fileio_read:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVE.L A0, D2  ; DOS Read uses D2=buffer, D3=len in the helper's stable calling convention
        MOVE.L D0, D3
        MOVEA.L GLOBALS_DOS_BASE(A4), A6
        JSR Read(A6)
        CMP.L #-1, D0
        BNE.S amigaosCliFileIoReadDone
        BSR.W amigaos_cli_fileio_capture_ioerr

amigaosCliFileIoReadDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Count a NUL-terminated string and forward it to the exact-byte write helper.
amigaos_cli_fileio_write_cstr:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEA.L A0, A2
        MOVEQ #0, D0
amigaosCliFileIoWriteCstrLoop:
        TST.B (A0)+
        BEQ.S amigaosCliFileIoWriteCstrEmit
        ADDQ.L #1, D0
        BRA.S amigaosCliFileIoWriteCstrLoop

amigaosCliFileIoWriteCstrEmit:
        MOVEA.L A2, A0
        BSR.W amigaos_cli_fileio_write_exact
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Preserve D1 across DOS Write calls because the report writer keeps the output handle
; in D1 while emitting decimal fields and repeated LEXHEX byte pairs.
amigaos_cli_fileio_write_exact:
        MOVEM.L D1-D7/A2-A6, -(SP)
        MOVE.L D0, D3
        MOVE.L A0, D2
        MOVEA.L GLOBALS_DOS_BASE(A4), A6
        JSR Write(A6)
        CMP.L #-1, D0
        BEQ.S amigaosCliFileIoWriteFail
        CMP.L D3, D0
        BNE.W amigaosCliFileIoWriteShort
        MOVEQ #0, D0
        BRA.W amigaosCliFileIoWriteDone

amigaosCliFileIoWriteFail:
        BSR.W amigaos_cli_fileio_capture_ioerr
        MOVEQ #1, D0
        BRA.W amigaosCliFileIoWriteDone

amigaosCliFileIoWriteShort:
        CLR.L GLOBALS_LAST_IOERR(A4)
        MOVEQ #1, D0

amigaosCliFileIoWriteDone:
        MOVEM.L (SP)+, D1-D7/A2-A6
        RTS

; Close a DOS file handle and preserve IoErr if the close itself fails.
amigaos_cli_fileio_close:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEA.L GLOBALS_DOS_BASE(A4), A6
        JSR Close(A6)
        TST.L D0
        BNE.W amigaosCliFileIoCloseOk
        BSR.W amigaos_cli_fileio_capture_ioerr
        MOVEQ #1, D0
        BRA.W amigaosCliFileIoCloseDone

amigaosCliFileIoCloseOk:
        MOVEQ #0, D0

amigaosCliFileIoCloseDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

; Cache the last DOS IoErr in globals so callers do not need to immediately
; inspect D0 after every failed host I/O operation.
amigaos_cli_fileio_capture_ioerr:
        MOVEA.L GLOBALS_DOS_BASE(A4), A6
        JSR IoErr(A6)
        MOVE.L D0, GLOBALS_LAST_IOERR(A4)
        RTS

        .endsection
        .section data, kind=data

; Static strings and lookup tables for the host/report layer.
dosName:
        .byte "dos.library", 0

usageMessage:
        .byte "Usage: tokvm <input-path> <output-path>", 10, 0
quotedPathMessage:
        .byte "tokvm: quoted paths are not supported", 10, 0
outputOpenMessage:
        .byte "tokvm: failed to open output file", 10, 0

reportHeader:
        .byte "OPFORGE-TOKVM 1", 10, 0
reportStatusPrefix:
        .byte "STATUS ", 0
reportTokensPrefix:
        .byte "TOKENS ", 0
reportCursorPrefix:
        .byte "CURSOR ", 0
reportTokenPrefix:
        .byte "TOKEN ", 0
reportKindPrefix:
        .byte " KIND ", 0
reportStartPrefix:
        .byte " START ", 0
reportEndPrefix:
        .byte " END ", 0
reportLenPrefix:
        .byte " LEN ", 0
reportLexhexPrefix:
        .byte " LEXHEX ", 0
reportEndLine:
        .byte "END", 10, 0
newlineString:
        .byte 10, 0
minusString:
        .byte "-", 0
hexDigits:
        .byte "0123456789ABCDEF"

kindNameIdentifier:
        .byte "identifier", 0
kindNameRegister:
        .byte "register", 0
kindNameNumber:
        .byte "number", 0
kindNameString:
        .byte "string", 0
kindNameComma:
        .byte "comma", 0
kindNameColon:
        .byte "colon", 0
kindNameDollar:
        .byte "dollar", 0
kindNameDot:
        .byte "dot", 0
kindNameHash:
        .byte "hash", 0
kindNameQuestion:
        .byte "question", 0
kindNameOpenBracket:
        .byte "open_bracket", 0
kindNameCloseBracket:
        .byte "close_bracket", 0
kindNameOpenBrace:
        .byte "open_brace", 0
kindNameCloseBrace:
        .byte "close_brace", 0
kindNameOpenParen:
        .byte "open_paren", 0
kindNameCloseParen:
        .byte "close_paren", 0
kindNameOpRange:
        .byte "op_range", 0
kindNameOpRangeInclusive:
        .byte "op_range_inclusive", 0
kindNameOpPlus:
        .byte "op_plus", 0
kindNameOpMinus:
        .byte "op_minus", 0
kindNameOpMultiply:
        .byte "op_multiply", 0
kindNameOpPower:
        .byte "op_power", 0
kindNameOpDivide:
        .byte "op_divide", 0
kindNameOpMod:
        .byte "op_mod", 0
kindNameOpShl:
        .byte "op_shl", 0
kindNameOpShr:
        .byte "op_shr", 0
kindNameOpBitNot:
        .byte "op_bit_not", 0
kindNameOpLogicNot:
        .byte "op_logic_not", 0
kindNameOpBitAnd:
        .byte "op_bit_and", 0
kindNameOpBitOr:
        .byte "op_bit_or", 0
kindNameOpBitXor:
        .byte "op_bit_xor", 0
kindNameOpLogicAnd:
        .byte "op_logic_and", 0
kindNameOpLogicOr:
        .byte "op_logic_or", 0
kindNameOpLogicXor:
        .byte "op_logic_xor", 0
kindNameOpEq:
        .byte "op_eq", 0
kindNameOpNe:
        .byte "op_ne", 0
kindNameOpGe:
        .byte "op_ge", 0
kindNameOpGt:
        .byte "op_gt", 0
kindNameOpLe:
        .byte "op_le", 0
kindNameOpLt:
        .byte "op_lt", 0
kindNameUnknown:
        .byte "unknown", 0

        .align 4
; kindNamePtrs is indexed directly by the TK_KIND_* numeric code.
; Each entry expands the compact native token record into the report text name
; that matches PortableTokenKind-oriented test expectations.
kindNamePtrs:
        .long kindNameIdentifier
        .long kindNameRegister
        .long kindNameNumber
        .long kindNameString
        .long kindNameComma
        .long kindNameColon
        .long kindNameDollar
        .long kindNameDot
        .long kindNameHash
        .long kindNameQuestion
        .long kindNameOpenBracket
        .long kindNameCloseBracket
        .long kindNameOpenBrace
        .long kindNameCloseBrace
        .long kindNameOpenParen
        .long kindNameCloseParen
        .long kindNameOpRange
        .long kindNameOpRangeInclusive
        .long kindNameOpPlus
        .long kindNameOpMinus
        .long kindNameOpMultiply
        .long kindNameOpPower
        .long kindNameOpDivide
        .long kindNameOpMod
        .long kindNameOpShl
        .long kindNameOpShr
        .long kindNameOpBitNot
        .long kindNameOpLogicNot
        .long kindNameOpBitAnd
        .long kindNameOpBitOr
        .long kindNameOpBitXor
        .long kindNameOpLogicAnd
        .long kindNameOpLogicOr
        .long kindNameOpLogicXor
        .long kindNameOpEq
        .long kindNameOpNe
        .long kindNameOpGe
        .long kindNameOpGt
        .long kindNameOpLe
        .long kindNameOpLt

        .endsection
        .section bss, kind=bss

; Caller-owned and harness-owned buffers.
; tokenBuffer and lexemeScratch together form the native token ABI surface that
; tokvm_amigaos_cli_harness_write_report rehydrates into OPFORGE-TOKVM 1 lines.
        .align 4
globals:
        .res byte, GLOBALS_SIZE
inputPathBuffer:
        .res byte, PATH_BUFFER_CAPACITY
outputPathBuffer:
        .res byte, PATH_BUFFER_CAPACITY
sourceBuffer:
        .res byte, SOURCE_BUFFER_CAPACITY
tokenBuffer:
        .res byte, TOKEN_RECORD_SIZE * TOKEN_BUFFER_CAPACITY
lexemeScratch:
        .res byte, SCRATCH_BUFFER_CAPACITY
decimalBuffer:
        .res byte, 16
decimalBufferEnd:
hexPairBuffer:
        .res byte, 2
inputProbeByte:
        .res byte, 1

        .endsection
        .endmodule
