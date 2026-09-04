; Explicit debug-only export of sealed, fixed-size bridge records.
; @opforge-owner: debug.amigaos.profile_export
; @opforge-slice: documentation/plans/slices/native-porting-slice-platform-io-v1.toml
; @opforge-role: implementation
;
; Instrumentation point: after progress Finish and before closing dos.library.
; Routine: opforgeProfileExportV1. Preserves D0-D7/A0-A6 and CCR; stack delta 0.
; Shared buffers touched: none. Reads only fixed profile records and constant
; filenames. No per-byte tracing or variable-size diagnostic buffer is used.
; All timing/counters are sealed before export. Missing/short files are not
; evidence: the host requires exact lengths, matching IDs and guest completion.
; Stabilization: retain as the explicit opt-in bridge export used by Item 0f.

	.module debug.amigaos.profile_export
	.cpu 68020
	.use opasm.amigaos.progress as progress
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	.use debug.amigaos.symbol_expr_profile as symbol_expr_profile
.endif
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	.use debug.amigaos.runtime_profile as runtime_profile
.endif
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	.use debug.amigaos.platform_profile as platform_profile
.endif

PROFILE_EXPORT_DOS_OPEN = -30
PROFILE_EXPORT_DOS_CLOSE = -36
PROFILE_EXPORT_DOS_WRITE = -48
PROFILE_EXPORT_MODE_NEWFILE = 1006

	.section code, kind=code
	.pub

; Inputs: A6 = open dos.library base. Outputs/clobbers: none; CCR preserved.
; Only an explicit OPFORGE_PROGRESS_EXPORT_RECORDS build invokes this entry.
opforgeProfileExportV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	jsr progress.opasmProgressGetRecordV1
	move.l #progress.OPASM_PROGRESS_RECORD_BYTES, d0
	lea ProgressFile, a1
	bsr.w writeRecord
.ifdef OPFORGE_PROGRESS_WORK_COUNTERS
	jsr progress.opasmProgressGetWorkRecordV1
	move.l #progress.OPASM_WORK_RECORD_BYTES, d0
	lea WorkFile, a1
	bsr.w writeRecord
.endif
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	jsr symbol_expr_profile.opforgeSymbolExprProfileGetRecordV1
	move.l #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_RECORD_BYTES, d0
	lea SymbolFile, a1
	bsr.w writeRecord
.endif
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	jsr runtime_profile.opforgeRuntimeProfileGetRecordV1
	move.l #runtime_profile.OPFORGE_RUNTIME_RECORD_BYTES, d0
	lea RuntimeFile, a1
	bsr.w writeRecord
.endif
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	move.l #platform_profile.OPFORGE_PLATFORM_RECORD_BYTES, d0
	lea PlatformFile, a1
	bsr.w writeRecord
.endif
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeProfileExportV1

	.priv

; Inputs: A0 bytes, D0 fixed length, A1 filename, A6 dos.library.
; Outputs/clobbers: none; CCR preserved. Open failure leaves no new artifact;
; short/failed write leaves an invalid-length artifact, rejected by the host.
writeRecord	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a2
	move.l d0, d4
	move.l a1, d1
	move.l #PROFILE_EXPORT_MODE_NEWFILE, d2
	jsr PROFILE_EXPORT_DOS_OPEN(a6)
	tst.l d0
	beq.s done
	move.l d0, d5
	move.l d0, d1
	move.l a2, d2
	move.l d4, d3
	jsr PROFILE_EXPORT_DOS_WRITE(a6)
	move.l d5, d1
	jsr PROFILE_EXPORT_DOS_CLOSE(a6)
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; writeRecord
	.endsection

	.section data, kind=data
ProgressFile
	.byte "opforge-profile.ofpr", 0
WorkFile
	.byte "opforge-profile.ofwk", 0
SymbolFile
	.byte "opforge-profile.ofse", 0
RuntimeFile
	.byte "opforge-profile.ofvm", 0
PlatformFile
	.byte "opforge-profile.ofio", 0
	.endsection
	.endmodule
