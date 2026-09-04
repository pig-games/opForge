; Bounded observation-only platform I/O, clear, and copy profile.
;
; @opforge-owner: debug.amigaos.platform_profile
; @opforge-slice: documentation/plans/slices/native-porting-slice-platform-io-v1.toml
; @opforge-role: implementation
;
; Passive observer ABI: all Record/Set/Class/Range/Begin/Finish entry points
; preserve D0-D7/A0-A6 and CCR with balanced stack saves. GetRecord returns A0
; and otherwise preserves registers/CCR. Only the private fixed record is
; touched; no DOS calls, request/service/last-error buffers, or allocations.
; Call sites are default-off and must stay outside compare/branch pairs.
; Stabilization: bounded Item 0e bridge; explicit residual coverage is documented.
; This provisional schema alone is not corpus-attribution evidence.

	.module debug.amigaos.platform_profile
	.cpu 68020

	.pub

OPFORGE_PLATFORM_MAGIC                   = $4f46494f; "OFIO"
OPFORGE_PLATFORM_SCHEMA_VERSION          = 2
OPFORGE_PLATFORM_RECORD_BYTES            = 528

OPFORGE_PLATFORM_FLAG_ACTIVE             = 1
OPFORGE_PLATFORM_FLAG_COMPLETE           = 2
OPFORGE_PLATFORM_FLAG_INCOMPLETE         = 4
OPFORGE_PLATFORM_FLAG_IO_ENABLED         = 8
OPFORGE_PLATFORM_FLAG_BULK_ENABLED       = 16

OPFORGE_PLATFORM_CLASS_SOURCE            = 1
OPFORGE_PLATFORM_CLASS_BOOTSTRAP         = 2
OPFORGE_PLATFORM_CLASS_MODULE            = 3
OPFORGE_PLATFORM_CLASS_PACKAGE           = 4
OPFORGE_PLATFORM_CLASS_ARTIFACT          = 5
OPFORGE_PLATFORM_CLASS_COUNT             = 5

OPFORGE_PLATFORM_RANGE_OTHER             = 0
OPFORGE_PLATFORM_RANGE_SESSION           = 1
OPFORGE_PLATFORM_RANGE_PACKAGE           = 2
OPFORGE_PLATFORM_RANGE_STATE             = 3
OPFORGE_PLATFORM_RANGE_PRESENCE          = 4
OPFORGE_PLATFORM_RANGE_COUNT             = 5
OPFORGE_PLATFORM_PHASE_COUNT             = 9
OPFORGE_PLATFORM_BULK_ROW_BYTES          = 24

OPFORGE_PLATFORM_OVERFLOW_OPENS          = 1
OPFORGE_PLATFORM_OVERFLOW_READS          = 2
OPFORGE_PLATFORM_OVERFLOW_READ_BYTES     = 4
OPFORGE_PLATFORM_OVERFLOW_WRITES         = 8
OPFORGE_PLATFORM_OVERFLOW_WRITE_BYTES    = 16
OPFORGE_PLATFORM_OVERFLOW_CLOSES         = 32
OPFORGE_PLATFORM_OVERFLOW_CLEAR          = 64
OPFORGE_PLATFORM_OVERFLOW_COPY           = 128
OPFORGE_PLATFORM_OVERFLOW_SOURCE         = 256
OPFORGE_PLATFORM_OVERFLOW_UNKNOWN_ID     = 512
OPFORGE_PLATFORM_OVERFLOW_SHORT          = 1024

OPFORGE_PLATFORM_MAGIC_OFFSET            = 0
OPFORGE_PLATFORM_SCHEMA_OFFSET           = 4
OPFORGE_PLATFORM_FLAGS_OFFSET            = 6
OPFORGE_PLATFORM_RUN_ID_OFFSET           = 8
OPFORGE_PLATFORM_PHASE_OFFSET            = 12
OPFORGE_PLATFORM_PASS_OFFSET             = 14
OPFORGE_PLATFORM_CURRENT_CLASS_OFFSET    = 16
OPFORGE_PLATFORM_CURRENT_RANGE_OFFSET    = 18
OPFORGE_PLATFORM_OPENS_OFFSET            = 20
OPFORGE_PLATFORM_CLOSES_OFFSET           = 40
OPFORGE_PLATFORM_READS_OFFSET            = 60
OPFORGE_PLATFORM_READ_BYTES_OFFSET       = 80
OPFORGE_PLATFORM_WRITES_OFFSET           = 100
OPFORGE_PLATFORM_WRITE_BYTES_OFFSET      = 120
OPFORGE_PLATFORM_SEEKS_OFFSET            = 140
OPFORGE_PLATFORM_CLEAR_CALLS_OFFSET      = 144
OPFORGE_PLATFORM_CLEAR_REQUESTED_OFFSET  = 148
OPFORGE_PLATFORM_CLEAR_COMPLETED_OFFSET  = 152
OPFORGE_PLATFORM_COPY_CALLS_OFFSET       = 156
OPFORGE_PLATFORM_COPY_REQUESTED_OFFSET   = 160
OPFORGE_PLATFORM_COPY_COMPLETED_OFFSET   = 164
OPFORGE_PLATFORM_SOURCE_BYTES_OFFSET     = 168
OPFORGE_PLATFORM_LOGICAL_LINES_OFFSET    = 172
OPFORGE_PLATFORM_MODULE_CANDIDATES_OFFSET = 176
OPFORGE_PLATFORM_SHORT_READS_OFFSET      = 180
OPFORGE_PLATFORM_OVERFLOW_OFFSET         = 184
OPFORGE_PLATFORM_EXIT_STATUS_OFFSET      = 188
OPFORGE_PLATFORM_BULK_RANGES_OFFSET      = 192
OPFORGE_PLATFORM_BULK_PHASES_OFFSET      = 312

	.section code, kind=code

; Return the authoritative fixed-size platform record.
; Inputs: none. Output: A0 = 528-byte record. Other registers and CCR preserved.
opforgePlatformProfileGetRecordV1	.block
	lea OpforgePlatformRecord, a0
	rts
	.bend  ; opforgePlatformProfileGetRecordV1

; Start one correlated platform profile.
; Inputs: D0.L = run ID. Outputs/clobbers: none; all registers and CCR preserved.
opforgePlatformProfileBeginRunV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	move.l d0, d7
	lea OpforgePlatformRecord, a5
	movea.l a5, a0
	moveq #0, d0
	move.w #(OPFORGE_PLATFORM_RECORD_BYTES / 4) - 1, d1
clearLoop
	move.l d0, (a0)+
	dbf d1, clearLoop
	move.l #OPFORGE_PLATFORM_MAGIC, OPFORGE_PLATFORM_MAGIC_OFFSET(a5)
	move.w #OPFORGE_PLATFORM_SCHEMA_VERSION, OPFORGE_PLATFORM_SCHEMA_OFFSET(a5)
	move.w #OPFORGE_PLATFORM_FLAG_ACTIVE, OPFORGE_PLATFORM_FLAGS_OFFSET(a5)
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	ori.w #OPFORGE_PLATFORM_FLAG_IO_ENABLED, OPFORGE_PLATFORM_FLAGS_OFFSET(a5)
	.endif
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	ori.w #OPFORGE_PLATFORM_FLAG_BULK_ENABLED, OPFORGE_PLATFORM_FLAGS_OFFSET(a5)
	.endif
	move.l d7, OPFORGE_PLATFORM_RUN_ID_OFFSET(a5)
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgePlatformProfileBeginRunV1

; Retain the current progress phase/pass.
; Inputs: D0.W = phase; D1.W = pass. Outputs/clobbers: none; CCR preserved.
opforgePlatformProfileSetContextV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	move.w d0, OPFORGE_PLATFORM_PHASE_OFFSET(a5)
	move.w d1, OPFORGE_PLATFORM_PASS_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgePlatformProfileSetContextV1

; Select the current I/O class. Inputs: D0.W = class. Outputs/clobbers: none.
opforgePlatformProfileSetClassV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	tst.w d0
	beq.s unknown
	cmpi.w #OPFORGE_PLATFORM_CLASS_COUNT, d0
	bhi.s unknown
	move.w d0, OPFORGE_PLATFORM_CURRENT_CLASS_OFFSET(a5)
	bra.s return
unknown
	ori.l #OPFORGE_PLATFORM_OVERFLOW_UNKNOWN_ID, OPFORGE_PLATFORM_OVERFLOW_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileSetClassV1

opforgePlatformProfileClassSourceV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_CLASS_SOURCE, d0
	bsr.w opforgePlatformProfileSetClassV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileClassSourceV1

opforgePlatformProfileClassBootstrapV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_CLASS_BOOTSTRAP, d0
	bsr.w opforgePlatformProfileSetClassV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileClassBootstrapV1

opforgePlatformProfileClassModuleV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_CLASS_MODULE, d0
	bsr.w opforgePlatformProfileSetClassV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileClassModuleV1

opforgePlatformProfileClassPackageV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_CLASS_PACKAGE, d0
	bsr.w opforgePlatformProfileSetClassV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileClassPackageV1

opforgePlatformProfileClassArtifactV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_CLASS_ARTIFACT, d0
	bsr.w opforgePlatformProfileSetClassV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileClassArtifactV1

; Select the NEXT bulk operation's range. Completion consumes the selection.
; Inputs: D0.W = range. Outputs/clobbers: none; CCR preserved.
opforgePlatformProfileSetRangeV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	cmpi.w #OPFORGE_PLATFORM_RANGE_COUNT - 1, d0
	bhi.s unknown
	move.w d0, OPFORGE_PLATFORM_CURRENT_RANGE_OFFSET(a5)
	bra.s return
unknown
	ori.l #OPFORGE_PLATFORM_OVERFLOW_UNKNOWN_ID, OPFORGE_PLATFORM_OVERFLOW_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileSetRangeV1

opforgePlatformProfileRangeSessionV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_RANGE_SESSION, d0
	bsr.w opforgePlatformProfileSetRangeV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRangeSessionV1

opforgePlatformProfileRangePackageV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_RANGE_PACKAGE, d0
	bsr.w opforgePlatformProfileSetRangeV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRangePackageV1

opforgePlatformProfileRangeStateV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_RANGE_STATE, d0
	bsr.w opforgePlatformProfileSetRangeV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRangeStateV1

; Select written-address presence storage for the next bulk operation.
; Inputs: none. Outputs/clobbers: none; CCR preserved.
opforgePlatformProfileRangePresenceV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #OPFORGE_PLATFORM_RANGE_PRESENCE, d0
	bsr.w opforgePlatformProfileSetRangeV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRangePresenceV1

; Count one open by current class. Inputs: none.
opforgePlatformProfileRecordOpenV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	bsr.w classOffset
	bmi.s return
	lea OPFORGE_PLATFORM_OPENS_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_PLATFORM_OVERFLOW_OPENS, d0
	bsr.w increment
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRecordOpenV1

; Count one close by current class. Inputs: none.
opforgePlatformProfileRecordCloseV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	bsr.w classOffset
	bmi.s return
	lea OPFORGE_PLATFORM_CLOSES_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_PLATFORM_OVERFLOW_CLOSES, d0
	bsr.w increment
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRecordCloseV1

; Count one read. Inputs: D0.L = completed bytes or -1; D1.L = requested bytes.
opforgePlatformProfileRecordReadV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.w return
	move.l d0, d6
	move.l d1, d7
	bsr.w classOffset
	bmi.w return
	lea OPFORGE_PLATFORM_READS_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_PLATFORM_OVERFLOW_READS, d0
	bsr.w increment
	move.l d6, d5
	bmi.s return
	cmp.l d7, d5
	bhs.s addBytes
	lea OPFORGE_PLATFORM_SHORT_READS_OFFSET(a5), a0
	move.l #OPFORGE_PLATFORM_OVERFLOW_SHORT, d0
	bsr.w increment
addBytes
	tst.l d5
	beq.s return
	lea OPFORGE_PLATFORM_READ_BYTES_OFFSET(a5), a0
	adda.w d4, a0
	move.l #OPFORGE_PLATFORM_OVERFLOW_READ_BYTES, d0
	move.l d5, d1
	bsr.w addTo
	move.w OPFORGE_PLATFORM_CURRENT_CLASS_OFFSET(a5), d2
	cmpi.w #OPFORGE_PLATFORM_CLASS_SOURCE, d2
	bne.s return
	lea OPFORGE_PLATFORM_SOURCE_BYTES_OFFSET(a5), a0
	move.l #OPFORGE_PLATFORM_OVERFLOW_SOURCE, d0
	move.l d5, d1
	bsr.w addTo
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRecordReadV1

; Count one write. Inputs: D0.L = completed bytes or -1; D1.L = requested bytes.
opforgePlatformProfileRecordWriteV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	move.l d0, d6
	bsr.w classOffset
	bmi.s return
	lea OPFORGE_PLATFORM_WRITES_OFFSET(a5), a0
	adda.w d4, a0
	moveq #OPFORGE_PLATFORM_OVERFLOW_WRITES, d0
	bsr.w increment
	move.l d6, d5
	bmi.s return
	beq.s return
	lea OPFORGE_PLATFORM_WRITE_BYTES_OFFSET(a5), a0
	adda.w d4, a0
	move.l #OPFORGE_PLATFORM_OVERFLOW_WRITE_BYTES, d0
	move.l d5, d1
	bsr.w addTo
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRecordWriteV1

; Record an already completed clear (deterministic oracles only).
; Inputs: D0.L = byte count. Outputs/clobbers: none; CCR preserved.
opforgePlatformProfileRecordClearV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	bsr.w opforgePlatformProfileClearRequestedV1
	bsr.w opforgePlatformProfileClearCompletedV1
	.endif
	rts
	.bend  ; opforgePlatformProfileRecordClearV1

; Record an already completed variable-length copy, including its terminator.
; Inputs: D0.L = copied bytes. Outputs/clobbers: none; CCR preserved.
; Fixed-size production helpers use separate request/completion entry points.
opforgePlatformProfileRecordCopyV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	bsr.w opforgePlatformProfileCopyRequestedV1
	bsr.w opforgePlatformProfileCopyCompletedV1
	.endif
	rts
	.bend  ; opforgePlatformProfileRecordCopyV1

; Count a clear request before its memory loop.
; Inputs: D0.L = requested bytes. Outputs/clobbers: none; CCR preserved.
opforgePlatformProfileClearRequestedV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	movem.l d6-d7, -(sp)
	moveq #0, d6
	moveq #0, d7
	bsr.w recordBulk
	movem.l (sp)+, d6-d7
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileClearRequestedV1

; Count bytes only after the clear loop returns. Consumes the selected range.
; Inputs: D0.L = completed bytes. Outputs/clobbers: none; CCR preserved.
opforgePlatformProfileClearCompletedV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	movem.l d6-d7, -(sp)
	moveq #0, d6
	moveq #1, d7
	bsr.w recordBulk
	movem.l (sp)+, d6-d7
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileClearCompletedV1

; Count a fixed-length copy request before its memory loop.
; Inputs: D0.L = requested bytes. Outputs/clobbers: none; CCR preserved.
opforgePlatformProfileCopyRequestedV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	movem.l d6-d7, -(sp)
	moveq #12, d6
	moveq #0, d7
	bsr.w recordBulk
	movem.l (sp)+, d6-d7
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileCopyRequestedV1

; Count bytes only after the copy loop returns. Consumes the selected range.
; Inputs: D0.L = completed bytes. Outputs/clobbers: none; CCR preserved.
opforgePlatformProfileCopyCompletedV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	move.w ccr, -(sp)
	movem.l d6-d7, -(sp)
	moveq #12, d6
	moveq #1, d7
	bsr.w recordBulk
	movem.l (sp)+, d6-d7
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileCopyCompletedV1

; Count one processed logical source line.
opforgePlatformProfileRecordLogicalLineV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	lea OPFORGE_PLATFORM_LOGICAL_LINES_OFFSET(a5), a0
	move.l #OPFORGE_PLATFORM_OVERFLOW_SOURCE, d0
	bsr.w increment
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRecordLogicalLineV1

; Count one module-discovery candidate file.
opforgePlatformProfileRecordModuleCandidateV1	.block
	.ifndef OPFORGE_PROGRESS_PLATFORM_NO_IO
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	lea OPFORGE_PLATFORM_MODULE_CANDIDATES_OFFSET(a5), a0
	move.l #OPFORGE_PLATFORM_OVERFLOW_SOURCE, d0
	bsr.w increment
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	.endif
	rts
	.bend  ; opforgePlatformProfileRecordModuleCandidateV1

; Seal the correlated record. Inputs: D0.L = guest/CLI status.
opforgePlatformProfileFinishV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.s return
	move.l d0, OPFORGE_PLATFORM_EXIT_STATUS_OFFSET(a5)
	andi.w #$fffe, OPFORGE_PLATFORM_FLAGS_OFFSET(a5)
	tst.l d0
	bne.s incomplete
	ori.w #OPFORGE_PLATFORM_FLAG_COMPLETE, OPFORGE_PLATFORM_FLAGS_OFFSET(a5)
	bra.s terminal
incomplete
	ori.w #OPFORGE_PLATFORM_FLAG_INCOMPLETE, OPFORGE_PLATFORM_FLAGS_OFFSET(a5)
terminal
	clr.w OPFORGE_PLATFORM_CURRENT_CLASS_OFFSET(a5)
	clr.w OPFORGE_PLATFORM_CURRENT_RANGE_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgePlatformProfileFinishV1

	.priv

; Add one bulk request/completion to aggregate, range, and phase totals.
; Inputs: D0.L bytes; D6.W = 0 clear / 12 copy; D7.W = 0 request / 1 done.
; Outputs/clobbers: none, CCR preserved. Caller owns request/done pairing;
; primitives cannot change phase or nest another bulk operation mid-loop.
recordBulk	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpforgePlatformRecord, a5
	bsr.w profileIsActive
	beq.w return
	move.l d0, d5
	moveq #0, d4
	move.w OPFORGE_PLATFORM_CURRENT_RANGE_OFFSET(a5), d4
	cmpi.w #OPFORGE_PLATFORM_RANGE_COUNT, d4
	bhs.w unknown
	mulu.w #OPFORGE_PLATFORM_BULK_ROW_BYTES, d4
	lea OPFORGE_PLATFORM_BULK_RANGES_OFFSET(a5), a2
	adda.l d4, a2
	adda.w d6, a2
	moveq #0, d4
	move.w OPFORGE_PLATFORM_PHASE_OFFSET(a5), d4
	cmpi.w #OPFORGE_PLATFORM_PHASE_COUNT, d4
	bhs.w unknown
	mulu.w #OPFORGE_PLATFORM_BULK_ROW_BYTES, d4
	lea OPFORGE_PLATFORM_BULK_PHASES_OFFSET(a5), a3
	adda.l d4, a3
	adda.w d6, a3
	lea OPFORGE_PLATFORM_CLEAR_CALLS_OFFSET(a5), a1
	adda.w d6, a1
	moveq #OPFORGE_PLATFORM_OVERFLOW_CLEAR, d0
	tst.w d6
	beq.s haveOverflowBit
	move.l #OPFORGE_PLATFORM_OVERFLOW_COPY, d0
haveOverflowBit
	tst.w d7
	bne.s completed
	movea.l a1, a0
	bsr.w increment
	movea.l a2, a0
	bsr.w increment
	movea.l a3, a0
	bsr.w increment
	moveq #4, d4
	bra.s addBytes
completed
	moveq #8, d4
addBytes
	move.l d5, d1
	movea.l a1, a0
	adda.w d4, a0
	bsr.w addTo
	movea.l a2, a0
	adda.w d4, a0
	bsr.w addTo
	movea.l a3, a0
	adda.w d4, a0
	bsr.w addTo
	tst.w d7
	beq.s return
	clr.w OPFORGE_PLATFORM_CURRENT_RANGE_OFFSET(a5)
	bra.s return
unknown
	ori.l #OPFORGE_PLATFORM_OVERFLOW_UNKNOWN_ID, OPFORGE_PLATFORM_OVERFLOW_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; recordBulk

profileIsActive	.block
	move.w OPFORGE_PLATFORM_FLAGS_OFFSET(a5), d3
	andi.w #OPFORGE_PLATFORM_FLAG_ACTIVE, d3
	rts
	.bend  ; profileIsActive

; Return D4.W class byte offset or N set on unknown class.
classOffset	.block
	moveq #0, d4
	move.w OPFORGE_PLATFORM_CURRENT_CLASS_OFFSET(a5), d4
	beq.s unknown
	cmpi.w #OPFORGE_PLATFORM_CLASS_COUNT, d4
	bhi.s unknown
	subq.w #1, d4
	lsl.w #2, d4
	tst.w d4
	rts
unknown
	ori.l #OPFORGE_PLATFORM_OVERFLOW_UNKNOWN_ID, OPFORGE_PLATFORM_OVERFLOW_OFFSET(a5)
	moveq #-1, d4
	rts
	.bend  ; classOffset

increment	.block
	cmpi.l #-1, (a0)
	beq.s overflow
	addq.l #1, (a0)
	rts
overflow
	or.l d0, OPFORGE_PLATFORM_OVERFLOW_OFFSET(a5)
	rts
	.bend  ; increment

; Add D1.L to (A0) saturating; D0.L = overflow bit.
addTo	.block
	tst.l d1
	beq.s return
	move.l (a0), d2
	cmpi.l #-1, d2
	beq.s overflow
	add.l d1, d2
	bcs.s overflow
	move.l d2, (a0)
return
	rts
overflow
	move.l #-1, (a0)
	or.l d0, OPFORGE_PLATFORM_OVERFLOW_OFFSET(a5)
	rts
	.bend  ; addTo

	.endsection

	.section bss, kind=bss
	.align 4
OpforgePlatformRecord
	.res byte, OPFORGE_PLATFORM_RECORD_BYTES
	.endsection

	.endmodule
