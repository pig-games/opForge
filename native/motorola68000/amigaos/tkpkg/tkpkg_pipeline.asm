; Package-backed pipeline-resolution module for the tkpkg native runtime.

	.module tkpkg.amigaos.pipeline
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN, STATUS_BAD_REQUEST_V1)
	.use tkpkg.amigaos.abi (STATUS_RUNTIME_ERROR_V1)
	.use tkpkg.amigaos.buffers (PACKAGE_STATE_LOADED, PACKAGE_STATE_PIPELINE_ACTIVE)
	.use tkpkg.amigaos.buffers (PIPELINE_ID_BUFFER_CAPACITY, SCOPED_OWNER_DIALECT)
	.use tkpkg.amigaos.buffers (SCOPED_OWNER_CPU, SCOPED_OWNER_FAMILY)
	.use tkpkg.amigaos.buffers (PackageStateFlags, PackageStorage, FamsChunkOffsetLo)
	.use tkpkg.amigaos.buffers (CpusChunkOffsetLo, DialChunkOffsetLo)
	.use tkpkg.amigaos.buffers (TkvmChunkOffsetLo, ActiveCpuBuffer)
	.use tkpkg.amigaos.buffers (ActiveDialectBuffer, ActiveFamilyBuffer)
	.use tkpkg.amigaos.buffers (ActiveTokenPolicyOffsetLo, ActiveTokenPolicyOwnerTag)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmOffsetLo, ActiveTokenizerVmOwnerTag)
	.use tkpkg.amigaos.buffers (PendingFamilyOffsetLo, PendingCpuOffsetLo)
	.use tkpkg.amigaos.buffers (PendingDialectOffsetLo, PendingDefaultDialectOffsetLo)
	.use tkpkg.amigaos.buffers (PendingCanonicalDialectOffsetLo)
	.use tkpkg.amigaos.buffers (PendingTokenPolicyOffsetLo)
	.use tkpkg.amigaos.buffers (PendingTokenPolicyOwnerTag)
	.use tkpkg.amigaos.buffers (PendingTokenizerVmOffsetLo)
	.use tkpkg.amigaos.buffers (PendingTokenizerVmOwnerTag)
	.use tkpkg.amigaos.token_policy (tkpkgTokenPolicyResolveLocatorV1)

NO_PACKAGE_TEXT_LEN                  = 41
UNRESOLVED_CPU_TEXT_LEN              = 33
UNRESOLVED_FAMILY_TEXT_LEN           = 33
UNRESOLVED_DIALECT_TEXT_LEN          = 34
MISSING_PROGRAM_TEXT_LEN             = 36
IDENTIFIER_TOO_LONG_TEXT_LEN         = 35
TOKENIZER_VM_ENTRY_PREFIX_SIZE      = 4
TOKENIZER_VM_ENTRY_FIXED_TAIL_SIZE  = 19

	.section data, kind=data

NoPackageText
	.byte "OTR001: set_pipeline requires load_package", 0

UnresolvedCpuText
	.byte "OTR004: unresolved package cpu id", 0

UnresolvedFamilyText
	.byte "OTR004: unresolved package family", 0

UnresolvedDialectText
	.byte "OTR004: unresolved package dialect", 0

MissingProgramText
	.byte "OTR001: missing tokenizer VM program", 0

IdentifierTooLongText
	.byte "OTR004: package identifier too long", 0

	.endsection

	.section code, kind=code

; ---------------------------------------------------------------------------
; Select the active package pipeline for a CPU/dialect request.
;
; Mirrors the Rust hierarchy execution-model selection step: resolve CPU,
; family, dialect, token policy, and tokenizer VM program from package chunks,
; then commit only a complete selection into active state.
;
; Inputs:
; - A0: validated tkpkg control block whose input window contains
;   `<cpu-id>\0<dialect-id?>`.
;
; Outputs:
; - D0: 0 on success, STATUS_BAD_REQUEST_V1 or STATUS_RUNTIME_ERROR_V1 on
;   failure.
; - A1/D1: runtime failure text pointer/length when D0 is runtime error.
; ---------------------------------------------------------------------------

tkpkgPipelineSetActiveV1
	btst #0, PackageStateFlags  ; require load_package before selecting any runtime pipeline
	bne.s tkpkgPipelineParseRequest
	lea NoPackageText, a1
	moveq #NO_PACKAGE_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgPipelineParseRequest
	bsr.w tkpkgPipelineParseRequestV1
	tst.b d0
	bne.w tkpkgPipelineDone
	bsr.w tkpkgPipelineResolveHierarchyV1
	tst.b d0
	bne.w tkpkgPipelineDone
	bsr.w tkpkgTokenPolicyResolveLocatorV1
	tst.b d0
	bne.w tkpkgPipelineDone
	bsr.w tkpkgPipelineResolveTokenizerVmLocatorV1
	tst.b d0
	bne.w tkpkgPipelineDone
	bsr.w tkpkgPipelineCommitActiveSelectionV1
	tst.b d0
	bne.w tkpkgPipelineDone
	moveq #0, d0

tkpkgPipelineDone
	rts

; Parse `<cpu-id>\0<dialect-id?>` into pending request locators.
tkpkgPipelineParseRequestV1
	lea PendingFamilyOffsetLo, a3
	moveq #29, d0

tkpkgPipelineClearPendingLoop
	clr.b (a3)+
	dbf d0, tkpkgPipelineClearPendingLoop
	moveq #0, d0
	move.b CB_INPUT_LEN(a0), d0
	moveq #0, d1
	move.b 19(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #2, d0
	blo.w tkpkgPipelineBadRequest
	move.w d0, d6
	moveq #0, d1
	move.b CB_INPUT_PTR(a0), d1
	moveq #0, d2
	move.b 17(a0), d2
	lsl.w #8, d2
	or.w d2, d1
	tst.w d1
	beq.w tkpkgPipelineBadRequest
	lea 0(a0, d1.W), a1
	moveq #0, d3
	move.w d6, d4

tkpkgPipelineSeparatorLoop
	tst.w d4
	beq.w tkpkgPipelineBadRequest
	tst.b 0(a1, d3.W)
	beq.s tkpkgPipelineSeparatorFound
	addq.w #1, d3
	subq.w #1, d4
	bra.w tkpkgPipelineSeparatorLoop

tkpkgPipelineSeparatorFound
	tst.w d3
	beq.w tkpkgPipelineBadRequest
	move.w d1, d4
	move.w d3, d5
	lea PendingCpuOffsetLo, a3
	move.b d4, (a3)+
	lsr.w #8, d4
	move.b d4, (a3)+
	move.b d5, (a3)+
	lsr.w #8, d5
	move.b d5, (a3)+
	move.w d6, d0
	move.w d3, d1
	sub.w d1, d0
	subq.w #1, d0
	beq.s tkpkgPipelineNoDialect
	lea PendingDialectOffsetLo, a3
	move.w d4, d2
	lsl.w #8, d2
	moveq #0, d5
	move.b PendingCpuOffsetLo, d5
	or.w d5, d2
	add.w d1, d2
	addq.w #1, d2
	move.b d2, (a3)+
	lsr.w #8, d2
	move.b d2, (a3)+
	move.b d0, (a3)+
	lsr.w #8, d0
	move.b d0, (a3)+
	moveq #0, d0
	rts

tkpkgPipelineNoDialect
	lea PendingDialectOffsetLo, a3
	clr.l (a3)
	moveq #0, d0
	rts

tkpkgPipelineBadRequest
	moveq #STATUS_BAD_REQUEST_V1, d0
	rts

; Resolve the pending CPU/family/dialect hierarchy before runtime locators.
tkpkgPipelineResolveHierarchyV1
	bsr.w tkpkgPipelineFindCpuEntryV1
	tst.b d0
	bne.w tkpkgPipelineCpuUnresolved
	bsr.w tkpkgPipelineFindFamilyEntryV1
	tst.b d0
	bne.w tkpkgPipelineFamilyUnresolved
	bsr.w tkpkgPipelineResolveSelectedDialectV1
	tst.b d0
	bne.w tkpkgPipelineDialectUnresolved
	moveq #0, d0
	rts

tkpkgPipelineCpuUnresolved
	lea UnresolvedCpuText, a1
	moveq #UNRESOLVED_CPU_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgPipelineFamilyUnresolved
	lea UnresolvedFamilyText, a1
	moveq #UNRESOLVED_FAMILY_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgPipelineDialectUnresolved
	lea UnresolvedDialectText, a1
	moveq #UNRESOLVED_DIALECT_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

; Find the CPUS record matching the requested CPU id and stage its family.
tkpkgPipelineFindCpuEntryV1
	lea PendingCpuOffsetLo, a3
	bsr.w tkpkgPipelineReadRequestLocatorPtrLenV1
	move.w d3, d5
	movea.l a1, a5
	lea CpusChunkOffsetLo, a3
	bsr.w tkpkgPipelineChunkPtrFromLocatorV1
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgPipelineCpuMissing
	tst.w d0
	beq.w tkpkgPipelineCpuMissing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

tkpkgPipelineCpuLoop
	bsr.w tkpkgPipelineLocateStringV1
	tst.b d1
	bne.w tkpkgPipelineCpuMissing
	move.w d0, d6
	movea.l a1, a4
	move.l a2, -(sp)
	move.w d6, d0
	move.w d5, d1
	movea.l a4, a1
	movea.l a5, a2
	bsr.w tkpkgPipelineStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w tkpkgPipelineSkipCpuEntry
	lea PendingCpuOffsetLo, a3
	movea.l a4, a1
	move.w d6, d0
	bsr.w tkpkgPipelineStorePackageStringLocatorV1
	bsr.w tkpkgPipelineLocateStringV1
	tst.b d1
	bne.w tkpkgPipelineCpuMissing
	lea PendingFamilyOffsetLo, a3
	bsr.w tkpkgPipelineStorePackageStringLocatorV1
	bsr.w tkpkgPipelineLocateOptionalStringV1
	tst.b d1
	bne.w tkpkgPipelineCpuMissing
	lea PendingDefaultDialectOffsetLo, a3
	bsr.w tkpkgPipelineStoreOptionalPackageStringLocatorV1
	moveq #0, d0
	rts

tkpkgPipelineSkipCpuEntry
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.w tkpkgPipelineCpuMissing
	bsr.w tkpkgPipelineSkipOptionalStringV1
	tst.b d1
	bne.w tkpkgPipelineCpuMissing
	dbf d7, tkpkgPipelineCpuLoop

tkpkgPipelineCpuMissing
	moveq #1, d0
	rts

; Find the FAMS record matching the family referenced by the selected CPU.
tkpkgPipelineFindFamilyEntryV1
	lea PackageStorage, a6
	lea PendingFamilyOffsetLo, a3
	bsr.w tkpkgPipelineReadLocatorPtrLenV1
	move.w d3, d5
	movea.l a1, a5
	lea FamsChunkOffsetLo, a3
	bsr.w tkpkgPipelineChunkPtrFromLocatorV1
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgPipelineFamilyMissing
	tst.w d0
	beq.w tkpkgPipelineFamilyMissing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

tkpkgPipelineFamilyLoop
	bsr.w tkpkgPipelineLocateStringV1
	tst.b d1
	bne.w tkpkgPipelineFamilyMissing
	move.w d0, d6
	movea.l a1, a4
	move.l a2, -(sp)
	move.w d6, d0
	move.w d5, d1
	movea.l a4, a1
	movea.l a5, a2
	bsr.w tkpkgPipelineStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w tkpkgPipelineSkipFamilyEntry
	bsr.w tkpkgPipelineLocateStringV1
	tst.b d1
	bne.w tkpkgPipelineFamilyMissing
	lea PendingCanonicalDialectOffsetLo, a3
	bsr.w tkpkgPipelineStorePackageStringLocatorV1
	moveq #0, d0
	rts

tkpkgPipelineSkipFamilyEntry
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.w tkpkgPipelineFamilyMissing
	dbf d7, tkpkgPipelineFamilyLoop

tkpkgPipelineFamilyMissing
	moveq #1, d0
	rts

; Choose requested dialect when present, otherwise CPU default, then family canonical.
tkpkgPipelineResolveSelectedDialectV1
	lea PendingDialectOffsetLo, a3
	bsr.w tkpkgPipelineReadLocatorPtrLenV1
	tst.w d3
	beq.s tkpkgPipelineDefaultDialect
	lea PendingDialectOffsetLo, a3
	bsr.w tkpkgPipelineFindRequestedDialectEntryV1
	tst.b d0
	beq.s tkpkgPipelineDialectDone
	moveq #1, d0
	rts

tkpkgPipelineDefaultDialect
	lea PendingDefaultDialectOffsetLo, a3
	bsr.w tkpkgPipelineReadLocatorPtrLenV1
	tst.w d3
	beq.s tkpkgPipelineCanonicalDialect
	lea PendingDefaultDialectOffsetLo, a3
	bsr.w tkpkgPipelineFindDialectEntryV1
	tst.b d0
	beq.s tkpkgPipelineDialectDone

tkpkgPipelineCanonicalDialect
	lea PendingCanonicalDialectOffsetLo, a3
	bsr.w tkpkgPipelineFindDialectEntryV1
	tst.b d0
	bne.s tkpkgPipelineDialectMissing

tkpkgPipelineDialectDone
	moveq #0, d0
	rts

tkpkgPipelineDialectMissing
	moveq #1, d0
	rts

; Resolve the caller-requested dialect id through the DIAL chunk.
tkpkgPipelineFindRequestedDialectEntryV1
	bsr.w tkpkgPipelineReadRequestLocatorPtrLenV1
	bra.s tkpkgPipelineFindDialectEntryLoaded

; Resolve a package-owned dialect locator through the DIAL chunk.
tkpkgPipelineFindDialectEntryV1
	bsr.w tkpkgPipelineReadLocatorPtrLenV1

tkpkgPipelineFindDialectEntryLoaded
	move.w d3, d5
	movea.l a1, a5
	lea PendingFamilyOffsetLo, a3
	bsr.w tkpkgPipelineReadLocatorPtrLenV1
	move.w d3, d6
	movea.l a1, a4
	lea DialChunkOffsetLo, a3
	bsr.w tkpkgPipelineChunkPtrFromLocatorV1
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgPipelineDialectNotFound
	tst.w d0
	beq.w tkpkgPipelineDialectNotFound
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

tkpkgPipelineDialectLoop
	bsr.w tkpkgPipelineLocateStringV1
	tst.b d1
	bne.w tkpkgPipelineDialectNotFound
	move.w d0, -(sp)
	movea.l a1, a0
	move.l a2, -(sp)
	move.w 4(sp), d0
	move.w d5, d1
	movea.l a0, a1
	movea.l a5, a2
	bsr.w tkpkgPipelineStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w tkpkgPipelineSkipDialectEntry
	bsr.w tkpkgPipelineLocateStringV1
	tst.b d1
	beq.s tkpkgPipelineDialectFamilyLoaded
	addq.w #2, sp
	bra.w tkpkgPipelineDialectNotFound

tkpkgPipelineDialectFamilyLoaded
	move.w d0, d2
	move.l a2, -(sp)
	move.w d2, d0
	move.w d6, d1
	movea.l a4, a2
	bsr.w tkpkgPipelineStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w tkpkgPipelineSkipDialectAllowList
	move.w d2, -(sp)
	bsr.w tkpkgPipelineDialectAllowsCpuV1
	move.w (sp)+, d2
	tst.b d0
	beq.w tkpkgPipelineDialectAccept
	addq.w #2, sp
	bra.w tkpkgPipelineDialectNext

tkpkgPipelineSkipDialectEntry
	addq.w #2, sp
	bsr.w tkpkgPipelineSkipStringV1
	bra.s tkpkgPipelineSkipDialectAllowListPayload

tkpkgPipelineSkipDialectAllowList
	addq.w #2, sp

tkpkgPipelineSkipDialectAllowListPayload
	bsr.w tkpkgPipelineSkipOptionalStringListV1
	tst.b d1
	bne.w tkpkgPipelineDialectNotFound

tkpkgPipelineDialectNext
	dbf d7, tkpkgPipelineDialectLoop

tkpkgPipelineDialectNotFound
	moveq #1, d0
	rts

tkpkgPipelineDialectAccept
	lea PendingDialectOffsetLo, a3
	movea.l a0, a1
	move.w (sp)+, d0
	bsr.w tkpkgPipelineStorePackageStringLocatorV1
	moveq #0, d0
	rts

tkpkgPipelineDialectAllowsCpuV1
	move.w d7, -(sp)
	moveq #1, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.s tkpkgPipelineDialectRejected
	move.b (a2)+, d0
	beq.s tkpkgPipelineDialectAllowed
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.s tkpkgPipelineDialectRejected
	move.w d0, d7
	lea 4(a2), a2
	tst.w d7
	beq.s tkpkgPipelineDialectRejected
	move.l a6, -(sp)
	lea PendingCpuOffsetLo, a3
	bsr.w tkpkgPipelineReadLocatorPtrLenV1
	movea.l (sp)+, a6
	move.w d3, d5
	movea.l a1, a5
	subq.w #1, d7

tkpkgPipelineAllowLoop
	bsr.w tkpkgPipelineLocateStringV1
	tst.b d1
	bne.s tkpkgPipelineDialectRejected
	move.w d0, d6
	movea.l a1, a4
	move.l a2, -(sp)
	move.w d6, d0
	move.w d5, d1
	movea.l a4, a1
	movea.l a5, a2
	bsr.w tkpkgPipelineStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	bne.s tkpkgPipelineDialectAllowed
	dbf d7, tkpkgPipelineAllowLoop

tkpkgPipelineDialectRejected
	move.w (sp)+, d7
	moveq #1, d0
	rts

tkpkgPipelineDialectAllowed
	move.w (sp)+, d7
	moveq #0, d0
	rts

; Resolve tokenizer VM program with dialect -> CPU -> family owner precedence.
tkpkgPipelineResolveTokenizerVmLocatorV1
	moveq #SCOPED_OWNER_DIALECT, d0
	lea PendingDialectOffsetLo, a3
	bsr.w tkpkgPipelineFindTokenizerVmOwnerV1
	tst.b d0
	beq.s tkpkgPipelineVmResolved
	moveq #SCOPED_OWNER_CPU, d0
	lea PendingCpuOffsetLo, a3
	bsr.w tkpkgPipelineFindTokenizerVmOwnerV1
	tst.b d0
	beq.s tkpkgPipelineVmResolved
	moveq #SCOPED_OWNER_FAMILY, d0
	lea PendingFamilyOffsetLo, a3
	bsr.w tkpkgPipelineFindTokenizerVmOwnerV1
	tst.b d0
	beq.s tkpkgPipelineVmResolved
	lea MissingProgramText, a1
	moveq #MISSING_PROGRAM_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgPipelineVmResolved
	moveq #0, d0
	rts

; Find a TKVM record matching the scoped owner locator in A3/D0.
tkpkgPipelineFindTokenizerVmOwnerV1
	move.b d0, d6
	lea PackageStorage, a6
	bsr.w tkpkgPipelineReadLocatorPtrLenV1
	move.w d3, d5
	movea.l a1, a5
	lea TkvmChunkOffsetLo, a3
	bsr.w tkpkgPipelineChunkPtrFromLocatorV1
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgPipelineVmOwnerMissing
	tst.w d0
	beq.w tkpkgPipelineVmOwnerMissing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

tkpkgPipelineVmLoop
	movea.l a2, a4
	moveq #1, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.w tkpkgPipelineVmOwnerMissing
	move.b (a2)+, d4
	bsr.w tkpkgPipelineLocateStringV1
	tst.b d1
	bne.w tkpkgPipelineVmOwnerMissing
	cmp.b d6, d4
	bne.w tkpkgPipelineVmSkipEntry
	move.w d0, d2
	move.l a2, -(sp)
	move.w d2, d0
	move.w d5, d1
	movea.l a5, a2
	bsr.w tkpkgPipelineStringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	bne.w tkpkgPipelineVmFound

tkpkgPipelineVmSkipEntry
	bsr.w tkpkgPipelineSkipTokenizerVmEntryV1
	tst.b d1
	bne.w tkpkgPipelineVmOwnerMissing
	dbf d7, tkpkgPipelineVmLoop

tkpkgPipelineVmOwnerMissing
	moveq #1, d0
	rts

tkpkgPipelineVmFound
	bsr.w tkpkgPipelineSkipTokenizerVmEntryV1
	tst.b d1
	bne.w tkpkgPipelineVmOwnerMissing
	lea PendingTokenizerVmOffsetLo, a3
	movea.l a4, a1
	move.l a2, d0
	sub.l a4, d0
	bsr.w tkpkgPipelineStoreRecordLocatorV1
	move.b d6, PendingTokenizerVmOwnerTag
	moveq #0, d0
	rts

; Skip one TKVM chunk entry while preserving the package cursor invariants.
tkpkgPipelineSkipTokenizerVmEntryV1
	move.w d7, -(sp)
	moveq #TOKENIZER_VM_ENTRY_PREFIX_SIZE, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	lea TOKENIZER_VM_ENTRY_PREFIX_SIZE(a2), a2
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	move.w d0, d7
	lea 4(a2), a2
	moveq #0, d0
	move.w d7, d0
	lsl.l #2, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	tst.w d7
	beq.s tkpkgPipelineVmAfterOffsets
	subq.w #1, d7

tkpkgPipelineVmOffsetLoop
	addq.w #4, a2
	dbf d7, tkpkgPipelineVmOffsetLoop

tkpkgPipelineVmAfterOffsets
	moveq #TOKENIZER_VM_ENTRY_FIXED_TAIL_SIZE, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	lea TOKENIZER_VM_ENTRY_FIXED_TAIL_SIZE(a2), a2
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	move.l d0, d2
	move.l d0, d3
	addq.l #4, d3
	move.l d3, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.w tkpkgPipelineVmSkipBoundsFail
	lea 4(a2), a2
	adda.l d2, a2
	move.w (sp)+, d7
	moveq #0, d1
	rts

tkpkgPipelineVmSkipBoundsFail
	move.w (sp)+, d7
	moveq #1, d1
	rts

; Commit fully resolved pending locators into active service state.
tkpkgPipelineCommitActiveSelectionV1
	lea PendingCpuOffsetLo, a3
	lea ActiveCpuBuffer.l, a2
	bsr.w tkpkgPipelineCopyLocatorToBufferV1
	tst.b d0
	bne.s tkpkgPipelineCommitDone
	lea PendingDialectOffsetLo, a3
	lea ActiveDialectBuffer.l, a2
	bsr.w tkpkgPipelineCopyLocatorToBufferV1
	tst.b d0
	bne.s tkpkgPipelineCommitDone
	lea PendingFamilyOffsetLo, a3
	lea ActiveFamilyBuffer.l, a2
	bsr.w tkpkgPipelineCopyLocatorToBufferV1
	tst.b d0
	bne.s tkpkgPipelineCommitDone
	lea PendingTokenPolicyOffsetLo, a3
	lea ActiveTokenPolicyOffsetLo.l, a2
	bsr.w tkpkgPipelineCopyRecordLocatorV1
	move.b PendingTokenPolicyOwnerTag, d0
	move.b d0, ActiveTokenPolicyOwnerTag
	lea PendingTokenizerVmOffsetLo, a3
	lea ActiveTokenizerVmOffsetLo.l, a2
	bsr.w tkpkgPipelineCopyRecordLocatorV1
	move.b PendingTokenizerVmOwnerTag, d0
	move.b d0, ActiveTokenizerVmOwnerTag
	ori.b #PACKAGE_STATE_PIPELINE_ACTIVE, PackageStateFlags
	moveq #0, d0

tkpkgPipelineCommitDone
	rts

tkpkgPipelineCopyLocatorToBufferV1
	bsr.w tkpkgPipelineReadLocatorPtrLenV1
	cmpi.w #PIPELINE_ID_BUFFER_CAPACITY, d3
	bhs.s tkpkgPipelineCopyBufferTooLong
	move.w d3, d2
	tst.w d2
	beq.s tkpkgPipelineCopyBufferDone
	subq.w #1, d2

tkpkgPipelineCopyBufferLoop
	move.b (a1)+, (a2)+
	dbf d2, tkpkgPipelineCopyBufferLoop

tkpkgPipelineCopyBufferDone
	clr.b (a2)
	moveq #0, d0
	rts

tkpkgPipelineCopyBufferTooLong
	lea IdentifierTooLongText, a1
	moveq #IDENTIFIER_TOO_LONG_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgPipelineCopyRecordLocatorV1
	move.l (a3), (a2)
	rts

tkpkgPipelineStorePackageStringLocatorV1
	move.l a6, -(sp)
	move.l a1, d2
	lea PackageStorage, a6
	sub.l a6, d2
	move.b d2, (a3)+
	lsr.w #8, d2
	move.b d2, (a3)+
	move.b d0, (a3)+
	lsr.w #8, d0
	move.b d0, (a3)+
	movea.l (sp)+, a6
	rts

tkpkgPipelineStoreOptionalPackageStringLocatorV1
	tst.w d0
	beq.s tkpkgPipelineClearOptionalLocator
	bsr.w tkpkgPipelineStorePackageStringLocatorV1
	rts

tkpkgPipelineClearOptionalLocator
	clr.l (a3)
	rts

tkpkgPipelineStoreRecordLocatorV1
	move.l a6, -(sp)
	move.l a1, d2
	lea PackageStorage, a6
	sub.l a6, d2
	move.b d2, (a3)+
	lsr.w #8, d2
	move.b d2, (a3)+
	move.b d0, (a3)+
	lsr.w #8, d0
	move.b d0, (a3)+
	movea.l (sp)+, a6
	rts

tkpkgPipelineReadLocatorPtrLenV1
	moveq #0, d2
	move.b (a3)+, d2
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d2
	moveq #0, d3
	move.b (a3)+, d3
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d3
	lea PackageStorage, a6
	lea 0(a6, d2.W), a1
	rts

tkpkgPipelineReadRequestLocatorPtrLenV1
	moveq #0, d2
	move.b (a3)+, d2
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d2
	moveq #0, d3
	move.b (a3)+, d3
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d3
	lea 0(a0, d2.W), a1
	rts

tkpkgPipelineChunkPtrFromLocatorV1
	moveq #0, d0
	move.b (a3)+, d0
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d7
	move.b (a3)+, d7
	moveq #0, d1
	move.b (a3)+, d1
	lsl.w #8, d1
	or.w d1, d7
	lea PackageStorage, a6
	lea 0(a6, d0.W), a2
	lea 0(a2, d7.W), a6
	rts

tkpkgPipelineLocateStringV1
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.s tkpkgPipelineLocateStringBoundsFail
	move.l d0, d2
	move.l d0, d3
	addq.l #4, d3
	move.l d3, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.s tkpkgPipelineLocateStringBoundsFail
	move.l d2, d0
	lea 4(a2), a1
	lea 4(a2), a2
	adda.l d0, a2
	moveq #0, d1
	rts

tkpkgPipelineLocateStringBoundsFail
	moveq #0, d0
	movea.l d0, a1
	moveq #1, d1
	rts

tkpkgPipelineSkipStringV1
	bsr.w tkpkgPipelineLocateStringV1
	rts

tkpkgPipelineLocateOptionalStringV1
	moveq #1, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.s tkpkgPipelineOptionalBoundsFail
	move.b (a2)+, d1
	beq.s tkpkgPipelineOptionalNone
	bsr.w tkpkgPipelineLocateStringV1
	rts

tkpkgPipelineOptionalNone
	moveq #0, d0
	movea.l d0, a1
	moveq #0, d1
	rts

tkpkgPipelineOptionalBoundsFail
	moveq #0, d0
	movea.l d0, a1
	moveq #1, d1
	rts

tkpkgPipelineSkipOptionalStringV1
	bsr.w tkpkgPipelineLocateOptionalStringV1
	rts

tkpkgPipelineSkipOptionalStringListV1
	move.w d7, -(sp)
	moveq #1, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.s tkpkgPipelineSkipListBoundsFail
	move.b (a2)+, d1
	beq.s tkpkgPipelineSkipListDone
	bsr.w tkpkgPipelineReadU32LeLow16V1
	tst.b d1
	bne.s tkpkgPipelineSkipListBoundsFail
	move.w d0, d7
	lea 4(a2), a2
	tst.w d7
	beq.s tkpkgPipelineSkipListDone
	subq.w #1, d7

tkpkgPipelineSkipListLoop
	bsr.w tkpkgPipelineSkipStringV1
	tst.b d1
	bne.s tkpkgPipelineSkipListBoundsFail
	dbf d7, tkpkgPipelineSkipListLoop

tkpkgPipelineSkipListDone
	move.w (sp)+, d7
	moveq #0, d1
	rts

tkpkgPipelineSkipListBoundsFail
	move.w (sp)+, d7
	moveq #1, d1
	rts

tkpkgPipelineReadU32LeLow16V1
	moveq #4, d0
	bsr.w tkpkgPipelineRequireBytesV1
	tst.b d1
	bne.s tkpkgPipelineReadU32BoundsFail
	moveq #0, d0
	move.b (a2), d0
	moveq #0, d1
	move.b 1(a2), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts

tkpkgPipelineReadU32BoundsFail
	moveq #0, d0
	moveq #1, d1
	rts

tkpkgPipelineRequireBytesV1
	movea.l a2, a1
	adda.l d0, a1
	cmpa.l a6, a1
	bhi.s tkpkgPipelineRequireBytesFail
	moveq #0, d1
	rts

tkpkgPipelineRequireBytesFail
	moveq #1, d1
	rts

tkpkgPipelineStringEqAsciiCasefoldV1
	cmp.w d1, d0
	bne.s tkpkgPipelineStringNoMatch
	move.w d0, d4
	tst.w d4
	beq.s tkpkgPipelineStringMatch
	subq.w #1, d4

tkpkgPipelineStringLoop
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d3
	move.b (a2)+, d3
	move.b d2, d0
	bsr.w tkpkgPipelineFoldAsciiLowerV1
	move.b d0, d2
	move.b d3, d0
	bsr.w tkpkgPipelineFoldAsciiLowerV1
	cmp.b d0, d2
	bne.s tkpkgPipelineStringNoMatch
	dbf d4, tkpkgPipelineStringLoop

tkpkgPipelineStringMatch
	moveq #1, d0
	rts

tkpkgPipelineStringNoMatch
	moveq #0, d0
	rts

tkpkgPipelineFoldAsciiLowerV1
	cmpi.b #'A', d0
	blo.s tkpkgPipelineFoldDone
	cmpi.b #'Z', d0
	bhi.s tkpkgPipelineFoldDone
	ori.b #$20, d0

tkpkgPipelineFoldDone
	rts

tkpkgPipelinePlaceholder
	rts

	.endsection
	.endmodule
