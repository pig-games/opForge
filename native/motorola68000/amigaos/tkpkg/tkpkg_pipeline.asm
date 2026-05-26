; Package-backed pipeline-resolution module for the tkpkg native runtime.

	.module tkpkg.amigaos.pipeline
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.token_policy as policy  ;(tkpkgTokenPolicyResolveLocatorV1)

NO_PACKAGE_TEXT_LEN                  = 41
UNRESOLVED_CPU_TEXT_LEN              = 33
UNRESOLVED_FAMILY_TEXT_LEN           = 33
UNRESOLVED_DIALECT_TEXT_LEN          = 34
MISSING_PROGRAM_TEXT_LEN             = 36
MISSING_PARSER_PROGRAM_TEXT_LEN      = 33
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

MissingParserProgramText
	.byte "OTR001: missing parser VM program", 0

IdentifierTooLongText
	.byte "OTR004: package identifier too long", 0

	.endsection

	.section code, kind=code
	.pub

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
; - D0: 0 on success, abi.STATUS_BAD_REQUEST_V1 or abi.STATUS_RUNTIME_ERROR_V1 on
;   failure.
; - A1/D1: runtime failure text pointer/length when D0 is runtime error.
; ---------------------------------------------------------------------------
tkpkgPipelineSetActiveV1	.block
	btst #0, buffers.PackageStateFlags  ; require load_package before selecting any runtime pipeline
	bne.s parseRequest
	lea NoPackageText, a1
	moveq #NO_PACKAGE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

parseRequest
	bsr.w parseRequestV1
	tst.b d0
	bne.w done
	bsr.w resolveHierarchyV1
	tst.b d0
	bne.w done
	bsr.w policy.resolveLocatorV1
	tst.b d0
	bne.w done
	bsr.w resolveTokenizerVmLocatorV1
	tst.b d0
	bne.w done
	bsr.w resolveParserVmLocatorV1
	tst.b d0
	bne.w done
	bsr.w commitActiveSelectionV1
	tst.b d0
	bne.w done
	moveq #0, d0

done
	rts

; Parse `<cpu-id>\0<dialect-id?>` into pending request locators.
	.bend  ; tkpkgPipelineSetActiveV1

	.priv

parseRequestV1	.block
	lea buffers.PendingFamilyOffsetLo, a3
	moveq #36, d0

clearPendingLoop
	clr.b (a3)+
	dbf d0, clearPendingLoop
	moveq #0, d0
	move.b abi.CB_INPUT_LEN(a0), d0
	moveq #0, d1
	move.b 19(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #2, d0
	blo.w badRequest
	move.w d0, d6
	moveq #0, d1
	move.b abi.CB_INPUT_PTR(a0), d1
	moveq #0, d2
	move.b 17(a0), d2
	lsl.w #8, d2
	or.w d2, d1
	tst.w d1
	beq.w badRequest
	lea 0(a0, d1.W), a1
	moveq #0, d3
	move.w d6, d4

separatorLoop
	tst.w d4
	beq.w badRequest
	tst.b 0(a1, d3.W)
	beq.s separatorFound
	addq.w #1, d3
	subq.w #1, d4
	bra.w separatorLoop

separatorFound
	tst.w d3
	beq.w badRequest
	move.w d1, d4
	move.w d3, d5
	lea buffers.PendingCpuOffsetLo, a3
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
	beq.s noDialect
	lea buffers.PendingDialectOffsetLo, a3
	move.w d4, d2
	lsl.w #8, d2
	moveq #0, d5
	move.b buffers.PendingCpuOffsetLo, d5
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

noDialect
	lea buffers.PendingDialectOffsetLo, a3
	clr.l (a3)
	moveq #0, d0
	rts

badRequest
	moveq #abi.STATUS_BAD_REQUEST_V1, d0
	rts
	.bend  ; parseRequestV1

; Resolve the pending CPU/family/dialect hierarchy before runtime locators.
resolveHierarchyV1	.block
	bsr.w findCpuEntryV1
	tst.b d0
	bne.w cpuUnresolved
	bsr.w findFamilyEntryV1
	tst.b d0
	bne.w familyUnresolved
	bsr.w resolveSelectedDialectV1
	tst.b d0
	bne.w dialectUnresolved
	moveq #0, d0
	rts

cpuUnresolved
	lea UnresolvedCpuText, a1
	moveq #UNRESOLVED_CPU_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

familyUnresolved
	lea UnresolvedFamilyText, a1
	moveq #UNRESOLVED_FAMILY_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

dialectUnresolved
	lea UnresolvedDialectText, a1
	moveq #UNRESOLVED_DIALECT_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts
	.bend  ; resolveHierarchyV1

; Find the CPUS record matching the requested CPU id and stage its family.
findCpuEntryV1	.block
	lea buffers.PendingCpuOffsetLo, a3
	bsr.w readRequestLocatorPtrLenV1
	move.w d3, d5
	movea.l a1, a5
	lea buffers.CpusChunkOffsetLo, a3
	bsr.w chunkPtrFromLocatorV1
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.w cpuMissing
	tst.w d0
	beq.w cpuMissing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

cpuLoop
	bsr.w locateStringV1
	tst.b d1
	bne.w cpuMissing
	move.w d0, d6
	movea.l a1, a4
	move.l a2, -(sp)
	move.w d6, d0
	move.w d5, d1
	movea.l a4, a1
	movea.l a5, a2
	bsr.w stringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w skipCpuEntry
	lea buffers.PendingCpuOffsetLo, a3
	movea.l a4, a1
	move.w d6, d0
	bsr.w storePackageStringLocatorV1
	bsr.w locateStringV1
	tst.b d1
	bne.w cpuMissing
	lea buffers.PendingFamilyOffsetLo, a3
	bsr.w storePackageStringLocatorV1
	bsr.w locateOptionalStringV1
	tst.b d1
	bne.w cpuMissing
	lea buffers.PendingDefaultDialectOffsetLo, a3
	bsr.w storeOptionalPackageStringLocatorV1
	moveq #0, d0
	rts

skipCpuEntry
	bsr.w skipStringV1
	tst.b d1
	bne.w cpuMissing
	bsr.w skipOptionalStringV1
	tst.b d1
	bne.w cpuMissing
	dbf d7, cpuLoop

cpuMissing
	moveq #1, d0
	rts
	.bend  ; findCpuEntryV1

; Find the FAMS record matching the family referenced by the selected CPU.
findFamilyEntryV1	.block
	lea buffers.PackageStorage, a6
	lea buffers.PendingFamilyOffsetLo, a3
	bsr.w readLocatorPtrLenV1
	move.w d3, d5
	movea.l a1, a5
	lea buffers.FamsChunkOffsetLo, a3
	bsr.w chunkPtrFromLocatorV1
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.w familyMissing
	tst.w d0
	beq.w familyMissing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

familyLoop
	bsr.w locateStringV1
	tst.b d1
	bne.w familyMissing
	move.w d0, d6
	movea.l a1, a4
	move.l a2, -(sp)
	move.w d6, d0
	move.w d5, d1
	movea.l a4, a1
	movea.l a5, a2
	bsr.w stringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w skipFamilyEntry
	bsr.w locateStringV1
	tst.b d1
	bne.w familyMissing
	lea buffers.PendingCanonicalDialectOffsetLo, a3
	bsr.w storePackageStringLocatorV1
	moveq #0, d0
	rts

skipFamilyEntry
	bsr.w skipStringV1
	tst.b d1
	bne.w familyMissing
	dbf d7, familyLoop

familyMissing
	moveq #1, d0
	rts
	.bend  ; findFamilyEntryV1

; Choose requested dialect when present, otherwise CPU default, then family canonical.
resolveSelectedDialectV1	.block
	lea buffers.PendingDialectOffsetLo, a3
	bsr.w readLocatorPtrLenV1
	tst.w d3
	beq.s defaultDialect
	lea buffers.PendingDialectOffsetLo, a3
	bsr.w findRequestedDialectEntryV1
	tst.b d0
	beq.s dialectDone
	moveq #1, d0
	rts

defaultDialect
	lea buffers.PendingDefaultDialectOffsetLo, a3
	bsr.w readLocatorPtrLenV1
	tst.w d3
	beq.s canonicalDialect
	lea buffers.PendingDefaultDialectOffsetLo, a3
	bsr.w findDialectEntryV1
	tst.b d0
	beq.s dialectDone

canonicalDialect
	lea buffers.PendingCanonicalDialectOffsetLo, a3
	bsr.w findDialectEntryV1
	tst.b d0
	bne.s dialectMissing

dialectDone
	moveq #0, d0
	rts

dialectMissing
	moveq #1, d0
	rts
	.bend  ; resolveSelectedDialectV1

; Resolve the caller-requested dialect id through the DIAL chunk.
findRequestedDialectEntryV1	.block
	bsr.w readRequestLocatorPtrLenV1
	bra.w findDialectEntryLoadedV1
	.bend  ; findRequestedDialectEntryV1

; Resolve a package-owned dialect locator through the DIAL chunk.
findDialectEntryV1	.block
	bsr.w readLocatorPtrLenV1
	bra.w findDialectEntryLoadedV1
	.bend  ; findDialectEntryV1

findDialectEntryLoadedV1	.block
	move.w d3, d5
	movea.l a1, a5
	lea buffers.PendingFamilyOffsetLo, a3
	bsr.w readLocatorPtrLenV1
	move.w d3, d6
	movea.l a1, a4
	lea buffers.DialChunkOffsetLo, a3
	bsr.w chunkPtrFromLocatorV1
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.w dialectNotFound
	tst.w d0
	beq.w dialectNotFound
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

dialectLoop
	bsr.w locateStringV1
	tst.b d1
	bne.w dialectNotFound
	move.w d0, -(sp)
	movea.l a1, a0
	move.l a2, -(sp)
	move.w 4(sp), d0
	move.w d5, d1
	movea.l a0, a1
	movea.l a5, a2
	bsr.w stringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w skipDialectEntry
	bsr.w locateStringV1
	tst.b d1
	beq.s dialectFamilyLoaded
	addq.w #2, sp
	bra.w dialectNotFound

dialectFamilyLoaded
	move.w d0, d2
	move.l a2, -(sp)
	move.w d2, d0
	move.w d6, d1
	movea.l a4, a2
	bsr.w stringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	beq.w skipDialectAllowList
	move.w d2, -(sp)
	bsr.w dialectAllowsCpuV1
	move.w (sp)+, d2
	tst.b d0
	beq.w dialectAccept
	addq.w #2, sp
	bra.w dialectNext

skipDialectEntry
	addq.w #2, sp
	bsr.w skipStringV1
	bra.s skipDialectAllowListPayload

skipDialectAllowList
	addq.w #2, sp

skipDialectAllowListPayload
	bsr.w skipOptionalStringListV1
	tst.b d1
	bne.w dialectNotFound

dialectNext
	dbf d7, dialectLoop

dialectNotFound
	moveq #1, d0
	rts

dialectAccept
	lea buffers.PendingDialectOffsetLo, a3
	movea.l a0, a1
	move.w (sp)+, d0
	bsr.w storePackageStringLocatorV1
	moveq #0, d0
	rts
	.bend  ; findDialectEntryLoadedV1

dialectAllowsCpuV1	.block
	move.w d7, -(sp)
	moveq #1, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.s dialectRejected
	move.b (a2)+, d0
	beq.s dialectAllowed
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.s dialectRejected
	move.w d0, d7
	lea 4(a2), a2
	tst.w d7
	beq.s dialectRejected
	move.l a6, -(sp)
	lea buffers.PendingCpuOffsetLo, a3
	bsr.w readLocatorPtrLenV1
	movea.l (sp)+, a6
	move.w d3, d5
	movea.l a1, a5
	subq.w #1, d7

allowLoop
	bsr.w locateStringV1
	tst.b d1
	bne.s dialectRejected
	move.w d0, d6
	movea.l a1, a4
	move.l a2, -(sp)
	move.w d6, d0
	move.w d5, d1
	movea.l a4, a1
	movea.l a5, a2
	bsr.w stringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	bne.s dialectAllowed
	dbf d7, allowLoop

dialectRejected
	move.w (sp)+, d7
	moveq #1, d0
	rts

dialectAllowed
	move.w (sp)+, d7
	moveq #0, d0
	rts
	.bend  ; dialectAllowsCpuV1

; Resolve tokenizer VM program with dialect -> CPU -> family owner precedence.
resolveTokenizerVmLocatorV1	.block
	moveq #buffers.SCOPED_OWNER_DIALECT, d0
	lea buffers.PendingDialectOffsetLo, a3
	bsr.w findTokenizerVmOwnerV1
	tst.b d0
	beq.s vmResolved
	moveq #buffers.SCOPED_OWNER_CPU, d0
	lea buffers.PendingCpuOffsetLo, a3
	bsr.w findTokenizerVmOwnerV1
	tst.b d0
	beq.s vmResolved
	moveq #buffers.SCOPED_OWNER_FAMILY, d0
	lea buffers.PendingFamilyOffsetLo, a3
	bsr.w findTokenizerVmOwnerV1
	tst.b d0
	beq.s vmResolved
	lea MissingProgramText, a1
	moveq #MISSING_PROGRAM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

vmResolved
	moveq #0, d0
	rts
	.bend  ; resolveTokenizerVmLocatorV1

; Resolve parser VM program with dialect -> CPU -> family owner precedence.
resolveParserVmLocatorV1	.block
	moveq #buffers.SCOPED_OWNER_DIALECT, d0
	lea buffers.PendingDialectOffsetLo, a3
	bsr.w findParserVmOwnerV1
	tst.b d0
	beq.s parserVmResolved
	moveq #buffers.SCOPED_OWNER_CPU, d0
	lea buffers.PendingCpuOffsetLo, a3
	bsr.w findParserVmOwnerV1
	tst.b d0
	beq.s parserVmResolved
	moveq #buffers.SCOPED_OWNER_FAMILY, d0
	lea buffers.PendingFamilyOffsetLo, a3
	bsr.w findParserVmOwnerV1
	tst.b d0
	beq.s parserVmResolved
	lea MissingParserProgramText, a1
	moveq #MISSING_PARSER_PROGRAM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

parserVmResolved
	moveq #0, d0
	rts
	.bend  ; resolveParserVmLocatorV1

; Find a TKVM record matching the scoped owner locator in A3/D0.
findTokenizerVmOwnerV1	.block
	move.b d0, d6
	lea buffers.PackageStorage, a6
	bsr.w readLocatorPtrLenV1
	move.w d3, d5
	movea.l a1, a5
	lea buffers.TkvmChunkOffsetLo, a3
	bsr.w chunkPtrFromLocatorV1
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.w vmOwnerMissing
	tst.w d0
	beq.w vmOwnerMissing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

vmLoop
	movea.l a2, a4
	moveq #1, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.w vmOwnerMissing
	move.b (a2)+, d4
	bsr.w locateStringV1
	tst.b d1
	bne.w vmOwnerMissing
	cmp.b d6, d4
	bne.w vmSkipEntry
	move.w d0, d2
	move.l a2, -(sp)
	move.w d2, d0
	move.w d5, d1
	movea.l a5, a2
	bsr.w stringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	bne.w vmFound

vmSkipEntry
	bsr.w skipTokenizerVmEntryV1
	tst.b d1
	bne.w vmOwnerMissing
	dbf d7, vmLoop

vmOwnerMissing
	moveq #1, d0
	rts

vmFound
	bsr.w skipTokenizerVmEntryV1
	tst.b d1
	bne.w vmOwnerMissing
	lea buffers.PendingTokenizerVmOffsetLo, a3
	movea.l a4, a1
	move.l a2, d0
	sub.l a4, d0
	bsr.w storeRecordLocatorV1
	move.b d6, buffers.PendingTokenizerVmOwnerTag
	moveq #0, d0
	rts
	.bend  ; findTokenizerVmOwnerV1

; Find a PRVM record matching the scoped owner locator in A3/D0.
findParserVmOwnerV1	.block
	move.b d0, d6
	lea buffers.PackageStorage, a6
	bsr.w readLocatorPtrLenV1
	move.w d3, d5
	movea.l a1, a5
	lea buffers.PrvmChunkOffsetLo, a3
	bsr.w chunkPtrFromLocatorV1
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.w parserOwnerMissing
	tst.w d0
	beq.w parserOwnerMissing
	move.w d0, d7
	subq.w #1, d7
	lea 4(a2), a2

parserLoop
	movea.l a2, a4
	moveq #1, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.w parserOwnerMissing
	move.b (a2)+, d4
	bsr.w locateStringV1
	tst.b d1
	bne.w parserOwnerMissing
	cmp.b d6, d4
	bne.w parserSkipEntry
	move.w d0, d2
	move.l a2, -(sp)
	move.w d2, d0
	move.w d5, d1
	movea.l a5, a2
	bsr.w stringEqAsciiCasefoldV1
	movea.l (sp)+, a2
	tst.b d0
	bne.w parserFound

parserSkipEntry
	bsr.w skipParserVmEntryV1
	tst.b d1
	bne.w parserOwnerMissing
	dbf d7, parserLoop

parserOwnerMissing
	moveq #1, d0
	rts

parserFound
	bsr.w skipParserVmEntryV1
	tst.b d1
	bne.w parserOwnerMissing
	lea buffers.PendingParserVmOffsetLo, a3
	movea.l a4, a1
	move.l a2, d0
	sub.l a4, d0
	bsr.w storeRecordLocatorV1
	move.b d6, buffers.PendingParserVmOwnerTag
	moveq #0, d0
	rts
	.bend  ; findParserVmOwnerV1

; Skip one TKVM chunk entry while preserving the package cursor invariants.
skipTokenizerVmEntryV1	.block
	move.w d7, -(sp)
	moveq #TOKENIZER_VM_ENTRY_PREFIX_SIZE, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.w vmSkipBoundsFail
	lea TOKENIZER_VM_ENTRY_PREFIX_SIZE(a2), a2
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.w vmSkipBoundsFail
	move.w d0, d7
	lea 4(a2), a2
	moveq #0, d0
	move.w d7, d0
	lsl.l #2, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.w vmSkipBoundsFail
	tst.w d7
	beq.s vmAfterOffsets
	subq.w #1, d7

vmOffsetLoop
	addq.w #4, a2
	dbf d7, vmOffsetLoop

vmAfterOffsets
	moveq #TOKENIZER_VM_ENTRY_FIXED_TAIL_SIZE, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.w vmSkipBoundsFail
	lea TOKENIZER_VM_ENTRY_FIXED_TAIL_SIZE(a2), a2
	bsr.w skipStringV1
	tst.b d1
	bne.w vmSkipBoundsFail
	bsr.w skipStringV1
	tst.b d1
	bne.w vmSkipBoundsFail
	bsr.w skipStringV1
	tst.b d1
	bne.w vmSkipBoundsFail
	bsr.w skipStringV1
	tst.b d1
	bne.w vmSkipBoundsFail
	bsr.w skipStringV1
	tst.b d1
	bne.w vmSkipBoundsFail
	bsr.w skipStringV1
	tst.b d1
	bne.w vmSkipBoundsFail
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.w vmSkipBoundsFail
	move.l d0, d2
	move.l d0, d3
	addq.l #4, d3
	move.l d3, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.w vmSkipBoundsFail
	lea 4(a2), a2
	adda.l d2, a2
	move.w (sp)+, d7
	moveq #0, d1
	rts

vmSkipBoundsFail
	move.w (sp)+, d7
	moveq #1, d1
	rts
	.bend  ; skipTokenizerVmEntryV1

; Skip one PRVM chunk entry while preserving the package cursor invariants.
skipParserVmEntryV1	.block
	moveq #2, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.w parserSkipBoundsFail
	lea 2(a2), a2
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.w parserSkipBoundsFail
	move.l d0, d3
	addq.l #4, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.w parserSkipBoundsFail
	lea 4(a2), a2
	adda.l d3, a2
	moveq #0, d1
	rts

parserSkipBoundsFail
	moveq #1, d1
	rts
	.bend  ; skipParserVmEntryV1

; Commit fully resolved pending locators into active service state.
commitActiveSelectionV1	.block
	lea buffers.PendingCpuOffsetLo, a3
	lea buffers.ActiveCpuBuffer.l, a2
	bsr.w copyLocatorToBufferV1
	tst.b d0
	bne.w commitDone
	lea buffers.PendingDialectOffsetLo, a3
	lea buffers.ActiveDialectBuffer.l, a2
	bsr.w copyLocatorToBufferV1
	tst.b d0
	bne.w commitDone
	lea buffers.PendingFamilyOffsetLo, a3
	lea buffers.ActiveFamilyBuffer.l, a2
	bsr.w copyLocatorToBufferV1
	tst.b d0
	bne.w commitDone
	lea buffers.PendingTokenPolicyOffsetLo, a3
	lea buffers.ActiveTokenPolicyOffsetLo.l, a2
	bsr.w copyRecordLocatorV1
	move.b buffers.PendingTokenPolicyOwnerTag, d0
	move.b d0, buffers.ActiveTokenPolicyOwnerTag
	lea buffers.PendingTokenizerVmOffsetLo, a3
	lea buffers.ActiveTokenizerVmOffsetLo.l, a2
	bsr.w copyRecordLocatorV1
	move.b buffers.PendingTokenizerVmOwnerTag, d0
	move.b d0, buffers.ActiveTokenizerVmOwnerTag
	lea buffers.PendingParserVmOffsetLo, a3
	lea buffers.ActiveParserVmOffsetLo.l, a2
	bsr.w copyRecordLocatorV1
	move.b buffers.PendingParserVmOwnerTag, d0
	move.b d0, buffers.ActiveParserVmOwnerTag
	ori.b #buffers.PACKAGE_STATE_PIPELINE_ACTIVE, buffers.PackageStateFlags
	moveq #0, d0

commitDone
	rts
	.bend  ; commitActiveSelectionV1

copyLocatorToBufferV1	.block
	bsr.w readLocatorPtrLenV1
	cmpi.w #buffers.PIPELINE_ID_BUFFER_CAPACITY, d3
	bhs.s copyBufferTooLong
	move.w d3, d2
	tst.w d2
	beq.s copyBufferDone
	subq.w #1, d2

copyBufferLoop
	move.b (a1)+, (a2)+
	dbf d2, copyBufferLoop

copyBufferDone
	clr.b (a2)
	moveq #0, d0
	rts

copyBufferTooLong
	lea IdentifierTooLongText, a1
	moveq #IDENTIFIER_TOO_LONG_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts
	.bend  ; copyLocatorToBufferV1

copyRecordLocatorV1	.block
	move.l (a3), (a2)
	rts
	.bend  ; copyRecordLocatorV1

storePackageStringLocatorV1	.block
	move.l a6, -(sp)
	move.l a1, d2
	lea buffers.PackageStorage, a6
	sub.l a6, d2
	move.b d2, (a3)+
	lsr.w #8, d2
	move.b d2, (a3)+
	move.b d0, (a3)+
	lsr.w #8, d0
	move.b d0, (a3)+
	movea.l (sp)+, a6
	rts
	.bend  ; storePackageStringLocatorV1

storeOptionalPackageStringLocatorV1	.block
	tst.w d0
	beq.s clearOptionalLocator
	bsr.w storePackageStringLocatorV1
	rts

clearOptionalLocator
	clr.l (a3)
	rts
	.bend  ; storeOptionalPackageStringLocatorV1

storeRecordLocatorV1	.block
	move.l a6, -(sp)
	move.l a1, d2
	lea buffers.PackageStorage, a6
	sub.l a6, d2
	move.b d2, (a3)+
	lsr.w #8, d2
	move.b d2, (a3)+
	move.b d0, (a3)+
	lsr.w #8, d0
	move.b d0, (a3)+
	movea.l (sp)+, a6
	rts
	.bend  ; storeRecordLocatorV1

readLocatorPtrLenV1	.block
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
	lea buffers.PackageStorage, a6
	lea 0(a6, d2.W), a1
	rts
	.bend  ; readLocatorPtrLenV1

readRequestLocatorPtrLenV1	.block
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
	.bend  ; readRequestLocatorPtrLenV1

chunkPtrFromLocatorV1	.block
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
	lea buffers.PackageStorage, a6
	lea 0(a6, d0.W), a2
	lea 0(a2, d7.W), a6
	rts
	.bend  ; chunkPtrFromLocatorV1

locateStringV1	.block
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.s locateStringBoundsFail
	move.l d0, d2
	move.l d0, d3
	addq.l #4, d3
	move.l d3, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.s locateStringBoundsFail
	move.l d2, d0
	lea 4(a2), a1
	lea 4(a2), a2
	adda.l d0, a2
	moveq #0, d1
	rts

locateStringBoundsFail
	moveq #0, d0
	movea.l d0, a1
	moveq #1, d1
	rts
	.bend  ; locateStringV1

skipStringV1	.block
	bsr.w locateStringV1
	rts
	.bend  ; skipStringV1

locateOptionalStringV1	.block
	moveq #1, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.s optionalBoundsFail
	move.b (a2)+, d1
	beq.s optionalNone
	bsr.w locateStringV1
	rts

optionalNone
	moveq #0, d0
	movea.l d0, a1
	moveq #0, d1
	rts

optionalBoundsFail
	moveq #0, d0
	movea.l d0, a1
	moveq #1, d1
	rts
	.bend  ; locateOptionalStringV1

skipOptionalStringV1	.block
	bsr.w locateOptionalStringV1
	rts
	.bend  ; skipOptionalStringV1

skipOptionalStringListV1	.block
	move.w d7, -(sp)
	moveq #1, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.s skipListBoundsFail
	move.b (a2)+, d1
	beq.s skipListDone
	bsr.w readU32LeLow16V1
	tst.b d1
	bne.s skipListBoundsFail
	move.w d0, d7
	lea 4(a2), a2
	tst.w d7
	beq.s skipListDone
	subq.w #1, d7

skipListLoop
	bsr.w skipStringV1
	tst.b d1
	bne.s skipListBoundsFail
	dbf d7, skipListLoop

skipListDone
	move.w (sp)+, d7
	moveq #0, d1
	rts

skipListBoundsFail
	move.w (sp)+, d7
	moveq #1, d1
	rts
	.bend  ; skipOptionalStringListV1

readU32LeLow16V1	.block
	moveq #4, d0
	bsr.w requireBytesV1
	tst.b d1
	bne.s readU32BoundsFail
	moveq #0, d0
	move.b (a2), d0
	moveq #0, d1
	move.b 1(a2), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts

readU32BoundsFail
	moveq #0, d0
	moveq #1, d1
	rts
	.bend  ; readU32LeLow16V1

requireBytesV1	.block
	movea.l a2, a1
	adda.l d0, a1
	cmpa.l a6, a1
	bhi.s requireBytesFail
	moveq #0, d1
	rts

requireBytesFail
	moveq #1, d1
	rts
	.bend  ; requireBytesV1

stringEqAsciiCasefoldV1	.block
	cmp.w d1, d0
	bne.s stringNoMatch
	move.w d0, d4
	tst.w d4
	beq.s stringMatch
	subq.w #1, d4

stringLoop
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d3
	move.b (a2)+, d3
	move.b d2, d0
	bsr.w foldAsciiLowerV1
	move.b d0, d2
	move.b d3, d0
	bsr.w foldAsciiLowerV1
	cmp.b d0, d2
	bne.s stringNoMatch
	dbf d4, stringLoop

stringMatch
	moveq #1, d0
	rts

stringNoMatch
	moveq #0, d0
	rts
	.bend  ; stringEqAsciiCasefoldV1

foldAsciiLowerV1	.block
	cmpi.b #'A', d0
	blo.s foldDone
	cmpi.b #'Z', d0
	bhi.s foldDone
	ori.b #$20, d0

foldDone
	rts
	.bend  ; foldAsciiLowerV1

placeholder	.block
	rts
	.bend  ; placeholder
	
	.endsection
	.endmodule
