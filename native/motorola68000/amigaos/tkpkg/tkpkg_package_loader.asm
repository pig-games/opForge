; Package-loader module for the tkpkg native runtime.

	.module tkpkg.amigaos.package_loader
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN)
	.use tkpkg.amigaos.buffers (PACKAGE_STORAGE_CAPACITY, PACKAGE_STATE_LOADED)
	.use tkpkg.amigaos.buffers (PACKAGE_STATE_CLEAR_LONGWORD_COUNT)
	.use tkpkg.amigaos.buffers (PACKAGE_CHUNK_FAMS, PACKAGE_CHUNK_CPUS)
	.use tkpkg.amigaos.buffers (PACKAGE_CHUNK_DIAL, PACKAGE_CHUNK_TOKS)
	.use tkpkg.amigaos.buffers (PACKAGE_CHUNK_TKVM, PACKAGE_CHUNK_TABL)
	.use tkpkg.amigaos.buffers (PACKAGE_REQUIRED_CHUNK_FLAGS)
	.use tkpkg.amigaos.buffers (PackageStorage, PackageStateFlags)
	.use tkpkg.amigaos.buffers (PackageChunkFlags, PackageStorageLen)
	.use tkpkg.amigaos.buffers (PackageStorageLenHi, FamsChunkOffsetLo)
	.use tkpkg.amigaos.buffers (CpusChunkOffsetLo, DialChunkOffsetLo)
	.use tkpkg.amigaos.buffers (ToksChunkOffsetLo, TkvmChunkOffsetLo)
	.use tkpkg.amigaos.buffers (TablChunkOffsetLo)
	.use tkpkg.amigaos.buffers (ActiveCpuBuffer, ActiveDialectBuffer)

OPASM_HEADER_SIZE                    = 12
OPASM_TOC_ENTRY_SIZE                 = 12
INVALID_MAGIC_TEXT_LEN               = 29
UNSUPPORTED_VERSION_TEXT_LEN         = 35
INVALID_ENDIAN_TEXT_LEN              = 33
UNEXPECTED_EOF_TEXT_LEN              = 30
DUPLICATE_CHUNK_TEXT_LEN             = 33
MISSING_CHUNK_TEXT_LEN               = 30
CHUNK_BOUNDS_TEXT_LEN                = 27
PACKAGE_STATE_CLEAR_LONGWORD_LAST    = PACKAGE_STATE_CLEAR_LONGWORD_COUNT - 1

	.section data, kind=data

InvalidMagicText
	.byte "OPC001: invalid package magic", 0

UnsupportedVersionText
	.byte "OPC002: unsupported package version", 0

InvalidEndianText
	.byte "OPC003: invalid endianness marker", 0

UnexpectedEofText
	.byte "OPC004: unexpected end of file", 0

DuplicateChunkText
	.byte "OPC005: duplicate tokenizer chunk", 0

MissingChunkText
	.byte "OPC006: missing required chunk", 0

ChunkBoundsText
	.byte "OPC007: chunk out of bounds", 0

	.endsection

	.section code, kind=code

; ---------------------------------------------------------------------------
; Load and validate an encoded `.opasm` hierarchy package from the service CB.
;
; This is the package equivalent of the Rust VM model-loader path: copy the
; package bytes into native-owned storage, validate the container header/TOC,
; and record locators for the chunks needed by pipeline/tokenizer/encoder work.
;
; Inputs:
; - A0: validated tkpkg control block with CB_INPUT_PTR/LEN pointing at package
;   bytes inside the control block window.
;
; Outputs:
; - D0: 0 on success, nonzero STATUS/runtime code on failure.
; - A1/D1: failure message pointer/length on runtime failure paths.
; - packageStorage/package chunk locators are updated on success.
; ---------------------------------------------------------------------------

tkpkgPackageLoaderLoadV1
	bsr.w tkpkgPackageLoaderClearLoadedStateV1
	bsr.w tkpkgPackageLoaderReadInputLenV1
	tst.w d0
	beq.w tkpkgPackageLoaderInvalidMagic
	cmpi.w #PACKAGE_STORAGE_CAPACITY, d0
	bhi.w tkpkgPackageLoaderChunkBounds
	move.b d0, PackageStorageLen  ; store low byte of package length for later bounded TOC walks
	lsr.w #8, d0
	move.b d0, PackageStorageLenHi  ; high byte keeps package length portable in byte-addressed state
	bsr.w tkpkgPackageLoaderReadInputOffsetV1
	lea 0(a0, d1.W), a1  ; A1: caller package bytes inside the control-block window
	lea PackageStorage, a2  ; A2: native package storage used by later locator reads
	bsr.w tkpkgPackageLoaderCopyInputBytesV1
	lea PackageStorage, a1
	bsr.w tkpkgPackageLoaderValidateHeaderV1
	tst.b d0
	bne.s tkpkgPackageLoaderDone
	bsr.w tkpkgPackageLoaderValidateTocV1
	tst.b d0
	bne.s tkpkgPackageLoaderDone
	move.b #PACKAGE_STATE_LOADED, PackageStateFlags
	moveq #0, d0

tkpkgPackageLoaderDone
	rts

; ---------------------------------------------------------------------------
; Clear all package-derived state before loading a new package.
;
; Inputs:
; - none.
;
; Outputs:
; - package state, chunk flags, chunk locators, and active pipeline buffers are
;   zeroed as one contiguous longword range.
; ---------------------------------------------------------------------------

tkpkgPackageLoaderClearLoadedStateV1
	lea PackageStateFlags, a3
	move.w #PACKAGE_STATE_CLEAR_LONGWORD_LAST, d0

tkpkgPackageLoaderClearStateLoop
	clr.l (a3)+
	dbf d0, tkpkgPackageLoaderClearStateLoop
	rts

; Read CB_INPUT_LEN as a native 16-bit little-endian service length.
tkpkgPackageLoaderReadInputLenV1
	moveq #0, d0
	move.b CB_INPUT_LEN(a0), d0
	moveq #0, d1
	move.b 19(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts

; Read CB_INPUT_PTR as a native 16-bit control-block-relative offset.
tkpkgPackageLoaderReadInputOffsetV1
	moveq #0, d1
	move.b CB_INPUT_PTR(a0), d1
	moveq #0, d2
	move.b 17(a0), d2
	lsl.w #8, d2
	or.w d2, d1
	rts

; Copy the currently recorded package length from A1 to package storage at A2.
tkpkgPackageLoaderCopyInputBytesV1
	moveq #0, d2
	move.b PackageStorageLen, d2
	moveq #0, d3
	move.b PackageStorageLenHi, d3
	lsl.w #8, d3
	or.w d3, d2
	tst.w d2
	beq.s tkpkgPackageLoaderCopyDone

tkpkgPackageLoaderCopyLoop
	move.b (a1)+, (a2)+
	subq.w #1, d2
	bne.s tkpkgPackageLoaderCopyLoop

tkpkgPackageLoaderCopyDone
	rts

; Validate the fixed package header before any TOC offsets are trusted.
tkpkgPackageLoaderValidateHeaderV1
	moveq #0, d0
	cmpi.b #'O', (a1)
	bne.w tkpkgPackageLoaderInvalidMagic
	cmpi.b #'P', 1(a1)
	bne.w tkpkgPackageLoaderInvalidMagic
	cmpi.b #'C', 2(a1)
	bne.w tkpkgPackageLoaderInvalidMagic
	cmpi.b #'P', 3(a1)
	bne.w tkpkgPackageLoaderInvalidMagic
	cmpi.b #$01, 4(a1)
	bne.w tkpkgPackageLoaderUnsupportedVersion
	tst.b 5(a1)
	bne.w tkpkgPackageLoaderUnsupportedVersion
	cmpi.b #$34, 6(a1)
	bne.w tkpkgPackageLoaderInvalidEndian
	cmpi.b #$12, 7(a1)
	bne.w tkpkgPackageLoaderInvalidEndian
	rts

; Walk the package TOC, reject duplicates/bounds failures, and store locators.
tkpkgPackageLoaderValidateTocV1
	moveq #0, d7
	move.b PackageStorageLen, d7
	moveq #0, d6
	move.b PackageStorageLenHi, d6
	lsl.w #8, d6
	or.w d6, d7
	moveq #0, d0
	move.b 8(a1), d0
	moveq #0, d1
	move.b 9(a1), d1
	lsl.w #8, d1
	or.w d1, d0
	move.w d0, d2
	lsl.w #2, d2
	move.w d0, d3
	lsl.w #3, d3
	add.w d3, d2
	addi.w #OPASM_HEADER_SIZE, d2
	cmp.w d7, d2
	bhi.w tkpkgPackageLoaderUnexpectedEof
	lea OPASM_HEADER_SIZE(a1), a2
	tst.w d0
	beq.w tkpkgPackageLoaderMissingChunk
	move.w d0, d2
	subq.w #1, d2

tkpkgPackageLoaderTocLoop
	lea 4(a2), a3
	bsr.w tkpkgPackageLoaderReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgPackageLoaderChunkBounds
	move.w d0, d4
	lea 8(a2), a3
	bsr.w tkpkgPackageLoaderReadU32LeLow16V1
	tst.b d1
	bne.w tkpkgPackageLoaderChunkBounds
	move.w d0, d5
	move.w d4, d6
	add.w d5, d6
	bcs.w tkpkgPackageLoaderChunkBounds
	cmp.w d7, d6
	bhi.w tkpkgPackageLoaderChunkBounds

	cmpi.b #'F', (a2)
	bne.s tkpkgPackageLoaderCheckCpus
	cmpi.b #'A', 1(a2)
	bne.s tkpkgPackageLoaderCheckCpus
	cmpi.b #'M', 2(a2)
	bne.s tkpkgPackageLoaderCheckCpus
	cmpi.b #'S', 3(a2)
	bne.s tkpkgPackageLoaderCheckCpus
	btst #0, PackageChunkFlags
	bne.w tkpkgPackageLoaderDuplicateChunk
	lea FamsChunkOffsetLo, a3
	bsr.w tkpkgPackageLoaderStoreLocatorV1
	ori.b #PACKAGE_CHUNK_FAMS, PackageChunkFlags
	bra.w tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckCpus
	cmpi.b #'C', (a2)
	bne.s tkpkgPackageLoaderCheckDial
	cmpi.b #'P', 1(a2)
	bne.s tkpkgPackageLoaderCheckDial
	cmpi.b #'U', 2(a2)
	bne.s tkpkgPackageLoaderCheckDial
	cmpi.b #'S', 3(a2)
	bne.s tkpkgPackageLoaderCheckDial
	btst #1, PackageChunkFlags
	bne.w tkpkgPackageLoaderDuplicateChunk
	lea CpusChunkOffsetLo, a3
	bsr.w tkpkgPackageLoaderStoreLocatorV1
	ori.b #PACKAGE_CHUNK_CPUS, PackageChunkFlags
	bra.w tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckDial
	cmpi.b #'D', (a2)
	bne.s tkpkgPackageLoaderCheckToks
	cmpi.b #'I', 1(a2)
	bne.s tkpkgPackageLoaderCheckToks
	cmpi.b #'A', 2(a2)
	bne.s tkpkgPackageLoaderCheckToks
	cmpi.b #'L', 3(a2)
	bne.s tkpkgPackageLoaderCheckToks
	btst #2, PackageChunkFlags
	bne.w tkpkgPackageLoaderDuplicateChunk
	lea DialChunkOffsetLo, a3
	bsr.w tkpkgPackageLoaderStoreLocatorV1
	ori.b #PACKAGE_CHUNK_DIAL, PackageChunkFlags
	bra.w tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckToks
	cmpi.b #'T', (a2)
	bne.s tkpkgPackageLoaderCheckTkvm
	cmpi.b #'O', 1(a2)
	bne.s tkpkgPackageLoaderCheckTkvm
	cmpi.b #'K', 2(a2)
	bne.s tkpkgPackageLoaderCheckTkvm
	cmpi.b #'S', 3(a2)
	bne.s tkpkgPackageLoaderCheckTkvm
	btst #3, PackageChunkFlags
	bne.w tkpkgPackageLoaderDuplicateChunk
	lea ToksChunkOffsetLo, a3
	bsr.w tkpkgPackageLoaderStoreLocatorV1
	ori.b #PACKAGE_CHUNK_TOKS, PackageChunkFlags
	bra.w tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckTkvm
	cmpi.b #'T', (a2)
	bne.s tkpkgPackageLoaderCheckTabl
	cmpi.b #'K', 1(a2)
	bne.s tkpkgPackageLoaderCheckTabl
	cmpi.b #'V', 2(a2)
	bne.s tkpkgPackageLoaderCheckTabl
	cmpi.b #'M', 3(a2)
	bne.s tkpkgPackageLoaderCheckTabl
	btst #4, PackageChunkFlags
	bne.w tkpkgPackageLoaderDuplicateChunk
	lea TkvmChunkOffsetLo, a3
	bsr.w tkpkgPackageLoaderStoreLocatorV1
	ori.b #PACKAGE_CHUNK_TKVM, PackageChunkFlags
	bra.s tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckTabl
	cmpi.b #'T', (a2)
	bne.s tkpkgPackageLoaderNextTocEntry
	cmpi.b #'A', 1(a2)
	bne.s tkpkgPackageLoaderNextTocEntry
	cmpi.b #'B', 2(a2)
	bne.s tkpkgPackageLoaderNextTocEntry
	cmpi.b #'L', 3(a2)
	bne.s tkpkgPackageLoaderNextTocEntry
	btst #5, PackageChunkFlags
	bne.w tkpkgPackageLoaderDuplicateChunk
	lea TablChunkOffsetLo, a3
	bsr.w tkpkgPackageLoaderStoreLocatorV1
	ori.b #PACKAGE_CHUNK_TABL, PackageChunkFlags

tkpkgPackageLoaderNextTocEntry
	lea OPASM_TOC_ENTRY_SIZE(a2), a2
	dbf d2, tkpkgPackageLoaderTocLoop
	move.b PackageChunkFlags, d0
	andi.b #PACKAGE_REQUIRED_CHUNK_FLAGS, d0
	cmpi.b #PACKAGE_REQUIRED_CHUNK_FLAGS, d0
	bne.w tkpkgPackageLoaderMissingChunk
	moveq #0, d0
	rts

tkpkgPackageLoaderReadU32LeLow16V1
	moveq #0, d0
	move.b (a3), d0
	moveq #0, d1
	move.b 1(a3), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	tst.b 2(a3)
	bne.s tkpkgPackageLoaderReadU32HighBits
	tst.b 3(a3)
	bne.s tkpkgPackageLoaderReadU32HighBits
	rts

tkpkgPackageLoaderReadU32HighBits
	moveq #1, d1
	rts

tkpkgPackageLoaderStoreLocatorV1
	move.b d4, (a3)+
	lsr.w #8, d4
	move.b d4, (a3)+
	move.b d5, (a3)+
	lsr.w #8, d5
	move.b d5, (a3)+
	rts

tkpkgPackageLoaderInvalidMagic
	lea InvalidMagicText, a1
	moveq #INVALID_MAGIC_TEXT_LEN, d1
	moveq #1, d0
	rts

tkpkgPackageLoaderUnsupportedVersion
	lea UnsupportedVersionText, a1
	moveq #UNSUPPORTED_VERSION_TEXT_LEN, d1
	moveq #1, d0
	rts

tkpkgPackageLoaderInvalidEndian
	lea InvalidEndianText, a1
	moveq #INVALID_ENDIAN_TEXT_LEN, d1
	moveq #1, d0
	rts

tkpkgPackageLoaderUnexpectedEof
	lea UnexpectedEofText, a1
	moveq #UNEXPECTED_EOF_TEXT_LEN, d1
	moveq #1, d0
	rts

tkpkgPackageLoaderDuplicateChunk
	lea DuplicateChunkText, a1
	moveq #DUPLICATE_CHUNK_TEXT_LEN, d1
	moveq #1, d0
	rts

tkpkgPackageLoaderMissingChunk
	lea MissingChunkText, a1
	moveq #MISSING_CHUNK_TEXT_LEN, d1
	moveq #1, d0
	rts

tkpkgPackageLoaderChunkBounds
	lea ChunkBoundsText, a1
	moveq #CHUNK_BOUNDS_TEXT_LEN, d1
	moveq #1, d0
	rts

	.endsection
	.endmodule
