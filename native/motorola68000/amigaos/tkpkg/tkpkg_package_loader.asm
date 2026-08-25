; Package-loader module for the tkpkg native runtime.

	.module tkpkg.amigaos.package_loader
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers

OPASM_HEADER_SIZE                    = 12
OPASM_TOC_ENTRY_SIZE                 = 12
INVALID_MAGIC_TEXT_LEN               = 29
UNSUPPORTED_VERSION_TEXT_LEN         = 35
INVALID_ENDIAN_TEXT_LEN              = 33
UNEXPECTED_EOF_TEXT_LEN              = 30
DUPLICATE_CHUNK_TEXT_LEN             = 33
MISSING_CHUNK_TEXT_LEN               = 30
CHUNK_BOUNDS_TEXT_LEN                = 27
PACKAGE_STATE_CLEAR_BYTE_LAST        = buffers.PACKAGE_STATE_CLEAR_BYTE_COUNT - 1

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
	.pub

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
tkpkgPackageLoaderLoadV1	.block
	bsr.w clearLoadedState
	bsr.w readInputLen
	beq.w invalidMagic
	cmpi.l #buffers.PACKAGE_STORAGE_CAPACITY, d0
	bhi.w chunkBounds
	bsr.w storePackageStorageLen
	bsr.w readInputOffset
	lea 0(a0, d1.W), a1  ; A1: caller package bytes inside the control-block window
	lea buffers.PackageStorage, a2  ; A2: native package storage used by later locator reads
	bsr.w copyInputBytes
	moveq #0, d0
	move.b buffers.PackageStorageLen, d0
	moveq #0, d1
	move.b buffers.PackageStorageLenMidLo, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b buffers.PackageStorageLenMidHi, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b buffers.PackageStorageLenHi, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	bsr.w validateStagedPackageV1
	rts
	.bend  ; tkpkgPackageLoaderLoadV1

; ---------------------------------------------------------------------------
; Validate a package that has already been copied into PackageStorage.
;
; Inputs:
; - D0.L: staged package byte length.
;
; Outputs:
; - D0: 0 on success, nonzero STATUS/runtime code on failure.
; - A1/D1: failure message pointer/length on runtime failure paths.
; - package chunk locators are updated on success.
; ---------------------------------------------------------------------------
tkpkgPackageLoaderLoadStagedV1	.block
	bsr.w clearLoadedState
	bsr.w validateStagedPackageV1
	rts
	.bend  ; tkpkgPackageLoaderLoadStagedV1

	.priv

validateStagedPackageV1	.block
	tst.l d0
	beq.w invalidMagic
	cmpi.l #buffers.PACKAGE_STORAGE_CAPACITY, d0
	bhi.w chunkBounds
	bsr.w storePackageStorageLen
	lea buffers.PackageStorage, a1
	bsr.w validateHeader
	bne.s done
	bsr.w validateToc
	bne.s done
	move.b #buffers.PACKAGE_STATE_LOADED, buffers.PackageStateFlags
	moveq #0, d0

done
	rts
	.bend  ; validateStagedPackageV1

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
clearLoadedState	.block
	lea buffers.PackageStateFlags, a3
	move.w #PACKAGE_STATE_CLEAR_BYTE_LAST, d0

loop
	clr.b (a3)+
	dbf d0, loop
	rts
	.bend  ; clearLoadedState

; Inputs:
; - A0: control block whose CB_INPUT_LEN fields contain the service input length.
;
; Outputs:
; - D0.W: native 16-bit little-endian input length.
;
; Clobbers:
; - D0-D1/CCR
;
; CCR:
; - Reflects D0.W on return.
readInputLen	.block
	moveq #0, d0
	move.b abi.CB_INPUT_LEN(a0), d0
	moveq #0, d1
	move.b 19(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts
	.bend  ; readInputLen

; Read CB_INPUT_PTR as a native 16-bit control-block-relative offset.
readInputOffset	.block
	moveq #0, d1
	move.b abi.CB_INPUT_PTR(a0), d1
	moveq #0, d2
	move.b 17(a0), d2
	lsl.w #8, d2
	or.w d2, d1
	rts
	.bend  ; readInputOffset

; Copy the currently recorded package length from A1 to package storage at A2.
copyInputBytes	.block
	moveq #0, d2
	move.b buffers.PackageStorageLen, d2
	moveq #0, d3
	move.b buffers.PackageStorageLenMidLo, d3
	lsl.l #8, d3
	or.l d3, d2
	moveq #0, d3
	move.b buffers.PackageStorageLenMidHi, d3
	lsl.l #8, d3
	lsl.l #8, d3
	or.l d3, d2
	moveq #0, d3
	move.b buffers.PackageStorageLenHi, d3
	lsl.l #8, d3
	lsl.l #8, d3
	lsl.l #8, d3
	or.l d3, d2
	beq.s done

loop
	move.b (a1)+, (a2)+
	subq.l #1, d2
	bne.s loop

done
	rts
	.bend  ; copyInputBytes

; Inputs:
; - A1: package storage base.
;
; Outputs:
; - D0: 0 on success, 1 on header validation failure.
; - A1/D1: diagnostic pointer/length on failure.
;
; Clobbers:
; - D0-D1/A1/CCR
;
; CCR:
; - Reflects D0 on return.
validateHeader	.block
	moveq #0, d0
	cmpi.b #'O', (a1)
	bne.w invalidMagic
	cmpi.b #'P', 1(a1)
	bne.w invalidMagic
	cmpi.b #'C', 2(a1)
	bne.w invalidMagic
	cmpi.b #'P', 3(a1)
	bne.w invalidMagic
	cmpi.b #$01, 4(a1)
	bne.w unsupportedVersion
	tst.b 5(a1)
	bne.w unsupportedVersion
	cmpi.b #$34, 6(a1)
	bne.w invalidEndian
	cmpi.b #$12, 7(a1)
	bne.w invalidEndian
	rts
	.bend  ; validateHeader

; Inputs:
; - A1: package storage base whose fixed header has already been validated.
;
; Outputs:
; - D0: 0 on success, 1 on TOC/bounds/required-chunk failure.
; - A1/D1: diagnostic pointer/length on failure.
; - package chunk locator/flag buffers updated on success.
;
; Clobbers:
; - D0-D7/A1-A3/CCR
;
; CCR:
; - Reflects D0 on return.
validateToc	.block
	moveq #0, d7
	move.b buffers.PackageStorageLen, d7
	moveq #0, d6
	move.b buffers.PackageStorageLenMidLo, d6
	lsl.l #8, d6
	or.l d6, d7
	moveq #0, d6
	move.b buffers.PackageStorageLenMidHi, d6
	lsl.l #8, d6
	lsl.l #8, d6
	or.l d6, d7
	moveq #0, d6
	move.b buffers.PackageStorageLenHi, d6
	lsl.l #8, d6
	lsl.l #8, d6
	lsl.l #8, d6
	or.l d6, d7
	moveq #0, d0
	move.b 8(a1), d0
	moveq #0, d1
	move.b 9(a1), d1
	lsl.l #8, d1
	or.l d1, d0
	move.l d0, d2
	lsl.l #2, d2
	move.l d0, d3
	lsl.l #3, d3
	add.l d3, d2
	addi.l #OPASM_HEADER_SIZE, d2
	cmp.l d7, d2
	bhi.w unexpectedEof
	lea OPASM_HEADER_SIZE(a1), a2
	tst.l d0
	beq.w missingChunk
	move.w d0, d2
	subq.w #1, d2

tocLoop
	lea 4(a2), a3
	bsr.w readU32Le
	tst.b d1
	bne.w chunkBounds
	move.l d0, d4
	lea 8(a2), a3
	bsr.w readU32Le
	tst.b d1
	bne.w chunkBounds
	move.l d0, d5
	move.l d4, d6
	add.l d5, d6
	bcs.w chunkBounds
	cmp.l d7, d6
	bhi.w chunkBounds

	cmpi.b #'F', (a2)
	bne.s checkCpus
	cmpi.b #'A', 1(a2)
	bne.s checkCpus
	cmpi.b #'M', 2(a2)
	bne.s checkCpus
	cmpi.b #'S', 3(a2)
	bne.s checkCpus
	btst #0, buffers.PackageChunkFlags
	bne.w duplicateChunk
	lea buffers.FamsChunkOffsetLo, a3
	bsr.w storeLocator
	bset #0, buffers.PackageChunkFlags
	bra.w nextTocEntry

checkCpus
	cmpi.b #'C', (a2)
	bne.s checkCals
	cmpi.b #'P', 1(a2)
	bne.s checkCals
	cmpi.b #'U', 2(a2)
	bne.s checkCals
	cmpi.b #'S', 3(a2)
	bne.s checkCals
	btst #1, buffers.PackageChunkFlags
	bne.w duplicateChunk
	lea buffers.CpusChunkOffsetLo, a3
	bsr.w storeLocator
	bset #1, buffers.PackageChunkFlags
	bra.w nextTocEntry

checkCals
	cmpi.b #'C', (a2)
	bne.s checkDial
	cmpi.b #'A', 1(a2)
	bne.s checkDial
	cmpi.b #'L', 2(a2)
	bne.s checkDial
	cmpi.b #'S', 3(a2)
	bne.s checkDial
	btst #5, buffers.PackageChunkFlagsHi
	bne.w duplicateChunk
	lea buffers.CalsChunkOffsetLo, a3
	bsr.w storeLocator
	bset #5, buffers.PackageChunkFlagsHi
	bra.w nextTocEntry

checkDial
	cmpi.b #'D', (a2)
	bne.s checkToks
	cmpi.b #'I', 1(a2)
	bne.s checkToks
	cmpi.b #'A', 2(a2)
	bne.s checkToks
	cmpi.b #'L', 3(a2)
	bne.s checkToks
	btst #2, buffers.PackageChunkFlags
	bne.w duplicateChunk
	lea buffers.DialChunkOffsetLo, a3
	bsr.w storeLocator
	bset #2, buffers.PackageChunkFlags
	bra.w nextTocEntry

checkToks
	cmpi.b #'T', (a2)
	bne.s checkTkvm
	cmpi.b #'O', 1(a2)
	bne.s checkTkvm
	cmpi.b #'K', 2(a2)
	bne.s checkTkvm
	cmpi.b #'S', 3(a2)
	bne.s checkTkvm
	btst #3, buffers.PackageChunkFlags
	bne.w duplicateChunk
	lea buffers.ToksChunkOffsetLo, a3
	bsr.w storeLocator
	bset #3, buffers.PackageChunkFlags
	bra.w nextTocEntry

checkTkvm
	cmpi.b #'T', (a2)
	bne.w checkTabl
	cmpi.b #'K', 1(a2)
	bne.w checkTabl
	cmpi.b #'V', 2(a2)
	bne.w checkTabl
	cmpi.b #'M', 3(a2)
	bne.w checkTabl
	btst #4, buffers.PackageChunkFlags
	bne.w duplicateChunk
	lea buffers.TkvmChunkOffsetLo, a3
	bsr.w storeLocator
	bset #4, buffers.PackageChunkFlags
	bra.w nextTocEntry

checkTabl
	cmpi.b #'T', (a2)
	bne.w checkMsel
	cmpi.b #'A', 1(a2)
	bne.w checkMsel
	cmpi.b #'B', 2(a2)
	bne.w checkMsel
	cmpi.b #'L', 3(a2)
	bne.w checkMsel
	btst #5, buffers.PackageChunkFlags
	bne.w duplicateChunk
	lea buffers.TablChunkOffsetLo, a3
	bsr.w storeLocator
	bset #5, buffers.PackageChunkFlags
	bra.w nextTocEntry

checkMsel
	cmpi.b #'M', (a2)
	bne.w checkPrvm
	cmpi.b #'S', 1(a2)
	bne.w checkPrvm
	cmpi.b #'E', 2(a2)
	bne.w checkPrvm
	cmpi.b #'L', 3(a2)
	bne.w checkPrvm
	btst #0, buffers.PackageChunkFlagsHi
	bne.w duplicateChunk
	lea buffers.MselChunkOffsetLo, a3
	bsr.w storeLocator
	bset #0, buffers.PackageChunkFlagsHi
	bra.w nextTocEntry

checkPrvm
	cmpi.b #'P', (a2)
	bne.w checkExpr
	cmpi.b #'R', 1(a2)
	bne.w checkExpr
	cmpi.b #'V', 2(a2)
	bne.w checkExpr
	cmpi.b #'M', 3(a2)
	bne.w checkExpr
	btst #1, buffers.PackageChunkFlagsHi
	bne.w duplicateChunk
	lea buffers.PrvmChunkOffsetLo, a3
	bsr.w storeLocator
	bset #1, buffers.PackageChunkFlagsHi
	bra.w nextTocEntry

checkExpr
	cmpi.b #'E', (a2)
	bne.w checkExvm
	cmpi.b #'X', 1(a2)
	bne.w checkExvm
	cmpi.b #'P', 2(a2)
	bne.w checkExvm
	cmpi.b #'R', 3(a2)
	bne.w checkExvm
	btst #6, buffers.PackageChunkFlags
	bne.w duplicateChunk
	lea buffers.ExprChunkOffsetLo, a3
	bsr.w storeLocator
	bset #6, buffers.PackageChunkFlags
	bra.w nextTocEntry

checkExvm
	cmpi.b #'E', (a2)
	bne.w checkCtbl
	cmpi.b #'X', 1(a2)
	bne.w checkCtbl
	cmpi.b #'V', 2(a2)
	bne.w checkCtbl
	cmpi.b #'M', 3(a2)
	bne.w checkCtbl
	btst #7, buffers.PackageChunkFlags
	bne.w duplicateChunk
	lea buffers.ExvmChunkOffsetLo, a3
	bsr.w storeLocator
	bset #7, buffers.PackageChunkFlags
	bra.w nextTocEntry

checkCtbl
	cmpi.b #'C', (a2)
	bne.w checkCsem
	cmpi.b #'T', 1(a2)
	bne.w checkCsem
	cmpi.b #'B', 2(a2)
	bne.w checkCsem
	cmpi.b #'L', 3(a2)
	bne.w checkCsem
	btst #2, buffers.PackageChunkFlagsHi
	bne.w duplicateChunk
	lea buffers.CtblChunkOffsetLo, a3
	bsr.w storeLocator
	bset #2, buffers.PackageChunkFlagsHi
	bra.w nextTocEntry

checkCsem
	cmpi.b #'C', (a2)
	bne.w checkRenc
	cmpi.b #'S', 1(a2)
	bne.w checkRenc
	cmpi.b #'E', 2(a2)
	bne.w checkRenc
	cmpi.b #'M', 3(a2)
	bne.w checkRenc
	btst #3, buffers.PackageChunkFlagsHi
	bne.w duplicateChunk
	lea buffers.CsemChunkOffsetLo, a3
	bsr.w storeLocator
	bset #3, buffers.PackageChunkFlagsHi
	bra.w nextTocEntry

checkRenc
	cmpi.b #'R', (a2)
	bne.w checkValp
	cmpi.b #'E', 1(a2)
	bne.w checkValp
	cmpi.b #'N', 2(a2)
	bne.w checkValp
	cmpi.b #'C', 3(a2)
	bne.w checkValp
	btst #0, buffers.PackageChunkFlagsExtra
	bne.w duplicateChunk
	lea buffers.RencChunkOffsetLo, a3
	bsr.w storeLocator
	bset #0, buffers.PackageChunkFlagsExtra
	bra.w nextTocEntry

checkValp
	cmpi.b #'V', (a2)
	bne.w checkCprd
	cmpi.b #'A', 1(a2)
	bne.w checkCmse
	cmpi.b #'L', 2(a2)
	bne.w checkCmse
	cmpi.b #'P', 3(a2)
	bne.w checkCmse
	btst #1, buffers.PackageChunkFlagsExtra
	bne.w duplicateChunk
	lea buffers.ValpChunkOffsetLo, a3
	bsr.w storeLocator
	bset #1, buffers.PackageChunkFlagsExtra
	bra.w nextTocEntry

checkCprd
	cmpi.b #'C', (a2)
	bne.w checkCmse
	cmpi.b #'P', 1(a2)
	bne.w checkCmse
	cmpi.b #'R', 2(a2)
	bne.w checkCmse
	cmpi.b #'D', 3(a2)
	bne.w checkCmse
	btst #2, buffers.PackageChunkFlagsExtra
	bne.w duplicateChunk
	lea buffers.CprdChunkOffsetLo, a3
	bsr.w storeLocator
	bset #2, buffers.PackageChunkFlagsExtra
	bra.w nextTocEntry

checkCmse
	cmpi.b #'C', (a2)
	bne.w nextTocEntry
	cmpi.b #'M', 1(a2)
	bne.w nextTocEntry
	cmpi.b #'S', 2(a2)
	bne.w nextTocEntry
	cmpi.b #'E', 3(a2)
	bne.w nextTocEntry
	btst #4, buffers.PackageChunkFlagsHi
	bne.w duplicateChunk
	lea buffers.CmseChunkOffsetLo, a3
	bsr.w storeLocator
	bset #4, buffers.PackageChunkFlagsHi
	bra.w nextTocEntry

nextTocEntry
	lea OPASM_TOC_ENTRY_SIZE(a2), a2
	dbf d2, tocLoop
	move.b buffers.PackageChunkFlags, d0
	andi.b #buffers.PACKAGE_REQUIRED_CHUNK_FLAGS, d0
	cmpi.b #buffers.PACKAGE_REQUIRED_CHUNK_FLAGS, d0
	bne.w missingChunk
	moveq #0, d0
	rts
	.bend  ; validateToc

storePackageStorageLen	.block
	move.b d0, buffers.PackageStorageLen
	lsr.l #8, d0
	move.b d0, buffers.PackageStorageLenMidLo
	lsr.l #8, d0
	move.b d0, buffers.PackageStorageLenMidHi
	lsr.l #8, d0
	move.b d0, buffers.PackageStorageLenHi
	rts
	.bend  ; storePackageStorageLen

readU32Le	.block
	moveq #0, d0
	move.b (a3), d0
	moveq #0, d1
	move.b 1(a3), d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b 2(a3), d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b 3(a3), d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	rts
	.bend  ; readU32Le

readU32LeLow16	.block
	moveq #0, d0
	move.b (a3), d0
	moveq #0, d1
	move.b 1(a3), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts
	.bend  ; readU32LeLow16

storeLocator	.block
	move.b d4, (a3)+
	lsr.l #8, d4
	move.b d4, (a3)+
	lsr.l #8, d4
	move.b d4, (a3)+
	lsr.l #8, d4
	move.b d4, (a3)+
	move.b d5, (a3)+
	lsr.l #8, d5
	move.b d5, (a3)+
	lsr.l #8, d5
	move.b d5, (a3)+
	lsr.l #8, d5
	move.b d5, (a3)+
	rts
	.bend  ; storeLocator

invalidMagic	.block
	lea InvalidMagicText, a1
	moveq #INVALID_MAGIC_TEXT_LEN, d1
	moveq #1, d0
	rts
	.bend  ; invalidMagic

unsupportedVersion	.block
	lea UnsupportedVersionText, a1
	moveq #UNSUPPORTED_VERSION_TEXT_LEN, d1
	moveq #1, d0
	rts
	.bend  ; unsupportedVersion

invalidEndian	.block
	lea InvalidEndianText, a1
	moveq #INVALID_ENDIAN_TEXT_LEN, d1
	moveq #1, d0
	rts
	.bend  ; invalidEndian

unexpectedEof	.block
	lea UnexpectedEofText, a1
	moveq #UNEXPECTED_EOF_TEXT_LEN, d1
	moveq #1, d0
	rts
	.bend  ; unexpectedEof

duplicateChunk	.block
	lea DuplicateChunkText, a1
	moveq #DUPLICATE_CHUNK_TEXT_LEN, d1
	moveq #1, d0
	rts
	.bend  ; duplicateChunk

missingChunk	.block
	lea MissingChunkText, a1
	moveq #MISSING_CHUNK_TEXT_LEN, d1
	moveq #1, d0
	rts
	.bend  ; missingChunk

chunkBounds	.block
	lea ChunkBoundsText, a1
	moveq #CHUNK_BOUNDS_TEXT_LEN, d1
	moveq #1, d0
	rts
	.bend  ; chunkBounds

	.endsection
	.endmodule
