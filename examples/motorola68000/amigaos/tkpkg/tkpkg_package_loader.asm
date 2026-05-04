; Package-loader module for the tkpkg native runtime.

        .module tkpkg.amigaos.package_loader
        .cpu 68020
        .pub
        .use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN)
        .use tkpkg.amigaos.buffers (PACKAGE_STORAGE_CAPACITY, PACKAGE_STATE_LOADED)
        .use tkpkg.amigaos.buffers (PACKAGE_STATE_CLEAR_LONGWORD_COUNT)
        .use tkpkg.amigaos.buffers (PACKAGE_CHUNK_FAMS, PACKAGE_CHUNK_CPUS)
        .use tkpkg.amigaos.buffers (PACKAGE_CHUNK_DIAL, PACKAGE_CHUNK_TOKS)
        .use tkpkg.amigaos.buffers (PACKAGE_CHUNK_TKVM, PACKAGE_REQUIRED_CHUNK_FLAGS)
        .use tkpkg.amigaos.buffers (packageStorage, packageStateFlags)
        .use tkpkg.amigaos.buffers (packageChunkFlags, packageStorageLen)
        .use tkpkg.amigaos.buffers (packageStorageLenHi, famsChunkOffsetLo)
        .use tkpkg.amigaos.buffers (cpusChunkOffsetLo, dialChunkOffsetLo)
        .use tkpkg.amigaos.buffers (toksChunkOffsetLo, tkvmChunkOffsetLo)
        .use tkpkg.amigaos.buffers (activeCpuBuffer, activeDialectBuffer)

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

invalidMagicText:
        .byte "OPC001: invalid package magic", 0

unsupportedVersionText:
        .byte "OPC002: unsupported package version", 0

invalidEndianText:
        .byte "OPC003: invalid endianness marker", 0

unexpectedEofText:
        .byte "OPC004: unexpected end of file", 0

duplicateChunkText:
        .byte "OPC005: duplicate tokenizer chunk", 0

missingChunkText:
        .byte "OPC006: missing required chunk", 0

chunkBoundsText:
        .byte "OPC007: chunk out of bounds", 0

        .endsection

        .section code, kind=code

tkpkg_package_loader_load_v1:
        BSR.W tkpkg_package_loader_clear_loaded_state_v1
        BSR.W tkpkg_package_loader_read_input_len_v1
        TST.W D0
        BEQ.W tkpkgPackageLoaderInvalidMagic
        CMPI.W #PACKAGE_STORAGE_CAPACITY, D0
        BHI.W tkpkgPackageLoaderChunkBounds
        MOVE.B D0, packageStorageLen
        LSR.W #8, D0
        MOVE.B D0, packageStorageLenHi
        BSR.W tkpkg_package_loader_read_input_offset_v1
        LEA 0(A0,D1.W), A1
        LEA packageStorage, A2
        BSR.W tkpkg_package_loader_copy_input_bytes_v1
        LEA packageStorage, A1
        BSR.W tkpkg_package_loader_validate_header_v1
        TST.B D0
        BNE.S tkpkgPackageLoaderDone
        BSR.W tkpkg_package_loader_validate_toc_v1
        TST.B D0
        BNE.S tkpkgPackageLoaderDone
        MOVE.B #PACKAGE_STATE_LOADED, packageStateFlags
        MOVEQ #0, D0

tkpkgPackageLoaderDone:
        RTS

tkpkg_package_loader_clear_loaded_state_v1:
        LEA packageStateFlags, A3
        MOVE.W #PACKAGE_STATE_CLEAR_LONGWORD_LAST, D0

tkpkgPackageLoaderClearStateLoop:
        CLR.L (A3)+
        DBF D0, tkpkgPackageLoaderClearStateLoop
        RTS

tkpkg_package_loader_read_input_len_v1:
        MOVEQ #0, D0
        MOVE.B CB_INPUT_LEN(A0), D0
        MOVEQ #0, D1
        MOVE.B 19(A0), D1
        LSL.W #8, D1
        OR.W D1, D0
        RTS

tkpkg_package_loader_read_input_offset_v1:
        MOVEQ #0, D1
        MOVE.B CB_INPUT_PTR(A0), D1
        MOVEQ #0, D2
        MOVE.B 17(A0), D2
        LSL.W #8, D2
        OR.W D2, D1
        RTS

tkpkg_package_loader_copy_input_bytes_v1:
        MOVEQ #0, D2
        MOVE.B packageStorageLen, D2
        MOVEQ #0, D3
        MOVE.B packageStorageLenHi, D3
        LSL.W #8, D3
        OR.W D3, D2
        TST.W D2
        BEQ.S tkpkgPackageLoaderCopyDone

tkpkgPackageLoaderCopyLoop:
        MOVE.B (A1)+, (A2)+
        SUBQ.W #1, D2
        BNE.S tkpkgPackageLoaderCopyLoop

tkpkgPackageLoaderCopyDone:
        RTS

tkpkg_package_loader_validate_header_v1:
        MOVEQ #0, D0
        CMPI.B #'O', (A1)
        BNE.W tkpkgPackageLoaderInvalidMagic
        CMPI.B #'P', 1(A1)
        BNE.W tkpkgPackageLoaderInvalidMagic
        CMPI.B #'C', 2(A1)
        BNE.W tkpkgPackageLoaderInvalidMagic
        CMPI.B #'P', 3(A1)
        BNE.W tkpkgPackageLoaderInvalidMagic
        CMPI.B #$01, 4(A1)
        BNE.W tkpkgPackageLoaderUnsupportedVersion
        TST.B 5(A1)
        BNE.W tkpkgPackageLoaderUnsupportedVersion
        CMPI.B #$34, 6(A1)
        BNE.W tkpkgPackageLoaderInvalidEndian
        CMPI.B #$12, 7(A1)
        BNE.W tkpkgPackageLoaderInvalidEndian
        RTS

tkpkg_package_loader_validate_toc_v1:
        MOVEQ #0, D7
        MOVE.B packageStorageLen, D7
        MOVEQ #0, D6
        MOVE.B packageStorageLenHi, D6
        LSL.W #8, D6
        OR.W D6, D7
        MOVEQ #0, D0
        MOVE.B 8(A1), D0
        MOVEQ #0, D1
        MOVE.B 9(A1), D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVE.W D0, D2
        LSL.W #2, D2
        MOVE.W D0, D3
        LSL.W #3, D3
        ADD.W D3, D2
        ADDI.W #OPASM_HEADER_SIZE, D2
        CMP.W D7, D2
        BHI.W tkpkgPackageLoaderUnexpectedEof
        LEA OPASM_HEADER_SIZE(A1), A2
        TST.W D0
        BEQ.W tkpkgPackageLoaderMissingChunk
        MOVE.W D0, D2
        SUBQ.W #1, D2

tkpkgPackageLoaderTocLoop:
        LEA 4(A2), A3
        BSR.W tkpkg_package_loader_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgPackageLoaderChunkBounds
        MOVE.W D0, D4
        LEA 8(A2), A3
        BSR.W tkpkg_package_loader_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgPackageLoaderChunkBounds
        MOVE.W D0, D5
        MOVE.W D4, D6
        ADD.W D5, D6
        BCS.W tkpkgPackageLoaderChunkBounds
        CMP.W D7, D6
        BHI.W tkpkgPackageLoaderChunkBounds

        CMPI.B #'F', (A2)
        BNE.S tkpkgPackageLoaderCheckCpus
        CMPI.B #'A', 1(A2)
        BNE.S tkpkgPackageLoaderCheckCpus
        CMPI.B #'M', 2(A2)
        BNE.S tkpkgPackageLoaderCheckCpus
        CMPI.B #'S', 3(A2)
        BNE.S tkpkgPackageLoaderCheckCpus
        BTST #0, packageChunkFlags
        BNE.W tkpkgPackageLoaderDuplicateChunk
        LEA famsChunkOffsetLo, A3
        BSR.W tkpkg_package_loader_store_locator_v1
        ORI.B #PACKAGE_CHUNK_FAMS, packageChunkFlags
        BRA.W tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckCpus:
        CMPI.B #'C', (A2)
        BNE.S tkpkgPackageLoaderCheckDial
        CMPI.B #'P', 1(A2)
        BNE.S tkpkgPackageLoaderCheckDial
        CMPI.B #'U', 2(A2)
        BNE.S tkpkgPackageLoaderCheckDial
        CMPI.B #'S', 3(A2)
        BNE.S tkpkgPackageLoaderCheckDial
        BTST #1, packageChunkFlags
        BNE.W tkpkgPackageLoaderDuplicateChunk
        LEA cpusChunkOffsetLo, A3
        BSR.W tkpkg_package_loader_store_locator_v1
        ORI.B #PACKAGE_CHUNK_CPUS, packageChunkFlags
        BRA.W tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckDial:
        CMPI.B #'D', (A2)
        BNE.S tkpkgPackageLoaderCheckToks
        CMPI.B #'I', 1(A2)
        BNE.S tkpkgPackageLoaderCheckToks
        CMPI.B #'A', 2(A2)
        BNE.S tkpkgPackageLoaderCheckToks
        CMPI.B #'L', 3(A2)
        BNE.S tkpkgPackageLoaderCheckToks
        BTST #2, packageChunkFlags
        BNE.W tkpkgPackageLoaderDuplicateChunk
        LEA dialChunkOffsetLo, A3
        BSR.W tkpkg_package_loader_store_locator_v1
        ORI.B #PACKAGE_CHUNK_DIAL, packageChunkFlags
        BRA.W tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckToks:
        CMPI.B #'T', (A2)
        BNE.S tkpkgPackageLoaderCheckTkvm
        CMPI.B #'O', 1(A2)
        BNE.S tkpkgPackageLoaderCheckTkvm
        CMPI.B #'K', 2(A2)
        BNE.S tkpkgPackageLoaderCheckTkvm
        CMPI.B #'S', 3(A2)
        BNE.S tkpkgPackageLoaderCheckTkvm
        BTST #3, packageChunkFlags
        BNE.W tkpkgPackageLoaderDuplicateChunk
        LEA toksChunkOffsetLo, A3
        BSR.W tkpkg_package_loader_store_locator_v1
        ORI.B #PACKAGE_CHUNK_TOKS, packageChunkFlags
        BRA.W tkpkgPackageLoaderNextTocEntry

tkpkgPackageLoaderCheckTkvm:
        CMPI.B #'T', (A2)
        BNE.S tkpkgPackageLoaderNextTocEntry
        CMPI.B #'K', 1(A2)
        BNE.S tkpkgPackageLoaderNextTocEntry
        CMPI.B #'V', 2(A2)
        BNE.S tkpkgPackageLoaderNextTocEntry
        CMPI.B #'M', 3(A2)
        BNE.S tkpkgPackageLoaderNextTocEntry
        BTST #4, packageChunkFlags
        BNE.W tkpkgPackageLoaderDuplicateChunk
        LEA tkvmChunkOffsetLo, A3
        BSR.W tkpkg_package_loader_store_locator_v1
        ORI.B #PACKAGE_CHUNK_TKVM, packageChunkFlags

tkpkgPackageLoaderNextTocEntry:
        LEA OPASM_TOC_ENTRY_SIZE(A2), A2
        DBF D2, tkpkgPackageLoaderTocLoop
        MOVE.B packageChunkFlags, D0
        CMPI.B #PACKAGE_REQUIRED_CHUNK_FLAGS, D0
        BNE.W tkpkgPackageLoaderMissingChunk
        MOVEQ #0, D0
        RTS

tkpkg_package_loader_read_u32_le_low16_v1:
        MOVEQ #0, D0
        MOVE.B (A3), D0
        MOVEQ #0, D1
        MOVE.B 1(A3), D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D1
        TST.B 2(A3)
        BNE.S tkpkgPackageLoaderReadU32HighBits
        TST.B 3(A3)
        BNE.S tkpkgPackageLoaderReadU32HighBits
        RTS

tkpkgPackageLoaderReadU32HighBits:
        MOVEQ #1, D1
        RTS

tkpkg_package_loader_store_locator_v1:
        MOVE.B D4, (A3)+
        LSR.W #8, D4
        MOVE.B D4, (A3)+
        MOVE.B D5, (A3)+
        LSR.W #8, D5
        MOVE.B D5, (A3)+
        RTS

tkpkgPackageLoaderInvalidMagic:
        LEA invalidMagicText, A1
        MOVEQ #INVALID_MAGIC_TEXT_LEN, D1
        MOVEQ #1, D0
        RTS

tkpkgPackageLoaderUnsupportedVersion:
        LEA unsupportedVersionText, A1
        MOVEQ #UNSUPPORTED_VERSION_TEXT_LEN, D1
        MOVEQ #1, D0
        RTS

tkpkgPackageLoaderInvalidEndian:
        LEA invalidEndianText, A1
        MOVEQ #INVALID_ENDIAN_TEXT_LEN, D1
        MOVEQ #1, D0
        RTS

tkpkgPackageLoaderUnexpectedEof:
        LEA unexpectedEofText, A1
        MOVEQ #UNEXPECTED_EOF_TEXT_LEN, D1
        MOVEQ #1, D0
        RTS

tkpkgPackageLoaderDuplicateChunk:
        LEA duplicateChunkText, A1
        MOVEQ #DUPLICATE_CHUNK_TEXT_LEN, D1
        MOVEQ #1, D0
        RTS

tkpkgPackageLoaderMissingChunk:
        LEA missingChunkText, A1
        MOVEQ #MISSING_CHUNK_TEXT_LEN, D1
        MOVEQ #1, D0
        RTS

tkpkgPackageLoaderChunkBounds:
        LEA chunkBoundsText, A1
        MOVEQ #CHUNK_BOUNDS_TEXT_LEN, D1
        MOVEQ #1, D0
        RTS

        .endsection
        .endmodule