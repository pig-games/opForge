; Package-backed token-policy owner resolution for the tkpkg native runtime.

        .module tkpkg.amigaos.token_policy
        .cpu 68020
        .pub
        .use tkpkg.amigaos.abi (STATUS_RUNTIME_ERROR_V1)
        .use tkpkg.amigaos.buffers (SCOPED_OWNER_DIALECT, SCOPED_OWNER_CPU)
        .use tkpkg.amigaos.buffers (SCOPED_OWNER_FAMILY, packageStorage)
        .use tkpkg.amigaos.buffers (toksChunkOffsetLo, pendingDialectOffsetLo)
        .use tkpkg.amigaos.buffers (pendingCpuOffsetLo, pendingFamilyOffsetLo)
        .use tkpkg.amigaos.buffers (pendingTokenPolicyOffsetLo)
        .use tkpkg.amigaos.buffers (pendingTokenPolicyOwnerTag)

MISSING_POLICY_TEXT_LEN             = 33
TOKS_ENTRY_FIXED_PREFIX_SIZE        = 9

        .section data, kind=data

missingPolicyText:
        .byte "OTR003: missing tokenizer policy", 0

        .endsection

        .section code, kind=code

; ---------------------------------------------------------------------------
; Resolve the active tokenizer token-policy locator.
;
; Token policy owner precedence matches the runtime model fallback order:
; dialect-specific policy first, then CPU, then family. The resolved record is
; staged in pendingTokenPolicy* fields for tkpkg_pipeline_commit_active_selection_v1.
;
; Inputs:
; - pendingDialectOffsetLo, pendingCpuOffsetLo, pendingFamilyOffsetLo contain
;   package string locators selected by the pipeline resolver.
;
; Outputs:
; - D0: 0 on success, STATUS_RUNTIME_ERROR_V1 when no policy can be found.
; - A1/D1: failure message pointer/length on missing-policy error.
; ---------------------------------------------------------------------------

tkpkg_token_policy_resolve_locator_v1:
        MOVEQ #SCOPED_OWNER_DIALECT, D0 ; prefer dialect-specific tokenization rules when present
        LEA pendingDialectOffsetLo, A3
        BSR.W tkpkg_token_policy_find_owner_v1
        TST.B D0
        BEQ.S tkpkgTokenPolicyResolveDone
        MOVEQ #SCOPED_OWNER_CPU, D0     ; fall back to CPU-local policy
        LEA pendingCpuOffsetLo, A3
        BSR.W tkpkg_token_policy_find_owner_v1
        TST.B D0
        BEQ.S tkpkgTokenPolicyResolveDone
        MOVEQ #SCOPED_OWNER_FAMILY, D0  ; final fallback is family-wide policy
        LEA pendingFamilyOffsetLo, A3
        BSR.W tkpkg_token_policy_find_owner_v1
        TST.B D0
        BEQ.S tkpkgTokenPolicyResolveDone
        LEA missingPolicyText, A1
        MOVEQ #MISSING_POLICY_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgTokenPolicyResolveDone:
        MOVEQ #0, D0
        RTS

; Find a TOKS record matching the scoped owner type in D0 and owner locator A3.
tkpkg_token_policy_find_owner_v1:
        MOVE.B D0, D6                   ; D6 keeps the scoped-owner tag while D0 is reused by helpers
        MOVE.L A3, -(SP)
        LEA pendingTokenPolicyOffsetLo, A3
        CLR.L (A3)+
        CLR.B (A3)
        MOVEA.L (SP)+, A3
        BSR.W tkpkg_token_policy_read_locator_ptr_len_v1
        MOVE.W D3, D5
        MOVEA.L A1, A5
        LEA toksChunkOffsetLo, A3
        BSR.W tkpkg_token_policy_chunk_ptr_from_locator_v1
        BSR.W tkpkg_token_policy_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgTokenPolicyOwnerMissing
        TST.W D0
        BEQ.W tkpkgTokenPolicyOwnerMissing
        MOVE.W D0, D7
        SUBQ.W #1, D7
        LEA 4(A2), A2

tkpkgTokenPolicyOwnerLoop:
        MOVEA.L A2, A4
        MOVEQ #1, D0
        BSR.W tkpkg_token_policy_require_bytes_v1
        TST.B D1
        BNE.W tkpkgTokenPolicyOwnerMissing
        MOVE.B (A2)+, D4
        BSR.W tkpkg_token_policy_locate_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicyOwnerMissing
        CMP.B D6, D4
        BNE.W tkpkgTokenPolicySkipEntry
        MOVE.W D0, D4
        MOVEA.L A1, A0
        MOVE.L A2, -(SP)
        MOVE.W D4, D0
        MOVE.W D5, D1
        MOVEA.L A0, A1
        MOVEA.L A5, A2
        BSR.W tkpkg_token_policy_string_eq_ascii_casefold_v1
        MOVEA.L (SP)+, A2
        TST.B D0
        BNE.W tkpkgTokenPolicyFound

tkpkgTokenPolicySkipEntry:
        BSR.W tkpkg_token_policy_skip_toks_entry_v1
        TST.B D1
        BNE.W tkpkgTokenPolicyOwnerMissing
        DBF D7, tkpkgTokenPolicyOwnerLoop

tkpkgTokenPolicyOwnerMissing:
        MOVEQ #1, D0
        RTS

tkpkgTokenPolicyFound:
        BSR.W tkpkg_token_policy_skip_toks_entry_v1
        TST.B D1
        BNE.W tkpkgTokenPolicyOwnerMissing
        LEA pendingTokenPolicyOffsetLo, A3
        MOVEA.L A4, A1
        MOVE.L A2, D0
        SUB.L A4, D0
        BSR.W tkpkg_token_policy_store_record_locator_v1
        MOVE.B D6, pendingTokenPolicyOwnerTag
        MOVEQ #0, D0
        RTS

; Skip one TOKS entry, including optional tail extension fields.
tkpkg_token_policy_skip_toks_entry_v1:
        MOVE.W D7, -(SP)
        MOVEQ #TOKS_ENTRY_FIXED_PREFIX_SIZE, D0
        BSR.W tkpkg_token_policy_require_bytes_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        LEA TOKS_ENTRY_FIXED_PREFIX_SIZE(A2), A2
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        MOVEQ #1, D0
        BSR.W tkpkg_token_policy_require_bytes_v1
        TST.B D1
        BEQ.S tkpkgTokenPolicyTailMarkerReady
        BRA.W tkpkgTokenPolicySkipBoundsFail

tkpkgTokenPolicyTailMarkerReady:
        CMPI.B #$FF, (A2)
        BEQ.S tkpkgTokenPolicySkipTailExt
        BRA.W tkpkgTokenPolicySkipDone

tkpkgTokenPolicySkipTailExt:
        MOVEQ #1, D0
        BSR.W tkpkg_token_policy_require_bytes_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        ADDQ.W #1, A2
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        MOVEQ #1, D0
        BSR.W tkpkg_token_policy_require_bytes_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        TST.B (A2)+
        BEQ.S tkpkgTokenPolicySkipTailStrings
        MOVEQ #1, D0
        BSR.W tkpkg_token_policy_require_bytes_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        ADDQ.W #1, A2

tkpkgTokenPolicySkipTailStrings:
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        BSR.W tkpkg_token_policy_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        MOVE.W D0, D7
        LEA 4(A2), A2
        TST.W D7
        BEQ.S tkpkgTokenPolicySkipDone
        SUBQ.W #1, D7

tkpkgTokenPolicySkipOperatorsLoop:
        BSR.W tkpkg_token_policy_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenPolicySkipBoundsFail
        DBF D7, tkpkgTokenPolicySkipOperatorsLoop

tkpkgTokenPolicySkipDone:
        MOVE.W (SP)+, D7
        MOVEQ #0, D1
        RTS

tkpkgTokenPolicySkipBoundsFail:
        MOVE.W (SP)+, D7
        MOVEQ #1, D1
        RTS

tkpkg_token_policy_store_record_locator_v1:
        MOVE.L A6, -(SP)
        MOVE.L A1, D2
        LEA packageStorage, A6
        SUB.L A6, D2
        MOVE.B D2, (A3)+
        LSR.W #8, D2
        MOVE.B D2, (A3)+
        MOVE.B D0, (A3)+
        LSR.W #8, D0
        MOVE.B D0, (A3)+
        MOVEA.L (SP)+, A6
        RTS

tkpkg_token_policy_read_locator_ptr_len_v1:
        MOVEQ #0, D2
        MOVE.B (A3)+, D2
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D2
        MOVEQ #0, D3
        MOVE.B (A3)+, D3
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D3
        LEA packageStorage, A6
        LEA 0(A6, D2.W), A1
        RTS

tkpkg_token_policy_chunk_ptr_from_locator_v1:
        MOVEQ #0, D0
        MOVE.B (A3)+, D0
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D7
        MOVE.B (A3)+, D7
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D7
        LEA packageStorage, A6
        LEA 0(A6, D0.W), A2
        LEA 0(A2, D7.W), A6
        RTS

; skip_string is an alias for callers that only need the A2 advance.
tkpkg_token_policy_skip_string_v1:
tkpkg_token_policy_locate_string_v1:
        BSR.W tkpkg_token_policy_read_u32_le_low16_v1
        TST.B D1
        BNE.S tkpkgTokenPolicyLocateStringBoundsFail
        MOVE.L D0, D2
        MOVE.L D0, D3
        ADDQ.L #4, D3
        MOVE.L D3, D0
        BSR.W tkpkg_token_policy_require_bytes_v1
        TST.B D1
        BNE.S tkpkgTokenPolicyLocateStringBoundsFail
        MOVE.L D2, D0
        LEA 4(A2), A1
        LEA 4(A2), A2
        ADDA.L D0, A2
        MOVEQ #0, D1
        RTS

tkpkgTokenPolicyLocateStringBoundsFail:
        MOVEQ #0, D0
        MOVEA.L D0, A1
        MOVEQ #1, D1
        RTS

tkpkg_token_policy_read_u32_le_low16_v1:
        MOVEQ #4, D0
        BSR.W tkpkg_token_policy_require_bytes_v1
        TST.B D1
        BNE.S tkpkgTokenPolicyReadU32BoundsFail
        MOVEQ #0, D0
        MOVE.B (A2), D0
        MOVEQ #0, D1
        MOVE.B 1(A2), D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D1
        RTS

tkpkgTokenPolicyReadU32BoundsFail:
        MOVEQ #0, D0
        MOVEQ #1, D1
        RTS

tkpkg_token_policy_require_bytes_v1:
        MOVEA.L A2, A1
        ADDA.L D0, A1
        CMPA.L A6, A1
        BHI.S tkpkgTokenPolicyRequireBytesFail
        MOVEQ #0, D1
        RTS

tkpkgTokenPolicyRequireBytesFail:
        MOVEQ #1, D1
        RTS

tkpkg_token_policy_string_eq_ascii_casefold_v1:
        CMP.W D1, D0
        BNE.S tkpkgTokenPolicyStringNoMatch
        MOVE.W D0, D4
        TST.W D4
        BEQ.S tkpkgTokenPolicyStringMatch
        SUBQ.W #1, D4

tkpkgTokenPolicyStringLoop:
        MOVEQ #0, D2
        MOVE.B (A1)+, D2
        MOVEQ #0, D3
        MOVE.B (A2)+, D3
        MOVE.B D2, D0
        BSR.W tkpkg_token_policy_fold_ascii_lower_v1
        MOVE.B D0, D2
        MOVE.B D3, D0
        BSR.W tkpkg_token_policy_fold_ascii_lower_v1
        CMP.B D0, D2
        BNE.S tkpkgTokenPolicyStringNoMatch
        DBF D4, tkpkgTokenPolicyStringLoop

tkpkgTokenPolicyStringMatch:
        MOVEQ #1, D0
        RTS

tkpkgTokenPolicyStringNoMatch:
        MOVEQ #0, D0
        RTS

tkpkg_token_policy_fold_ascii_lower_v1:
        CMPI.B #'A', D0
        BLO.S tkpkgTokenPolicyFoldDone
        CMPI.B #'Z', D0
        BHI.S tkpkgTokenPolicyFoldDone
        ORI.B #$20, D0

tkpkgTokenPolicyFoldDone:
        RTS

tkpkg_token_policy_placeholder:
        RTS

        .endsection
        .endmodule
