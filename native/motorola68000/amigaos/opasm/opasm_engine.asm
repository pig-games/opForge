; Native opasm assembly-engine staging for the AmigaOS CLI.
;
; This module owns the transitional two-pass loop. The CLI supplies a compact
; context with session pointers and host callbacks for the currently supported
; smoke semantics while opasm owns pass ordering and statement iteration.

        .module opasm.amigaos.engine
        .cpu 68020
        .pub

TOKEN_BUFFER_CAPACITY           = 64
NATIVE_SOURCE_RECORD_CAPACITY   = 512
NATIVE_STATEMENT_TABLE_CAPACITY = 16
NATIVE_LABEL_TABLE_CAPACITY     = 16
NATIVE_IMAGE_BUFFER_CAPACITY    = 4096

        .section code

OPASM_ENGINE_CTX_SESSION_PASS_PTR = 0
OPASM_ENGINE_CTX_STMT_COUNT_PTR   = 4
OPASM_ENGINE_CTX_BIN_REQUESTED_PTR = 8
OPASM_ENGINE_CTX_PASS1_BEGIN_CB   = 12
OPASM_ENGINE_CTX_PASS2_BEGIN_CB   = 16
OPASM_ENGINE_CTX_PASS1_OK_CB      = 20
OPASM_ENGINE_CTX_PASS2_OK_CB      = 24
OPASM_ENGINE_CTX_RECORD_LABEL_CB  = 28
OPASM_ENGINE_CTX_ADVANCE_PC_CB    = 32
OPASM_ENGINE_CTX_EMIT_IMAGE_CB    = 36

; A4: opasm engine context pointer.
; Returns D0=0 on success, non-zero on failure.
opasm_engine_run_two_pass_v1:
        MOVEM.L D1-D7/A0-A5,-(SP)
        MOVEA.L A4,A5
        BSR.W opasmEngineRunPassOne
        TST.L D0
        BNE.S opasmEngineRunTwoPassDone
        BSR.W opasmEngineRunPassTwo

opasmEngineRunTwoPassDone:
        MOVEM.L (SP)+,D1-D7/A0-A5
        RTS

opasmEngineRunPassOne:
        MOVEA.L OPASM_ENGINE_CTX_SESSION_PASS_PTR(A5),A0
        MOVE.W #1,(A0)
        MOVEA.L OPASM_ENGINE_CTX_PASS1_BEGIN_CB(A5),A0
        JSR (A0)
        TST.L D0
        BNE.S opasmEngineRunPassOneReturn
        CLR.W D7

opasmEnginePassOneLoop:
        MOVEA.L OPASM_ENGINE_CTX_STMT_COUNT_PTR(A5),A0
        MOVE.W (A0),D0
        CMP.W D0,D7
        BHS.S opasmEnginePassOneOk
        MOVEQ #0,D0
        MOVE.W D7,D0
        MOVEA.L OPASM_ENGINE_CTX_RECORD_LABEL_CB(A5),A0
        JSR (A0)
        TST.L D0
        BNE.S opasmEngineRunPassOneReturn
        MOVEQ #0,D0
        MOVE.W D7,D0
        MOVEA.L OPASM_ENGINE_CTX_ADVANCE_PC_CB(A5),A0
        JSR (A0)
        TST.L D0
        BNE.S opasmEngineRunPassOneReturn
        ADDQ.W #1,D7
        BRA.S opasmEnginePassOneLoop

opasmEnginePassOneOk:
        MOVEA.L OPASM_ENGINE_CTX_PASS1_OK_CB(A5),A0
        JSR (A0)

opasmEngineRunPassOneReturn:
        RTS

opasmEngineRunPassTwo:
        MOVEA.L OPASM_ENGINE_CTX_SESSION_PASS_PTR(A5),A0
        MOVE.W #2,(A0)
        MOVEA.L OPASM_ENGINE_CTX_PASS2_BEGIN_CB(A5),A0
        JSR (A0)
        TST.L D0
        BNE.S opasmEngineRunPassTwoReturn
        CLR.W D7

opasmEnginePassTwoLoop:
        MOVEA.L OPASM_ENGINE_CTX_STMT_COUNT_PTR(A5),A0
        MOVE.W (A0),D0
        CMP.W D0,D7
        BHS.S opasmEnginePassTwoOk
        MOVEA.L OPASM_ENGINE_CTX_BIN_REQUESTED_PTR(A5),A0
        TST.W (A0)
        BEQ.S opasmEnginePassTwoAdvanceOnly
        MOVEQ #0,D0
        MOVE.W D7,D0
        MOVEA.L OPASM_ENGINE_CTX_EMIT_IMAGE_CB(A5),A0
        JSR (A0)
        TST.L D0
        BNE.S opasmEngineRunPassTwoReturn

opasmEnginePassTwoAdvanceOnly:
        MOVEQ #0,D0
        MOVE.W D7,D0
        MOVEA.L OPASM_ENGINE_CTX_ADVANCE_PC_CB(A5),A0
        JSR (A0)
        TST.L D0
        BNE.S opasmEngineRunPassTwoReturn
        ADDQ.W #1,D7
        BRA.S opasmEnginePassTwoLoop

opasmEnginePassTwoOk:
        MOVEA.L OPASM_ENGINE_CTX_PASS2_OK_CB(A5),A0
        JSR (A0)

opasmEngineRunPassTwoReturn:
        RTS

        .endsection

        .section bss, kind=bss
        .align 4

opasmEngineAssemblySessionStart:
opasmEngineStmtCount:
        .res word,1
opasmEngineSessionPass:
        .res word,1
opasmEngineSourceRecordCount:
        .res word,1
opasmEngineLabelCount:
        .res word,1
opasmEngineImageByteCount:
        .res word,1
opasmEngineSessionCpuName:
        .res byte,TOKEN_BUFFER_CAPACITY
opasmEngineSessionOrigin:
        .res long,1
opasmEngineSessionCurrentPc:
        .res long,1
opasmEngineSourceLineNumTable:
        .res long,NATIVE_SOURCE_RECORD_CAPACITY
opasmEngineSourceLineLenTable:
        .res word,NATIVE_SOURCE_RECORD_CAPACITY
opasmEngineStmtLineTable:
        .res long,NATIVE_STATEMENT_TABLE_CAPACITY
opasmEngineStmtLabelLenTable:
        .res word,NATIVE_STATEMENT_TABLE_CAPACITY
opasmEngineStmtMnemLenTable:
        .res word,NATIVE_STATEMENT_TABLE_CAPACITY
opasmEngineStmtOperandLenTable:
        .res word,NATIVE_STATEMENT_TABLE_CAPACITY
opasmEngineStmtMnemOffTable:
        .res long,NATIVE_STATEMENT_TABLE_CAPACITY
opasmEngineStmtLabelNameTable:
        .res byte,NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
opasmEngineStmtMnemNameTable:
        .res byte,NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
opasmEngineStmtOperandNameTable:
        .res byte,NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
opasmEngineLabelValueTable:
        .res long,NATIVE_LABEL_TABLE_CAPACITY
opasmEngineLabelNameTable:
        .res byte,NATIVE_LABEL_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
opasmEngineImageBuffer:
        .res byte,NATIVE_IMAGE_BUFFER_CAPACITY
opasmEngineAssemblySessionEnd:

        .endsection
        .endmodule
