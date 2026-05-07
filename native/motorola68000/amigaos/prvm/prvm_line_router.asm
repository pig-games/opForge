; Native opcore-style one-line router for PRVM statement delegation.

	.module prvm.amigaos.line_router
	.cpu 68020
	.pub
	.use prvm.amigaos.interpreter (prvmRun68000)

PRVM_REQUEST_FRAME_SIZE             = 112
PRVM_MAGIC_OPRP                     = $4F505250
PRVM_ABI_VERSION_V1                 = 1
PRVM_CALL_MODE_START                = 0
PRVM_ENTRY_KIND_OPASM_STATEMENT     = 1

PRVM_STATUS_OK                      = 0
PRVM_STATUS_NEWLINE_UNSUPPORTED     = 2
PRVM_STATUS_INVALID_ARGUMENT        = 4
PRVM_STATUS_UNSUPPORTED_ROUTE       = 100

PRVM_ROUTE_MAGIC_OPLR               = $4F504C52
PRVM_ROUTE_FRAME_SIZE               = 116
PRVM_ROUTE_ABI_VERSION_V1           = 1

ROUTE_FRAME_MAGIC                   = 0
ROUTE_FRAME_ABI_VERSION             = 4
ROUTE_FRAME_FRAME_SIZE              = 6
ROUTE_FRAME_PROCESSOR_PTR           = 8
ROUTE_FRAME_PROCESSOR_LEN           = 12
ROUTE_FRAME_KIND_PTR                = 16
ROUTE_FRAME_KIND_LEN                = 20
ROUTE_FRAME_LINE_NUM                = 24
ROUTE_FRAME_SOURCE_PTR              = 28
ROUTE_FRAME_SOURCE_LEN              = 32
ROUTE_FRAME_TOKEN_PTR               = 36
ROUTE_FRAME_TOKEN_COUNT             = 40
ROUTE_FRAME_TOKEN_RECORD_SIZE       = 44
ROUTE_FRAME_LEXEME_PTR              = 48
ROUTE_FRAME_LEXEME_LEN              = 52
ROUTE_FRAME_PROGRAM_PTR             = 56
ROUTE_FRAME_PROGRAM_LEN             = 60
ROUTE_FRAME_RESULT_PTR              = 64
ROUTE_FRAME_RESULT_CAPACITY         = 68
ROUTE_FRAME_DIAGNOSTIC_PTR          = 72
ROUTE_FRAME_DIAGNOSTIC_CAPACITY     = 76
ROUTE_FRAME_RESUME_PTR              = 80
ROUTE_FRAME_RESUME_CAPACITY         = 84
ROUTE_FRAME_EXPR_REQUEST_PTR        = 88
ROUTE_FRAME_EXPR_REQUEST_SIZE       = 92
ROUTE_FRAME_EXPR_RESULT_PTR         = 96
ROUTE_FRAME_EXPR_RESULT_COUNT       = 100
ROUTE_FRAME_PARSER_CONTRACT_VERSION = 104
ROUTE_FRAME_STEP_BUDGET             = 108
ROUTE_FRAME_FLAGS                   = 112

	.section code, kind=code

; ---------------------------------------------------------------------------
; Native opcore-style one-line router.
;
; Call ABI:
; - A0: PRVM_ROUTE_FRAME_V1 pointer
; - D0: route frame size in bytes
;
; Return ABI:
; - forwards D0-D3 from prvm_run_68000 on success
; - returns deterministic nonzero status with D1-D3 cleared on route failure
; ---------------------------------------------------------------------------

prvmRouteLine68000
	movem.l d4-d7/a2-a4, -(sp)
	movea.l a0, a4

	cmpi.l #PRVM_ROUTE_FRAME_SIZE, d0
	bne.w prvmRouteInvalidArgument
	cmpi.l #PRVM_ROUTE_MAGIC_OPLR, ROUTE_FRAME_MAGIC(a4)
	bne.w prvmRouteInvalidArgument
	cmpi.w #PRVM_ROUTE_ABI_VERSION_V1, ROUTE_FRAME_ABI_VERSION(a4)
	bne.w prvmRouteInvalidArgument
	cmpi.w #PRVM_ROUTE_FRAME_SIZE, ROUTE_FRAME_FRAME_SIZE(a4)
	bne.w prvmRouteInvalidArgument

	movea.l ROUTE_FRAME_PROCESSOR_PTR(a4), a0
	move.l ROUTE_FRAME_PROCESSOR_LEN(a4), d0
	lea ProcessorAsmText(PC), a1
	moveq #3, d1
	bsr.w prvmRouteCompareText
	tst.l d0
	bne.w prvmRouteUnsupported

	movea.l ROUTE_FRAME_KIND_PTR(a4), a0
	move.l ROUTE_FRAME_KIND_LEN(a4), d0
	lea KindStatementText(PC), a1
	moveq #9, d1
	bsr.w prvmRouteCompareText
	tst.l d0
	bne.w prvmRouteUnsupported

	movea.l ROUTE_FRAME_SOURCE_PTR(a4), a0
	move.l ROUTE_FRAME_SOURCE_LEN(a4), d0
	bsr.w prvmRouteRejectNewline
	tst.l d0
	bne.w prvmRouteNewlineUnsupported

	bsr.w prvmRouteBuildRequestFrame
	lea PrvmRouteRequestFrame(PC), a0
	move.l #PRVM_REQUEST_FRAME_SIZE, d0
	movea.l PrvmRouteInterpreterEntryPtr(PC), a1
	jsr (a1)
	bra.s prvmRouteDone

prvmRouteInvalidArgument
	move.l #PRVM_STATUS_INVALID_ARGUMENT, d0
	bra.s prvmRouteClearTail

prvmRouteUnsupported
	move.l #PRVM_STATUS_UNSUPPORTED_ROUTE, d0
	bra.s prvmRouteClearTail

prvmRouteNewlineUnsupported
	move.l #PRVM_STATUS_NEWLINE_UNSUPPORTED, d0

prvmRouteClearTail
	clr.l d1
	clr.l d2
	clr.l d3

prvmRouteDone
	movem.l (sp)+, d4-d7/a2-a4
	rts

prvmRouteCompareText
	cmp.l d1, d0
	bne.s prvmRouteCompareMismatch
	subq.l #1, d1
	bmi.s prvmRouteCompareMatch

prvmRouteCompareLoop
	move.b (a0)+, d2
	cmp.b (a1)+, d2
	bne.s prvmRouteCompareMismatch
	dbra d1, prvmRouteCompareLoop

prvmRouteCompareMatch
	clr.l d0
	rts

prvmRouteCompareMismatch
	moveq #1, d0
	rts

prvmRouteRejectNewline
	tst.l d0
	beq.s prvmRouteNoNewline
	subq.l #1, d0

prvmRouteNewlineLoop
	move.b (a0)+, d1
	cmpi.b #10, d1
	beq.s prvmRouteFoundNewline
	cmpi.b #13, d1
	beq.s prvmRouteFoundNewline
	dbra d0, prvmRouteNewlineLoop

prvmRouteNoNewline
	clr.l d0
	rts

prvmRouteFoundNewline
	moveq #1, d0
	rts

prvmRouteBuildRequestFrame
	lea PrvmRouteRequestFrame(PC), a0
	move.l #PRVM_MAGIC_OPRP, 0(a0)
	move.w #PRVM_ABI_VERSION_V1, 4(a0)
	move.w #PRVM_REQUEST_FRAME_SIZE, 6(a0)
	move.w #PRVM_CALL_MODE_START, 8(a0)
	move.w #PRVM_ENTRY_KIND_OPASM_STATEMENT, 10(a0)
	move.l ROUTE_FRAME_LINE_NUM(a4), 12(a0)
	move.l ROUTE_FRAME_SOURCE_PTR(a4), 16(a0)
	move.l ROUTE_FRAME_SOURCE_LEN(a4), 20(a0)
	move.l ROUTE_FRAME_TOKEN_PTR(a4), 24(a0)
	move.l ROUTE_FRAME_TOKEN_COUNT(a4), 28(a0)
	move.w ROUTE_FRAME_TOKEN_RECORD_SIZE(a4), 32(a0)
	clr.w 34(a0)
	move.l ROUTE_FRAME_LEXEME_PTR(a4), 36(a0)
	move.l ROUTE_FRAME_LEXEME_LEN(a4), 40(a0)
	move.l ROUTE_FRAME_PROGRAM_PTR(a4), 44(a0)
	move.l ROUTE_FRAME_PROGRAM_LEN(a4), 48(a0)
	move.l ROUTE_FRAME_RESULT_PTR(a4), 52(a0)
	move.l ROUTE_FRAME_RESULT_CAPACITY(a4), 56(a0)
	move.l ROUTE_FRAME_DIAGNOSTIC_PTR(a4), 60(a0)
	move.l ROUTE_FRAME_DIAGNOSTIC_CAPACITY(a4), 64(a0)
	move.l ROUTE_FRAME_RESUME_PTR(a4), 68(a0)
	move.l ROUTE_FRAME_RESUME_CAPACITY(a4), 72(a0)
	move.l ROUTE_FRAME_EXPR_REQUEST_PTR(a4), 76(a0)
	move.l ROUTE_FRAME_EXPR_REQUEST_SIZE(a4), 80(a0)
	move.l ROUTE_FRAME_EXPR_RESULT_PTR(a4), 84(a0)
	move.l ROUTE_FRAME_EXPR_RESULT_COUNT(a4), 88(a0)
	move.l ROUTE_FRAME_PARSER_CONTRACT_VERSION(a4), 92(a0)
	move.l ROUTE_FRAME_STEP_BUDGET(a4), 96(a0)
	move.l ROUTE_FRAME_FLAGS(a4), 100(a0)
	clr.l 104(a0)
	clr.l 108(a0)
	rts

ProcessorAsmText
	.byte "asm"
KindStatementText
	.byte "statement"

PrvmRouteRequestFrame
	.fill byte, 112, 0
PrvmRouteInterpreterEntryPtr
	.long prvmRun68000

	.endsection
	.endmodule
