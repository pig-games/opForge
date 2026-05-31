; Native whole-file iterator over newline-free PRVM line routes.

	.module prvm.amigaos.line_iterator
	.cpu 68020
	.pub
	.use prvm.amigaos.line_router

PRVM_ROUTE_MAGIC_OPLR               = $4F504C52
PRVM_ROUTE_FRAME_SIZE               = 116
PRVM_ROUTE_ABI_VERSION_V1           = 1

PRVM_ITER_MAGIC_OPLI                = $4F504C49
PRVM_ITER_FRAME_SIZE                = 116
PRVM_ITER_ABI_VERSION_V1            = 1

PRVM_ITER_STATUS_OK                 = 0
PRVM_ITER_STATUS_INVALID_ARGUMENT   = 4

ITER_FRAME_MAGIC                    = 0
ITER_FRAME_ABI_VERSION              = 4
ITER_FRAME_FRAME_SIZE               = 6
ITER_FRAME_PROCESSOR_PTR            = 8
ITER_FRAME_PROCESSOR_LEN            = 12
ITER_FRAME_KIND_PTR                 = 16
ITER_FRAME_KIND_LEN                 = 20
ITER_FRAME_START_LINE_NUM           = 24
ITER_FRAME_SOURCE_PTR               = 28
ITER_FRAME_SOURCE_LEN               = 32
ITER_FRAME_TOKEN_PTR                = 36
ITER_FRAME_TOKEN_COUNT              = 40
ITER_FRAME_TOKEN_RECORD_SIZE        = 44
ITER_FRAME_LEXEME_PTR               = 48
ITER_FRAME_LEXEME_LEN               = 52
ITER_FRAME_PROGRAM_PTR              = 56
ITER_FRAME_PROGRAM_LEN              = 60
ITER_FRAME_RESULT_PTR               = 64
ITER_FRAME_RESULT_CAPACITY          = 68
ITER_FRAME_DIAGNOSTIC_PTR           = 72
ITER_FRAME_DIAGNOSTIC_CAPACITY      = 76
ITER_FRAME_RESUME_PTR               = 80
ITER_FRAME_RESUME_CAPACITY          = 84
ITER_FRAME_EXPR_REQUEST_PTR         = 88
ITER_FRAME_EXPR_REQUEST_SIZE        = 92
ITER_FRAME_EXPR_RESULT_PTR          = 96
ITER_FRAME_EXPR_RESULT_COUNT        = 100
ITER_FRAME_PARSER_CONTRACT_VERSION  = 104
ITER_FRAME_STEP_BUDGET              = 108
ITER_FRAME_FLAGS                    = 112

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

	.pub

; ---------------------------------------------------------------------------
; Native whole-file iterator.
;
; Call ABI:
; - A0: PRVM_ITER_FRAME_V1 pointer
; - D0: iterator frame size in bytes
;
; Return ABI:
; - D0: route status or zero when every routed line succeeds
; - D1: number of nonblank routed lines
; - D2: one-based line number for the first failing line, or zero
; - D3: total logical line count observed
;
; Clobbers:
; - D0-D7/A0-A6/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
prvmIterateLines68000	.block
	movem.l d4-d7/a2-a6, -(sp)
	movea.l a0, a6
	clr.l d5
	clr.l d6
	clr.l d7

	cmpi.l #PRVM_ITER_FRAME_SIZE, d0
	bne.w invalidArgument
	cmpi.l #PRVM_ITER_MAGIC_OPLI, ITER_FRAME_MAGIC(a6)
	bne.w invalidArgument
	cmpi.w #PRVM_ITER_ABI_VERSION_V1, ITER_FRAME_ABI_VERSION(a6)
	bne.w invalidArgument
	cmpi.w #PRVM_ITER_FRAME_SIZE, ITER_FRAME_FRAME_SIZE(a6)
	bne.w invalidArgument

	move.l ITER_FRAME_START_LINE_NUM(a6), d6
	tst.l d6
	bne.s startLineReady
	moveq #1, d6

startLineReady
	movea.l ITER_FRAME_SOURCE_PTR(a6), a2
	move.l ITER_FRAME_SOURCE_LEN(a6), d4

nextLine
	tst.l d4
	beq.w success
	movea.l a2, a3
	move.l d4, d0
	bsr.w findLineEnd
	move.l d0, d3
	move.l d1, d4
	movea.l a2, a0
	move.l d3, d0
	bsr.w trimCr
	move.l d0, d3
	addq.l #1, d7
	movea.l a2, a0
	move.l d3, d0
	bsr.w lineIsBlank
	bne.s skipRoute

	movea.l a2, a0
	move.l d3, d0
	bsr.w buildRouteFrame
	lea PrvmIteratorRouteFrame(PC), a0
	move.l #PRVM_ROUTE_FRAME_SIZE, d0
	movea.l PrvmIteratorRouteEntryPtr(PC), a1
	jsr (a1)
	tst.l d0
	bne.w failFast
	addq.l #1, d5

skipRoute
	movea.l a3, a2
	addq.l #1, d6
	bra.w nextLine

success
	move.l #PRVM_ITER_STATUS_OK, d0
	move.l d5, d1
	clr.l d2
	move.l d7, d3
	bra.s done

failFast
	move.l d5, d1
	move.l d6, d2
	move.l d7, d3
	bra.s done

invalidArgument
	move.l #PRVM_ITER_STATUS_INVALID_ARGUMENT, d0
	clr.l d1
	clr.l d2
	clr.l d3

done
	movem.l (sp)+, d4-d7/a2-a6
	rts
	.bend  ; prvmIterateLines68000
	
	.priv

; Find the current line length up to LF and return the remaining tail length.
; Inputs: A3 = current source cursor; D0 = remaining source length.
; Outputs: D0 = line length before LF; D1 = remaining length after LF; A3
; advanced past the scanned bytes and optional LF.
; Clobbers: D2/CCR.
; CCR: reflects the final length-transfer path, not a stable status value.
findLineEnd	.block
	clr.l d1

loop
	tst.l d0
	beq.s done
	move.b (a3)+, d2
	subq.l #1, d0
	cmpi.b #10, d2
	beq.s done
	addq.l #1, d1
	bra.s loop

done
	move.l d0, d2
	move.l d1, d0
	move.l d2, d1
	rts
	.bend  ; findLineEnd

; Trim one trailing CR from the current line span.
; Inputs: A0 = line start pointer; D0 = line length.
; Outputs: D0 = line length after optional CR trim.
; Clobbers: A1/CCR.
; CCR: reflects D0 on return.
trimCr	.block
	tst.l d0
	beq.s done
	movea.l a0, a1
	adda.l d0, a1
	subq.l #1, a1
	cmpi.b #13, (a1)
	bne.s done
	subq.l #1, d0

done
	rts
	.bend  ; trimCr

; Report whether the current line span is blank or whitespace-only.
; Inputs: A0 = line start pointer; D0 = line length.
; Outputs: D0 = 1 when blank or whitespace-only, otherwise 0.
; Clobbers: D2/CCR.
; CCR: reflects D0 on return.
lineIsBlank	.block
	tst.l d0
	beq.s blank
	subq.l #1, d0

loop
	move.b (a0)+, d2
	cmpi.b #32, d2
	beq.s next
	cmpi.b #9, d2
	bne.s notBlank

next
	dbra d0, loop

blank
	moveq #1, d0
	rts

notBlank
	clr.l d0
	rts
	.bend  ; lineIsBlank

; Populate the shared route frame for the current logical line.
; Inputs: A0 = line start pointer; D0 = trimmed line length; A6 = iterator
; frame base; D6 = current one-based line number.
; Outputs: PrvmIteratorRouteFrame is populated for the route call.
; Clobbers: A1/CCR.
; CCR: unspecified on return.
buildRouteFrame	.block
	lea PrvmIteratorRouteFrame(PC), a1
	move.l #PRVM_ROUTE_MAGIC_OPLR, ROUTE_FRAME_MAGIC(a1)
	move.w #PRVM_ROUTE_ABI_VERSION_V1, ROUTE_FRAME_ABI_VERSION(a1)
	move.w #PRVM_ROUTE_FRAME_SIZE, ROUTE_FRAME_FRAME_SIZE(a1)
	move.l ITER_FRAME_PROCESSOR_PTR(a6), ROUTE_FRAME_PROCESSOR_PTR(a1)
	move.l ITER_FRAME_PROCESSOR_LEN(a6), ROUTE_FRAME_PROCESSOR_LEN(a1)
	move.l ITER_FRAME_KIND_PTR(a6), ROUTE_FRAME_KIND_PTR(a1)
	move.l ITER_FRAME_KIND_LEN(a6), ROUTE_FRAME_KIND_LEN(a1)
	move.l d6, ROUTE_FRAME_LINE_NUM(a1)
	move.l a0, ROUTE_FRAME_SOURCE_PTR(a1)
	move.l d0, ROUTE_FRAME_SOURCE_LEN(a1)
	move.l ITER_FRAME_TOKEN_PTR(a6), ROUTE_FRAME_TOKEN_PTR(a1)
	move.l ITER_FRAME_TOKEN_COUNT(a6), ROUTE_FRAME_TOKEN_COUNT(a1)
	move.w ITER_FRAME_TOKEN_RECORD_SIZE(a6), ROUTE_FRAME_TOKEN_RECORD_SIZE(a1)
	clr.w 46(a1)
	move.l ITER_FRAME_LEXEME_PTR(a6), ROUTE_FRAME_LEXEME_PTR(a1)
	move.l ITER_FRAME_LEXEME_LEN(a6), ROUTE_FRAME_LEXEME_LEN(a1)
	move.l ITER_FRAME_PROGRAM_PTR(a6), ROUTE_FRAME_PROGRAM_PTR(a1)
	move.l ITER_FRAME_PROGRAM_LEN(a6), ROUTE_FRAME_PROGRAM_LEN(a1)
	move.l ITER_FRAME_RESULT_PTR(a6), ROUTE_FRAME_RESULT_PTR(a1)
	move.l ITER_FRAME_RESULT_CAPACITY(a6), ROUTE_FRAME_RESULT_CAPACITY(a1)
	move.l ITER_FRAME_DIAGNOSTIC_PTR(a6), ROUTE_FRAME_DIAGNOSTIC_PTR(a1)
	move.l ITER_FRAME_DIAGNOSTIC_CAPACITY(a6), ROUTE_FRAME_DIAGNOSTIC_CAPACITY(a1)
	move.l ITER_FRAME_RESUME_PTR(a6), ROUTE_FRAME_RESUME_PTR(a1)
	move.l ITER_FRAME_RESUME_CAPACITY(a6), ROUTE_FRAME_RESUME_CAPACITY(a1)
	move.l ITER_FRAME_EXPR_REQUEST_PTR(a6), ROUTE_FRAME_EXPR_REQUEST_PTR(a1)
	move.l ITER_FRAME_EXPR_REQUEST_SIZE(a6), ROUTE_FRAME_EXPR_REQUEST_SIZE(a1)
	move.l ITER_FRAME_EXPR_RESULT_PTR(a6), ROUTE_FRAME_EXPR_RESULT_PTR(a1)
	move.l ITER_FRAME_EXPR_RESULT_COUNT(a6), ROUTE_FRAME_EXPR_RESULT_COUNT(a1)
	move.l ITER_FRAME_PARSER_CONTRACT_VERSION(a6), ROUTE_FRAME_PARSER_CONTRACT_VERSION(a1)
	move.l ITER_FRAME_STEP_BUDGET(a6), ROUTE_FRAME_STEP_BUDGET(a1)
	move.l ITER_FRAME_FLAGS(a6), ROUTE_FRAME_FLAGS(a1)
	rts
	.bend  ; buildRouteFrame

PrvmIteratorRouteFrame
	.fill byte, 116, 0
PrvmIteratorRouteEntryPtr
	.long line_router.prvmRouteLine68000

	.endsection
	.endmodule
