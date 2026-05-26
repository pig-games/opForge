; Native AmigaOS opForge CLI process entry.

	.module opforge.cli.entry
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.run

	.section entry, kind=code
	.pub

; ---------------------------------------------------------------------------
; AmigaOS process entry for the native opForge CLI.
;
; Workbench launches are rejected for this deliverable slice because the native
; host contract is currently Shell/file based. Shell launches hand off to
; opforgeNativeCliRun, which mirrors the Rust CLI orchestration surface for the
; supported native subset.
;
; Inputs:
; - AmigaOS process context; no explicit arguments.
;
; Outputs:
; - D0: AmigaDOS return code.
; ---------------------------------------------------------------------------
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	clr.l d2  ; no Workbench startup message is pending until GetMsg succeeds

	suba.l a1, a1  ; Exec FindTask(NULL) => current process
	movea.l constants.SYS_BASE.W, a6  ; Exec base for process and Workbench-message calls
	jsr constants.FIND_TASK(a6)

	movea.l d0, a2
	tst.l constants.PR_CLI(a2)  ; nonzero means Shell launch; zero means Workbench activation
	bne.w cli

	lea constants.PR_MSG_PORT(a2), a0
	jsr constants.WAIT_PORT(a6)
	lea constants.PR_MSG_PORT(a2), a0
	jsr constants.GET_MSG(a6)
	move.l d0, d2  ; preserve startup message so ReplyMsg can be sent before exit
	moveq #constants.RETURN_WORKBENCH_UNSUPPORTED, d7
	bra.w reply

cli
	jsr run.opforgeNativeCliRun  ; run the Shell-native CLI host path
	move.l d0, d7  ; keep return code stable across optional Workbench reply path

reply
	tst.l d2
	beq.w done
	jsr constants.FORBID(a6)
	movea.l d2, a1
	jsr constants.REPLY_MSG(a6)

done
	move.l d7, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start

	.endsection
	.endmodule
