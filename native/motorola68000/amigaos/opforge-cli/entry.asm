; Native AmigaOS opForge CLI process entry.

	.module opforge.cli.entry
	.cpu 68020

	.use opforge.cli.constants (SYS_BASE, PR_CLI, PR_MSG_PORT)
	.use opforge.cli.constants (FIND_TASK, WAIT_PORT, GET_MSG, REPLY_MSG, FORBID)
	.use opforge.cli.constants (RETURN_WORKBENCH_UNSUPPORTED)
	.use opforge.cli.run (opforgeNativeCliRun)

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
	movea.l SYS_BASE.W, a6  ; Exec base for process and Workbench-message calls
	jsr FIND_TASK(a6)

	movea.l d0, a2
	tst.l PR_CLI(a2)  ; nonzero means Shell launch; zero means Workbench activation
	bne.w cli

	lea PR_MSG_PORT(a2), a0
	jsr WAIT_PORT(a6)
	lea PR_MSG_PORT(a2), a0
	jsr GET_MSG(a6)
	move.l d0, d2  ; preserve startup message so ReplyMsg can be sent before exit
	moveq #RETURN_WORKBENCH_UNSUPPORTED, d7
	bra.w reply

cli
	jsr opforgeNativeCliRun  ; run the Shell-native CLI host path
	move.l d0, d7  ; keep return code stable across optional Workbench reply path

reply
	tst.l d2
	beq.w done
	jsr FORBID(a6)
	movea.l d2, a1
	jsr REPLY_MSG(a6)

done
	move.l d7, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start

	.endsection
	.endmodule
