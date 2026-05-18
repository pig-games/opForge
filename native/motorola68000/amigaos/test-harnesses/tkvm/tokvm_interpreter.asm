; Native AmigaOS 68020-baseline tokenizer VM executable.
;
; This root file is now a true composition module: it owns only the Amiga entry
; path and pulls the CLI harness and tokenizer VM in through `.use`.

	.module main
	.cpu 68020
	.use tokvm.amigaos.cli_harness (SYS_BASE, PR_CLI, PR_MSG_PORT)
	.use tokvm.amigaos.cli_harness (FIND_TASK, WAIT_PORT, GET_MSG, REPLY_MSG, FORBID)
	.use tokvm.amigaos.cli_harness (RETURN_WORKBENCH_UNSUPPORTED, tokvmAmigaosCliHarnessRun)
	.use tkvm.amigaos.runtime (tkvmRun68000)

	.section entry, kind=code

	.pub

; ---------------------------------------------------------------------------
; AmigaOS process entry.
;
; Workbench launches are rejected because this first native slice is a Shell
; tool with a narrow CLI/file-I/O contract. Once CLI execution is confirmed, we
; hand off to the imported tokvm.amigaos.cli_harness module.
; ---------------------------------------------------------------------------

start	.block
	movem.l d2-d7/a2-a6, -(sp)  ; preserve callee-owned state across the CLI harness call
	clr.l d2  ; no Workbench startup message is pending until GetMsg succeeds

	suba.l a1, a1  ; Exec FindTask(NULL) => current process, same host contract as C/Rust launchers
	movea.l SYS_BASE.W, a6  ; Exec base for FindTask/WaitPort/GetMsg/ReplyMsg/Forbid
	jsr FIND_TASK(a6)

	movea.l d0, a2
	tst.l PR_CLI(a2)  ; tokvm's first native slice only supports Shell launches, not Workbench icons
	bne.w cli

	lea PR_MSG_PORT(a2), a0
	jsr WAIT_PORT(a6)
	lea PR_MSG_PORT(a2), a0
	jsr GET_MSG(a6)
	move.l d0, d2  ; cache the startup message so we can ReplyMsg before returning
	moveq #RETURN_WORKBENCH_UNSUPPORTED, d7  ; host-visible status for unsupported Workbench activation
	bra.w reply

cli
	jsr tokvmAmigaosCliHarnessRun  ; hand off to the module that mirrors the Rust host/report bridge
	move.l d0, d7  ; keep the CLI/report status live through the reply/epilogue path

reply
	tst.l d2  ; only Workbench launches require a reply to the startup message
	beq.w done
	jsr FORBID(a6)
	movea.l d2, a1
	jsr REPLY_MSG(a6)

done
	move.l d7, d0  ; Amiga return register carries the harness/native VM outcome
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start

	.endsection
	.output "build/tokvm", format=hunk, sections=entry, code, data, bss
	.endmodule