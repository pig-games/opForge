; Native AmigaOS 68020-baseline tokenizer VM executable.
;
; This root file is now a true composition module: it owns only the Amiga entry
; path and pulls the CLI harness and tokenizer VM in through `.use`.

        .module main
        .cpu 68020
        .use tokvm.amigaos.cli_harness (SysBase, pr_CLI, pr_MsgPort)
        .use tokvm.amigaos.cli_harness (FindTask, WaitPort, GetMsg, ReplyMsg, Forbid)
        .use tokvm.amigaos.cli_harness (RETURN_WORKBENCH_UNSUPPORTED, tokvm_amigaos_cli_harness_run)
        .use tokvm.amigaos.tokenizer_vm (abiMarker)

        .section entry, kind=code

; ---------------------------------------------------------------------------
; AmigaOS process entry.
;
; Workbench launches are rejected because this first native slice is a Shell
; tool with a narrow CLI/file-I/O contract. Once CLI execution is confirmed, we
; hand off to the imported tokvm.amigaos.cli_harness module.
; ---------------------------------------------------------------------------

start:
        MOVEM.L D2-D7/A2-A6, -(SP)  ; preserve callee-owned state across the CLI harness call
        CLR.L D2  ; no Workbench startup message is pending until GetMsg succeeds

        SUBA.L A1, A1  ; Exec FindTask(NULL) => current process, same host contract as C/Rust launchers
        MOVEA.L SysBase.W, A6  ; Exec base for FindTask/WaitPort/GetMsg/ReplyMsg/Forbid
        JSR FindTask(A6)

        MOVEA.L D0, A2
        TST.L pr_CLI(A2)  ; tokvm's first native slice only supports Shell launches, not Workbench icons
        BNE.W startCli

        LEA pr_MsgPort(A2), A0
        JSR WaitPort(A6)
        LEA pr_MsgPort(A2), A0
        JSR GetMsg(A6)
        MOVE.L D0, D2  ; cache the startup message so we can ReplyMsg before returning
        MOVEQ #RETURN_WORKBENCH_UNSUPPORTED, D7  ; host-visible status for unsupported Workbench activation
        BRA.W startReply

startCli:
        JSR tokvm_amigaos_cli_harness_run  ; hand off to the module that mirrors the Rust host/report bridge
        MOVE.L D0, D7  ; keep the CLI/report status live through the reply/epilogue path

startReply:
        TST.L D2  ; only Workbench launches require a reply to the startup message
        BEQ.W startDone
        JSR Forbid(A6)
        MOVEA.L D2, A1
        JSR ReplyMsg(A6)

startDone:
        MOVE.L D7, D0  ; Amiga return register carries the harness/native VM outcome
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

        .endsection
        .output "build/tokvm", format=hunk, sections=entry, code, data, bss
        .endmodule