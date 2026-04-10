; AmigaOS Hunk executable example.
; Translated from Sakura-IT ASM/MiniStartup/startup.s into opForge's
; v0.3 sectioned Hunk notation.

        .module main
        .cpu 68000

SysBase                 = 4

; exec.library LVOs
OpenLibrary             = -552
CloseLibrary            = -414
FindTask                = -294
WaitPort                = -384
GetMsg                  = -372
ReplyMsg                = -378
Forbid                  = -132

; intuition.library LVOs
DisplayAlert            = -90

; Minimal offsets translated from dos/dosextens.i as used by the source.
pr_CLI                  = 172
pr_MsgPort              = 92

        .section code, kind=code

start:
        MOVEM.L D2/D3/A2/A6,-(SP)
        CLR.L D2

        SUBA.L A1,A1
        MOVEA.L SysBase.W,A6
        JSR FindTask(A6)

        MOVEA.L D0,A2
        TST.L pr_CLI(A2)
        BNE.S no_workbench

        LEA pr_MsgPort(A2),A0
        JSR WaitPort(A6)
        LEA pr_MsgPort(A2),A0
        JSR GetMsg(A6)
        MOVE.L D0,D2

no_workbench:
        BSR.S main
        MOVE.L D0,D3

        TST.L D2
        BEQ.S no_reply_needed
        JSR Forbid(A6)
        MOVEA.L D2,A1
        JSR ReplyMsg(A6)

no_reply_needed:
        MOVE.L D3,D0
        MOVEM.L (SP)+,D2/D3/A2/A6
        RTS

main:
        MOVE.L A6,-(SP)

        LEA intui_name,A1
        MOVEQ #36,D0
        MOVEA.L SysBase.W,A6
        JSR OpenLibrary(A6)

        TST.L D0
        BEQ.S no_intui

        MOVEA.L D0,A6
        MOVE.L #$01234567,D0
        MOVEQ #28,D1
        LEA hello_alert,A0
        JSR DisplayAlert(A6)

        MOVEA.L A6,A1
        MOVEA.L SysBase.W,A6
        JSR CloseLibrary(A6)

no_intui:
        CLR.L D0
        MOVEA.L (SP)+,A6
        RTS

        .endsection
        .section data, kind=data

intui_name:
        .byte "intuition.library",0
        .align 2

hello_alert:
        .word 16
        .byte 16,"Hello World!",0,0

        .endsection
        .output "build/workbench_startup_alert.hunk", format=hunk, sections=code,data
        .endmodule
