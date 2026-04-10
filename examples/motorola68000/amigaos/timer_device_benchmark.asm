; AmigaOS Hunk executable example.
; Translated from Sakura-IT ASM/Benchmark/benchmark.s into opForge's
; v0.3 sectioned Hunk notation.

        .module main
        .cpu 68000

; exec.library LVOs
Forbid                  = -132
Permit                  = -138
FindTask                = -294
GetMsg                  = -372
ReplyMsg                = -378
WaitPort                = -384
CloseLibrary            = -414
OpenDevice              = -444
CloseDevice             = -450
OpenLibrary             = -552
CreateIORequest         = -654
DeleteIORequest         = -660
CreateMsgPort           = -666
DeleteMsgPort           = -672

; dos.library
VPrintf                 = -954

; timer.device
SubTime                 = -48
GetSysTime              = -66

; Minimal offsets translated from includes used by the source.
SysBaseOff              = 0
DosBaseOff              = 4
TimerPortOff            = 8
TimerRequestOff         = 12
TimerBaseOff            = 16
GlobalsSize             = 20

pr_CLI                  = 172
pr_MsgPort              = 92
IOTV_SIZE               = 48
IO_DEVICE               = 20
UNIT_MICROHZ            = 0

        .section code, kind=code

start:
        MOVEM.L D2/D3/A2/A4/A6,-(SP)
        LEA globals,A4
        MOVEA.L 4.W,A6
        MOVEQ #0,D2
        MOVE.L A6,SysBaseOff(A4)

        SUBA.L A1,A1
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
        BSR.W main
        MOVE.L D0,D3

        TST.L D2
        BEQ.S no_reply_needed
        JSR Forbid(A6)
        MOVEA.L D2,A1
        JSR ReplyMsg(A6)

no_reply_needed:
        MOVE.L D3,D0
        MOVEM.L (SP)+,D2/D3/A2/A4/A6
        RTS

main:
        MOVEM.L A2/A6,-(SP)

        LEA dos_name,A1
        MOVEQ #36,D0
        MOVEA.L SysBaseOff(A4),A6
        JSR OpenLibrary(A6)

        MOVE.L D0,DosBaseOff(A4)
        MOVEA.L D0,A2
        BEQ.S no_dos

        BSR.S get_timer_port

        MOVEA.L A2,A1
        JSR CloseLibrary(A6)

no_dos:
        MOVEQ #0,D0
        MOVEM.L (SP)+,A2/A6
        RTS

get_timer_port:
        MOVEA.L SysBaseOff(A4),A6
        JSR CreateMsgPort(A6)

        MOVE.L D0,TimerPortOff(A4)
        BEQ.S no_timer_port
        BSR.S get_timer_request

        MOVEA.L TimerPortOff(A4),A0
        JSR DeleteMsgPort(A6)

no_timer_port:
        RTS

get_timer_request:
        MOVEA.L D0,A0
        MOVEQ #IOTV_SIZE,D0
        JSR CreateIORequest(A6)

        MOVE.L D0,TimerRequestOff(A4)
        BEQ.S no_timer_request
        BSR.S open_timer_device

        MOVEA.L TimerRequestOff(A4),A0
        JSR DeleteIORequest(A6)

no_timer_request:
        RTS

open_timer_device:
        MOVE.L A2,-(SP)
        MOVEA.L D0,A1
        MOVEQ #UNIT_MICROHZ,D0
        LEA timer_name,A0
        MOVEQ #0,D1
        MOVEA.L A1,A2
        JSR OpenDevice(A6)
        TST.B D0
        BNE.S device_failed

        MOVE.L IO_DEVICE(A2),TimerBaseOff(A4)
        BSR.S measure_time

        MOVEA.L A2,A1
        JSR CloseDevice(A6)

device_failed:
        MOVEA.L (SP)+,A2
        RTS

measure_time:
        MOVE.L A6,-(SP)
        LEA start_seconds,A0
        MOVEA.L TimerBaseOff(A4),A6
        JSR GetSysTime(A6)

        MOVEA.L SysBaseOff(A4),A6
        JSR Forbid(A6)
        JSR benchmarked_code
        JSR Permit(A6)

        MOVEA.L TimerBaseOff(A4),A6
        LEA end_seconds,A0
        JSR GetSysTime(A6)

        LEA end_seconds,A0
        LEA start_seconds,A1
        JSR SubTime(A6)

        BSR.S print_diff

        MOVEA.L (SP)+,A6
        RTS

print_diff:
        MOVEM.L D2/A6,-(SP)

        MOVE.L #pfmt,D1
        MOVE.L #end_seconds,D2
        MOVEA.L DosBaseOff(A4),A6
        JSR VPrintf(A6)

        MOVEM.L (SP)+,D2/A6
        RTS

benchmarked_code:
        MOVE.W #9999,D0
loop:
        DBF D0,loop
        RTS

        .endsection
        .section data, kind=data

start_seconds:
        .long 0
start_micros:
        .long 0
end_seconds:
        .long 0
end_micros:
        .long 0

dos_name:
        .byte "dos.library",0
timer_name:
        .byte "timer.device",0
        .align 4

pfmt:
        .byte "Time: %ld.%06ld seconds.",10,0

        .endsection
        .section bss, kind=bss

globals:
        .res byte, GlobalsSize

        .endsection
        .output "build/timer_device_benchmark.hunk", format=hunk, sections=code,data,bss
        .endmodule
