; AmigaOS "Hello World" executable for the v0.3 executable-complete Hunk subset.
; Uses unplaced Hunk output plus bare-symbol executable notation where the
; supported matrix now allows it.
; Uses dos.library/PutStr() so requires Kickstart 2.0+.
; The program is CLI-oriented and does not include Workbench startup handling.

        .module main
        .cpu 68000

SysBase         = 4

; LVO offsets in the relevant library jump tables.
OpenLibrary     = -552
CloseLibrary    = -414
PutStr          = -948

        .section code, kind=code

start:
        LEA dos_name,A1             ; "dos.library" name string
        MOVEQ #36,D0                ; minimum required version (Kickstart 2.0)
        MOVEA.L SysBase.W,A6
        JSR OpenLibrary(A6)

        TST.L D0                    ; zero if OpenLibrary() failed
        BEQ no_dos

        MOVE.L #hello,D1            ; PutStr() expects the string pointer in D1
        MOVEA.L D0,A6               ; DOSBase
        JSR PutStr(A6)

        MOVEA.L A6,A1               ; library to close
        MOVEA.L SysBase.W,A6
        JSR CloseLibrary(A6)

no_dos:
        CLR.L D0                    ; return 0 to the system
        RTS

        .endsection
        .section data, kind=data

dos_name:
        .byte "dos.library",0
hello:
        .byte "Hello World!",10,0

        .endsection
        .output "build/helloworld.hunk", format=hunk, sections=code,data
        .endmodule
