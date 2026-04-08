; AmigaOS file-output smoke executable for the current Hunk path.
; Writes a fixed text file to T: so it can be checked after running under AmigaOS.
; The program is CLI-oriented and does not include Workbench startup handling.

        .module main
        .cpu 68000

SysBase         = 4
RETURN_OK       = 0
RETURN_FAIL     = 20
MODE_NEWFILE    = 1006

; LVO offsets in the relevant library jump tables.
OpenLibrary     = -552
CloseLibrary    = -414
Open            = -30
Close           = -36
Write           = -48

message_len     = 19

        .region ram, $2000, $20ff
        .section code, kind=code

start:
        MOVEQ #RETURN_FAIL,D7

        LEA dos_name.L,A1
        MOVEQ #36,D0
        MOVEA.L SysBase.W,A6
        JSR OpenLibrary(A6)

        TST.L D0
        BEQ done

        MOVEA.L D0,A5

        LEA output_path.L,A0
        MOVE.L A0,D1
        MOVE.L #MODE_NEWFILE,D2
        MOVEA.L A5,A6
        JSR Open(A6)

        TST.L D0
        BEQ close_dos

        MOVE.L D0,D6
        MOVE.L D6,D1
        MOVE.L #message,D2
        MOVE.L #message_len,D3
        MOVEA.L A5,A6
        JSR Write(A6)

        CMP.L #message_len,D0
        BNE close_file

        MOVEQ #RETURN_OK,D7

close_file:
        MOVE.L D6,D1
        MOVEA.L A5,A6
        JSR Close(A6)

close_dos:
        MOVEA.L A5,A1
        MOVEA.L SysBase.W,A6
        JSR CloseLibrary(A6)

done:
        MOVE.L D7,D0
        RTS

dos_name:
        .byte "dos.library",0
output_path:
        .byte "T:opforge-writefile.txt",0
message:
        .byte "Hello from opForge",10

        .endsection
        .place code in ram
        .output "build/out.hunk", format=hunk, sections=code
        .endmodule
