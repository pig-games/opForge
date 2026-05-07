; Native AmigaOS 6502 first-run artifact contract.
; Rust reference output for the native .bin/.prg/.hex/.lst parity matrix.

        .module main
        .cpu 6502

        .region ram, $0800, $083f, align=1

        .section code, align=1
OFFSET  .const $02
VALUE   .var   $10

start
        lda #$42
        sta $0200 + OFFSET
        beq done
        bne start
        ldx #VALUE
        inx
done
        .byte $aa, $0c, $08
        .word start + 3
        .text "OK"
        .fill byte, 2, $ff
        .endsection

        .place code in ram

        .output "build/6502-first-run.prg", format=prg, loadaddr=$0800, contiguous=false, sections=code

        .endmodule
        .end
