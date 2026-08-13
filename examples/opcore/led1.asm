; 65C02 memory-mapped LED blink example.
;
; This keeps the original corpus case's layout purpose: code is assembled at
; a ROM address, enters through a reset-style jump, toggles an output, uses
; nested delay loops, and repeats forever.

        .cpu 65c02
        .org $8000

        jmp START

LED_PORT .const $4000

START   lda #$01        ; LED on
        sta LED_PORT

        ldx #$ff        ; Delay
D1_OUT  ldy #$ff
D1_IN   dey
        bne D1_IN
        dex
        bne D1_OUT

        stz LED_PORT    ; LED off

        ldx #$ff        ; Delay
D2_OUT  ldy #$ff
D2_IN   dey
        bne D2_IN
        dex
        bne D2_OUT

        bra START       ; Loop forever
