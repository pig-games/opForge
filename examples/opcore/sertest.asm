; 65C02 test program to bit-bang one character through a memory-mapped port.
;
; The byte is framed with a start bit and stop bits, shifted least-significant
; bit first, delayed between writes, and sent repeatedly.

        .cpu 65c02
        .org $0800

SERIAL_PORT .const $4001
BITTIME     .const $13
OUTBITS     .const 11
TXBYTE      .const $20

START   lda #'T'
        sta TXBYTE
        sei
        ldx #OUTBITS
        clc                     ; Start bit

SEND_BIT
        lda #$00
        ror a                   ; Carry becomes the output bit
        sta SERIAL_PORT

        ldy #BITTIME
BIT_DELAY
        dey
        bne BIT_DELAY

        sec                     ; Shift in stop bits behind the character
        lda TXBYTE
        ror a                   ; Next data bit moves into carry
        sta TXBYTE
        dex
        bne SEND_BIT
        cli

        ldx #$ff
CHAR_DELAY
        dex
        bne CHAR_DELAY
        bra START
