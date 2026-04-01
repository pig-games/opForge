; Curated shared-subset fixture derived from examples/mos6502/6502_simple.asm.

start
        clc
        lda #$42
        sta $20
        ldx #$10
        ldy #$08
        adc #$01
        cmp #$43
        bne fail
        jmp done

fail
        nop

done
        rts