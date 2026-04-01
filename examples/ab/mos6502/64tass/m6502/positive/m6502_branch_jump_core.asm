; Curated shared-subset fixture derived from examples/mos6502/6502_allmodes.asm.

entry
        lda #$01
        sta $40
        lda $40
        bne branch_taken
        nop

branch_taken
        ldx #$02
        inx
        cpx #$03
        beq done
        nop

done
        jmp finish

finish
        rts