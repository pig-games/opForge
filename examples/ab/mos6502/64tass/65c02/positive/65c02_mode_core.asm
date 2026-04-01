; Curated shared-subset fixture derived from examples/mos6502/65c02_allmodes.asm.

        lda ($20)
        sta ($30)
        adc ($40)
        and ($60)
        ora ($70)
        eor ($80)
        cmp ($90)
        bit $60,x
        bit $5678,x
        jmp ($1234,x)
        rts