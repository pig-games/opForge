; Curated shared-subset fixture derived from examples/mos6502/65816_simple.asm.

        rep #$30
        sep #$20
        xce
        xba
        pea $1234
        pei ($20)
        cop #$7f
        wdm #$42
        ora $10,s
        ora ($11,s),y
        rtl