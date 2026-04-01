; Curated shared-subset fixture derived from examples/mos6502/65c02_simple.asm.

        stz $20
        stz $1234
        phx
        phy
        plx
        ply
        inc a
        dec a
        tsb $40
        trb $30
        bit #$55
        rts