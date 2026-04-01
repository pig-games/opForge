; Curated shared-subset fixture with explicit width-state transitions.

        .al
        .xl
        lda #$1234
        ldx #$5678

        .as
        .xs
        lda #$12
        ldx #$34

        xba
        xce
        rts