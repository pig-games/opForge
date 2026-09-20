; Register endpoints and signed-immediate boundaries from package predicates.
        .cpu m68000
        .org $1000
        move.l d0,d1
        move.w d7,d0
        move.l d0,d7
        moveq #127,d7
        moveq #-128,d0
        .end
