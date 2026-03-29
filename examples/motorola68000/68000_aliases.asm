; Motorola 68000 idiomatic alias spellings
; Shows accepted alternative source forms that map to baseline canonical modes.

        .cpu 68000
        .org $1000

start:
; ========================================
; BRANCH DEFAULTS
; ========================================
        BRA.W branch_target
        BRA branch_target
        BNE.W done
        BNE done
        BSR.W helper
        BSR helper

branch_target:
; ========================================
; ZERO-DISPLACEMENT INDEXED FORMS
; ========================================
        MOVE.W 0(A0,D1.W),D0
        MOVE.W (A0,D1),D0
        MOVE.W (A0,D1.W*1),D0
        MOVE.W 0(PC,D2.W),D3
        MOVE.W (PC,D2),D3
        MOVE.W (PC,D2.W*1),D3

; ========================================
; ZERO-DISPLACEMENT PC-RELATIVE SHORTHAND
; ========================================
        JMP 0(PC)
        JMP (PC)

; ========================================
; ABSOLUTE .W / .L FORMS
; ========================================
        MOVE.W ($1234).W,D4
        MOVE.W $1234.W,D4
        MOVE.L ($123456).L,D5
        MOVE.L $123456.L,D5

done:
        RTS

helper:
        RTS

        .end
