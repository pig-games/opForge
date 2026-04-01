; 68000 all addressing and operand forms
; Covers the currently supported canonical 68000 operand families in one file.

        .cpu 68000
        .org $1000

start:
; ========================================
; REGISTER DIRECT
; ========================================
        MOVE.B D0,D1
        MOVEA.L A0,A1
        EXG D2,D3
        EXG A2,A3
        EXG D4,A4

; ========================================
; ADDRESS REGISTER INDIRECT FAMILY
; ========================================
        MOVE.W (A0),D0
        MOVE.W (A0)+,D1
        MOVE.W -(A1),D2
        MOVE.W 4(A2),D3
        ; Canonical / alias pair for zero-displacement indexed form
        MOVE.W 0(A3,D4.W),D5
        MOVE.W (A3,D4),D5

; ========================================
; ABSOLUTE ADDRESS FAMILY
; ========================================
        ; Canonical / alias pair for absolute short
        MOVE.W ($1234).W,D6
        MOVE.W $1234.W,D6
        ; Canonical / alias pair for absolute long
        MOVE.L ($123456).L,D7
        MOVE.L $123456.L,D7
        PEA ($1234).W
        JMP ($123456).L

; ========================================
; PC-RELATIVE FAMILY
; ========================================
        MOVE.W 8(PC),D0
        ; Canonical / alias pair for zero-displacement PC-indexed form
        MOVE.W 0(PC,D1.W),D2
        MOVE.W (PC,D1),D2
        ; Canonical / alias pair for word-sized branch defaulting
        BSR.W helper
        BSR helper
        JMP 4(PC)

; ========================================
; IMMEDIATE FAMILY
; ========================================
        MOVE.W #$1234,D3
        ORI.B #$12,D0
        ADDI.W #1,4(A0)
        CMPI.W #$1234,($1234).W

; ========================================
; CONTROL / SPECIAL TRANSFER FORMS
; ========================================
        LEA 4(A0,D1.W),A1
        PEA 4(A0)
        JSR (A0)
        MOVEM.W D0-D2/A6,-(A7)
        MOVEM.L (A0)+,D1/D3/A2-A4
        MOVEP.W D5,4(A1)
        MOVEP.L 6(A2),D6

helper:
        RTS

        .end
