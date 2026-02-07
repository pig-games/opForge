; 65816 MVP Addressing/Instruction Coverage
; Covers currently implemented 65816-specific forms in this branch.

        .cpu 65816
        .org $0800

; ========================================
; CONTROL FLOW / LONG CONTROL
; ========================================
        brl long_target     ; 82 xx xx
        nop                 ; EA
long_target:
        jml $C0FFEE         ; 5C EE FF C0
        jsl $00ABCD         ; 22 CD AB 00
        jml [$3456]         ; DC 56 34
        rtl                 ; 6B

; ========================================
; MODE/CONTROL
; ========================================
        rep #$30            ; C2 30
        sep #$10            ; E2 10
        xce                 ; FB
        xba                 ; EB

; ========================================
; STACK/REGISTER CONTROL
; ========================================
        phb                 ; 8B
        plb                 ; AB
        phd                 ; 0B
        pld                 ; 2B
        phk                 ; 4B
        tcd                 ; 5B
        tdc                 ; 7B
        tcs                 ; 1B
        tsc                 ; 3B

; ========================================
; MEMORY / IMMEDIATE CONTROL
; ========================================
        pea $4567           ; F4 67 45
        pei ($44)           ; D4 44
        per rel_target      ; 62 xx xx
        cop #$99            ; 02 99
        wdm #$55            ; 42 55

; ========================================
; BLOCK MOVE
; ========================================
        mvn $01,$02         ; 54 01 02
        mvp $03,$04         ; 44 03 04

; ========================================
; 65816 STACK-RELATIVE ADDRESSING
; ========================================
        ora $20,s           ; 03 20
        ora ($21,s),y       ; 13 21

; Shared family instructions still valid on 65816
        lda #$12
        sta $2000

rel_target:
        rts

        .end
