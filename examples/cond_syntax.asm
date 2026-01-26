; Conditional assembly (.if/.elseif/.else/.endif) coverage
        org     2000h

TRUE    equ     1
FALSE   equ     0

; simple if/else
        .if TRUE
                db 1
        .else
                db 2
        .endif

; elseif chain
        .if 0
                db 3
        .elseif 0
                db 4
        .elseif 5
                db 5
        .else
                db 6
        .endif

; nested conditionals
        .if 1
                .if 0
                        db 7
                .else
                        db 8
                .endif
        .else
                db 9
        .endif

; labels on conditionals
LABIF:  .if TRUE
                db 10
LABELSE:.else
                db 11
LABEND: .endif
