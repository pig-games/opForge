; Test and demonstrate assembly errors with 65C02 source.
; Lines marked with ERR are expected diagnostics; the checked .err artifact
; records the first fail-closed diagnostic from this catalogue.

        .cpu 65c02
        .org $2000

; constants and numeric syntax
good_dec .const 55          ; decimal constant
bad_dec  .const 5X5         ; ERR: invalid number
good_hex .const $05A5       ; hexadecimal constant
good_bin .const %01100110   ; binary constant
bad_bin  .const %01101210   ; ERR: invalid binary digit

known   .const $77
badstr  .byte "where is the end ; ERR: unterminated string
special .byte "\t\n\r", known, missing_symbol ; ERR: undefined symbol

; column-zero rules
LABEL1  NOP                 ; label with instruction is valid
LABEL2  LDA #$11            ; label with operand is valid
NAME1   .const 7            ; symbol can start in column zero
123                           ; ERR: not a symbol
"abc"                         ; ERR: not a symbol
+                             ; ERR: not a symbol
lda #$12                      ; ERR: instruction cannot start in column zero

; extra, missing, and incorrect operands
        nop X                ; ERR: extra register argument
        nop $12              ; ERR: extra expression argument
        lda                  ; ERR: missing operand
        lda #missing_symbol  ; ERR: undefined expression symbol
        sta #$12             ; ERR: immediate store is unsupported
        jmp X                ; ERR: target must be an expression
        bra                  ; ERR: missing branch target
        lda #1+2+            ; ERR: incomplete expression

; characters in expressions
        lda #'P'             ; one character is a byte
        .word 'QR'           ; two characters form a word
        lda #'abc'           ; ERR: string is not a scalar byte expression
        .word 'a' * 'B'      ; unusual but legal

; arithmetic and output width
divzero .const 23 / 0        ; ERR: divide by zero
wideok  .word 255 + 10       ; valid 16-bit value
widebad .byte 255 + 10       ; ERR: 8-bit value overflow

; assignment errors
undef   += 1                 ; ERR: symbol has not been defined
const1  = 1                  ; valid assignment
const1  += 1                 ; ERR: symbol is read-only

; symbol redefinition
equ1    .const 1             ; valid constant
equ1    .const 2             ; ERR: constant redefined
set1    .var 1               ; valid variable
set1    .var 2               ; valid variable reassignment
set1    .const 3             ; ERR: variable redefined as constant
equ2    .const 2             ; valid constant
equ2    .var 3               ; ERR: constant redefined as variable

lab3    jmp $2000            ; valid label
lab3    jmp $2001            ; ERR: label redefined
lab4    jmp $2002            ; valid label

; conditional directive structure
YES     .const 1
TRUE    .const 1
NO      .const 0
FALSE   .const 0

        .elseif YES          ; ERR: missing .if
        .else                ; ERR: missing .if
        .endif               ; ERR: missing .if

        .if FALSE
        .else
        .else                ; ERR: multiple .else
        .endif

        .if FALSE
        .else
        .elseif TRUE         ; ERR: .elseif after .else
        .endif

        .if FALSE || 3+1 == 4
            ldx #$be
            lda #$ef
                                ; ERR: missing .endif
