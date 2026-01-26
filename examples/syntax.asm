; The assembler runs two passes over the source file.  The first pass builds
; the symbol table and the second emits the hex file and list file.
;
; Note that the assembler is case-insensitive for instructions, directives,
; register names, and labels.  The labels "label2" and "LABEL2" are the same,
; and an error would be thrown if both were defined.
 
; The ORG directive sets the current address for assembly.  A file can
; contain more than one ORG.  The assembler does not detect overlapping
; ORG directives and will silently overwrite output in that case.
        org     RAMST + 02000h


; Column one must contain whitespace or a label or a comment start.
; Labels can be up to 32 characters and must start with an alpha.  
; Numbers and the underscore character are permitted in a label.
; Labels end with a colon character.
LABEL1: db      123             ; labels can be on the same line as code
LABEL2:
        POP     PSW             ; or the label can be before the code
NOSPACE:push    D               ; a space is not required after a label
VeryVeryVeryLongLabel:
    db  "Labels can be up to 32 characters && must start with an alpha."

; Expression operators are symbolic, so names like HIGH, LOW, AND, or OR can be
; used as labels if desired.

; The EQU directive is used to define a constant value.  It does not emit
; any code.  The name label for an EQU does not need the trailing but it
; does need to be followed by whitespace.
RAMST   equ     00000h          
ROMST   equ     0c000h         

; Constants can be decimal, binary, hex, or octal.  Binary is indicated with a
; trailing 'b' hex with a trailing 'h', and octal with a trailing 'o' or 'q'
; character.  Decimal constants can have a trailing 'd' character, but it is
; not required.  All constants must start with a numeric character, so hex 
; values starting with A-F must have a leading zero.
num0    equ     55              ; Decimal constant
num1    equ     55D             ; Decimal constant with optional suffix
num3    equ     0a6h            ; Hex with leading zero
num4    equ     1AB4H           ; Hex without leading zero
num5    equ     01100101b       ; Binary
num6    equ     1357o           ; Octal with trailing 'o'
num7    equ     1011Q           ; Octal with trailing 'q'

; The DB directive places one or more bytes of data in the output.
d1:     db      5               ; single byte
d2:     db      12h,34h,56h,78h ; multiple bytes
d3:     db      5, 02Ah, 1011B  ; Mixed decimal, hex, and binary

; The DB directive can also be used with strings.  Each octet in the string
; generates one octet of output.  Strings and numeric constants can be mixed
; in a single directive.
str1:   db      'T'             ; Single character constant
str2:   DB      "Welcome"       ; String constant
str3:   db      "red","green"   ; Multiple strings
str4:   db      3,"red",4,"blue"; Mixed strings and numerics

; Note that a single character string can also be used anywhere a numeric
; would be allowed.  It evaluates to the ASCII value of the single character.
        mvi     c, 65           ; Move the letter 'A' into register C.
        mvi     c, 041H         ; Move the letter 'A' into register C.
        mvi     c, 'A'          ; Move the letter 'A' into register C.

; A two character string can also be used anywhere a numeric would be allowed.
; It evaluates to the 16-bit value of ASCII value of the two characters, where
; the MSB is the first char and the LSB is the second.
        lxi     h,04142H        ; All of these evaluate to 4142H
        lxi     h,'AB'          ; All of these evaluate to 4142H
        lxi     h,"AB"          ; All of these evaluate to 4142H
        lxi     h,'A'*256 | 'B' ; All of these evaluate to 4142H


; Some common C-style string escapes are supported: CR, LF, tab, NULL, and
; hex value.  Hex escapes can use upper or lower case and must be 2 digits.
    db  "\r\n\t\x2a\x2B\0"

; The backslash can also be used to escape quotes or the backslash character
; itself.  Embedded quotes can also be handled by placing double quotes
; inside single quotes or single quotes inside double quotes.
    db  '\\'                    ; Backslash character.
    db  '\''                    ; Single quote character.
    db  "'"                     ; Same string using double quotes.
    db  "A \"quoted\" string"   ; Quotes within quotes.
    db  'A "quoted" string'     ; Same string using single quotes.
    db  "This isn't bad"        ; Single quote in double quotes.
    
; The following DBs are all equivalent  
CR  equ 13
LF  equ '\n'
    db  "ABC123\r\n"
    db  "ABC123",CR,LF
    db  "ABC123",13,10
    db  'A','B','C','1','2','3','\r','\n'
    db  "ABC",31h,32h,"3",'\r',LF

; The DW directive stores one or more 16 bit values.
words1: dw      0203h           ; One word value in hex
words2: dw      num4            ; One word value in decimal
words3: dw      1010101001010101B ; One word value in binary
words4: dw      02h, 03h        ; Two word values
words5: dw      02h, 03, 04ffh  ; Three word values

; Note that DW stores the two octet values in intel (little endian) order, so
; the following two declarations are equivalent:
        dw      01234h
        db      034h, 012h

; The DS directive reserves space, but does not generate any output.  It
; simply advances the target address for the next code or data.
StrSize equ     32
buffer: ds  StrSize * 4     ; Reserve space for 4 strings

; Expressions can be used in place of any numeric constant.
t:      dw      1024 * 3
        LXI     SP, 32 * 4 + 2
        lxi     h, str4 + 5
        lxi     d, buffer + StrSize
        db      7+7*7+7/(7+7-7)
        mvi     c, 'A' | 020H
        mvi     c, 'a' & 11011111b

; Operators and precedence (highest to lowest):
;   unary: + - ~ ! < >
;   power: **
;   * / %
;   + -
;   shifts: << >>
;   comparisons: == != <> < <= > >= (also =)
;   bitwise: & ^ |
;   logical: && ^^ ||
;   ternary: ?:
PREC:
        dw      (8+7)           ; Parenthesis have highest precedence
        dw      -1              ; unary plus/minus
        db      >512            ; high byte
        db      <512            ; low byte
        dw      2 ** 3          ; power
        dw      100 / 10 % 4    ; multiply/divide/mod
        dw      2 + 8 - 1       ; add/subtract
        dw      2 <= 3          ; comparisons
        dw      23H & 0FH       ; bitwise AND
        dw      23H | 0FH ^ 0FFH; bitwise OR/XOR
        dw      !0              ; logical NOT
        dw      1 && 0          ; logical AND
        dw      1 ^^ 0          ; logical XOR
        dw      1 ? 2 : 3       ; ternary

; Logical operators treat any non-zero value as TRUE and return 1 (true) or 0 (false).
        db      !0              ; TRUE
        db      !1              ; FALSE
        db      2 && 3          ; TRUE
        db      0 || 4          ; TRUE
        db      2 ^^ 3          ; FALSE

; Address
; The $ symbol is used in an expression to represent the current address.
; This is useful for calculating the size of objects
hello:  db  "Hello, world"
strLen  equ     $ - hello       ; Length of the string

jump_tab:                       ; Jump table.  Each entry is 3 octets.
        db      'a'             ; ADD command
        dw      0100h           ; Handler address
        db      'e'             ; EXAMINE command
        dw      0142h
        db      'p'             ; PRINT command
        dw      0220h
        db      's'             ; STEP command
        dw      0334h
        db      'x'             ; EXIT command
        dw      0434h
entries equ     ($-jump_tab) / 3 ; Number of entries in the table


; It is legal, though probably not wise, to have a label with the same name
; as a register.  This looks confusing, but it will work.  All of the examples
; below load register pair HL with the address of the word at label "SP".  None
; of these have any relation to the SP register.
; Labels that match registers are not permitted in Intel85, but this change was
; added to support some code that was developed with a different assembler.
SP:     dw      256             ; define a word at location named SP
        LXI     H,SP            ; laod address of "SP" word into HL pair
        LXI     H,SP+0
        LXI     H,0+SP


; Conditional directives
YES         EQU 1
TRUE        EQU 0ffffh
NO          EQU 0
FALSE       EQU 0

; simple .if/.else conditional
        .if TRUE                        ; match - all code in this block is included
                org 4000h               ; all labels, directives, and code included
EX1AVAR         equ 1234h
EX1ADATA:       dw  EX1AVAR
EX1A:           mov a,b

        .else                           ; skip - no code in this block is included
                org 8000h               ; all labels, directives, and code skipped
EX1BVAR         equ 5678h
EX1BDATA:       dw  EX1VAR
EX1B:           mov a,c
        .endif                          ; END conditional block


; .if/.elseif/.else
                org 1000h
        .if YES == NO                   ; skip FALSE
                mov a,b
        .elseif !TRUE                   ; skip FALSE
                mov a,c
        .elseif !FALSE                  ; match TRUE
                mov a,d
        .elseif TRUE                    ; skip - already matched a previous block
                mov a,e
        .else                           ; skip - already matched a previous block
                mov a,h
        .endif


; TRUE/FALSE values in conditionals
; Any non-zero value is TRUE, zero is FALSE.
        .if 0                           ; FALSE
                adi 11h
        .elseif 4                       ; TRUE
                adi 22h
        .elseif 4-4                     ; skipped (previous branch matched)
                adi 33h
        .endif


; nested conditionals
                org 0f000h
        .if FALSE                       ; level 1 - skip
          .if 0 != 1                    ; level 2 - skip
LABEL1:         ori 03h                 ; label and code skipped
                jmp 45
          .elseif FALSE                 ; level 2 - skip
LABEL1:         ori 30h                 ; label and code included
                jmp 67
          .else                         ; level 2 - skip
                jmp 12
          .endif                        ; end level 2

        .elseif YES                     ; level 1 - match
                jmp 5555
          .if !FALSE                    ; level 2 - match
                jmp 6666
            .if 0                       ; level 3 - skip
                jmp 2222
            .else                       ; level 3 match
                mov c,a
                jmp 3333
            .endif                      ; end level 3
                mvi a,11h               ; included in level 2 match
                mvi b,22h               ; included in level 2 match

          .else                         ; level 2 - skip
                jmp 4444
          .endif                        ; end level 2
                mvi a,66h               ; included in level 1 match
                mvi b,77h
        .else                           ; level 1 - skip
                mvi a,66h
                mvi b,77h
                jmp 12                  ; skipped from level 1 .else
        .endif                          ; end level 1


; Labels and conditionals
; Any of the conditional directives (.if/.elseif/.else/.endif) can have a label.
; This label will be included in the symbol table as long as the directive is
; processed.  It does not matter if the directive evaluates to TRUE.
; A label will not be included in the symbol table if the directive is nested
; within another .if block that is false because the nested directive is not
; evaluated in that case.
IFLAB:  .if TRUE                        ; label included
                jmp 6666
ELSELAB:.else                           ; label included
                mov c,a
NOLAB1:         jmp 3333                ; labels ignored because they are nested in
NOLAB2:   .if YES                       ; the .else above that did not match
NOLAB3:         jmp 1111
NOLAB4:   .endif
ENDLAB: .endif                          ; label included
