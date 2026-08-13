; The assembler runs two passes over the source file.  The first pass builds
; the symbol table and the second emits the hex file and list file.
;
; Note that the assembler is case-insensitive for instructions, directives,
; register names, and labels.  The labels "label2" and "LABEL2" are the same,
; and an error would be thrown if both were defined.
        .cpu 65c02
 
; The .org directive sets the current address for assembly.  A file can
; contain more than one .org.  The assembler does not detect overlapping
; .org directives and will silently overwrite output in that case.
        .org     RAMST + 02000h


; Column one must contain whitespace or a label or a comment start.
; Labels can be up to 32 characters and must start with an alpha.  
; Numbers and the underscore character are permitted in a label.
; Labels can end with a colon, but the colon is optional.
LABEL1 .byte      123             ; labels can be on the same line as code
LABEL2
        pla                     ; or the label can be before the code
NOSPACE pha                     ; a space is not required after a label
VeryVeryVeryLongLabel
    .byte  "Labels can be up to 32 characters && must start with an alpha."

; Expression operators are symbolic, so names like HIGH, LOW, AND, or OR can be
; used as labels if desired.

; The .const directive is used to define a constant value.  It does not emit
; any code.  The label colon is optional for .const, and the label must be
; followed by whitespace.
RAMST   .const     00000h          
ROMST   .const     0c000h         

; Constants can be decimal, binary, hex, or octal.  Binary is indicated with a
; trailing 'b' hex with a trailing 'h', and octal with a trailing 'o' or 'q'
; character.  Decimal constants can have a trailing 'd' character, but it is
; not required.  All constants must start with a numeric character, so hex 
; values starting with A-F must have a leading zero.
num0    .const     55              ; Decimal constant
num1    .const     55D             ; Decimal constant with optional suffix
num3    .const     0a6h            ; Hex with leading zero
num4    .const     1AB4H           ; Hex without leading zero
num5    .const     01100101b       ; Binary
num6    .const     1357o           ; Octal with trailing 'o'
num7    .const     1011Q           ; Octal with trailing 'q'

; The .byte directive places one or more bytes of data in the output.
d1     .byte      5               ; single byte
d2     .byte      12h,34h,56h,78h ; multiple bytes
d3     .byte      5, 02Ah, 1011B  ; Mixed decimal, hex, and binary

; The .byte directive can also be used with strings.  Each octet in the string
; generates one octet of output.  Strings and numeric constants can be mixed
; in a single directive.
str1   .byte      'T'             ; Single character constant
str2   .byte      "Welcome"       ; String constant
str3   .byte      "red","green"   ; Multiple strings
str4   .byte      3,"red",4,"blue"; Mixed strings and numerics

; Note that a single character string can also be used anywhere a numeric
; would be allowed.  It evaluates to the ASCII value of the single character.
        lda #65                 ; Move the letter 'A' into the accumulator.
        lda #041H               ; Move the letter 'A' into the accumulator.
        lda #'A'                ; Move the letter 'A' into the accumulator.

; A two character string can also be used anywhere a numeric would be allowed.
; It evaluates to the 16-bit value of ASCII value of the two characters, where
; the MSB is the first char and the LSB is the second.
        .word 04142H            ; All of these evaluate to 4142H
        .word 'AB'              ; All of these evaluate to 4142H
        .word "AB"              ; All of these evaluate to 4142H
        .word 'A'*256 | 'B'     ; All of these evaluate to 4142H


; Some common C-style string escapes are supported: CR, LF, tab, NULL, and
; hex value.  Hex escapes can use upper or lower case and must be 2 digits.
    .byte  "\r\n\t\x2a\x2B\0"

; The backslash can also be used to escape quotes or the backslash character
; itself.  Embedded quotes can also be handled by placing double quotes
; inside single quotes or single quotes inside double quotes.
    .byte  '\\'                    ; Backslash character.
    .byte  '\''                    ; Single quote character.
    .byte  "'"                     ; Same string using double quotes.
    .byte  "A \"quoted\" string"   ; Quotes within quotes.
    .byte  'A "quoted" string'     ; Same string using single quotes.
    .byte  "This isn't bad"        ; Single quote in double quotes.
    
; The following .byte directives are all equivalent  
CR  .const 13
LF  .const '\n'
    .byte  "ABC123\r\n"
    .byte  "ABC123",CR,LF
    .byte  "ABC123",13,10
    .byte  'A','B','C','1','2','3','\r','\n'
    .byte  "ABC",31h,32h,"3",'\r',LF

; The .word directive stores one or more 16 bit values.
words1 .word      0203h           ; One word value in hex
words2 .word      num4            ; One word value in decimal
words3 .word      1010101001010101B ; One word value in binary
words4 .word      02h, 03h        ; Two word values
words5 .word      02h, 03, 04ffh  ; Three word values

; Note that .word stores the two octet values in 6502 little-endian order, so
; the following two declarations are equivalent:
        .word      01234h
        .byte      034h, 012h

; The .ds directive reserves space, but does not generate any output.  It
; simply advances the target address for the next code or data.
StrSize .const     32
buffer .ds  StrSize * 4     ; Reserve space for 4 strings

; Expressions can be used in place of any numeric constant.
t      .word      1024 * 3
        .word 32 * 4 + 2
        .word str4 + 5
        .word buffer + StrSize
        .byte      7+7*7+7/(7+7-7)
        lda #'A' | 020H
        lda #'a' & 11011111b

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
PREC
        .word      (8+7)           ; Parenthesis have highest precedence
        .word      -1              ; unary plus/minus
        .byte      >512            ; high byte
        .byte      <512            ; low byte
        .word      2 ** 3          ; power
        .word      100 / 10 % 4    ; multiply/divide/mod
        .word      2 + 8 - 1       ; add/subtract
        .word      2 <= 3          ; comparisons
        .word      23H & 0FH       ; bitwise AND
        .word      23H | 0FH ^ 0FFH; bitwise OR/XOR
        .word      !0              ; logical NOT
        .word      1 && 0          ; logical AND
        .word      1 ^^ 0          ; logical XOR
        .word      1 ? 2 : 3       ; ternary

; Logical operators treat any non-zero value as TRUE and return 1 (true) or 0 (false).
        .byte      !0              ; TRUE
        .byte      !1              ; FALSE
        .byte      2 && 3          ; TRUE
        .byte      0 || 4          ; TRUE
        .byte      2 ^^ 3          ; FALSE

; Address
; The $ symbol is used in an expression to represent the current address.
; This is useful for calculating the size of objects
hello  .byte  "Hello, world"
strLen  .const     $ - hello       ; Length of the string

jump_tab                       ; Jump table.  Each entry is 3 octets.
        .byte      'a'             ; ADD command
        .word      0100h           ; Handler address
        .byte      'e'             ; EXAMINE command
        .word      0142h
        .byte      'p'             ; PRINT command
        .word      0220h
        .byte      's'             ; STEP command
        .word      0334h
        .byte      'x'             ; EXIT command
        .word      0434h
entries .const     ($-jump_tab) / 3 ; Number of entries in the table


; It is legal, though probably not wise, to have a label with the same name
; as a register.  This looks confusing, but it will work.  All of the examples
; below emit the address of the word at label "SP". None of these have any
; relation to the 65C02 stack pointer.
SP     .word      256             ; define a word at location named SP
        .word SP                ; store address of "SP" word
        .word SP+0
        .word 0+SP


; Conditional directives
YES         .const 1
TRUE        .const 0ffffh
NO          .const 0
FALSE       .const 0

; simple .if/.else conditional
        .if TRUE                        ; match - all code in this block is included
                .org 4000h               ; all labels, directives, and code included
EX1AVAR         .const 1234h
EX1ADATA       .word  EX1AVAR
EX1A           tax

        .else                           ; skip - no code in this block is included
                .org 8000h               ; all labels, directives, and code skipped
EX1BVAR         .const 5678h
EX1BDATA       .word  EX1VAR
EX1B           tax
        .endif                          ; END conditional block


; .if/.elseif/.else
                .org 1000h
        .if YES == NO                   ; skip FALSE
                tax
        .elseif !TRUE                   ; skip FALSE
                tax
        .elseif !FALSE                  ; match TRUE
                tax
        .elseif TRUE                    ; skip - already matched a previous block
                tax
        .else                           ; skip - already matched a previous block
                tax
        .endif


; TRUE/FALSE values in conditionals
; Any non-zero value is TRUE, zero is FALSE.
        .if 0                           ; FALSE
                adc #11h
        .elseif 4                       ; TRUE
                adc #22h
        .elseif 4-4                     ; skipped (previous branch matched)
                adc #33h
        .endif


; nested conditionals
                .org 0f000h
        .if FALSE                       ; level 1 - skip
          .if 0 != 1                    ; level 2 - skip
LABEL1         ora #03h                ; label and code skipped
                jmp 45
          .elseif FALSE                 ; level 2 - skip
LABEL1         ora #30h                ; label and code included
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
                tax
                jmp 3333
            .endif                      ; end level 3
                lda #11h                ; included in level 2 match
                lda #22h                ; included in level 2 match

          .else                         ; level 2 - skip
                jmp 4444
          .endif                        ; end level 2
                lda #66h                ; included in level 1 match
                lda #77h
        .else                           ; level 1 - skip
                lda #66h
                lda #77h
                jmp 12                  ; skipped from level 1 .else
        .endif                          ; end level 1


; Labels and conditionals
; Any of the conditional directives (.if/.elseif/.else/.endif) can have a label.
; This label will be included in the symbol table as long as the directive is
; processed.  It does not matter if the directive evaluates to TRUE.
; A label will not be included in the symbol table if the directive is nested
; within another .if block that is false because the nested directive is not
; evaluated in that case.
IFLAB  .if TRUE                        ; label included
                jmp 6666
ELSELAB .else                           ; label included
                tax
NOLAB1         jmp 3333                ; labels ignored because they are nested in
NOLAB2   .if YES                       ; the .else above that did not match
NOLAB3         jmp 1111
NOLAB4   .endif
ENDLAB .endif                          ; label included
