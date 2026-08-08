        .word TAB2-1
        nop	
        JMP  EXEC

EXEC   nop
CHKIO  rts

TAB2   .byte      'abcd'

