; Boundary span example for .statement signatures

        .cpu 65c02
        .org $2000

.statement sta "[" byte:a ","[{char:reg}]
    ; Body is ignored by assembler for now
.endstatement

        .byte 1
        .end
