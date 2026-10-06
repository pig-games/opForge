; Immutable counts can be defined after the loop, including dependency chains.
.for count
    .byte $12
.endfor

count = base + 1
base .const 1
