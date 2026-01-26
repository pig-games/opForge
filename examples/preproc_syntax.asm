; Preprocessor directive and macro coverage
; This file is preprocessed first, so we can safely use .define/.ifdef/.include.

.define VAL 7
.define ADD(a,b) (a + b)
.define TWICE(x) (x + x)

.ifdef VAL
        db ADD(1,2)
.else
        db 0
.endif

.ifndef UNKNOWN
        db TWICE(3)
.else
        db 0
.endif

.include "preproc_syntax.inc"
