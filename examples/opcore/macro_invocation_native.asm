; Native macro invocation subset: no segment or statement definitions.

	.cpu 65c02
	.org $0800

COPY .macro src, dst
	lda .src
	sta .dst
.endmacro

PAIR .macro a, b=2
	.byte .a, .b
.endmacro

TEXT .macro msg
	.byte @1
	.word .@
.endmacro

LOCAL .macro
local	.const 9
.endmacro

	.COPY $12, $34
	.PAIR 1
	.TEXT 1+2
foo	.LOCAL
	.word foo.local
	.end
