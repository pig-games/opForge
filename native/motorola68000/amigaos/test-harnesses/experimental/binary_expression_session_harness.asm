; Two live frontend sessions restore their own package expression grammar.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use experimental.amigaos.binary_frontend as frontend
	.use opasm.amigaos.binary_expression as expression
	.use experimental.amigaos.binary_prepare as prepare
	.use experimental.amigaos.binary_package as package
	.use tkvm.amigaos.runtime as tokens
	.use exprvm.amigaos.runtime as runtime

SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
OPEN = -30
CLOSE = -36
WRITE = -48
MODE_NEWFILE = 1006
RETURN_FAIL = 20

	.section entry, kind=code
	.pub
start	.block
	move.l #RETURN_FAIL, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	bsr.w evaluateSessions
	bne.w closeDos
	bsr.w writeOutput
	bne.w closeDos
	clr.l ReturnCode
closeDos
	movea.l DosBase, a1
	movea.l SYS_BASE.w, a6
	jsr CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	rts
	.bend  ; start

evaluateSessions	.block
	lea ResultBuffer, a5
	lea PackageA, a0
	jsr frontend.scratchSize
	bne.w failed
	cmpi.l #262144, d1
	bhi.w failed
	move.l #PackageA, SessionA+frontend.Frame.Package
	move.l #ScratchA, SessionA+frontend.Frame.Scratch
	move.l #PackageB, SessionB+frontend.Frame.Package
	move.l #ScratchB, SessionB+frontend.Frame.Scratch
	lea SessionA, a0
	jsr frontend.begin
	move.l d0, (a5)+
	bsr.w compileScalar
	move.l d0, (a5)+
	bsr.w prepareScalar
	move.l d0, (a5)+
	lea SessionB, a0
	jsr frontend.begin
	move.l d0, (a5)+
	bsr.w compileScalar
	move.l d0, (a5)+
	lea SessionB, a0
	jsr frontend.finish
	move.l d0, (a5)+
	bsr.w compileLeaf
	move.l d0, (a5)+
	lea SessionA, a0
	jsr frontend.activate
	move.l d0, (a5)+
	bsr.w compileScalar
	move.l d0, (a5)+
	lea SessionA, a0
	jsr frontend.finish
	move.l d0, (a5)+
	bsr.w compileLeaf
	move.l d0, (a5)+
	move.l #44, OutputLength
	moveq #0, d0
	rts
failed
	moveq #1, d0
	rts
	.bend  ; evaluateSessions
compileScalar	.block
	lea Scalar, a0
	lea ScalarEnd, a1
	lea Compiled, a3
	lea Compiled+256, a4
	jsr expression.compile
	tst.l d0
	bne.w done
	cmpa.l a1, a0
	bne.w failed
	lea Compiled, a0
	lea ExpectedCompiled, a1
	moveq #ExpectedCompiledEnd-ExpectedCompiled-1, d2
compare
	move.b (a0)+, d0
	cmp.b (a1)+, d0
	bne.w failed
	dbra d2, compare
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	rts
	.bend  ; compileScalar
; Exercise statement preparation with a live package and a larger symbol ID.
; D0/CCR=status; other registers preserved. Compare the entire prepared record.
prepareScalar	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea PackageA, a0
	move.w package.Header.WordDirective(a0), d0
	lea StatementDirective, a1
	move.w d0, (a1)
	lea ExpectedDirective, a1
	move.w d0, (a1)
	lea Statement, a0
	lea Prepared, a1
	lea PackageA, a2
	jsr prepare.line
	tst.l d0
	bne.w done
	cmpi.l #ExpectedPreparedEnd-ExpectedPrepared, d1
	bne.w failed
	lea Prepared, a0
	lea ExpectedPrepared, a1
	moveq #ExpectedPreparedEnd-ExpectedPrepared-1, d2
compare
	move.b (a0)+, d0
	cmp.b (a1)+, d0
	bne.w failed
	dbra d2, compare
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepareScalar
compileLeaf	.block
	lea Leaf, a0
	lea LeafEnd, a1
	lea Compiled, a3
	lea Compiled+256, a4
	jsr expression.compile
	rts
	.bend  ; compileLeaf
writeOutput	.block
	move.l #OutputPath, d1
	move.l #MODE_NEWFILE, d2
	movea.l DosBase, a6
	jsr OPEN(a6)
	tst.l d0
	beq.s fail
	move.l d0, d4
	move.l d0, d1
	move.l #ResultBuffer, d2
	move.l OutputLength, d3
	jsr WRITE(a6)
	move.l d0, -(sp)
	move.l d4, d1
	jsr CLOSE(a6)
	move.l (sp)+, d0
	cmp.l OutputLength, d0
	bne.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; writeOutput
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
OutputPath	.byte "Work:expression-session.bin", 0
	.align 2
Leaf	.byte tokens.TK_KIND_NUMBER
	.long 7
LeafEnd
; Bound symbol7 plus a folded literal subtree. Only the numeric ID is retained.
Scalar	.byte tokens.TK_KIND_IDENTIFIER
	.word 7
	.byte 0, tokens.TK_KIND_OP_PLUS, tokens.TK_KIND_OPEN_PAREN, tokens.TK_KIND_NUMBER
	.long 3
	.byte tokens.TK_KIND_OP_MULTIPLY, tokens.TK_KIND_NUMBER
	.long 2
	.byte tokens.TK_KIND_CLOSE_PAREN
ScalarEnd
ExpectedCompiled	.byte expression.COMPILED_TAG, 7, runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, 7, 0
	.byte runtime.COMPACT_I8, 6, runtime.COMPACT_ADD, runtime.EXPRVM_V2_OPCODE_END
ExpectedCompiledEnd
	.align 2
Statement	.byte StatementEnd-Statement-1, 0, 0, 0, 7, tokens.TK_KIND_IDENTIFIER
StatementDirective	.word 0
	.byte 0, tokens.TK_KIND_IDENTIFIER
	.word $0307
	.byte 0, tokens.TK_KIND_OP_PLUS, tokens.TK_KIND_OPEN_PAREN, tokens.TK_KIND_NUMBER
	.long 3
	.byte tokens.TK_KIND_OP_MULTIPLY, tokens.TK_KIND_NUMBER
	.long 2
	.byte tokens.TK_KIND_CLOSE_PAREN
StatementEnd
ExpectedPrepared	.byte ExpectedPreparedEnd-ExpectedPrepared-1, 0, 0, 0, 7, tokens.TK_KIND_IDENTIFIER
ExpectedDirective	.word 0
	.byte 0, expression.COMPILED_TAG, 7, runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, 7, 3
	.byte runtime.COMPACT_I8, 6, runtime.COMPACT_ADD, runtime.EXPRVM_V2_OPCODE_END
ExpectedPreparedEnd
	.align 4
PackageA	.incbin "package-a.bin"
	.align 4
PackageB	.incbin "package-b.bin"
	.endsection
	.section bss, kind=bss
DosBase	.res long, 1
ReturnCode	.res long, 1
OutputLength	.res long, 1
ResultBuffer	.res byte, 44
Compiled	.res byte, 256
Prepared	.res byte, 256
	.align 4
SessionA	.res byte, frontend.FRAME_BYTES
SessionB	.res byte, frontend.FRAME_BYTES
ScratchA	.res byte, 262144
ScratchB	.res byte, 262144
	.endsection
	.output "expression-session.hunk", format=hunk, sections=entry,code,data,bss
	.endmodule
