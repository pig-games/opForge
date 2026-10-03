; Inspect bounded expression and operand records for section or address-target
; references before an instruction or data value is emitted.
; @opforge-owner: experimental.amigaos.binary_hunk_references
	.module experimental.amigaos.binary_hunk_references
	.cpu 68020
	.use exprvm.amigaos.runtime as runtime
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_dependencies as dependencies
	.use experimental.amigaos.binary_mutable as mutable
	.pub
STATUS_CLEAR = 0
STATUS_SECTION = 1
STATUS_BAD = 2
WRAPPER = $81
	.section code, kind=code

; A0=bounded compiled expression wrapper, A1=end, A2=Context.
; D0/CCR=0 if absolute, 1 if it references a section symbol or current PC,
; 2 if malformed. D1.W is section-reference count, capped at two. A0 advances
; past the wrapper. Other registers are preserved.
expression	.block
	move.l d7, -(sp)
	moveq #0, d7
	bsr.w scanExpression
	move.l (sp)+, d7
	tst.l d0
	rts
	.bend  ; expression

; The same bounded scan, classifying any nonabsolute symbol as a target.
; D0/CCR=0 clear, 1 target, 2 malformed; D1.W=count capped at two.
targets	.block
	move.l d7, -(sp)
	moveq #1, d7
	bsr.w scanExpression
	move.l (sp)+, d7
	tst.l d0
	rts
	.bend  ; targets
; A0=compiled wrapper,A1=end,A2=Context. Classify a bounded postfix
; expression's relocation identity without evaluating its scalar value.
; D0=STATUS_CLEAR (absolute), STATUS_SECTION (one base), STATUS_BAD;
; D1=base ID, or $ffff. Other registers preserved; A0 advances.
; Undefined symbols are provisionally scalar in pass 1. The caller must evaluate
; through ExprVM and honor its unresolved flag before using the value or identity.
; Numeric evaluation remains ExprVM-owned. Only base+absolute,
; absolute+base, base-absolute and unary plus preserve a base. Subtracting
; two symbols with the same nonzero section provenance cancels their bases.
affineTarget	.block
	movem.l d2-d7/a1-a6, -(sp)
	suba.w #runtime.EXPRVM_STACK_CAPACITY*2, sp
	movea.l sp, a5
	moveq #0, d6
	move.l a1, d0
	sub.l a0, d0
	bcs.w bad
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #WRAPPER, (a0)+
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
	beq.w bad
	movea.l a0, a4
	adda.l d1, a4
	cmpa.l a1, a4
	bhi.w bad
next
	cmpa.l a4, a0
	bhs.w bad
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #runtime.EXPRVM_V2_OPCODE_END, d3
	beq.w end
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d3
	beq.w symbol
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d3
	beq.w currentAddress
	cmpi.b #runtime.COMPACT_I8, d3
	beq.w byte
	cmpi.b #runtime.COMPACT_I16, d3
	beq.w word
	cmpi.b #runtime.COMPACT_I32, d3
	beq.w long
	cmpi.b #runtime.COMPACT_U32, d3
	beq.w long
	cmpi.b #runtime.COMPACT_I64, d3
	beq.w pair
	cmpi.b #runtime.COMPACT_NEGATE, d3
	beq.w unaryConstant
	cmpi.b #runtime.COMPACT_ADD, d3
	beq.w add
	cmpi.b #runtime.COMPACT_SUBTRACT, d3
	beq.w subtract
	cmpi.b #runtime.COMPACT_MULTIPLY, d3
	beq.w binaryConstant
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d3
	beq.w unary
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d3
	bne.w bad
	moveq #1, d1
	bsr.w available
	bne.w bad
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #runtime.EXPRVM_BINARY_ADD, d3
	beq.w add
	cmpi.b #runtime.EXPRVM_BINARY_SUBTRACT, d3
	beq.w subtract
	bra.w binaryConstant
currentAddress
	; Flat current-PC values are scalars. Section-relative current-PC
	; provenance is not yet represented and must still fail closed.
	tst.w pkg.Context.Relocatable(a2)
	bne.w bad
	bra.w absolute
byte
	moveq #1, d1
	bra.w literal
word
	moveq #2, d1
	bra.w literal
long
	moveq #4, d1
	bra.w literal
pair
	moveq #8, d1
literal
	bsr.w available
	bne.w bad
	adda.l d1, a0
	moveq #-1, d1
	bra.w push
symbol
	moveq #2, d1
	bsr.w available
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
	moveq #0, d2
	move.b (a0)+, d2
	lsl.w #8, d2
	or.w d2, d1
	cmpi.w #$ffff, d1
	beq.w bad  ; the absolute proof sentinel cannot name a target
	cmp.l pkg.Context.Count(a2), d1
	bhs.w bad
	movea.l pkg.Context.Defined(a2), a3
	move.l a3, d0
	beq.w bad
	cmpi.b #dependencies.ABSOLUTE, 0(a3, d1.l)
	beq.w absolute
	cmpi.b #mutable.DEFINED, 0(a3, d1.l)
	beq.w absolute
	cmpi.b #mutable.SNAPSHOT_ABSOLUTE, 0(a3, d1.l)
	beq.w absolute
	tst.b 0(a3, d1.l)
	bne.w defined
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w bad
	bra.w absolute  ; fixed-width pass-1 fixups defer value and identity together
defined
	; Flat labels are scalars. In Hunk output retain layout-dependent identity,
	; including aliases whose missing section provenance must later reject.
	tst.w pkg.Context.Relocatable(a2)
	bne.w push
absolute
	moveq #-1, d1
push
	cmpi.l #runtime.EXPRVM_STACK_CAPACITY, d6
	bhs.w bad
	move.l d6, d0
	add.l d0, d0
	move.w d1, 0(a5, d0.l)
	addq.l #1, d6
	bra.w next
unary
	moveq #1, d1
	bsr.w available
	bne.w bad
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #runtime.EXPRVM_UNARY_PLUS, d3
	beq.w unaryPlus
unaryConstant
	tst.l d6
	beq.w bad
	move.l d6, d0
	subq.l #1, d0
	add.l d0, d0
	cmpi.w #$ffff, 0(a5, d0.l)
	bne.w bad
	bra.w next
unaryPlus
	tst.l d6
	beq.w bad
	bra.w next
add
	moveq #0, d3
	bra.w binary
subtract
	moveq #1, d3
	bra.w binary
binaryConstant
	moveq #2, d3
binary
	cmpi.l #2, d6
	blo.w bad
	subq.l #1, d6
	moveq #0, d7
	move.l d6, d0
	add.l d0, d0
	move.w 0(a5, d0.l), d7
	subq.l #2, d0
	moveq #0, d5
	move.w 0(a5, d0.l), d5
	cmpi.w #$ffff, d7
	beq.w rightConstant
	cmpi.l #1, d3
	beq.w difference
	tst.l d3
	bne.w bad
	cmpi.w #$ffff, d5
	bne.w bad
	move.w d7, 0(a5, d0.l)
	bra.w next
difference
	; The stack retains symbol IDs, not section IDs: resolve both here so
	; different labels in one section cancel without granting cross-section math.
	cmpi.w #$ffff, d5
	beq.w bad
	movea.l pkg.Context.SectionIds(a2), a3
	move.l a3, d1
	beq.w bad
	moveq #0, d1
	move.b 0(a3, d5.l), d1
	beq.w bad
	cmp.b 0(a3, d7.l), d1
	bne.w bad
	move.w #$ffff, 0(a5, d0.l)
	bra.w next
rightConstant
	cmpi.l #2, d3
	bne.w next
	cmpi.w #$ffff, d5
	bne.w bad
	bra.w next
end
	cmpa.l a4, a0
	bne.w bad
	cmpi.l #1, d6
	bne.w bad
	moveq #0, d1
	move.w (a5), d1
	moveq #STATUS_CLEAR, d0
	cmpi.w #$ffff, d1
	beq.w done
	moveq #STATUS_SECTION, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
	moveq #-1, d1
done
	adda.w #runtime.EXPRVM_STACK_CAPACITY*2, sp
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; affineTarget
	.priv
scanExpression	.block
	movem.l d2-d7/a1-a6, -(sp)
	moveq #0, d6
	move.l a1, d1
	sub.l a0, d1
	bcs.w bad
	cmpi.l #3, d1
	blo.w bad
	cmpi.b #WRAPPER, (a0)+
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
	beq.w bad
	movea.l a0, a4
	adda.l d1, a4
	cmpa.l a1, a4
	bhi.w bad
	moveq #STATUS_CLEAR, d5
next
	cmpa.l a4, a0
	bhs.w bad
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #runtime.EXPRVM_V2_OPCODE_END, d1
	beq.w end
	cmpi.b #runtime.COMPACT_I8, d1
	beq.w byte
	cmpi.b #runtime.COMPACT_I16, d1
	beq.w word
	cmpi.b #runtime.COMPACT_I32, d1
	beq.w long
	cmpi.b #runtime.COMPACT_U32, d1
	beq.w long
	cmpi.b #runtime.COMPACT_I64, d1
	beq.w pair
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d1
	beq.w current
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d1
	beq.w symbol
	cmpi.b #runtime.COMPACT_NEGATE, d1
	beq.w next
	cmpi.b #runtime.COMPACT_ADD, d1
	beq.w next
	cmpi.b #runtime.COMPACT_SUBTRACT, d1
	beq.w next
	cmpi.b #runtime.COMPACT_MULTIPLY, d1
	beq.w next
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d1
	beq.w byte
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d1
	beq.w byte
	bra.w bad
current
	moveq #STATUS_SECTION, d5
	bsr.w countReference
	bra.w next
symbol
	moveq #2, d1
	bsr.w available
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
	moveq #0, d2
	move.b (a0)+, d2
	lsl.w #8, d2
	or.w d2, d1
	bsr.w sectionId
	bne.w bad
	bra.w next
byte
	moveq #1, d1
	bra.w skip
word
	moveq #2, d1
	bra.w skip
long
	moveq #4, d1
	bra.w skip
pair
	moveq #8, d1
skip
	bsr.w available
	bne.w bad
	adda.l d1, a0
	bra.w next
end
	cmpa.l a4, a0
	bne.w bad
	move.l d5, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	move.w d6, d1
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; scanExpression
	.pub

; A0=prepared operand tokens, A1=end, A2=Context. The same statuses as
; expression are returned; D1.W is section-reference count, capped at two.
; A0 advances to the end. Other registers are preserved.
tokens	.block
	movem.l d2-d7/a1-a6, -(sp)
	moveq #0, d7
	movea.l a1, a4
	moveq #STATUS_CLEAR, d5
	moveq #0, d6
next
	cmpa.l a1, a0
	beq.w clear
	bhi.w bad
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #WRAPPER, d1
	beq.w wrapped
	cmpi.b #1, d1
	bls.w name
	cmpi.b #2, d1
	beq.w number
	cmpi.b #3, d1
	beq.w string
	cmpi.b #4, d1
	blo.w bad
	cmpi.b #40, d1
	bhi.w bad
	bra.w next
wrapped
	subq.l #1, a0
	; Cancelled same-section bases need no instruction relocation. Keep the
	; conservative scan for surviving bases and positional/unsupported forms;
	; the package projection still owns their fixup proof or rejection.
	move.l a0, -(sp)
	jsr affineTarget
	tst.l d0
	bne.w wrappedReferences
	addq.l #4, sp
	bra.w next
wrappedReferences
	movea.l (sp)+, a0
	jsr expression
	cmpi.l #STATUS_BAD, d0
	beq.w bad
	tst.l d0
	beq.w next
	moveq #STATUS_SECTION, d5
	add.w d1, d6
	cmpi.w #2, d6
	bls.w next
	moveq #2, d6
	bra.w next
name
	moveq #3, d1
	bsr.w available
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
	lsl.w #8, d1  ; packed source IDs are big-endian; expression IDs are little-endian
	moveq #0, d2
	move.b (a0)+, d2
	or.w d2, d1
	tst.b (a0)+  ; qualifier is not part of the numeric ID
	bsr.w sectionId
	bne.w bad
	bra.w next
number
	moveq #4, d1
	bra.w skipToken
string
	moveq #1, d1
	bsr.w available
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
skipToken
	bsr.w available
	bne.w bad
	adda.l d1, a0
	bra.w next
clear
	move.l d5, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	move.w d6, d1
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; tokens
	.priv

; D1=bytes needed at A0. Uses the current bounded end in A4 for expression
; and A1 for tokens; callers set A4 to their own end before entry.
available	.block
	move.l a4, d0
	sub.l a0, d0
	bcs.w bad
	cmp.l d1, d0
	blo.w bad
	moveq #STATUS_CLEAR, d0
	rts
bad
	moveq #STATUS_BAD, d0
	rts
	.bend  ; available

; D1=numeric symbol ID, A2=Context, D5=accumulated reference status.
sectionId	.block
	cmp.l pkg.Context.Count(a2), d1
	bhs.w bad
	tst.w d7
	beq.w section
	movea.l pkg.Context.Defined(a2), a3
	move.l a3, d0
	beq.w bad
	moveq #0, d0
	move.b 0(a3, d1.l), d0
	beq.w found  ; an unresolved symbol may become a layout label
	cmpi.b #dependencies.ABSOLUTE, d0
	beq.w clear
	cmpi.b #mutable.DEFINED, d0
	beq.w clear
	cmpi.b #mutable.SNAPSHOT_ABSOLUTE, d0
	beq.w clear
	bra.w found
section
	movea.l pkg.Context.SectionIds(a2), a3
	move.l a3, d0
	beq.w bad
	tst.b 0(a3, d1.l)
	beq.w clear
found
	moveq #STATUS_SECTION, d5
	bsr.w countReference
clear
	moveq #STATUS_CLEAR, d0
	rts
bad
	moveq #STATUS_BAD, d0
	rts
	.bend  ; sectionId

; Saturate the reference count: callers only need zero, one, or many.
countReference	.block
	cmpi.w #2, d6
	bhs.w done
	addq.w #1, d6
done
	rts
	.bend  ; countReference
	.endsection
	.endmodule
