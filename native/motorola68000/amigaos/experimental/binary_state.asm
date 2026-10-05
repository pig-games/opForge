; Numeric runtime state lowered from the selected package STVM profile.
; @opforge-owner: experimental.amigaos.binary_state
	.module experimental.amigaos.binary_state
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.pub
HEADER_BYTES = 20
KEY_LIMIT = 255
DIRECTIVE_BYTES = 12
ARGUMENT_BYTES = 12
GUARD_BYTES = 8
CLAUSE_BYTES = 12
GUARD_ALLOWED = 0
GUARD_REFUSAL = 1
GUARD_MISMATCH = 2
GUARD_INVALID = 3
Header	.struct
Keys	.word ?
Directives	.word ?
Guards	.word ?
Reserved	.word ?
Defaults	.long ?
DirectiveRows	.long ?
GuardRows	.long ?
	.endstruct
Directive	.struct
Head	.word ?
Key	.word ?
Arguments	.word ?
Reserved	.word ?
Rows	.long ?
	.endstruct
Argument	.struct
Kind	.word ?
Allowed	.word ?
Match	.long ?
Value	.long ?
	.endstruct
Guard	.struct
Clauses	.word ?
Reserved	.word ?
Rows	.long ?
	.endstruct
Clause	.struct
Key	.word ?
Values	.word ?
Failure	.word ?  ; zero mismatches; one records a diagnostic refusal
Reserved	.word ?
Rows	.long ?
	.endstruct
	.section bss, kind=bss
	.priv
Values	.res long, KEY_LIMIT
	.endsection
	.section code, kind=code
	.pub
; A2=readable package with validated outer header. D0/CCR=0 valid, 1 invalid.
; Preserves other registers. Validate every numeric table before execution.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a2, a5
	bsr.w locate
	bne.w bad
	move.l a4, d0
	beq.w rows
	cmpi.w #KEY_LIMIT, Header.Keys(a4)
	bhi.w bad
	tst.w Header.Reserved(a4)
	bne.w bad
	moveq #0, d1
	move.w Header.Keys(a4), d1
	lsl.l #2, d1
	move.l Header.Defaults(a4), d0
	bsr.w span
	bne.w bad
	moveq #0, d1
	move.w Header.Directives(a4), d1
	mulu.w #DIRECTIVE_BYTES, d1
	move.l Header.DirectiveRows(a4), d0
	bsr.w span
	bne.w bad
	movea.l a3, a0
	moveq #0, d7
	move.w Header.Directives(a4), d7
directives
	tst.w d7
	beq.w guards
	move.w Directive.Head(a0), d0
	cmp.w package.Header.NameCount(a5), d0
	bhs.w bad
	move.w Directive.Key(a0), d0
	cmp.w Header.Keys(a4), d0
	bhs.w bad
	tst.w Directive.Reserved(a0)
	bne.w bad
	moveq #0, d6
	move.w Directive.Arguments(a0), d6
	beq.w bad
	move.l d6, d1
	mulu.w #ARGUMENT_BYTES, d1
	move.l Directive.Rows(a0), d0
	bsr.w span
	bne.w bad
arguments
	tst.w Argument.Kind(a3)
	bne.w bad
	cmpi.w #1, Argument.Allowed(a3)
	bhi.w bad
	move.l Argument.Match(a3), d0
	moveq #0, d1
	move.w package.Header.NameCount(a5), d1
	cmp.l d1, d0
	bhs.w bad
	adda.w #ARGUMENT_BYTES, a3
	subq.w #1, d6
	bne.w arguments
	adda.w #DIRECTIVE_BYTES, a0
	subq.w #1, d7
	bra.w directives
guards
	moveq #0, d1
	move.w Header.Guards(a4), d1
	mulu.w #GUARD_BYTES, d1
	move.l Header.GuardRows(a4), d0
	bsr.w span
	bne.w bad
	movea.l a3, a0
	moveq #0, d7
	move.w Header.Guards(a4), d7
guardLoop
	tst.w d7
	beq.w rows
	tst.w Guard.Reserved(a0)
	bne.w bad
	moveq #0, d5
	move.w Guard.Clauses(a0), d5
	beq.w bad
	move.l d5, d1
	mulu.w #CLAUSE_BYTES, d1
	move.l Guard.Rows(a0), d0
	bsr.w span
	bne.w bad
	movea.l a3, a1
clauseLoop
	tst.w Clause.Reserved(a1)
	bne.w bad
	cmpi.w #1, Clause.Failure(a1)
	bhi.w bad
	move.w Clause.Key(a1), d0
	cmp.w Header.Keys(a4), d0
	bhs.w bad
	moveq #0, d1
	move.w Clause.Values(a1), d1
	beq.w bad
	lsl.l #2, d1
	move.l Clause.Rows(a1), d0
	bsr.w span
	bne.w bad
	adda.w #CLAUSE_BYTES, a1
	subq.w #1, d5
	bne.w clauseLoop
	adda.w #GUARD_BYTES, a0
	subq.w #1, d7
	bra.w guardLoop
rows
	moveq #0, d6
	move.l a4, d0
	beq.w guardCountReady
	move.w Header.Guards(a4), d6
guardCountReady
	move.l package.Header.RowCount(a5), d7
	cmpi.l #65535, d7
	bhi.w bad
	move.l d7, d1
	mulu.w #package.ROW_BYTES, d1
	move.l package.Header.Rows(a5), d0
	cmpi.l #package.HEADER_BYTES, d0
	blo.w bad
	add.l d0, d1
	bcs.w bad
	cmp.l package.Header.RuntimeBytes(a5), d1
	bhi.w bad
	lea 0(a5, d0.l), a0
rowLoop
	tst.l d7
	beq.w ok
	cmp.w package.Row.StateGuard(a0), d6
	blo.w bad
	adda.w #package.ROW_BYTES, a0
	subq.l #1, d7
	bra.w rowLoop
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validate

; A2=validated package. Restore defaults for every ordered record sweep.
; D0/CCR=status; other registers preserved. No source or package string lookup.
reset	.block
	movem.l d1-d2/a0-a6, -(sp)
	bsr.w locate
	bne.w bad
	move.l a4, d0
	beq.w ok
	moveq #0, d1
	move.w Header.Keys(a4), d1
	beq.w ok
	movea.l a4, a0
	adda.l Header.Defaults(a4), a0
	lea Values, a1
copy
	move.l (a0)+, (a1)+
	subq.w #1, d1
	bne.w copy
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a6
	tst.l d0
	rts
	.bend  ; reset

; D0.W=head ID,A2=validated package. D0=0 found,A0=directive row; 2 absent.
; Preserves other registers except A0; CCR=D0.
find	.block
	movem.l d1-d3/a3-a6, -(sp)
	move.w d0, d3
	bsr.w locate
	bne.w bad
	move.l a4, d0
	beq.w absent
	moveq #0, d2
	move.w Header.Directives(a4), d2
	movea.l a4, a0
	adda.l Header.DirectiveRows(a4), a0
next
	tst.w d2
	beq.w absent
	cmp.w Directive.Head(a0), d3
	beq.w found
	adda.w #DIRECTIVE_BYTES, a0
	subq.w #1, d2
	bra.w next
found
	moveq #0, d0
	bra.w done
absent
	moveq #2, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a3-a6
	tst.l d0
	rts
	.bend  ; find

; D0.W=head,A0=packed operands,A1=bounded end,A2=validated package.
; D0/CCR=0 applied,1 invalid/illegal,2 absent. All other registers preserved.
; A state change is transactional: validate the whole operand before assignment.
apply	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	bsr.w find
	cmpi.l #2, d0
	beq.w done
	tst.l d0
	bne.w bad
	movea.l a0, a6
	move.l a1, d0
	sub.l a5, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #1, (a5)
	bhi.w bad
	tst.b 3(a5)
	bne.w bad
	moveq #0, d3
	move.w 1(a5), d3
	movea.l a2, a4
	adda.l package.Header.StatePlan(a2), a4
	movea.l a4, a0
	adda.l Directive.Rows(a6), a0
	moveq #0, d2
	move.w Directive.Arguments(a6), d2
argumentLoop
	tst.w d2
	beq.w bad
	cmp.l Argument.Match(a0), d3
	beq.w selected
	adda.w #ARGUMENT_BYTES, a0
	subq.w #1, d2
	bra.w argumentLoop
selected
	tst.w Argument.Allowed(a0)
	beq.w bad
	moveq #0, d1
	move.w Directive.Key(a6), d1
	lsl.l #2, d1
	lea Values, a1
	move.l Argument.Value(a0), 0(a1, d1.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; apply

; D0.W=one-based guard (zero unguarded),A2=validated package.
; D0/CCR=0 allowed,1 diagnostic refusal,2 mismatch,3 invalid; others preserved.
check	.block
	movem.l d1-d4/a0-a6, -(sp)
	moveq #0, d3
	move.w d0, d3
	beq.w ok
	bsr.w locate
	bne.w bad
	move.l a4, d0
	beq.w bad
	cmp.w Header.Guards(a4), d3
	bhi.w bad
	subq.w #1, d3
	mulu.w #GUARD_BYTES, d3
	movea.l a4, a0
	adda.l Header.GuardRows(a4), a0
	adda.l d3, a0
	moveq #0, d4
	move.w Guard.Clauses(a0), d4
	movea.l a4, a5
	adda.l Guard.Rows(a0), a5
checkClause
	moveq #0, d1
	move.w Clause.Key(a5), d1
	lsl.l #2, d1
	lea Values, a1
	move.l 0(a1, d1.l), d3
	moveq #0, d2
	move.w Clause.Values(a5), d2
	movea.l a4, a0
	adda.l Clause.Rows(a5), a0
next
	cmp.l (a0)+, d3
	beq.w matched
	subq.w #1, d2
	bne.w next
	tst.w Clause.Failure(a5)
	bne.w refused
	moveq #GUARD_MISMATCH, d0
	bra.w done
matched
	adda.w #CLAUSE_BYTES, a5
	subq.w #1, d4
	bne.w checkClause
ok
	moveq #GUARD_ALLOWED, d0
	bra.w done
refused
	moveq #GUARD_REFUSAL, d0
	bra.w done
bad
	moveq #GUARD_INVALID, d0
done
	movem.l (sp)+, d1-d4/a0-a6
	tst.l d0
	rts
	.bend  ; check
	.priv

; A2=outer package. A4=plan or zero,A6=end; D0/CCR=status.
; D1/A3 clobbered. The state block is within the retained runtime region.
locate	.block
	move.l package.Header.RuntimeBytes(a2), d0
	cmp.l package.Header.Bytes(a2), d0
	bhi.w bad
	move.l package.Header.StatePlan(a2), d0
	move.l package.Header.StatePlanBytes(a2), d1
	tst.l d0
	bne.w present
	tst.l d1
	bne.w bad
	suba.l a4, a4
	moveq #0, d0
	rts
present
	cmpi.l #package.HEADER_BYTES, d0
	blo.w bad
	btst #0, d0
	bne.w bad
	cmpi.l #HEADER_BYTES, d1
	blo.w bad
	add.l d0, d1
	bcs.w bad
	cmp.l package.Header.RuntimeBytes(a2), d1
	bhi.w bad
	lea 0(a2, d0.l), a4
	lea 0(a2, d1.l), a6
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; locate

; D0=plan offset,D1=length,A4=plan,A6=end. A3=span, D0/CCR=status.
; Preserves D1; rejects wrapped and out-of-region ranges.
span	.block
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	movea.l a4, a3
	adda.l d0, a3
	move.l a3, d0
	cmp.l a4, d0
	blo.w bad
	add.l d1, d0
	bcs.w bad
	cmp.l a6, d0
	bhi.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; span
	.endsection
	.endmodule
