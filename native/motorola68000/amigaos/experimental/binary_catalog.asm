; Bounded, data-driven catalog lookup. Offsets are relative to catalog base.
	.module experimental.amigaos.binary_catalog
	.cpu 68020
	.pub
OK = 0
UNAVAILABLE = 1
INPUT_LIMIT = 255
CASE_DELTA = $20; ASCII lowercase offset from the corresponding uppercase letter
Header	.struct
Bytes	.long ?
Entries	.long ?
Aliases	.long ?
Count	.word ?
AliasCount	.word ?
.endstruct
HEADER_BYTES = Header.AliasCount+2
Entry	.struct
Key	.long ?
KeyBytes	.word ?
Default	.word ?
Cpu	.long ?
CpuBytes	.word ?
DialectBytes	.word ?
Dialect	.long ?
Payload	.long ?
PayloadBytes	.long ?
.endstruct
ENTRY_BYTES = Entry.PayloadBytes+4
Alias	.struct
Name	.long ?
NameBytes	.word ?
CpuBytes	.word ?
Cpu	.long ?
.endstruct
ALIAS_BYTES = Alias.Cpu+4
	.section code, kind=code
; A0=catalog, D0=readable bytes, A1=CPU NUL string, A2=dialect NUL string
; (zero selects default). D0/CCR=status; A1=Entry on success, zero on failure.
; Preserves D1-D7/A0/A2-A6. Inputs are readable CLI buffers of at most 255 bytes.
find	.block
	movem.l d1-d7/a0/a2-a6, -(sp)
	movea.l a0, a4
	move.l d0, d7
	movea.l a1, a5
	movea.l a2, a6
	cmpi.l #HEADER_BYTES, d7
	blo.w bad
	move.l a4, d0
	btst #0, d0
	bne.w bad
	add.l d7, d0
	bcs.w bad
	cmp.l Header.Bytes(a4), d7
	bne.w bad
	move.l Header.Entries(a4), d0
	moveq #0, d1
	move.w Header.Count(a4), d1
	mulu.w #ENTRY_BYTES, d1
	bsr.w table
	bne.w bad
	move.l Header.Aliases(a4), d0
	moveq #0, d1
	move.w Header.AliasCount(a4), d1
	mulu.w #ALIAS_BYTES, d1
	bsr.w table
	bne.w bad
	movea.l a3, a1
	moveq #0, d6
	move.w Header.AliasCount(a4), d6
aliasLoop
	tst.l d6
	beq.w entries
	move.l Alias.Name(a1), d0
	moveq #0, d1
	move.w Alias.NameBytes(a1), d1
	bsr.w string
	bne.w bad
	movea.l a5, a2
	bsr.w equal
	bne.w nextAlias
	move.l Alias.Cpu(a1), d0
	moveq #0, d1
	move.w Alias.CpuBytes(a1), d1
	bsr.w string
	bne.w bad
	; Alias canonical names are length-delimited, so retain their span directly.
	movea.l a3, a5
	move.l d1, d5
	bra.w entryStart
nextAlias
	adda.w #ALIAS_BYTES, a1
	subq.l #1, d6
	bra.w aliasLoop
entries
	moveq #-1, d5
entryStart
	move.l Header.Entries(a4), d0
	lea 0(a4, d0.l), a1
	moveq #0, d6
	move.w Header.Count(a4), d6
entryLoop
	tst.l d6
	beq.w bad
	move.l Entry.Cpu(a1), d0
	moveq #0, d1
	move.w Entry.CpuBytes(a1), d1
	bsr.w string
	bne.w bad
	tst.l d5
	bmi.w cliCpu
	cmp.l d5, d1
	bne.w nextEntry
	movea.l a5, a2
	bsr.w equalSpan
	bra.w cpuCompared
cliCpu
	movea.l a5, a2
	bsr.w equal
cpuCompared
	bne.w nextEntry
	move.l Entry.Dialect(a1), d0
	moveq #0, d1
	move.w Entry.DialectBytes(a1), d1
	bsr.w string
	bne.w bad
	move.l a6, d0
	beq.w default
	movea.l a6, a2
	bsr.w equal
	bne.w nextEntry
	bra.w selected
default
	cmpi.w #1, Entry.Default(a1)
	bne.w nextEntry
selected
	move.l Entry.Key(a1), d0
	moveq #0, d1
	move.w Entry.KeyBytes(a1), d1
	bsr.w string
	bne.w bad
	move.l Entry.Payload(a1), d0
	move.l Entry.PayloadBytes(a1), d1
	bne.w embedded
	tst.l d0
	bne.w bad
	bra.w success
embedded
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	btst #0, d0
	bne.w bad
	bsr.w span
	bne.w bad
success
	moveq #OK, d0
	bra.w done
nextEntry
	adda.w #ENTRY_BYTES, a1
	subq.l #1, d6
	bra.w entryLoop
bad
	suba.l a1, a1
	moveq #UNAVAILABLE, d0
done
	movem.l (sp)+, d1-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; find
	.priv
; D0=offset,D1=length; D0/CCR=status,A3=span. D2 is scratch.
table	.block
	btst #0, d0
	bne.w bad
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	bra.w span
bad
	moveq #UNAVAILABLE, d0
	rts
	.bend  ; table
string	.block
	tst.l d1
	beq.w bad
	cmpi.l #INPUT_LIMIT, d1
	bhi.w bad
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	bsr.w span
	bne.w done
	cmp.l d2, d1  ; string terminator must remain inside the readable catalog
	bhs.w bad
	tst.b 0(a3, d1.l)
	bne.w bad
	moveq #OK, d0
done
	rts
bad
	moveq #UNAVAILABLE, d0
	rts
	.bend  ; string
span	.block
	cmp.l d7, d0
	bhi.w bad
	move.l d7, d2
	sub.l d0, d2
	cmp.l d2, d1
	bhi.w bad
	lea 0(a4, d0.l), a3
	moveq #OK, d0
	rts
bad
	moveq #UNAVAILABLE, d0
	rts
	.bend  ; span
; Compare a bounded catalog span A3/D1 with NUL CLI A2; ASCII case insensitive.
; D0/CCR=0 equal; clobbers D2-D4/A0, preserves A2/A3/D1.
equal	.block
	bsr.w equalSpan
	bne.w done
	movea.l a2, a0
	tst.b 0(a0, d1.l)
	bne.w bad
	moveq #OK, d0
done
	rts
bad
	moveq #UNAVAILABLE, d0
	rts
	.bend  ; equal
; As equal, but A2 is another already validated span of the same length.
equalSpan	.block
	moveq #0, d2
loop
	cmp.l d1, d2
	beq.w good
	moveq #0, d3
	move.b 0(a3, d2.l), d3
	moveq #0, d4
	move.b 0(a2, d2.l), d4
	beq.w bad
	cmpi.b #'A', d3
	blo.w firstFolded
	cmpi.b #'Z', d3
	bhi.w firstFolded
	addi.b #CASE_DELTA, d3
firstFolded
	cmpi.b #'A', d4
	blo.w secondFolded
	cmpi.b #'Z', d4
	bhi.w secondFolded
	addi.b #CASE_DELTA, d4
secondFolded
	cmp.b d3, d4
	bne.w bad
	addq.l #1, d2
	bra.w loop
good
	moveq #OK, d0
	rts
bad
	moveq #UNAVAILABLE, d0
	rts
	.bend  ; equalSpan
	.endsection
	.endmodule
