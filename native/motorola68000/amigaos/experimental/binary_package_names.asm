; Preparation-only dictionary spelling views for numeric package identities.
; @opforge-owner: experimental.amigaos.binary_package_names
	.module experimental.amigaos.binary_package_names
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.pub
	.section code, kind=code
; A0=validated package,D0=numeric name ID. D0/CCR=0 found,1 absent;
; A1/D1=first borrowed unqualified dictionary witness/bytes on success.
; Aliases can share an ID: this is not the original source spelling. Other
; registers kept. Shared keyword callers require consistent witness meanings.
; The dictionary has already passed bounds/alignment validation. This view
; belongs to acquisition; runtime records retain the numeric identity only.
spelling	.block
	movem.l d2-d4/a0/a2, -(sp)
	moveq #0, d4
	move.w package.Header.NameCount(a0), d4
	cmp.l d4, d0
	bhs.w absent
	move.l d0, d4
	movea.l a0, a2
	adda.l package.Header.Dictionary(a0), a2
	move.l package.Header.DictionaryCount(a0), d2
next
	tst.l d2
	beq.w absent
	cmp.w package.DictionaryEntry.Name(a2), d4
	bne.w advance
	tst.b package.DictionaryEntry.Qualifier(a2)
	bne.w advance
	moveq #0, d1
	move.w package.DictionaryEntry.Length(a2), d1
	lea package.DICTIONARY_ENTRY_BYTES(a2), a1
	moveq #0, d0
	bra.w done
advance
	moveq #0, d3
	move.w package.DictionaryEntry.Length(a2), d3
	addi.l #package.DICTIONARY_ENTRY_BYTES+1, d3
	andi.l #$fffffffe, d3
	adda.l d3, a2
	subq.l #1, d2
	bra.w next
absent
	moveq #1, d0
done
	movem.l (sp)+, d2-d4/a0/a2
	tst.l d0
	rts
	.bend  ; spelling
	.endsection
	.endmodule
