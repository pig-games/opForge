; Test-only envelope assertions; injected into a disposable source snapshot.
; @opforge-owner: tests.amigaos.binary_fragment_probe
	.module tests.amigaos.binary_fragment_probe
	.cpu 68020
	.use experimental.amigaos.binary_fragment as fragment
	.use experimental.amigaos.binary_fragment_sites as sites
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as source
	.include "memory_telemetry.i"
PROBE_STAGE	.macro value
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l .value, ProbeStage
	move.w (sp)+, ccr
.endif
.endif
.endmacro
PROBE_CASE_STAGE	.macro remaining
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #54, d0
	sub.l .remaining, d0
	move.l d0, ProbeStage
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
PROBE_FAILURE	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d2-d3, -(sp)
	cmpi.l #80, ProbeStage
	blo.w otherFailure
	cmpi.l #85, ProbeStage
	bhi.w otherFailure
	; LastFailCapacity=count; LastFailUsed=long ID, or guard ID at84/85.
	move.l State+fragment.State.SiteCount, d2
	move.l State+fragment.State.Sites+sites.Site.Symbol, d3
	cmpi.l #84, ProbeStage
	blo.w otherFailure
	move.l State+fragment.State.Sites+sites.SITE_BYTES+sites.Site.Symbol, d3
otherFailure
	.MEMORY_FAILURE #0, ProbeStage, d2, d3
	movem.l (sp)+, d2-d3
	move.w (sp)+, ccr
.endif
.endif
.endmacro
	.pub
	.section code, kind=code
; A0=Frame; D0/CCR=status, all other registers preserved.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	lea State, a0
	movea.l a5, a1
	move.l #$13579bdf, d2
	movem.l d1-d7/a0-a6, Before
	.PROBE_STAGE #1
	jsr fragment.prepare
	move.w ccr, Flags
	movem.l d1-d7/a0-a6, After
	tst.l d0
	bne.w bad
	btst #2, Flags+1
	beq.w bad
	.PROBE_STAGE #2
	lea Before, a2
	lea After, a3
	moveq #13, d7
preservedValues
	cmpm.l (a2)+, (a3)+
	bne.w bad
	dbra d7, preservedValues
	.PROBE_STAGE #3
	; Occupied setup preserves every owned pointer and descriptor byte.
	lea State, a2
	lea OwnedBefore, a3
	moveq #fragment.STATE_BYTES/4-1, d7
saveOwner
	move.l (a2)+, (a3)+
	dbra d7, saveOwner
	jsr fragment.prepare
	beq.w bad
	lea State, a2
	lea OwnedBefore, a3
	moveq #fragment.STATE_BYTES/4-1, d7
checkOwner
	cmpm.l (a2)+, (a3)+
	bne.w bad
	dbra d7, checkOwner
	.PROBE_STAGE #4
	lea State, a6
	move.l fragment.State.Backing+memory.Block.Used(a6), d0
	cmp.l assembly.Frame.RecordBytes(a5), d0
	bne.w bad
	movea.l fragment.State.Backing+memory.Block.Pointer(a6), a1
	cmpa.l assembly.Frame.Records(a5), a1
	beq.w bad
	lea fragment.State.Fragment(a6), a0
	jsr fragment.resolve
	bne.w bad
	tst.l d1
	beq.w bad
	.PROBE_STAGE #5
	lea InvalidDescriptorCases, a2
	moveq #4, d7
; Stages50..54 identify Owner0,Identity0,Bytes0,BytesMax,StartMax.
invalidDescriptor
	.PROBE_CASE_STAGE d7
	move.l (a2)+, d2
	move.l (a2)+, d3
	bsr.w rejectMalformed
	bne.w bad
	dbra d7, invalidDescriptor
	.PROBE_STAGE #6
	; A null backing cannot publish either output.
	suba.l a1, a1
	move.l fragment.State.Backing+memory.Block.Used(a6), d0
	jsr fragment.resolve
	beq.w bad
	tst.l d1
	bne.w bad
	move.l a1, d0
	bne.w bad
	.PROBE_STAGE #7
	; Each mapped record is an exact copy; declarations retain numeric identity.
	movea.l fragment.State.Maps+memory.Block.Pointer(a6), a2
	move.l fragment.State.MapCount(a6), d7
	beq.w bad
maps
	move.l fragment.Record.Local(a2), d4
	add.l fragment.State.Fragment+fragment.Descriptor.Start(a6), d4
	movea.l assembly.Frame.Records(a5), a0
	adda.l d4, a0
	movea.l fragment.State.Backing+memory.Block.Pointer(a6), a1
	adda.l d4, a1
	move.l fragment.Record.Bytes(a2), d5
	beq.w bad
	move.w source.RecordHeader.Line(a0), d0
	cmp.w fragment.Record.Line(a2), d0
	bne.w bad
	cmpi.w #fragment.NO_ID, fragment.Record.Declaration(a2)
	beq.w bytes
	move.w source.DeclarationRecord.Name+source.NameToken.Id(a0), d0
	cmp.w fragment.Record.Declaration(a2), d0
	bne.w bad
bytes
	cmpm.b (a0)+, (a1)+
	bne.w bad
	subq.l #1, d5
	bne.w bytes
	adda.w #fragment.RECORD_BYTES, a2
	subq.l #1, d7
	bne.w maps
	.PROBE_STAGE #80
	cmpi.l #sites.SITE_COUNT, fragment.State.SiteCount(a6)
	bne.w bad
	.PROBE_STAGE #81
	cmpi.l #sites.LONG_REFERENCE, fragment.State.Sites+sites.Site.Role(a6)
	bne.w bad
	.PROBE_STAGE #82
	cmpi.l #sites.FORWARD_GUARD, fragment.State.Sites+sites.SITE_BYTES+sites.Site.Role(a6)
	bne.w bad
	.PROBE_STAGE #83
	move.l fragment.State.Sites+sites.Site.Symbol(a6), d0
	cmpi.l #sites.NO_SYMBOL, d0
	beq.w bad
	.PROBE_STAGE #84
	move.l fragment.State.Sites+sites.SITE_BYTES+sites.Site.Symbol(a6), d1
	cmpi.l #sites.NO_SYMBOL, d1
	beq.w bad
	.PROBE_STAGE #85
	cmp.l d0, d1
	beq.w bad
	move.l fragment.State.Fragment+fragment.Descriptor.Identity(a6), d6
	lea State, a0
	moveq #0, d0
	jsr fragment.release
	bne.w bad
	.PROBE_STAGE #9
	; Empty backing setup must fail without publishing any owned state.
	move.l assembly.Frame.RecordBytes(a5), d7
	clr.l assembly.Frame.RecordBytes(a5)
	movea.l a5, a1
	jsr fragment.prepare
	move.l d7, assembly.Frame.RecordBytes(a5)
	tst.l d0
	beq.w bad
	tst.l fragment.State.Backing+memory.Block.Pointer(a6)
	bne.w bad
	tst.l fragment.State.Maps+memory.Block.Pointer(a6)
	bne.w bad
	tst.l fragment.State.Index+memory.Block.Pointer(a6)
	bne.w bad
	.PROBE_STAGE #10
	jsr fragment.prepare
	bne.w bad
	cmp.l fragment.State.Fragment+fragment.Descriptor.Identity(a6), d6
	bne.w bad
	moveq #0, d0
	jsr fragment.release
	bne.w bad
	.PROBE_STAGE #11
	; A fragment driver cannot replace a frozen active source session.
	movea.l a5, a0
	jsr assembly.beginSizing
	bne.w bad
	.PROBE_STAGE #12
	move.l assembly.Frame.Records(a5), d6
	jsr fragment.run
	tst.l d0
	beq.w bad
	cmp.l assembly.Frame.Records(a5), d6
	bne.w bad
	.PROBE_STAGE #13
	movea.l a5, a0
	jsr assembly.release
	bne.w bad
	.PROBE_STAGE #14
	movea.l a5, a0
	jsr fragment.run
	bne.w bad
	bra.w done
bad
	.PROBE_FAILURE
	lea State, a0
	jsr fragment.release
	movea.l a5, a0
	jsr assembly.release
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend
	.priv
; D2=descriptor field offset,D3=invalid value; retain the real descriptor.
rejectMalformed	.block
	movem.l d2-d4/a0-a2, -(sp)
	lea 0(a0, d2.l), a2
	move.l (a2), d4
	move.l d3, (a2)
	movea.l fragment.State.Backing+memory.Block.Pointer(a6), a1
	move.l fragment.State.Backing+memory.Block.Used(a6), d0
	jsr fragment.resolve
	move.l d4, (a2)
	tst.l d0
	beq.w bad
	tst.l d1
	bne.w bad
	move.l a1, d0
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d4/a0-a2
	tst.l d0
	rts
	.bend
InvalidDescriptorCases
	.long fragment.Descriptor.Owner, 0
	.long fragment.Descriptor.Identity, 0
	.long fragment.Descriptor.Bytes, 0
	.long fragment.Descriptor.Bytes, $ffffffff
	.long fragment.Descriptor.Start, $ffffffff
	.endsection
	.section bss, kind=bss
	.priv
State	.res byte, fragment.STATE_BYTES
Before	.res long, 14
After	.res long, 14
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
ProbeStage	.res long, 1
.endif
.endif
Flags	.res word, 1
OwnedBefore	.res byte, fragment.STATE_BYTES
	.endsection
	.endmodule
