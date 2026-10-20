; First preparation completion failure. Imported only under all three gates.
; @opforge-owner: debug.amigaos.binding_diagnostic
	.module debug.amigaos.binding_diagnostic
	.cpu 68020
	.use debug.amigaos.memory_profile as memory_profile
	.use experimental.amigaos.binary_memory as memory
	.pub
CURRENT = 1
IMPORTS = 2
SECTIONS = 3
OUTPUTS = 4
EXPLICIT = 5
UNRESOLVED = 6
VISIBILITY = 7
REMAP = 8
IMPORT_MODULE = 9
SELECTED_MODULE = 10
IMPORT_SELECTION = 11
SELECTED_SELECTION = 12
PROXY = 13
TARGET_DECLARATION = 14
DECLARED_CANONICAL = 15
DECLARED_LEAF = 16
MEMBER_ACQUISITION = 17
BLOCK_INDEX = 18
BLOCK_REFERENCES = 19
BINDING_IDENTITY = 20
VALUE_DECLARED_BIT = 0
IMPORT_PROXY_BIT = 3
ENTRY_CAPACITY = 32; bounded prefix through preparation-only register metadata
; Numeric entry bytes are copied without interpreting binding semantics.
; No preparation owner is imported by this observer.
; Word offsets into the existing owned scope/entry layout. Each diagnostic
; caller emits this descriptor under the same gates, using its actual types.
View	.struct
Count	.word ?
Base	.word ?
Current	.word ?
Entries	.word ?  ; embedded lexical storage state offset
Arena	.word ?  ; same lexical storage state offset
ArenaUsed	.word ?  ; reserved, zero
EntryBytes	.word ?
Name	.word ?
Length	.word ?
Flags	.word ?  ; DECLARED is bit zero in the current shared entry ABI
Owner	.word ?  ; one-based source-entry index, zero when absent
NameReader	.long ?  ; checked name copy supplied by the owner
EntryReader	.long ?  ; checked entry copy supplied by the owner
Target	.word ?  ; absolute canonical value ID within an entry
ModuleFlags	.word ?  ; optional memory.Block offsets within scope, zero absent
ModuleOwners	.word ?
ModuleOrigins	.word ?
.endstruct
VIEW_BYTES = View.ModuleOrigins+2
Snapshot	.struct
Stage	.long ?
Index	.long ?
Related	.long ?
Base	.word ?
Count	.word ?
Current	.long ?
Entry	.res ENTRY_CAPACITY
NameBytes	.long ?
Name	.res 256
ModuleValid	.long ?  ; bit0 source, bit1 resolved target; numeric copies only
SourceModule	.res 3*4  ; flags, declaration owner, reference origin
TargetModule	.res 3*4
.endstruct
SNAPSHOT_BYTES = Snapshot.TargetModule+3*4
PAYLOAD_LONGS = (Snapshot.Name+256-Snapshot.Entry)/4
MODULE_SOURCE_PROGRESS = 459
MODULE_TARGET_PROGRESS = 460
RemapCapture	.struct
Cursor	.long ?  ; offset from packed base, never a retained pointer
Record	.long ?
Bytes	.long ?
Head	.res 32
Near	.res 32
.endstruct
REMAP_CAPTURE_BYTES = RemapCapture.Near+32
REMAP_PROGRESS = 336
RECORD_CONTEXT_PROGRESS = 343
PendingCheck	.struct
Stage	.long ?
Index	.long ?
Related	.long ?
.endstruct
	.section code, kind=code
; No inputs/results. Registers, CCR and stack preserved. Begin one completion.
clear	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea Record, a0
	move.w #SNAPSHOT_BYTES*4/4-1, d0
loop
	clr.l (a0)+
	dbra d0, loop
	lea RemapRecord, a0
	moveq #REMAP_CAPTURE_BYTES/4-1, d0
remapClear
	clr.l (a0)+
	dbra d0, remapClear
	clr.l CaptureStage
	lea Pending, a0
	clr.l PendingCheck.Stage(a0)
	clr.l PendingCheck.Index(a0)
	clr.l PendingCheck.Related(a0)
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; clear
; D0=stage,D1=entry,D2=related. Mark the check about to execute, without
; copying source data. All registers/CCR preserved; fixed twelve-byte storage.
attempt	.block
	move.w ccr, -(sp)
	move.l a0, -(sp)
	lea Record, a0
	tst.l Snapshot.Stage(a0)
	bne.w done
	lea Pending, a0
	move.l d0, PendingCheck.Stage(a0)
	move.l d1, PendingCheck.Index(a0)
	move.l d2, PendingCheck.Related(a0)
done
	movea.l (sp)+, a0
	move.w (sp)+, ccr
	rts
	.bend  ; attempt
; D0=stage,D1=canonical source ID,D2=source base. Mark the canonical target
; after a successful bind, retaining the previous origin index as Related.
; All registers/CCR/stack preserved. The completed binder owns ID validity.
attemptTarget	.block
	move.w ccr, -(sp)
	movem.l d1/a0, -(sp)
	lea Record, a0
	tst.l Snapshot.Stage(a0)
	bne.w done
	lea Pending, a0
	move.l PendingCheck.Index(a0), PendingCheck.Related(a0)
	sub.l d2, d1
	move.l d1, PendingCheck.Index(a0)
	move.l d0, PendingCheck.Stage(a0)
done
	movem.l (sp)+, d1/a0
	move.w (sp)+, ccr
	rts
	.bend  ; attemptTarget
; D0=consumer stage,A0=failed cursor,A1=packed base,A2=exclusive packed limit. Failure-only
; bounded observation. Scan length-prefixed records to find the containing
; record; copy at most 32 bytes from its head and around the cursor. No pointers
; survive, no allocation/IO, and all registers/CCR/stack are preserved.
recordFailure	.block
	move.w ccr, -(sp)
	movem.l d0-d3/a0-a4, -(sp)
	tst.l CaptureStage
	bne.w done
	move.l d0, CaptureStage
	lea RemapRecord, a4
	move.l a0, d0
	sub.l a1, d0
	move.l d0, RemapCapture.Cursor(a4)
	move.l #-1, RemapCapture.Record(a4)
	cmpa.l a1, a0
	blo.w done
	cmpa.l a2, a0
	bhi.w done
	movea.l a1, a3
scan
	cmpa.l a2, a3
	bhs.w near
	moveq #0, d1
	move.b (a3), d1
	addq.l #1, d1
	move.l a2, d2
	sub.l a3, d2
	cmp.l d1, d2
	blo.w near
	move.l a0, d2
	sub.l a3, d2
	cmp.l d1, d2
	bls.w found
	adda.l d1, a3
	bra.w scan
found
	move.l a3, d2
	sub.l a1, d2
	move.l d2, RemapCapture.Record(a4)
	move.l d1, RemapCapture.Bytes(a4)
	cmpi.l #32, d1
	bls.w headLength
	moveq #32, d1
headLength
	lea RemapCapture.Head(a4), a1
	subq.w #1, d1
copyHead
	move.b (a3)+, (a1)+
	dbra d1, copyHead
near
	; Include the consumed opcode before the cursor when within the stream.
	tst.l d0
	beq.w nearLength
	subq.l #1, a0
nearLength
	move.l a2, d1
	sub.l a0, d1
	beq.w done
	cmpi.l #32, d1
	bls.w nearReady
	moveq #32, d1
nearReady
	lea RemapCapture.Near(a4), a1
	subq.w #1, d1
copyNear
	move.b (a0)+, (a1)+
	dbra d1, copyNear
done
	movem.l (sp)+, d0-d3/a0-a4
	move.w (sp)+, ccr
	rts
	.bend  ; recordFailure
; D0=stage,D1=logical record offset,A0=cursor,A1=record,A2=record end.
; Observe only this leased record, then rebase captured numeric offsets. Never
; scan preceding pages or retain the borrowed view. Registers/CCR preserved.
recordViewFailure	.block
	move.w ccr, -(sp)
	movem.l d0-d1/a0, -(sp)
	tst.l CaptureStage
	bne.w done
	bsr.w recordFailure
	lea RemapRecord, a0
	add.l d1, RemapCapture.Cursor(a0)
	cmpi.l #-1, RemapCapture.Record(a0)
	beq.w done
	add.l d1, RemapCapture.Record(a0)
done
	movem.l (sp)+, d0-d1/a0
	move.w (sp)+, ccr
	rts
	.bend  ; recordViewFailure
; D0=status,A0=scope state,A1=View descriptor. Commit the pending check only
; for actual failure. First actual failure wins. Registers/CCR/stack preserved;
; copied payloads are checked by the owner. Sealed payloads may fault from disk.
commit	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	tst.l d0
	beq.w done
	movea.l a0, a5
	movea.l a1, a4
	lea Record, a6
	tst.l Snapshot.Stage(a6)
	bne.w done
	lea Pending, a0
	move.l PendingCheck.Stage(a0), d0
	beq.w done
	move.l d0, Snapshot.Stage(a6)
	move.l PendingCheck.Index(a0), d1
	move.l d1, Snapshot.Index(a6)
	move.l PendingCheck.Related(a0), Snapshot.Related(a6)
	moveq #0, d0
	move.w View.Base(a4), d0
	move.w 0(a5, d0.l), Snapshot.Base(a6)
	move.w View.Count(a4), d0
	moveq #0, d3
	move.w 0(a5, d0.l), d3
	move.w d3, Snapshot.Count(a6)
	move.w View.Current(a4), d0
	moveq #0, d2
	move.w 0(a5, d0.l), d2
	move.l d2, Snapshot.Current(a6)
	cmp.l d3, d1
	bhs.w done
	moveq #0, d6
	move.w View.EntryBytes(a4), d6
	beq.w done
	moveq #0, d0
	move.w View.Name(a4), d0
	addq.l #4, d0
	cmp.l d6, d0
	bhi.w done
	move.w View.Length(a4), d0
	addq.l #2, d0
	cmp.l d6, d0
	bhi.w done
	move.w View.Flags(a4), d0
	addq.l #2, d0
	cmp.l d6, d0
	bhi.w done
	move.w View.Owner(a4), d0
	addq.l #2, d0
	cmp.l d6, d0
	bhi.w done
	move.w View.Target(a4), d0
	addq.l #2, d0
	cmp.l d6, d0
	bhi.w done
	cmpi.l #256, d6
	bhi.w done
	move.l d1, d0
	bsr.w readEntry
	bne.w done
	bsr.w snapshotModules
	bsr.w snapshotEntry
	beq.w done
	cmpi.l #TARGET_DECLARATION, Snapshot.Stage(a6)
	bne.w done
	bsr.w findDeclared
	beq.w done
	move.l Snapshot.Index(a6), Snapshot.Related(a6)
	move.l d1, Snapshot.Index(a6)
	move.l d0, Snapshot.Stage(a6)
	bsr.w snapshotModules
	bsr.w snapshotEntry
	bsr.w correlate
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; commit
	.priv

; Copy source and resolved-target module metadata while the scope is live.
; Optional descriptor blocks are bounded independently; failed copies stay absent.
; No metadata pointer survives commit, and the semantic state is never changed.
snapshotModules	.block
	movem.l d0-d3/a0-a2, -(sp)
	clr.l Snapshot.ModuleValid(a6)
	move.l Snapshot.Index(a6), d1
	lea Snapshot.SourceModule(a6), a0
	bsr.w moduleRow
	bne.w target
	ori.l #1, Snapshot.ModuleValid(a6)
target
	moveq #0, d0
	move.w View.Target(a4), d0
	moveq #0, d1
	move.w 0(a3, d0.l), d1
	sub.w Snapshot.Base(a6), d1
	bcs.w done
	lea Snapshot.TargetModule(a6), a0
	bsr.w moduleRow
	bne.w done
	ori.l #2, Snapshot.ModuleValid(a6)
done
	movem.l (sp)+, d0-d3/a0-a2
	rts
	.bend  ; snapshotModules

; A0=three-long destination,D1=source index,A4=View,A5=scope,A6=Snapshot.
; D0/CCR=status; remaining registers kept. Three checked u16 cells become u32.
moduleRow	.block
	movem.l d1-d4/a0-a2, -(sp)
	moveq #0, d0
	move.w Snapshot.Count(a6), d0
	cmp.l d0, d1
	bhs.w bad
	add.l d1, d1
	lea View.ModuleFlags(a4), a1
	moveq #2, d4
field
	moveq #0, d0
	move.w (a1)+, d0
	beq.w bad
	lea 0(a5, d0.l), a2
	move.l memory.Block.Used(a2), d2
	cmp.l memory.Block.Capacity(a2), d2
	bhi.w bad
	move.l d1, d3
	addq.l #2, d3
	cmp.l d2, d3
	bhi.w bad
	move.l memory.Block.Pointer(a2), d0
	beq.w bad
	btst #0, d0
	bne.w bad
	add.l d0, d2
	bcs.w bad
	movea.l d0, a2
	moveq #0, d0
	move.w 0(a2, d1.l), d0
	move.l d0, (a0)+
	dbra d4, field
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a2
	tst.l d0
	rts
	.bend  ; moduleRow

; A3=bounded entry,A4=View,A5=scope,A6=Snapshot. Replace only the entry/name
; payload. D0/CCR=one for a valid nonempty name, zero otherwise; scratch D2/D3/D6/D7
; and A0-A2. Descriptor fields were checked against EntryBytes by commit.
snapshotEntry	.block
	lea Snapshot.Entry(a6), a2
	moveq #PAYLOAD_LONGS-1, d0
clearPayload
	clr.l (a2)+
	dbra d0, clearPayload
	moveq #0, d6
	move.w View.EntryBytes(a4), d6
	cmpi.l #ENTRY_CAPACITY, d6
	bls.w prefixReady
	moveq #ENTRY_CAPACITY, d6
prefixReady
	movea.l a3, a0
	lea Snapshot.Entry(a6), a2
entryLoop
	move.b (a0)+, (a2)+
	subq.l #1, d6
	bne.w entryLoop
	moveq #0, d0
	move.w View.Name(a4), d0
	move.l 0(a3, d0.l), d2
	move.w View.Length(a4), d0
	moveq #0, d7
	move.w 0(a3, d0.l), d7
	beq.w invalid
	cmpi.l #255, d7
	bhi.w invalid
	bsr.w resolveName
	bne.w invalid
	move.l d7, Snapshot.NameBytes(a6)
	lea Snapshot.Name(a6), a1
nameLoop
	move.b (a0)+, (a1)+
	subq.l #1, d7
	bne.w nameLoop
	moveq #1, d0
	rts
invalid
	moveq #0, d0
	rts
	.bend  ; snapshotEntry

; D0=index,A4=View,A5=scope -> A3=owned entry,D0/CCR=status.
; Other registers preserved. No entry pointer borrows cache storage.
readEntry	.block
	movem.l d1/a0-a2, -(sp)
	move.l d0, d1
	moveq #0, d0
	move.w View.Entries(a4), d0
	lea 0(a5, d0.l), a0
	lea EntryCopy, a1
	movea.l View.EntryReader(a4), a2
	move.l d1, d0
	jsr (a2)
	lea EntryCopy, a3
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend  ; readEntry

; D2=name handle,D7=length,A4=View,A5=scope -> A0=owned spelling.
; D0/CCR=status; other registers preserved. The callback copies bounded bytes.
resolveName	.block
	movem.l d1/a1-a2, -(sp)
	moveq #0, d0
	move.w View.Arena(a4), d0
	lea 0(a5, d0.l), a0
	lea NameCopy, a1
	move.l d2, d0
	move.l d7, d1
	movea.l View.NameReader(a4), a2
	jsr (a2)
	lea NameCopy, a0
	movem.l (sp)+, d1/a1-a2
	tst.l d0
	rts
	.bend  ; resolveName

; A3=primary declaration,A4=View,A5=scope,A6=primary Snapshot. Retain its
; one-based owner and immediate entry neighbors without changing the primary.
; Related points back to the primary declaration; invalid indices emit nothing.
; Scratch views use the same bounded copy routine and no extra dynamic storage.
correlate	.block
	movem.l d4-d5/a3/a6, -(sp)
	move.l Snapshot.Index(a6), d4
	move.l Snapshot.Stage(a6), d5
	moveq #0, d0
	move.w View.Owner(a4), d0
	moveq #0, d1
	move.w 0(a3, d0.l), d1
	beq.w neighbors
	subq.l #1, d1
	lea OwnerRecord, a6
	bsr.w snapshotRelated
neighbors
	move.l d4, d1
	subq.l #1, d1
	lea PreviousRecord, a6
	bsr.w snapshotRelated
	move.l d4, d1
	addq.l #1, d1
	lea NextRecord, a6
	bsr.w snapshotRelated
	movem.l (sp)+, d4-d5/a3/a6
	rts
	.bend  ; correlate

; D1=related index,D4=primary index,D5=stage,A6=correlation Snapshot.
; A4/A5 retain descriptor/scope. Index bounds precede every source entry read.
; D4/D5/A4-A6 preserved; remaining registers scratch.
snapshotRelated	.block
	lea Record, a0
	moveq #0, d0
	move.w Snapshot.Count(a0), d0
	cmp.l d0, d1
	bhs.w done
	move.l d5, Snapshot.Stage(a6)
	move.l d1, Snapshot.Index(a6)
	move.l d4, Snapshot.Related(a6)
	move.l Snapshot.Base(a0), Snapshot.Base(a6)
	move.l Snapshot.Current(a0), Snapshot.Current(a6)
	move.l d1, d0
	bsr.w readEntry
	bne.w done
	bsr.w snapshotEntry
done
	rts
	.bend  ; snapshotRelated

; Find an existing declaration without using the binding hash. Exact folded
; canonical matches take priority over the first folded leaf match. D0/CCR=stage
; 15/16 with D1=index,A3=entry, or zero when absent. The original snapshot name
; remains immutable throughout the bounded scan; sealed reads may fault.
findDeclared	.block
	moveq #0, d4
	moveq #0, d5
	move.w Snapshot.Count(a6), d5
	moveq #-1, d6  ; first declared leaf match, if any
next
	cmp.l d5, d4
	bhs.w finished
	move.l d4, d0
	bsr.w readEntry
	bne.w advance
	move.w View.Flags(a4), d2
	move.w 0(a3, d2.l), d0
	btst #VALUE_DECLARED_BIT, d0
	beq.w advance
	btst #IMPORT_PROXY_BIT, d0
	bne.w advance  ; validated reference proxies are not owner declarations
	move.w View.Length(a4), d2
	moveq #0, d7
	move.w 0(a3, d2.l), d7
	beq.w advance
	cmpi.l #255, d7
	bhi.w advance
	move.w View.Name(a4), d2
	move.l 0(a3, d2.l), d2
	bsr.w resolveName
	bne.w advance
	movea.l a0, a2
	cmp.l Snapshot.NameBytes(a6), d7
	bne.w tryLeaf
	lea Snapshot.Name(a6), a0
	movea.l a2, a1
	move.l d7, d3
	bsr.w sameSpelling
	beq.w tryLeaf
	move.l d4, d1
	moveq #DECLARED_CANONICAL, d0
	rts
tryLeaf
	tst.l d6
	bpl.w advance
	movea.l a2, a0
	move.l d7, d3
	bsr.w leafSpelling
	movea.l a0, a2
	move.l d3, d7
	lea Snapshot.Name(a6), a0
	move.l Snapshot.NameBytes(a6), d3
	bsr.w leafSpelling
	cmp.l d7, d3
	bne.w advance
	tst.l d3
	beq.w advance
	movea.l a2, a1
	bsr.w sameSpelling
	beq.w advance
	move.l d4, d6
advance
	addq.l #1, d4
	bra.w next
finished
	tst.l d6
	bmi.w absent
	move.l d6, d1
	move.l d6, d0
	bsr.w readEntry
	bne.w absent
	moveq #DECLARED_LEAF, d0
	rts
absent
	moveq #0, d0
	rts
	.bend  ; findDeclared

; A0/A1=bounded names,D3=equal nonzero lengths. D0/CCR=one when ASCII folded
; spellings match. Scratch D1/D3 and A0/A1; caller retains the candidate view.
sameSpelling	.block
next
	moveq #0, d0
	move.b (a0)+, d0
	bsr.w foldByte
	move.l d0, d1
	move.b (a1)+, d0
	bsr.w foldByte
	cmp.b d1, d0
	bne.w different
	subq.l #1, d3
	bne.w next
	moveq #1, d0
	rts
different
	moveq #0, d0
	rts
	.bend  ; sameSpelling

; A0=name,D3=bounded length. Return A0/D3 after the final period, preserving
; the complete name when there is none. Scratch D0/A1 only.
leafSpelling	.block
	movea.l a0, a1
	move.l d3, d0
next
	tst.l d0
	beq.w done
	cmpi.b #'.', (a1)+
	bne.w advance
	movea.l a1, a0
	move.l d0, d3
	subq.l #1, d3
advance
	subq.l #1, d0
	bra.w next
done
	rts
	.bend  ; leafSpelling

; D0=byte. Fold spelling only; no semantic identity or instruction dispatch.
foldByte	.block
	cmpi.b #'A', d0
	blo.w done
	cmpi.b #'Z', d0
	bhi.w done
	addi.b #32, d0
done
	rts
	.bend  ; foldByte
	.pub
; A0=dos.library. Emit five numeric rows then 22 zero-padded name rows through
; the established progress ABI. Phases32..36 contain metadata;64..85 contain
; successive twelve-byte canonical-name chunks (last row contains four bytes).
; All registers, CCR and stack preserved. No output if no failure was latched.
report	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a6
	lea Record, a5
	tst.l Snapshot.Stage(a5)
	beq.w recordCapture
	moveq #32, d4
	moveq #64, d5
	bsr.w reportSnapshot
	btst #0, Snapshot.ModuleValid+3(a5)
	beq.w targetModule
	move.l #MODULE_SOURCE_PROGRESS, d0
	lea Snapshot.SourceModule(a5), a4
	bsr.w reportModule
targetModule
	btst #1, Snapshot.ModuleValid+3(a5)
	beq.w relatedModules
	move.l #MODULE_TARGET_PROGRESS, d0
	lea Snapshot.TargetModule(a5), a4
	bsr.w reportModule
relatedModules
	lea OwnerRecord, a5
	moveq #96, d4
	move.l #128, d5
	bsr.w reportSnapshot
	lea PreviousRecord, a5
	move.l #160, d4
	move.l #192, d5
	bsr.w reportSnapshot
	lea NextRecord, a5
	move.l #224, d4
	move.l #256, d5
	bsr.w reportSnapshot
recordCapture
	tst.l CaptureStage
	beq.w done
	lea RemapRecord, a4
	move.l #REMAP_PROGRESS, d7
	moveq #REMAP_CAPTURE_BYTES/12-1, d6
remapRows
	move.l d7, d0
	move.l (a4)+, d1
	move.l (a4)+, d2
	move.l (a4)+, d3
	movea.l a6, a0
	jsr memory_profile.progress
	addq.l #1, d7
	dbra d6, remapRows
	move.l d7, d0
	move.l (a4), d1
	moveq #0, d2
	moveq #0, d3
	movea.l a6, a0
	jsr memory_profile.progress
	move.l #RECORD_CONTEXT_PROGRESS, d0
	move.l CaptureStage, d1
	moveq #0, d2
	moveq #0, d3
	movea.l a6, a0
	jsr memory_profile.progress
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; report
	.priv

; D0=named phase,A4=owned flags/owner/origin row,A6=dos.
reportModule	.block
	move.l (a4)+, d1
	move.l (a4)+, d2
	move.l (a4), d3
	movea.l a6, a0
	jsr memory_profile.progress
	rts
	.bend  ; reportModule

; A5=Snapshot,A6=dos,D4=metadata phase base,D5=name phase base. Empty optional
; views emit nothing. D4/D5/A5/A6 retained; scratch D0-D3/D6-D7/A0/A4.
reportSnapshot	.block
	tst.l Snapshot.Stage(a5)
	beq.w done
	move.l d4, d0
	move.l Snapshot.Stage(a5), d1
	move.l Snapshot.Index(a5), d2
	move.l Snapshot.Related(a5), d3
	movea.l a6, a0
	jsr memory_profile.progress
	move.l d4, d0
	addq.l #1, d0
	move.l Snapshot.Base(a5), d1
	move.l Snapshot.Current(a5), d2
	move.l Snapshot.NameBytes(a5), d3
	movea.l a6, a0
	jsr memory_profile.progress
	lea Snapshot.Entry(a5), a4
	; Keep the existing seven-long metadata report; newer captured bytes stay opaque.
	move.l d4, d7
	addq.l #2, d7
	moveq #1, d6
metadataLoop
	move.l d7, d0
	move.l (a4)+, d1
	move.l (a4)+, d2
	move.l (a4)+, d3
	movea.l a6, a0
	jsr memory_profile.progress
	addq.l #1, d7
	dbra d6, metadataLoop
	move.l d4, d0
	addq.l #4, d0
	move.l (a4), d1
	moveq #0, d2
	moveq #0, d3
	movea.l a6, a0
	jsr memory_profile.progress
	lea Snapshot.Name(a5), a4
	move.l d5, d7
	moveq #20, d6
nameLoop
	move.l d7, d0
	move.l (a4)+, d1
	move.l (a4)+, d2
	move.l (a4)+, d3
	movea.l a6, a0
	jsr memory_profile.progress
	addq.l #1, d7
	dbra d6, nameLoop
	move.l d5, d0
	addi.l #21, d0
	move.l (a4), d1
	moveq #0, d2
	moveq #0, d3
	movea.l a6, a0
	jsr memory_profile.progress
done
	rts
	.bend  ; reportSnapshot
	.endsection
	.section bss, kind=bss
	.align 4
	.priv
Record	.res byte, SNAPSHOT_BYTES
OwnerRecord	.res byte, SNAPSHOT_BYTES
PreviousRecord	.res byte, SNAPSHOT_BYTES
NextRecord	.res byte, SNAPSHOT_BYTES
Pending	.res byte, PendingCheck.Related+4
RemapRecord	.res byte, REMAP_CAPTURE_BYTES
CaptureStage	.res long, 1
EntryCopy	.res byte, 256
NameCopy	.res byte, 256
	.endsection
	.endmodule
