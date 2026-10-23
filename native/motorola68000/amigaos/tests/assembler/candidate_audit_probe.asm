; Passive observer preservation and independent 64-bit carry/ownership checks.
; @opforge-owner: opforge.tests.assembler.candidate_audit_probe
	.module opforge.tests.assembler.candidate_audit_probe
	.cpu 68020
	.use opforge.debug.lazy_work as audit
	.use opforge.assembler.source.capture as capture
	.use opforge.platform.memory as memory
	.section code, kind=code
	.pub
; D0/CCR=status 41..45; all other registers preserved. State reset before return.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l sp, SavedSp
	jsr audit.clear
	lea PlanBlock, a0
	move.l a0, Request+capture.Frame.Plans
	movem.l Seed, d0-d7/a0-a6
	lea Request, a0
	move.w #$15, ccr
	movem.l d0-d7/a0-a6, Before
	move.w ccr, BeforeFlags
	jsr audit.candidateBegin
	jsr audit.candidateEnd
	move.w ccr, AfterFlags
	movem.l d0-d7/a0-a6, After
	moveq #41, d0
	move.l sp, d1
	cmp.l SavedSp, d1
	bne.w done
	move.w BeforeFlags, d1
	cmp.w AfterFlags, d1
	bne.w done
	lea Before, a0
	lea After, a1
	moveq #14, d2
preserved
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w done
	dbra d2, preserved
	jsr audit.getCandidates
	moveq #42, d0
	tst.l audit.CandidateFields.Errors(a0)
	bne.w done
	tst.l audit.CandidateFields.Created(a0)
	bne.w done
	tst.l audit.CandidateFields.PlanBytes(a0)
	bne.w done
	jsr audit.clear
	move.l #capture.MAGIC, Record+capture.Record.Magic
	move.l #capture.HEADER_BYTES, Record+capture.Record.Bytes
	lea Record, a0
	moveq #0, d0
	moveq #0, d1
	moveq #0, d3
	move.w #$15, ccr
	jsr audit.candidateOwner
	move.w ccr, AfterFlags
	moveq #43, d0
	cmpi.w #$15, AfterFlags
	bne.w done
	cmpi.l #audit.CANDIDATE_NON_MACRO, Record+capture.Record.CandidateOwner
	bne.w done
	jsr audit.candidateOwner
	jsr audit.getCandidates
	cmpi.l #audit.CANDIDATE_DUPLICATE, audit.CandidateFields.Errors(a0)
	bne.w done
	cmpi.l #1, audit.CandidateFields.Duplicates(a0)
	bne.w done
	jsr audit.clear
	move.l #1, Record+capture.Record.CandidateSample
	move.l #$ffffffff, Record+capture.Record.CandidateTicks
	; Three maximum 32-bit samples must accumulate as 00000002:fffffffd.
	lea Record, a0
	moveq #2, d7
records
	jsr audit.candidateRecord
	dbra d7, records
	jsr audit.getCandidates
	moveq #44, d0
	tst.l audit.CandidateFields.Errors(a0)
	bne.w done
	cmpi.l #3, audit.CandidateFields.Vectors+audit.CandidateVector.Records(a0)
	bne.w done
	cmpi.l #2, audit.CandidateFields.Vectors+audit.CandidateVector.TicksHigh(a0)
	bne.w done
	cmpi.l #$fffffffd, audit.CandidateFields.Vectors+audit.CandidateVector.TicksLow(a0)
	bne.w done
	cmpi.l #3, audit.CandidateFields.Vectors+audit.CANDIDATE_VECTOR_BYTES+audit.CandidateVector.Samples(a0)
	bne.w done
	; Unknown ownership must stay in its own group; no unused-macro inference.
	clr.l Record+capture.Record.CandidateOwner
	lea Record, a0
	jsr audit.candidateRecord
	jsr audit.getCandidates
	moveq #45, d0
	cmpi.l #1, audit.CandidateFields.Unassigned(a0)
	bne.w done
	cmpi.l #1, audit.CandidateFields.Vectors+audit.CANDIDATE_UNKNOWN_GROUP*audit.CANDIDATE_VECTOR_BYTES+audit.CandidateVector.Records(a0)
	bne.w done
	moveq #0, d0
done
	jsr audit.clear
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.endsection
	.section data, kind=data
PlanBlock	.long 0, 7, 7
	.endsection
	.section bss, kind=bss
Record	.res byte, capture.HEADER_BYTES
Request	.res byte, capture.FRAME_BYTES
Before	.res long, 15
After	.res long, 15
BeforeFlags	.res word, 1
AfterFlags	.res word, 1
SavedSp	.res long, 1
	.endsection
	.section data, kind=data
Seed	.long $10101010, $21212121, $32323232, $43434343
	.long $54545454, $65656565, $76767676, $87878787
	.long $18181818, $29292929, $3a3a3a3a, $4b4b4b4b
	.long $5c5c5c5c, $6d6d6d6d, $7e7e7e7e
	.endsection
	.endmodule
