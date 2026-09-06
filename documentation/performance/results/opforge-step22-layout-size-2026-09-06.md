# Step22 — placed-section group-size accumulator

The native `placeSectionV1` group-size sum used D2 while table-pointer helpers overwrite D2 with scaled indices. Four instruction operands now use D3, which alignment no longer needs and name matching preserves. Membership, alignment, overflow checks, ABI, storage and loop count remain unchanged.

The complete three-section live Rust oracle has sizes 5/3/4, alignments 1/2/1, bases $8000/$8006/$8009, and region used13/free243. The earlier source-audit estimate17 was wrong and is explicitly corrected; no measured runtime or old guest output is rewritten. The recorded B09 native map before this repair had used25 and BSS base$800D, exactly accounted for by the clobbered scaled-index sum.

Host placement and source-boundary proof pass (Level B). Fresh native BIN/map proof passed in64.69seconds with exact live Rust artifacts, fresh protocol and explicit zero guest exit (Level D). A separate host test proves two same-name section members2+3 reserve5bytes; this host proof is not a native multi-member execution claim. Full B09 disposition is recorded below; required gates and independent compliance pass. Compound expressions, HEX, map-symbol and listing differences remain Phase A debt.

Performance contribution for this step: expected removal of erroneous reserved span, with identical placement complexity and no added storage; runtime gain unmeasured. Total current Phase A runtime gain remains unmeasured. The separately proven Step19 source fixture retains 1,608-to-8 DOS reads (99.5% fewer); this is not a whole-product runtime total.

## B09 disposition

Fresh full B09 compared all eleven artifacts under the same repaired package and unchanged120-second bound. Eight remain exact; HEX, map and listing still differ. The native map now records used13/free243 and bases$8000/$8006/$8009, confirming the accumulator correction on the original workload. Remaining map differences are symbol prefix/visibility, not the corrected section reservation. This is not full-map or B09 parity. The 75.628538625-second whole invocation is a failed-case duration, not a speedup measurement.

The required non-LSP Rust gate passed in267.733176875seconds with explicit exit0; native/workflow, inventory and formatter gates pass. Independent plan compliance passes with no findings after the gate receipt. Focused commit follows final artifact validation. No Phase A closure is claimed.
