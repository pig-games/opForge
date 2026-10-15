; Source build configuration shared by the feature-owning modules.
; Rust command-line definitions select the same constant as a native source
; configuration. Generated experimental exports replace this include explicitly.
.ifdef OPFORGE_CHUNKED_CAPTURE
CHUNKED_CAPTURE = 1
.else
CHUNKED_CAPTURE = 0
.endif
.if CHUNKED_CAPTURE
; These experimental observers scan a contiguous arena per logical region.
; Reject that unsupported combination rather than publish misleading accounts.
.ifdef OPFORGE_LAZY_AUDIT
.error "chunked capture does not support the contiguous-region lazy observer"
.endif
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
.error "chunked capture does not support the contiguous-region lifetime observer"
.endif
.endif
.ifdef OPFORGE_DEFERRED_MACRO_CANDIDATES
ACQUIRE_MACRO_CANDIDATES = 1
.else
ACQUIRE_MACRO_CANDIDATES = 0
.endif
.if ACQUIRE_MACRO_CANDIDATES
DEFER_MACRO_PLANS = 1
.else
.ifdef OPFORGE_DEFERRED_MACRO_PLANS
DEFER_MACRO_PLANS = 1
.else
DEFER_MACRO_PLANS = 0
.endif

.endif

; Experimental acquisition interface; ordinary eager services remain available.
.if ACQUIRE_MACRO_CANDIDATES
BINARY_MACRO_VIEW = 1
.else
.ifdef OPFORGE_BINARY_MACRO_VIEW
BINARY_MACRO_VIEW = 1
.else
BINARY_MACRO_VIEW = 0
.endif
.endif

; Whole-line construction is an incremental experiment over the three prior modes.
.ifdef OPFORGE_DEFERRED_MACRO_LINES
DEFER_MACRO_LINES = 1
.else
DEFER_MACRO_LINES = 0
.endif
.if DEFER_MACRO_LINES
.if ACQUIRE_MACRO_CANDIDATES = 0
.error "whole-line plans require deferred macro candidates"
.endif
.endif
