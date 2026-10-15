; Source build configuration shared by the feature-owning modules.
; Rust command-line definitions select the same constant as a native source
; configuration. Generated experimental exports replace this include explicitly.
.ifdef OPFORGE_DEFERRED_MACRO_PLANS
DEFER_MACRO_PLANS = 1
.else
DEFER_MACRO_PLANS = 0
.endif

; Experimental acquisition interface; ordinary eager services remain available.
.ifdef OPFORGE_BINARY_MACRO_VIEW
BINARY_MACRO_VIEW = 1
.else
BINARY_MACRO_VIEW = 0
.endif
