; Shared text directive parity fixture. Expected byte values come from live Rust
; assembly; the native text owner never selects behavior using this path.
.module main
.org $1000
.byte "AZaz09",0
.enc petscii
.byte "AZaz09",0
.text "Az"
.null "OK"
.ptext "dog"
.encoding ascii
.byte "done",0
.endmodule
; Entering another module resets the active profile to ASCII.
.module second
.byte "Az"
.endmodule
