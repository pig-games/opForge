; Module import across files

.include "module_use_lib.inc"

.module app.main
    .use lib.math as M
    .use lib.math (VALUE as V)
    .byte V
    .byte M.VALUE
.endmodule

.end
