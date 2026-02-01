; Demonstrate statement signature syntax.

.statement move.[{char size}] reg dst, reg src
    nop
.endstatement

.statement sta "[" byte a ","[{char reg}]
.endstatement

start:
    nop
