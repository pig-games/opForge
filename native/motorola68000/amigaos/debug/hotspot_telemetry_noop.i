; Disabled telemetry API: keep signatures without capturing implementation bodies.
HOTSPOT_CLEAR	.macro
.endmacro
HOTSPOT_ENTER	.macro slot
.endmacro
HOTSPOT_LEAVE	.macro slot
.endmacro
HOTSPOT_REPORT	.macro dosbase
.endmacro
HOTSPOT_COUNT .macro counterID, amount
.endmacro
HOTSPOT_COUNT_IF_ZERO .macro value, counterID, amount
.endmacro
HOTSPOT_COUNT_IF_EQ_BYTE .macro value, expected, counterID, amount
.endmacro
HOTSPOT_COUNT_IF_EQ .macro value, expected, counterID, amount
.endmacro
HOTSPOT_COUNT_IF_NE_WORD .macro value, expected, counterID, amount
.endmacro
