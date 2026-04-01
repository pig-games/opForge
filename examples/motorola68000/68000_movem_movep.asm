; SPDX-License-Identifier: GPL-3.0-or-later

.cpu 68000

    MOVEM.W D0-D2/A6,-(A7)
    MOVEM.L (A0)+,D1/D3/A2-A4
    MOVEM.W D7,($1234).W
    MOVEM.L 4(PC),D0-D1/A6-A7
    MOVEP.W D5,4(A1)
    MOVEP.L 6(A2),D6
    RTS
