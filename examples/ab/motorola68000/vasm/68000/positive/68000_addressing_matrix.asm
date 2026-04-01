; SPDX-License-Identifier: GPL-3.0-or-later


    MOVEA.L A0,A1
    MOVE.W (A0),D0
    MOVE.W (A0)+,D1
    MOVE.W -(A1),D2
    MOVE.W 4(A2),D3
    MOVE.W 6(A3,D4.L),D5
    MOVE.W ($1234).W,D6
    MOVE.L ($123456).L,D7
    MOVE.W 8(PC),D0
    MOVE.W 6(PC,D1.W),D2
    MOVE.W #$1234,D3
    RTS
