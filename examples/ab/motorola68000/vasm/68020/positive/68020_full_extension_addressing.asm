; Motorola 68020 full-extension addressing smoke fixture

start:
    MOVE.W (4.W,A0,D1.L*4),D0
    MOVE.W 4.W(A0,D1.L*4),D1
    MOVE.W ([A0,D1.L*4],8.W),D2
    MOVE.W ([A3],D2.W*2,8.L),D3
    MOVES.W D0,(4.W,A0,D1.L*4)
    MOVES.L ([A0,D1.L*4],8.W),A2
    RTS

