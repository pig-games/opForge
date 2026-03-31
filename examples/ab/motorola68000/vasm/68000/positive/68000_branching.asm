; Basic Motorola 68000 branch smoke fixture

start:
    MOVEQ #1,D0
    BRA.W middle
    MOVEQ #0,D0
middle:
    ADDQ.W #1,D0
    BNE.W done
    SUBQ.W #1,D0
done:
    RTS

