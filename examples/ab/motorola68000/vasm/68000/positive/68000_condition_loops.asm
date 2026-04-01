; Motorola 68000 condition-code and decrement-loop smoke fixture

start:
    SNE D0
    ST (A0)
loop:
    DBRA D1,loop
    DBNE D2,done
done:
    RTS

