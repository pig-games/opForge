; Motorola 68000 condition-code and decrement-loop smoke fixture
.cpu 68000
.org $1000

start:
    SNE D0
    ST (A0)
loop:
    DBRA D1,loop
    DBNE D2,done
done:
    RTS

.end
