; Adapted from Sakura-IT Amiga-programming-examples:
; ASM/RAWImageView/Show320x256x4RawPicture.s
;
; Manual-only example. The examples/manual tree is intentionally skipped by
; automatic example/reference tests so this can track broader Amiga hardware
; source shapes before the regular test suite owns them.
;
; Demonstrates .incbin plus Hunk section memory attributes. The copper list,
; bitplane pointers, and raw image data live in CHIP memory because the custom
; chips must be able to DMA from that data.

        .module main
        .cpu 68000

        .section code, kind=code
Zab_sys
        move.w  $dff01c,d0
        ori.w   #$8000,d0
        move.w  d0,Old_INT
        move.w  #$7fff,$dff09a
        move.w  $dff002,d0
        ori.w   #$8000,d0
        move.w  d0,Old_DMA
        move.w  #$7fff,$dff096
        move.w  #$83c0,$dff096
        movea.l $0004,a6
        movea.l 156(a6),a1
        move.l  38(a1),Old_COPPER
        move.l  a7,Old_STACK

Wstep
        move.l  #COPPER,$dff080
        move.l  #0,$dff088
        move.l  #3,d0
        move.l  #40,d1
        move.l  #256,d2
        mulu.w  d2,d1
        move.l  #Image,d2
        lea     Registers,a1

Init
        swap    d2
        move.w  d2,2(a1)
        swap    d2
        move.w  d2,6(a1)
        addq.l  #8,a1
        add.l   d1,d2
        dbf     d0,Init

        lea     PalleteRegisters,a1
        movea.l #ImagePallete-32,A0
        moveq   #15,d0
        move.l  #$00000180,d1

PalleteLoop
        move.w  d1,(a1)+
        addq.w  #2,d1
        move.w  (A0)+,(a1)+
        dbf     d0,PalleteLoop

Stop
        btst    #6,$bfe001
        bne.s   Stop

exitproc
        movea.l Old_STACK,a7
        move.l  Old_COPPER,$dff080
        move.w  Old_DMA,$dff096
        move.w  Old_INT,$dff09a
        move.l  #0,d0
        rts

Old_INT
        .word 0
Old_DMA
        .word 0
Old_COPPER
        .long 0
Old_STACK
        .long 0
        .endsection

        .section data, kind=data, memory=chip
COPPER
        .long $01fc0000
        .long $01004200,$01020000
        .long $01040000,$01060000
        .long $008e2c81,$00902cc1
        .long $00920038,$009400d0
        .long $01fc0000
        .long $01080000,$010a0000
Registers
        .long $00e00000,$00e20000
        .long $00e40000,$00e60000
        .long $00e80000,$00ea0000
        .long $00ec0000,$00ee0000
        .long $00f00000,$00f20000
        .long $00f40000,$00f60000
        .long $00f80000,$00fa0000
        .long $00fc0000,$00fe0000

PalleteRegisters
        .long $01800fff,$0182000f
        .long $01840000,$018600f0
        .long $01880000,$018A0f00
        .long $018c0000,$018e0f00
        .long $01900000,$019200f0
        .long $01940000,$019600f0
        .long $01980000,$018A000f
        .long $019C0fff,$019E0000
        .long $01A00000,$01A20000
        .long $01A40000,$01A60000
        .long $01A80000,$01AA0000
        .long $01AC0000,$01AE0000
        .long $01B00000,$01B20000
        .long $01B40000,$01B60000
        .long $01B80000,$01BA0000
        .long $01BC0000,$01BE0000
        .long $FFFFFFFE

Image
        .incbin "RAW/Kubus16.raw"
ImagePallete

        .endsection
        .output "build/rawimageview_320x256x4_incbin.hunk", format=hunk, sections=code,data
        .endmodule
