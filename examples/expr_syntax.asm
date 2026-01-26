; Expression syntax coverage for 64tass-style operators and literals
        org     0000h

; numeric literals: decimal, suffixes, prefixed
num_dec         equ     123
num_hex         equ     0a6h
num_hex_pref    equ     $1f
num_bin         equ     1010b
num_bin_pref    equ     %1010
num_oct         equ     17o
num_oct_q       equ     17q

; unary operators and bytes
hi_byte         equ     >1234h
lo_byte         equ     <1234h
not_zero        equ     !0
not_one         equ     !1
bit_not         equ     ~00ffh
neg_one         equ     -1

; arithmetic, power, shifts
pow1            equ     2 ** 3
pow2            equ     3 ** 2 ** 2
shift_l         equ     1 << 4
shift_r         equ     0ffh >> 4

; comparisons
cmp_eq          equ     (3 == 3)
cmp_ne          equ     (3 != 4)
cmp_ne_alt      equ     (3 <> 4)
cmp_le          equ     (3 <= 4)
cmp_lt          equ     (3 < 4)
cmp_ge          equ     (4 >= 3)
cmp_gt          equ     (4 > 3)
cmp_eq_alias    equ     (4 = 4)

; bitwise and logical
bit_and         equ     (0f0h & 00fh)
bit_or          equ     (0f0h | 00fh)
bit_xor         equ     (0f0h ^ 00fh)
log_and         equ     (2 && 3)
log_or          equ     (0 || 3)
log_xor         equ     (2 ^^ 3)

; ternary
ternary1        equ     0 ? 1 : 2
ternary2        equ     5 ? 7 : 9
ternary3        equ     (0 || 1) ? (2 + 3) : (4 + 5)

; string constants in expressions
char_a          equ     'A'
char_ab         equ     'AB'

; data emissions
        db      num_dec, num_hex, num_hex_pref, num_bin, num_bin_pref
        db      num_oct, num_oct_q, hi_byte, lo_byte, not_zero, not_one
        dw      bit_not, neg_one, pow1, pow2
        db      shift_l, shift_r
        db      cmp_eq, cmp_ne, cmp_ne_alt, cmp_le, cmp_lt, cmp_ge, cmp_gt, cmp_eq_alias
        db      bit_and, bit_or, bit_xor, log_and, log_or, log_xor
        db      ternary1, ternary2, ternary3
        db      char_a
        dw      char_ab
