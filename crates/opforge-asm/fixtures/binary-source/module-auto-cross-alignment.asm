.module width_fixture
.cpu 68020
.section code,kind=code
entry .block
 bsr target
 .align 8
target
 rts
.bend
.endsection
.output "width.hunk",format=hunk,sections=code
.endmodule
