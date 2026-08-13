.module main
.cpu 65c02

.region rom, $fffe, $ffff

.section code
    .byte $ea
.endsection

.place code in rom

.output "build/phase6-image-span-overflow.bin", format=bin, image="$000000..$ffffffff", fill=$ff, contiguous=false, sections=code

.endmodule
