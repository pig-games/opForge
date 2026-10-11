; Custom registry fixture: dynamic escape lengths/counts, empty replacements,
; same-pattern replacement, longest match and independent live clone payloads.
.module main
.org $1200
.encode game
.cdef "A","Z",1
.tdef "xy",$40
.tdef "!?",$80,$81
.edef "{cr}",13
.edef "{erase}",""
.edef "{long-pattern-over-sixteen}","replacement-over-sixteen"
.edef "{1}",1
.edef "{2}",2
.edef "{3}",3
.edef "{4}",4
.edef "{5}",5
.edef "{6}",6
.edef "{7}",7
.edef "{8}",8
.edef "{9}",9
.edef "{cr}",10
.edef "A!",$ff
.endencode
.enc game
.byte "A{cr}xy!?{erase}{long-pattern-over-sixteen}{9}A!",0
.encode cloned,game
.edef "{cr}",11
.endencode
.enc cloned
.byte "A{cr}xy{9}"
.enc game
.byte "A{cr}xy{9}"
.encode shifted,petscii
.edef "{home}",19
.endencode
.enc shifted
.null "Az{home}"
.ptext "{home}{home}"
.endmodule
