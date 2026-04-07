; Motorola 68080 strict Apollo-compatibility request in full-profile build

        .cpu 68080
        .apollo on
        .apollo off
        .org $1000

start:
        MOV3Q #1,D0

        .end
