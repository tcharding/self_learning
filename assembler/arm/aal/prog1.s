	.global main

main:
	mov	r0, #0x11
	lsl	r1, r0, #1
	add	r2, r1, r1, LSL #2

	bx	lr

