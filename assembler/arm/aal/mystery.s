	.global main

main:
	mov	r6, #1
	mov	r8, #2
	mov	r9, #4
	mov	r10, #8

	movs	r0, r6, lsl #5
	add	r0, r8, r8, lsl #2
	rsb	r0, r9, r9, lsl #3

	bx	lr
	
