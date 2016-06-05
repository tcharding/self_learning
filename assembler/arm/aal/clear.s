	.global main

main:
	mov	r0, #0xFF	@ dirty value

	;; mov	r0, #0
	
	;; mov	r1, #0
	;; mov	r0, r1

	sub	r0, r0, r0

	bx	lr
