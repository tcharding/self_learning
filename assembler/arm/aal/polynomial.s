/* Calculate 6x(2) - 9x + 2 */
	.global main

main:
	/* r3 holds x */
	mov	r3, #2

	/* 6 . x squared */
	mov	r0, #6
	mul	r0, r0, r3
	mul	r0, r0, r3

	/* - 9x */
	mov	r1, #9
	mul	r1, r1, r3
	sub	r0, r0, r1

	/* + 2 */
	add	r0, r0, #2
	
	bx	lr
	
