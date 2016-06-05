	.global main

main:
	mov	r6, #12
	mov	r4, r6
loop:	subs	r4, r4, #1
	mulne	r7, r6, r4
	mov	r6, r7
	bne	loop

	mov	r7, #1
	swi	0
	
	
