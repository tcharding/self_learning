	.global main

main:
	ldr	r0, =0xF631024C
	ldr	r1, =0x17539ABD
	eor	r0, r0, r1
	eor	r1, r0, r1
	eor	r0, r0, r1

	mov	r7, #1
	swi	0
	
