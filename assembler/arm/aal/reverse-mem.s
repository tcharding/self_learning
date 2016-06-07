/*
This does not work! seg faults (because data is read only?)
	Where should data be, which section?
*/
	.align	2
data:
	.word 0xFEBBAAAA, 0x12340000, 0x88881111
	.word 0x00000013, 0x80808080, 0xFFFF0000
	.word 0xFEBBAAAA, 0x12340000, 0x88881111
	.word 0x00000013, 0x80808080, 0xFFFF0000
	.word 0xFEBBAAAA, 0x12340000, 0x88881111
	.word 0x00000013, 0x80808080, 0xFFFF0000
	.word 0xFEBBAAAA, 0x12340000, 0x88881111
	.word 0x00000013, 0x80808080, 0xFFFF0000

	.text
	.align	2
	.global main
	.type 	main, %function
main:
	stmfd   sp!, {fp, lr}

	mov	r0, #16		@ loop counter
	adr	r1, data
loop:
	rsb	r2, r0, #16	@ front element index
	ldr	r3, [r1, r2, lsl #2] @ lsl #2 for 4 * index
	add	r4, r0, #16	@ back element index
	ldr	r5, [r1, r4, lsl #2]


	str	r3, [r1, r4]
	str	r5, [r1, r2]

	subs	r0, #1
	bgt	loop

	mov	r0, #0
	ldmfd	sp!, {fp, lr}
	bx	lr

	.align	2
	.size	main, .-main
