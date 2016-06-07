        .align  2
.LC0:
        .ascii  "maximum: %p\012\000"

values:
	.word 0xFEBBAAAA, 0x12340000, 0x88881111
	.word 0x00000013, 0x80808080, 0xFFFFFFFF

	.text
	.align 2
	.global main
	.type	main, %function
main:
	/* find maximum value in values */
	stmfd   sp!, {fp, lr}
	
	mov	r0, #0		@ maximum so far
	
	mov	r2, #6		@ counter
	adr	r3, values
loop:	
	ldr	r1, [r3], #4
	cmp	r0, r1
	movcc	r0, r1

	subs	r2, #1
	bge	loop

        mov     r1, r0
        ldr     r0, .L3
        bl      printf
	
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L3:
        .word   .LC0
        .size   main, .-main
