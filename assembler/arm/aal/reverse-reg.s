	.align  2
.msg:
        .ascii  "result: %p\012\000"

	.text
	.align 2
	.global main
	.type	main, %function
main:
	/* reverse bits in r0 */
	stmfd   sp!, {fp, lr}

	ldr	r0, =0x01020408
	mov	r1, #31		@ loop counter
	mov	r2, #0		@ shift counter
	mov	r3, #0		@ shifted bit
	mov	r4, #0		@ accumulated result
loop:
	mov	r2, r1
	rsb	r2, r2, #31
	mov	r3, r0, lsr r2
	and	r3, #1
	mov	r3, r3, lsl r1
	
	orr	r4, r4, r3

	subs	r1, #1
	bge	loop

	mov	r1, r4
	ldr	r0, .L1
	bl	printf
	
	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L1:
        .word   .msg
        .size   main, .-main

