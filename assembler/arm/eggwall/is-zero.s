	.section        .rodata
        .align 2
zero:
        .asciz "isZero returned 0\n"
one:
	.asciz "isZero returned 1\n"	

	.text
        .align  2
        .global isZero
        .type   isZero, %function
isZero:
        stmfd   sp!, {fp, ip, lr}
	cmp	r0, #0
	movne	r0, #1
        bx      lr
        .size   isZero, .-isZero

        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	@ test cast 1 should print is zero
        mov     r0, #0
	
	b	isZero
	
	ldr	r0, =zero
	ldrne	r0, =one
print:
        @ Print the message
        bl      puts
	
        @ Return 0
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr

