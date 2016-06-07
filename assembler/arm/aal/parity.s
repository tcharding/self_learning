	.section        .rodata
        .align  2
.debug:
	.ascii	"value in r1 is: %p\012\000"

	.text
        .align  2
        .global nBits
        .type   nBits, %function
nBits:
	/*
	Counts number of set bits in r1
	result in r0
	*/
        stmfd   sp!, {fp, ip, lr}

	mov	r2, #31		@ loop counter
	mov	r0, #0		@ result
loop:
	/* bitshift input and inc result if bit is set */
	mov	r3, r0, lsr r2
	and	r3, #1
	cmp	r3, #1
	addeq	r0, #1
	
	subs	r2, #1
	bge	loop

        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   nBits, .-nBits

	.text
	.align	2
	.global isEvenParity
	.type isEvenParity, %function
isEvenParity:
	/* Checks r1 for parity, r0 is 0 if even 0xDEADDEAD if not */
	stmfd	sp!, {fp, ip, lr}

	bl	nBits
	mov	r0, r1, lsr #1	@ divide by two
	and	r0, #1		@ 1 if odd
	cmp	r0, #1
	moveq	r0, #0xDEADDEAD
	
	ldmfd	sp!, {fp, ip, lr}
	bx	lr
	.size	isEvenParity, .-isEvenParity

	.text
	.align	2
	.global isOddParity
	.type isOddParity, %function
isOddParity:
	/* Checks r1 for parity, r0 is 1 if odd 0 if not */
	stmfd	sp!, {fp, ip, lr}

	bl	nBits
	mov	r0, r1, lsr #1	@ divide by two
	and	r0, #1		@ 1 if odd
	cmp	r0, #0
	moveq	r0, #0xDEADDEAD

	ldmfd	sp!, {fp, ip, lr}
	bx	lr
	.size	isOddParity, .-isOddParity

        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	/* test nBits */
	ldr 	r1, =#0xC3	@ 4 bits set
	bl	nBits
	cmp	r0, #4
	bne	err

	ldr 	r1, =#0xC3C4	@ 7 bits set
	bl	nBits
	cmp	r0, #7
	bne	err

	bl	exit
	
err:
	mov	r0, r1
	ldr	r1, .L1
	bl	printf
exit:	
	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L1:
	.word	.debug
        .size   main, .-main
	
