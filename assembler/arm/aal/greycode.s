	.section        .rodata
        .align  2
.debug:
	.ascii	"value in r0 is: %p\012\000"

	.text
        .align  2
        .global grey
        .type   grey, %function
grey:
	/* translates 2 bit grey code in r1 into 3 bit grey code in r0 */
        stmfd   sp!, {r4, r5, r6, fp, ip, lr}

	/* set up */
	ldr	r0, =#0
	ldr	r2, =#0		@ N
	ldr	r3, =#0		@ F

	mov	r4, r1		@ save argument
	ldr	r5, =#0 	@ fill bits
	ldr	r6, =#4		@ counter
	
	mvn	r5, r5		@ set fill bits to ones
	
loop:

	
	mov	r1, r4
	bl	orBits
	add	r2, #1		@ inc N
	
	mov	r1, r4
	bl	orBits
	add	r2, #1		@ inc N
	
	mov	r1, r5		
	bl	orBits
	add	r3, #1		@ int F
	
	subs	r6, #1
	bne	loop

	cmp	r5, #0
	beq	done
	
	/* re-set counter and set fill bits to zero's */
	ldr	r6, =#4		@ counter
	ldr	r2, =#0		@ N
	mvn	r5, r5		@ set fill bits to ones
	bl	loop
	
done:
        ldmfd   sp!, {r4, r5, r6, fp, ip, lr}
        bx      lr
        .size   grey, .-grey

	.text
        .align  2
        .global orBits
        .type   orBits, %function
orBits:
	/* logical OR bit N from src with bit N+F in dst
	r0 -> dst
	r1 -> src
	r2 -> N
	r3 -> F

	Don't clobber r2, and r3
	*/
        stmfd   sp!, {r2, r3, r4, fp, ip, lr}

	mov	r1, r1, lsr r2
	and	r1, #1
	add	r4, r2, r3
	mov	r1, r1, lsl r4
	orr	r0, r0, r1
	
        ldmfd   sp!, {r2, r3, r4, fp, ip, lr}
        bx      lr
        .size   orBits, .-orBits

        .text
        .align  2
        .global testOrBits
        .type   testOrBits, %function

testOrBits:
        stmfd   sp!, {fp, ip, lr}

	/*  test orBits */
	ldr	r0, =#0
	ldr	r1, =#0xFF
	ldr	r2, =#6
	ldr 	r3, =#3

	bl	orBits
	mov	r1, r0
	ldr	r0, .L1
	bl	printf
	
	ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   testOrBits, .-testOrBits

        .text
        .align  2
        .global main
        .type   main, %function

main:
        stmfd   sp!, {fp, lr}

@	bl	testOrBits 
	
	ldr	r1, =#0xB4	@ 10 11 01 00
	bl	grey		@ expect: 0x4C8DEC

	mov	r1, r0
	ldr	r0, .L1
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
	
