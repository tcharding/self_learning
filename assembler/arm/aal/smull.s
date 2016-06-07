	.section        .rodata
        .align  2
.debug:
	.ascii	"value in r0 is: %p\012\000"
.high:
        .ascii  "Error: \nbits  0-31: %p\012\000"
.low:	
        .ascii  "bits 32-63: %p\012\000"
.store:
	.ascii 	"stored sign bit: %p\012\000"

	.text
        .align  2
        .global smull
        .type   smull, %function
smull:
        stmfd   sp!, {fp, ip, lr}

	/* store sign using r1 and r2 */
	mov	r1, r5
	mov	r2, r6
	bl	storeSign	@ puts sign in r0
	/* NOTE: we should be using the stack for this */
	mov	r9, r0	
	
	/* convert 2's compliment to unsigned absolute value */
	mov	r1, r5
	bl	toPos
	mov	r5, r0
	
	mov	r1, r6
	bl	toPos
	mov	r6, r0
	
	/* do multiplication using unsigned multiply */
	umull	r3, r4, r5, r6

	/* convert to 2's compliment if result is negative */
	cmp	r9, #1
	bne	smullEnd
	mvn	r3, r3
	add	r3, #1
	mvn	r4, r4
	ldr	r0, =#1
	orr	r4, r0, lsl #31
	
smullEnd:	
        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   smull, .-smull

	/* puts sign bit into r0 of multiplying r1 and r2, eg
	r0 (0) when r1(-1) r2(-1)
	r0 (1) when r1(-1) r2(1)
	*/
	.text
        .align  2
        .global storeSign
        .type   storeSign, %function
storeSign:
        stmfd   sp!, {fp, ip, lr}

	mov	r0, #0
	mov	r1, r1, lsr #31
	mov	r2, r2, lsr #31
	eor	r0, r1, r2

        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   storeSign, .-storeSign

	.text
        .align  2
        .global toPos
        .type   toPos, %function
toPos:
        stmfd   sp!, {fp, ip, lr}

	mov	r2, r1, lsr #31
	cmp	r2, #1
	bne	.toPosEnd
	mvn	r1, r1
	add	r1, #1
.toPosEnd:
	mov	r0, r1
	
        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   toPos, .-toPos


        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	/* clear result registers */
	mov	r3, #0		
	mov	r4, #0

	/* test suite

	r3 -> result (low order, bits 0-31)
	r4 -> result (high order, bits 31-63)

	r5 -> operand 1
	r6 -> operand 2
	*/

	/* unsigned multiply */
				/* operands */
	ldr	r5, =#0x7FFFFFFF
	ldr	r6, =#0x2
				/* expected result */
	smull	r7, r8, r5, r6
	
	bl	smull
	cmp	r3, r7
	bne	.err
	cmp	r4, r8
	bne	.err

	/* signed multiply, positive result */
				/* operands */
	ldr	r5, =#0xFFFFFFFF
	ldr	r6, =#0xFFFFFFFF
				/* expected result */
	smull	r7, r8, r5, r6
	
	bl	smull
	cmp	r3, r7
	bne	.err
	cmp	r4, r8
	bne	.err

	/* signed multiply, negative result */
				/* operands */
	ldr	r5, =#0xFFFFFFFF
	ldr	r6, =#0x2
				/* expected result */
	smull	r7, r8, r5, r6
	
	bl	smull
	cmp	r3, r7
	bne	.err
	cmp	r4, r8
	bne	.err

	b	.end

.err:	
	mov	r1, r3
        ldr     r0, .L3
        bl      printf

	mov	r1, r4
        ldr     r0, .L4
        bl      printf
.end:	
	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L1:
	.word	.debug
.L3:
        .word   .high
.L4:
	.word	.low
.L5:
	.word	.store
        .size   main, .-main
	
