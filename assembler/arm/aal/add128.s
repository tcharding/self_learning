	.section        .rodata
        .align  2
.errMsg:
	.ascii	"error: values do not match: %p\012\000"
.outWord:
	.ascii 	"%p\0000"
.nl:
	.ascii	"\012\000"

	.text
        .align  2
        .global out128
        .type   out128, %function
out128:	/* print registers 0, 1, 2, 3 */
        stmfd   sp!, {r4, r5, r6, r7, fp, ip, lr}

	mov	r4, r0
	mov	r5, r1
	mov	r6, r2
	mov	r7, r3

	mov	r1, r4
	ldr	r0, .L1
	bl	printf
	mov	r1, r5
	ldr	r0, .L1
	bl	printf
	mov	r1, r6
	ldr	r0, .L1
	bl	printf
	mov	r1, r7
	ldr	r0, .L1
	bl	printf
	
	ldr	r0, .L2
	bl	printf
	
        ldmfd   sp!, {r4, r5, r6, r7, fp, ip, lr}
        bx      lr

.L1:
	.word	.outWord
.L2:
	.word	.nl
        .size   out128, .-out128

	.text
        .align  2
        .global out128
        .type   out128, %function
add128:	/*
	unsigned add 128 bit values in r4-r7 and r8-r11
	result in r0-r3
	*/
        stmfd   sp!, {fp, ip, lr}

	adds	r0, r4, r8
	adcs	r1, r5, r9
	adcs	r2, r6, r10
	adcs	r3, r7, r11
		
        ldmfd   sp!, {fp, ip, lr}
        bx      lr
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	/* test with no carry */
	ldr	r4, =0x1
	ldr	r5, =0x02
	ldr	r6, =0x03
	ldr	r7, =0x04
	
	ldr	r8, =0x5
	ldr	r9, =0x06
	ldr	r10, =0x07
	ldr	r11, =0x08

	bl	add128
	/* expected: 6 8 10 12 */
	cmp	r0, #6
	movne	r1, r0
	bne	err
	cmp	r1, #8
	bne	err
	cmp 	r2, #10
	bne	err
	cmp	r3, #12
	bne	err
	bl	end

	/* test with carry */
	ldr	r4, =0x80000000
	ldr	r5, =0x02
	ldr	r6, =0x03
	ldr	r7, =0x04
	
	ldr	r8, =0x5
	ldr	r9, =0x06
	ldr	r10, =0x07
	ldr	r11, =0x08

	bl	add128
	/* expected: 2 9 10 12 */
	cmp	r0, #2
	movne	r1, r0
	bne	err
	cmp	r1, #9
	bne	err
	cmp 	r2, #10
	bne	err
	cmp	r3, #12
	bne	err
	bl	end

err:
	ldr	r0, .L0
	bl	printf
end:	
	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L0:
	.word	.errMsg
        .size   main, .-main
	
