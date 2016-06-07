	.section        .rodata
        .align  2
.debug:
	.ascii	"value in r1 is: %p\012\000"

	.text
        .align  2
        .global div16
        .type   div16, %function
div16:
        stmfd   sp!, {r3, r4, fp, ip, lr}

	ldr	r4, =#0		@ flag: val was negative
	mov	r3, r0		@ save copy of arg
	ldr	r2, =#1
	tst	r0, r2, lsl #31
	ldreq	r4, =#1
	mov	r0, r3		@ restore r0
	bleq	toPos

	mov	r1, r0, lsr #4	@ do the division

	cmp	r4, #0
	beq	done
	/* back to two's compliment */
	mvn	r1, r1
	add	r1, #1
done:	
        ldmfd   sp!, {r3, r4, fp, ip, lr}
        bx      lr
        .size   div16, .-div16

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

	/* test positive operand */
	ldr	r0, =0x20
	ldr	r4, =0x2	@ expected result
	bl	div16

	cmp	r1, r4
	bne	err
	
	/* test negative operand */
	ldr	r0, =0xFFFFFFE0
	ldr	r4, =0xFFFFFFFE	 	@ expected result 
	bl	div16

	cmp	r1, r4
	bne	err
	
	bl 	end
	
err:	
	ldr	r0, .L1
	bl	printf
end:	
	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L1:
	.word	.debug
        .size   main, .-main
	
