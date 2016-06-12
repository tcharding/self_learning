.section 	.rodata
.align  	2
.err:
.ascii	"Error: n: %d exp: %d got: %d\012\000"

@@@ fact-reg: calculate factorial
@@@ inputs:
@@@ 	r0 = n
@@@ outputs:
@@@ 	r1 = n!
.text
.align  2
.global fact_reg
.type   fact_reg, %function
fact_reg:
        stmfd   sp!, {fp, lr}

	mov	r1, #1
	cmp	r0, #1
	ble	done
loop:	
	mul	r1, r1, r0
	subs	r0, r0, #1
	bne	loop
				
done:	ldmfd   sp!, {fp, lr}
        bx      lr
        .size   fact_reg, .-fact_reg

@@@ fact_stack: calculate factorial (arg on stack)
@@@ inputs:
@@@ 	n on stack
@@@ outputs:
@@@ 	n! on stack
.text
.align  2
.global fact_stack
.type   fact_stack, %function
fact_stack:
        stmfd   sp!, {fp, lr}
	
	mov	r1, #1
	ldr	r0, [sp, #8]
	cmp	r0, #1
	ble	stdone
stloop:	
	mul	r1, r1, r0
	subs	r0, r0, #1
	bne	stloop

	str	r1, [sp, #8]
stdone:	ldmfd   sp!, {fp, lr}
        bx      lr
        .size   fact_stack, .-fact_stack

@@@ Test factorial
.text
.align  2
.global main
.type   main, %function
main:
        stmfd   sp!, {fp, lr}

	@@ test fact_reg
	mov	r4, #5
	mov	r5, #120
	mov	r0, r4
	bl	fact_reg
	cmp	r1, r5
	bne	err

	@@ test fact_stack
	mov	r4, #4
	mov	r5, #24
	stmfd 	sp!, {r4}
	bl	fact_stack
	ldmfd 	sp!, {r1}
	cmp	r1, #24
	bne	err

	b	exit

err:	mov	r3, r1
	mov	r1, r4
	mov	r2, r5

	ldr	r0, =.err
	bl	printf

exit:	@@ return 0 
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
.align  2
.size   main, .-main
	
	
