.section 	.rodata
.align  	2
.err:
	.ascii	"Error: %d\012\000"

.section 	.data	
.jump:				@ jump table
.word	sine
.word	cosine
.word	tangent

@@@ trig_func: conditionally execute trig function using jump table
@@@ inputs:
@@@ 	r0 = command	0 = sine, 1 = cosine, 2 = tangent
@@@	r1 = angle in degrees (0 - 360 for sin/cos, 0 - 45 for tan)
@@@ outputs:
@@@ 	r0 = result in Q31 notation
@@@ 
.text
.align  2
.global trig_func
.type   trig_func, %function
trig_func:
	@@ r2 = address of trig function
        stmfd   sp!, {fp, ip, lr}

	ldr	r2, =.jump
	ldr	lr, =return
	ldr	pc, [r2, r0, lsl #2]
return:	
	ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   trig_func, .-trig_func
	
@@@ jump-trig: example jump table implementation
@@@ 
.text
.align  2
.global main
.type   main, %function
main:
	@@ r0 = command to call
	@@ r1 = angle
	@@ r4 = expected result
        stmfd   sp!, {fp, lr}

	ldr	r0, =#0		@ test sine
	ldr	r1, =#127	@ test value 127
	ldr	r4, =#1715056640 @ expected result

	bl	trig_func
	cmp	r0, r4
	bne	err

	ldr	r0, =#1		@ test cosine
	ldr	r1, =#127	@ test value 127
	ldr	r4, =#-1292387968 @ expected result
	
	bl	trig_func
	cmp	r0, r4
	bne	err

	ldr	r0, =#2		@ test tangent
	ldr	r1, =#1	@ test value 
	ldr	r4, =#37484468	@ expected value

	bl	trig_func
	cmp	r0, r4
	bne	err

	b	exit

err:
	mov	r5, r0
	mov	r1, r4		@ expected
	ldr	r0, =.err
	bl	printf
	mov	r1, r5		@ got
	ldr	r0, =.err
	bl	printf

	
exit:	@@ return 0 
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
.align  2
.size   main, .-main
	
	
