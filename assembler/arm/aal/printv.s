@@@ Printv: print a vector of words
@@@ inputs:	
@@@ 	r0 = start of vector
@@@ 	r1 = number of words
@@@ 	r2 = initial message string (or 0)
@@@ 	r3 = datum output string
@@@ local:
@@@ 	r4 = address to print next
@@@ 	r5 = loop counter
@@@ 	r6 = start of initial output string
@@@ 	r7 = start of subsequent output string
@@@ 
.equ 	datum_size, 4
.global printv
printv:	
	stmfd   sp!, {r4, r5, r6, r7, fp, ip, lr}

	@@ copy registers, this should be done using stack
	mov 	r4, r0
	mov	r5, r1
	mov	r6, r2
	mov	r7, r3
	
	cmp	r5, #1
	blt	out		@ nothing to print

	@@ initial print
	cmp	r0, #0
	beq	loop		@ no initial print
	mov	r0, r6
	bl	printf
loop:
	ldr	r1, [r4], #datum_size
	mov	r0, r7
	bl	printf

	subs	r5, #1
	bne	loop
out:	
	ldmfd 	sp!, {r4, r5, r6, r7, fp, ip, lr}
	bx	lr
.size	printv, .-printv
