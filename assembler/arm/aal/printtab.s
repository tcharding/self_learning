@@@ Printtab: print first word of elements of a table 
@@@ inputs:	
@@@ 	r0 = start of vector
@@@ 	r1 = number of words
@@@ 	r2 = element size in bytes
@@@ 	r3 = datum output string
@@@ local:
@@@ 	r4 = address to print next
@@@ 	r5 = loop counter
@@@ 	r6 = element size
@@@ 	r7 = start of subsequent output string
@@@ 
.global printtab
printtab:	
	stmfd   sp!, {r4, r5, r6, r7, fp, ip, lr}

	@@ copy registers, this should be done using stack
	mov 	r4, r0
	mov	r5, r1
	mov	r6, r2
	mov	r7, r3
	
	cmp	r5, #1
	blt	out		@ nothing to print

loop:
	ldr	r1, [r4], r6
	mov	r0, r7
	bl	printf

	subs	r5, #1
	bne	loop
out:	
	ldmfd 	sp!, {r4, r5, r6, r7, fp, ip, lr}
	bx	lr
.size	printtab, .-printtab
