@@@ Bubble sort: sort table on first word of each element
@@@ inputs:
@@@ 	r0 = start address of words to sort
@@@ 	r1 = number of words (N)
@@@ 	r2 = size of elements in bytes
@@@ locals:
@@@ 	r2 = swapped flag
@@@ 	r3 = loop counter
@@@ 
@@@ 	r4 = tmp lower element
@@@ 	r5 = tmp higher element
@@@ 	r6 = current element pointer
@@@ 	r7 = element size
.global	sort
.text
.align	2
.type	sort, %function
sort:
	stmfd	sp!, {r4, r5, r6, fp, ip, lr}

	cmp	r1, #1
	ble	out		@ nothing to sort
	sub	r1, #1		@ need N-1 comparisons

	mov	r3, r1		@ initialise loop counter
	mov 	r6, r0		@ start address of words
	mov	r7, r2
	mov	r2, #0		@ clear flag

loop:
	ldr	r4, [r6], r7
	ldr	r5, [r6]	@ no inc, we need this address again

	cmp	r4, r5
	ble	no_swap
	sub	r6, r7
	str	r5, [r6], r7
	str	r4, [r6]
	mov	r2, #1		@ set flag
no_swap:
	subs	r3, r3, #1	@ dec counter and loop
	bne	loop

	cmp	r2, #0		@ check if we swapped
	beq	out
	
	/*	sub	r1, #1		@ N - 1 (optimisation) */
	mov	r2, #0		@ clear flag
	mov	r6, r0		@ reset address
	mov	r3, r1		@ reset counter
	b	loop

out:	
	ldmfd	sp!, {r4, r5, r6, fp, ip, lr}
	bx	lr
	.size	sort, .-sort
