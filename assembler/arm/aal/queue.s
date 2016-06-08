	.section        .rodata
.align  2
.debug:
.ascii "r1: %p\012\000"
.queue:
.ascii	"\nqueue:\n\000"
.out_word:
.ascii "\t%p\n\000"
.preamble:	
.ascii "\Queue of %d words.\n\n\000"	

.section .data
.align	2
.values:
	@@ random que of words
.word	0x6,0x1,0x36,0x2,0x85,0x23,0xFF,0x32,0x1,0x99
.equ	.nvalues, 10


@@@ rm_front: remove first element from queue
@@@ 		shift all elements in memory
@@@ inputs:
@@@ 	r0 = address of queue
@@@ 	r1 = number of elements in queue
@@@ outputs:
@@@ 	r0 = address of queue
.text
.align  2
.global rm_front
.type   rm_front, %function
rm_front:
	@@ locals:
	@@ 	r2 = temp element
	@@ 	r4 = address of queue
        stmfd   sp!, {r4, fp, ip, lr}

	mov	r4, r0		@ save address
	
	subs	r1, r1, #1	@ init counter
	ble	out		@ only one element 
loop:	
	ldr	r2, [r0, #4]
	str	r2, [r0], #4

	subs	r1, r1, #1
	bne	loop

out:	mov	r0, r4		@ restore address
	ldmfd   sp!, {r4, fp, ip, lr}
        bx      lr
        .size   rm_front, .-rm_front

@@@ Test remove front of queue
.global	main
.text
.align	2
.type 	main, %function
main:
	@@ r4 = num values in queue

	@@ test rm_front with 10 elements
	stmfd   sp!, {fp, lr}
	ldr	r4, =.nvalues
	
	ldr	r0, =.values
	mov	r1, r4
	ldr	r2, =.preamble
	ldr	r3, =.out_word
	bl	printv

	ldr	r0, =.values
	mov 	r1, r4
	bl	rm_front
	sub	r4, r4, #1		@ one elment gone
	
	ldr	r0, =.values
	mov	r1, r4
	ldr	r2, =.preamble
	ldr	r3, =.out_word
	bl	printv

	@@ test rm_front with 1 elements
	ldr	r4, =#1
	
	ldr	r0, =.values
	mov	r1, r4
	ldr	r2, =.preamble
	ldr	r3, =.out_word
	bl	printv

	ldr	r0, =.values
	mov 	r1, r4
	bl	rm_front
	sub	r4, r4, #1		@ one elment gone
	
	ldr	r0, =.values
	mov	r1, r4
	ldr	r2, =.preamble
	ldr	r3, =.out_word
	bl	printv

	
exit:
	/* return */
	ldmfd	sp!, {fp, lr}
	mov	r0, #0
	bx	lr

.align  2	
.size   main, .-main
	
