        .section        .rodata
        .align  2
.LC0:
        .ascii  "ascii: %p\012\000" 
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	@	ldr	r1, =#4
	ldr	r1, =#14	

	mov	r0, #0
	cmp	r1, #10
	blt	units
	ldr 	r2, =#0x300	
	add	r0, r0, r2, lsl #4
units:
	add 	r0, #0x30
	add	r0, r1
	
	/* print r0 */
        mov     r1, r0
        ldr     r0, .L3
        bl      printf

	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L3:
        .word   .LC0
        .size   main, .-main
	
