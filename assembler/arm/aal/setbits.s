        .section        .rodata
        .align  2
.LC0:
        .ascii  "data: %p\012\000" 
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	ldr	r0, =#0xFF0
	
	mov	r1, #1
	mov	r3, r1
	add	r3, r3, r1, lsl #2
	add	r3, r3, r1, lsl #12

	orr	r0, r3
	
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
	
