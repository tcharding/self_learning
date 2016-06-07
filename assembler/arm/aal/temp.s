        .section        .rodata
        .align  2
.LC0:
        .ascii  "Test program: %p\012\000" 
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	mov	r0, #2
	mov	r1, #1
	add	r0, r0, r1, lsl #2
	
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
	
