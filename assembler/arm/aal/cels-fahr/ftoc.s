        .text
        .align  2
        .global unq15
        .type   unq15, %function
unq15:
        stmfd   sp!, {fp, ip, lr}

	mov	r0, r0, LSR #15

        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   unq15, .-unq15
	
        .section        .rodata
        .align  2
.LC0:
        .ascii  "Celsius: %d\012\000" 
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	/*
	C = 5/9 (F - 32)
	
	r0 -> Celsius
	r1 -> Fahrenheit
	*/
	
	/* setup */
	mov	r1, #86		@ 30 Celsius = 86 Fahrenheit
	ldr	r2, =0x471C	@ 5/9 in Q15

	/* F - 32 */
	mov	r4, r1
	sub	r4, #32
	mul	r0, r4, r2
        bl      unq15

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

