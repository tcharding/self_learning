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
        .ascii  "Fahrenheit: %d\012\000" 
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	/*
	r0 -> Celsius
	r1 -> Fahrenheit
	
	F = C * 9/5 + 32
	*/
	
	/* setup */
	mov	r1, #30		@ 30 Celsius = 86 Fahrenheit
	ldr	r2, =0xE666	@ 9/5 in Q15

	mul	r0, r1, r2	@ C * 9/5
	add	r0, r0, #32	
	
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

