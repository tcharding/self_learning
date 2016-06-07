	.text
        .align  2
        .global nsetbits
        .type   nsetbits, %function
nsetbits:	
        stmfd   sp!, {fp, ip, lr}

	ldr	r2, =#32	@ counter
	ldr	r0, =#0		@ total
loop:
	subs	r2, #1
	mov	r3, r1, lsr r2
	and	r3, #1
	add	r0, r0, r3
	bne	loop

        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   nsetbits, .-nsetbits
	
        .section        .rodata
        .align  2
.LC0:
        .ascii  "Number of set bits: %d\012\000" 
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

	mov	r1, #0b11100111
	bl	nsetbits

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
	
