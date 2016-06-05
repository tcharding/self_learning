        .align  2
.LC0:
        .ascii  "The number is %p\012\000"

table:
	.word 0xFEBBAAAA, 0x12340000, 0x88881111
	.word 0x00000013, 0x80808080, 0xFFFF0000
/*
	.word 0x01, 0x02, 0x04, 0x08
	.word 0x10, 0x20, 0x40, 0x80
*/	

	.text
	.align 2
	.global main
	.type	main, %function
main:
	stmfd   sp!, {fp, lr}
	
	mov	r0, #0		@ total
	mov	r1, #6		@ counter
	adr	r2, table
loop:	
	sub	r1, #1
	ldr	r3, [r2], #4
	add	r0, r0, r3
	bne	loop

        mov     r1, r0
        ldr     r0, .L3
        bl      printf
	
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L3:
        .word   .LC0
        .size   main, .-main
