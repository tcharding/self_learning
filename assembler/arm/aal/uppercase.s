	.section        .rodata
        .align  2
.msg:
	.ascii	"uppercase char is: %c\012\000"
	
	.text
        .align  2
        .global main
        .type   main, %function
main:
	stmfd   sp!, {fp, lr}

	ldr	r1, =0x61

	cmp	r1, #0x5B
	subgt	r1, r1, #32

	ldr	r0, .L0
	bl	printf
		
	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L0:
	.word 	.msg
        .size   main, .-main
	
