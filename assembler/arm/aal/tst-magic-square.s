.section 	.rodata
.align  	2
.debug:
.ascii	"value in r1 is: %p\012\000"
.yes:
.ascii	"\nMatrix is magic.\n\000"
.no:
.ascii	"\nMatrix is not magic.\n\000"
	
.section 	.data
.align		2	
.matrix:
.byte 0x10, 0x03, 0x02, 0x0D
.byte 0x05, 0x0A, 0x0B, 0x08
.byte 0x09, 0x06, 0x07, 0x0C
.byte 0x04, 0x0F, 0x0E, 0x01

.text
.align  2
.global main
.type   main, %function
main:
        stmfd   sp!, {fp, lr}

	bl	test
	bl	exit
	
	ldr	r0, =4		@ N
	ldr	r1, =.matrix
	bl	isMagic

	mov	r3, r0		@ save return value
	cmp	r3, #0
	beq	no

	ldr	r0, =.yes
	bl	printf
	b	debug

no:	ldr	r0, =.no
	bl	printf

debug:	mov	r1, r3
	ldr	r0, =.debug
	bl	printf

exit:	@@ return 0 
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
.align  2
.size   main, .-main
	
	
