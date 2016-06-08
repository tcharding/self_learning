.section        .rodata
.align  2
.debug:
.ascii "r1: %p\012\000"
.initial:
.ascii	"words are:\012\000"
.subsequent:
.ascii "%p\012\000"
.nl:
.ascii "\012\000"
.preamble:	
.ascii "Printing %d words.\012\000"	

.section .data
.align	2
values:
	@@ some (not so) random words */
.word	0xffffffff,0x7ffb0280,0x7fec0a00,0x7fd31780
.word	0x7847d900,0x777f9000,0x76adf600,0x75d31a80
.word	0x620dbe80,0x609a5300,0x5f1f5e80,0x5d9cff80
.word	0x40000000,0x3e0e3dc0,0x3c17a500,0x3a1c5c40
.word	0x1ef74c00,0x1ccb3240,0x1a9cd9a0,0x186c6de0

@@@ 
@@@ Test printv
@@@ 
.global	main
.text
.align	2
.type 	main, %function
main:
	stmfd   sp!, {fp, lr}

	ldr	r1, =#4
	ldr	r0, preamble
	bl	printf
	
	ldr	r0, =values
	ldr	r1, =#4
	ldr	r2, initial
	ldr	r3, subsequent

	bl	printv
	
	ldr	r1, =#20
	ldr	r0, preamble
	bl	printf

	ldr	r0, =values
	ldr	r1, =#20
	ldr	r2, initial
	ldr	r3, subsequent

	bl	printv

	@@ return
	ldmfd	sp!, {fp, lr}
	mov	r0, #0
	bx	lr

.align  2
initial:
.word	.initial
subsequent:
.word	.subsequent
nl:
.word	.nl	
preamble:	
.word	.preamble
.size   main, .-main
	
