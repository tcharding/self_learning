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
.tab:
.word	0x6			@ key
.ascii	"six         "	
.word	0x36			@ key
.ascii	"thirty six  "	
.word	0x85			@ key
.ascii	"eighty five "		
.word	0xFF			@ key
.ascii	"ff          "		
.word	0x1			@ key
.ascii	"one         "

@@@ 
@@@ Test printtab
@@@ 
.global	main
.text
.align	2
.type 	main, %function
main:
	stmfd   sp!, {fp, lr}

	ldr	r1, =#2
	ldr	r0, preamble
	bl	printf
	
	ldr	r0, =.tab
	ldr	r1, =#2
	ldr	r2, =#16
	ldr	r3, subsequent

	bl	printtab
	
	ldr	r1, =#5
	ldr	r0, preamble
	bl	printf

	ldr	r0, =.tab
	ldr	r1, =#5
	ldr	r2, =#16
	ldr	r3, subsequent

	bl	printtab

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
	
