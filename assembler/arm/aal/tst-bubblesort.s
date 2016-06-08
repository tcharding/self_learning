.section        .rodata
.align  2
.debug:
.ascii "r1: %p\012\000"
.unsorted:
.ascii	"unsorted:\012\000"
.sorted:
.ascii	"sorted:\012\000"
.out_word:
.ascii "%p\012\000"
.preamble:	
.ascii "\nSorting %d Words.\n\n\000"	

.section .data
.align	2
values:
	@@ some (not so) random words
.word	0x6,0x1,0x36,0x2,0x85,0x23,0xFF,0x32,0x1,0x99
.word	0xffffffff,0x7ffb0280,0x1a9cd9a0,0x0000000e
.word	0x620dbe80,0x609a5300,0x5f1f5e80,0x5d9cff80
.word	0x7847d900,0x777f9000,0x76adf600,0x75d31a80
.word	0x1ef74c00,0x1ccb3240,0x7fec0a00,0x7fd31780
.word	0x40000000,0x3e0e3dc0,0x3c17a500,0xF0000001

	@@ Print sort and print N words
	@@ r0 contains N
.global	printnw
.text
.align	2
.type 	printnw, %function
printnw:
	stmfd	sp!, {r4, fp, ip, lr}

	mov 	r4, r0		@ save
	
	mov	r1, r4
	ldr	r0, =.preamble
	bl	printf

	ldr	r0, =values
	mov	r1, r4
	ldr	r2, =.unsorted
	ldr	r3, =.out_word
	bl	printv

	ldr	r0, =values
	mov	r1, r4
	mov	r2, #4
	bl	sort

	ldr	r0, =values
	mov	r1, r4
	ldr	r2, =.sorted
	ldr	r3, =.out_word
	bl	printv

	ldmfd	sp!, {r4, fp, ip, lr}
	mov	r0, #0
	bx	lr

.align  2	
.size   printnw, .-printnw

	@@ Test bubble sort
.global	main
.text
.align	2
.type 	main, %function
main:
	stmfd   sp!, {fp, lr}

	mov	r0, #2
	bl	printnw

	mov	r0, #10
	bl	printnw

	mov	r0, #30
	bl	printnw

exit:
	/* return */
	ldmfd	sp!, {fp, lr}
	mov	r0, #0
	bx	lr

.align  2	
.size   main, .-main
	
