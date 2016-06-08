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
.ascii "\nInitial table of %d elements.\n\n\000"	
.output:
.ascii	"index of element: %p\012\000"

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

.text
.align  2
.global main
.type   main, %function
main:
        stmfd   sp!, {fp, lr}
	
	mov	r1, #5
	ldr	r0, =.preamble
	bl	printf

	ldr	r0, =.tab
	mov	r1, #5
	ldr	r2, =#16
	ldr	r3, =.out_word
	bl	printtab

	ldr	r0, =.tab
	mov	r1, #5
	mov	r2, #16
	bl	sort

	mov	r1, #5
	ldr	r0, =.sorted
	bl	printf

	ldr	r0, =.tab
	mov	r1, #5
	ldr	r2, =#16
	ldr	r3, =.out_word
	bl	printtab

	ldr	r0, =.tab
	mov	r1, #5
	ldr	r2, =#0x85
	bl	search

	mov	r1, r0
	ldr	r0, =.output
	bl	printf
	
exit:	@@ return 0 
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
.align  2
.size   main, .-main
	
	
