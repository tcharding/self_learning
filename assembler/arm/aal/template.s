.section 	.rodata
.align  	2
.debug:
	.ascii	"value in r1 is: %p\012\000"

.text
.align  2
.global name
.type   name, %function
name:
        stmfd   sp!, {fp, lr}

        ldmfd   sp!, {fp, lr}
        bx      lr
        .size   name, .-name

.text
.align  2
.global main
.type   main, %function
main:
        stmfd   sp!, {fp, lr}
	

	
	@@ return 0 
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
.align  2
.size   main, .-main
	
	
