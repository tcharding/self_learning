	.section        .data
        .align 2
higher:
        .asciz "Yes, r2 is higher than or equal to r1\n"
lower:
        .asciz "No, r2 is lower than r1\n"
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}
	
        @ Load some values
        mov     r1, #32
        mov     r2, #33
	
        @ Check if r2 is lower than r1
        cmp     r2, r1
	
        @ If it is greater or equal, jump ahead
        bge     greaterOrEqual
	
        @ Otherwise it was lower
        ldr     r0, =lower
        @ Now skip past to the common point again
        b       common
	
greaterOrEqual:
        ldr     r0, =higher
	
common:
        @ Print the message
        bl      puts
	
        @ Return 0
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
