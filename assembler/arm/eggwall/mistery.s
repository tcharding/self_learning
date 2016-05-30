        .text
        .align  2
        .global multiplyByTen
        .type   multiplyByTen, %function
multiplyByTen:
        stmfd   sp!, {fp, ip, lr}
        mov     r3, r0, asl #3
        add     r0, r3, r0, asl #1
        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   multiplyByTen, .-multiplyByTen
	
        .section        .rodata
        .align  2
.LC0:
        .ascii  "The number is %d\012\000"
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}
	
        mov     r0, #32
        bl      multiplyByTen
	
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

