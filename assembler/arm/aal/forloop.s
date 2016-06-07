        .align  2
.LC0:
        .ascii  "r0: %p\012\000"

arrayb:
	.byte	0xA, 0x9, 0x8, 0x7, 0x6, 0x5, 0x4, 0x3
	
	.text
        .align  2
        .global main
        .type   main, %function

main:
        stmfd   sp!, {fp, lr}

	adr	r1, arrayb
	adr 	r2, arraya
	mov	r4, #7		@ counter
loop:
	rsb	r3, r4, #7
	ldrb	r5, [r1, r3]
@	strb	r5, [r2, r0]

	mov 	r1, r5
	ldr	r0, .L1
	bl	printf

	subs	r4, #1
	bge	loop

	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L1:
        .word   .LC0

arraya:
	.byte	0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0
        .size   main, .-main
