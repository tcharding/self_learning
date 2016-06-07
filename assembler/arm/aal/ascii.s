	.text
        .align  2
        .global bz9
        .type   bz9, %function
bz9:
        stmfd   sp!, {fp, ip, lr}

	cmp	r0, #0
	blt	no
	cmp	r0, #9
	bgt	no
yes:
	ldr	r1, =#1
	bl	out
no:
	ldr	r1, =#0
out:	
        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   bz9, .-bz9

        .section        .rodata
        .align  2
.LC0:
        .ascii  "Hex value: %p\012\000" 
	
        .text
        .align  2
        .global main
        .type   main, %function
main:
        stmfd   sp!, {fp, lr}

@	ldr 	r0, =#0x38	@ '8' in ASCII
	ldr	r0, =#0x41	@ 'A' in ASCII (10)

	/* convert ASCII to binary */
	ldr	r4, =#1
	bic	r0, r4, lsl #7
	sub	r0, r0, #0x30
	bl	bz9
	cmp	r1, #1
	beq	done
	sub	r0, #7
done:	
	/* print r0 */
        mov     r1, r0
        ldr     r0, .L3
        bl      printf

	/* return 0 */
        mov     r0, #0
        ldmfd   sp!, {fp, lr}
        bx      lr
	
        .align  2
.L3:
        .word   .LC0
        .size   main, .-main
	
