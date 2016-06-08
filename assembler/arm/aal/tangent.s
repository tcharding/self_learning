.section        .rodata
.align  2
.debug:
.ascii	"value in r1 is: %d\012\000"

.section .data
.align	2
.table:
@@@ tangent values for 0 - 45 degrees (Q31 notation)
.word	0x0,0x23bf7b4,0x47848a8,0x6b54c50
.word	0x8f35ca0,0xb32d420,0xd740e40,0xfb76790
.word	0x11fd3e00,0x1445f100,0x1691e1e0,0x18e17440
.word	0x1b350da0,0x1d8d16c0,0x1fe9fae0,0x224c28c0
.word	0x24b412c0,0x27222ec0,0x2996f800,0x2c12ed40
.word	0x2e969380,0x31227500,0x33b721c0,0x365530c0
.word	0x38fd4100,0x3baff840,0x3e6e0580,0x41382180
.word	0x440f0e00,0x46f39980,0x49e69d00,0x4ce90000
.word	0x4ffbb800,0x531fc980,0x56564b80,0x59a06680
.word	0x5cff5880,0x60747580,0x64012b00,0x67a70100
.word	0x6b679e00,0x6f44c980,0x73407080,0x775ca780
.word	0x7b9bb080,0xffffffff
	
.global	tangent
.text
.align	2
.type 	tangent, %function
@@ Calculate the tangent of value in degrees in Q31 notation
@@ inputs:	
@@ 	r1 = tangent argument (in degrees, from 0 - 45)
@@ outputs:	
@@ 	r0 = return value in Q31 notation
@@ locals:	
@@ 	r4 = starting address of table
@@ 	r7 = copy of input arg
@@ 	*/
tangent:
	stmfd   sp!, {r4, r7, fp, ip, lr}

	ldr	r1, =#1		@ test value, exp: 37484466
	
	mov	r7, r1
	ldr	r4, =.table

	cmp	r1, #45		@ only support 0 - 45 degrees
	movgt	r0, #-1
	bgt	exit

retval:
	ldr	r0, [r4, r1, lsl #2] 
	
exit:	ldmfd	sp!, {r4, r7, fp, ip, lr}
	bx	lr
.size   tangent, .-tangent
	
