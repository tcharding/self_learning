.section        .rodata
.align  2
.debug:
.ascii	"value in r1 is: %d\012\000"

.section .data
.align	2
table:
@@@ sine values for 0 - 90 degrees (Q31 notation)
.word	0x0,0x23be164,0x4779630,0x6b2f1d0
.word	0x8edc7b0,0xb27eb60,0xd613050,0xf996a20
.word	0x11d06ca0,0x14060b60,0x163a1a80,0x186c6de0
.word	0x1a9cd9a0,0x1ccb3240,0x1ef74c00,0x2120fb80
.word	0x234815c0,0x256c6f80,0x278dde80,0x29ac37c0
.word	0x2bc75100,0x2ddf0040,0x2ff31bc0,0x32037a40
.word	0x340ff240,0x36185b00,0x381c8bc0,0x3a1c5c40
.word	0x3c17a500,0x3e0e3dc0,0x40000000,0x41ecc480
.word	0x43d46500,0x45b6bb80,0x4793a200,0x496af400
.word	0x4b3c8c00,0x4d084680,0x4ecdff00,0x508d9200
.word	0x5246dd80,0x53f9be00,0x55a61280,0x574bb900
.word	0x58ea9100,0x5a827980,0x5c135380,0x5d9cff80
.word	0x5f1f5e80,0x609a5300,0x620dbe80,0x63798500
.word	0x64dd8980,0x6639b000,0x678dde80,0x68d9f980
.word	0x6a1de700,0x6b598e80,0x6c8cd700,0x6db7a880
.word	0x6ed9eb80,0x6ff38a00,0x71046d00,0x720c8080
.word	0x730baf00,0x7401e500,0x74ef0e80,0x75d31a80
.word	0x76adf600,0x777f9000,0x7847d900,0x7906c080
.word	0x79bc3880,0x7a683180,0x7b0a9f80,0x7ba37500
.word	0x7c32a680,0x7cb82880,0x7d33f100,0x7da5f580
.word	0x7e0e2e00,0x7e6c9280,0x7ec11a80,0x7f0bc080
.word	0x7f4c7e80,0x7f834f00,0x7fb02e00,0x7fd31780
.word	0x7fec0a00,0x7ffb0280,0xffffffff
	
.global	sine
.text
.align	2
.type 	sine, %function
@@ Calculate the sine of value in degrees in Q31 notation
@@ inputs:	
@@ 	r1 = sine argument (in degrees, from 0 - 360)
@@ outputs:	
@@ 	r0 = return value in Q31 notation
@@ locals:	
@@ 	r2 = 270 vaule
@@ 	r4 = starting address of table
@@ 	r7 = copy of input arg
@@ 	
sine:
	stmfd   sp!, {r4, r7, fp, ip, lr}

	ldr	r1, =#127	@ test value 127, exp: 1715056699
	
	mov	r7, r1
	ldr	r2, =#270	@ constant won't fit into rotation scheme
	ldr	r4, =table

	cmp	r1, #90
	ble	retval
	cmp	r1, #180
	rsble	r1, r1, #180
	ble	retval
	cmp 	r1, r2
	suble	r1, r1, #180
	ble	retval
	rsb	r1, r1, #360

retval:
	/* negate if in second or third quadrant */
	ldr	r0, [r4, r1, lsl #2] 
	cmp	r7, #180
	rsbgt	r0, r0, #0
	
	ldmfd	sp!, {r4, r7, fp, ip, lr}
	bx	lr
.size   sine, .-sine
	
