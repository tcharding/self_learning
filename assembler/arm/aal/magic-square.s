@@@ 
@@@ Definition of magic square:
@@@
@@@ 1. All rows, columns and diagonals add up to
@@@      N ( N*N  + 1 ) / 2
@@@ 2. And all values are unique in the range (1 - N*N)
@@@ 

@@@ Check if matrix is a magic square
@@@ inputs:
@@@ 	r0 = N
@@@ 	r1 = address of matrix (one byte per value)
@@@ outputs:
@@@ 	r0 = 1 if magic, 0 if not magic
.text
.align  2
.global isMagic
.type   isMagic, %function
	@@ locals:
	@@ 	r4 = N
	@@  	r5 = address of matrix
isMagic:
        stmfd   sp!, {r4, r5, fp, lr}

	mov	r4, r0
	mov	r5, r1
	
	bl	isUnique
	cmp	r0, #0
	beq	1f		@ r0 already contains 0

	mov	r0, r4
	mov	r1, r5
	bl	calcSum
	mov	r6, r0	
	
	@@ check rows sum
	mov	r0, r4
	mov	r1, r5


1:	ldmfd   sp!, {r4, r5, fp, lr}
        bx      lr
        .size   isMagic, .-isMagic

@@@ Check if matrix contains all unique values
@@@ inputs:
@@@ 	r0 = N ( 0 < N < 32 )
@@@ 	r1 = address of matrix (one byte per value)
@@@ outputs:
@@@ 	r0 = 1 if unique, 0 if not, -1 on error (eg value out of range)

.text
.align  2
.global isUnique
.type   isUnique, %function

	@@ locals:

	@@ 	r1 = value
	@@ 	r2 = loop counter
	@@  	r3 = bit array of values found so far
	@@ 	r4 = N
	@@  	r5 = address of next vaule to read
	@@ 	r6 = N*N
isUnique:
        stmfd   sp!, {r4, r5, r6, fp, lr}
	@@ save registers
	mov	r4, r0
	mov	r5, r1

	@@ setup
	mul	r6, r4, r4	@ N*N
	mov	r3, #0		@ one bit for each unique value
	mov	r2, r6		@ loop counter
	sub	r2, #1
	
2:
	mov	r1, #0		@ clear r0
	ldrb	r1, [r5], #1	@ single byte values

	@@ check value range
	cmp	r1, r6
	movgt	r0, #-1		@ error value too big
	bgt	1f
	
	cmp	r1, #0
	movle	r0, #-1		@ error vaule too small
	ble	1f

	@@ check if value seen before
	mov	r0, r3, lsr r1
	and 	r0, #1
	cmp	r0, #1
	moveq	r0, #0	
	beq	1f		@ value already seen
	
	@@ set bit n for value
	mov	r0, #1
	orr	r3, r3, r0, lsl r1 @ set bit

	mov	r1, r3

	subs	r2, r2, #1
	bne	2b

	mov	r0, #1
1:	ldmfd   sp!, {r4, r5, r6, fp, lr}
        bx      lr
        .size   isUnique, .-isUnique


@@@ Calculate N ( N * N + 1 ) / 2
@@@ inputs:
@@@ 	r0 = N
@@@ outputs:
@@@ 	r0 = result
.text
.align  2
.global calcSum
.type   calcSum, %function
calcSum:
        stmfd   sp!, {r4-r5, fp, lr}

	mul	r2, r0, r0
	add	r2, r2, #1
	mul	r2, r2, r0
	mov	r0, r2, lsr #1

        ldmfd   sp!, {r4-r5,fp, lr}
        bx      lr
        .size   calcSum, .-calcSum

@@@ Check rows, columns and diagonals sum correctly
@@@ inputs:
@@@ 	r0 = N
@@@ 	r1 = address of matrix (one byte per value)
@@@ outputs:
@@@ 	r0 = 1 if correct, 0 if not incorrect
.text
.align  2
.global isSum
.type   isSum, %function
	@@ locals
	@@ 	r0 = running total

	@@ 	r2 = address to read value from
	@@ 	r3 = the value 
	@@ 	r4 = N
	@@ 	r5 = address
	@@ 	r6 = required sum
	@@ 	r8 = row loop counter
	@@  	r9 = column loop counter
	@@ 	r10 = index into matrix (linear 0 - N-1)
isSum:
        stmfd   sp!, {r4-r10, fp, lr}
	@@ setup
				@ save registers
	mov	r4, r0
	mov	r5, r1

	bl	isSumRows
	cmp	r0, #1
	bne	2f

	mov	r0, r4
	mov	r1, r5
	bl	isSumCols
	cmp	r0, #1
	bne	2f

	mov	r0, #1		@ sums correctly
	
2:	ldmfd   sp!, {r4-r10, fp, lr}
        bx      lr
        .size   isSum, .-isSum

@@@ isSumRows: isSum helper routine, check rows sum correctly
@@@ inputs:
@@@ 	r0 = N
@@@ 	r1 = address of matrix (one byte per value)
@@@ outputs:
@@@ 	r0 = 1 if correct, 0 if not incorrect
.text
.align  2
.global isSumRows
.type   isSumRows, %function
	@@ locals
	@@ 	r0 = running total

	@@ 	r2 = address to read value from
	@@ 	r3 = the value 
	@@ 	r4 = N
	@@ 	r5 = address
	@@ 	r6 = required sum
	@@ 	r8 = row loop counter
	@@  	r9 = column loop counter
	@@ 	r10 = index into matrix (linear 0 - N-1)
isSumRows:
        stmfd   sp!, {r4-r10, fp, lr}
	@@ setup
				@ save registers
	mov	r4, r0
	mov	r5, r1

				@ setup counters
	sub	r8, r4, #1
	mov	r9, r8
				@ get sum
	bl	calcSum
	mov	r6, r0
				@ index starts at 16 (decremented at top of loop)
	mul	r10, r4, r4

2:				@ loop over rows
	sub	r9, r4, #1	@ reset column counter
	mov	r0, #0		@ reset running total
3:				@ loop over columns
	sub	r10, #1		@ decrement matrix index
	add	r2, r5, r10
	
	ldrb	r3, [r2]
	
	add	r0, r0, r3

	subs	r9, #1
	bge	3b

	@@ check total
	cmp	r0, r6
	bne	0f

	subs	r8, #1
	bge	2b

	mov	r0, #1		@ sums correctly
	b	4f
	
0:				@ incorrect sum
	mov	r0, #0

4:	ldmfd   sp!, {r4-r10, fp, lr}
        bx      lr
        .size   isSumRows, .-isSumRows

@@@ isSumCols: isSum helper routine, check cols sum correctly
@@@ inputs:
@@@ 	r0 = N
@@@ 	r1 = address of matrix (one byte per value)
@@@ outputs:
@@@ 	r0 = 1 if correct, 0 if not incorrect
.text
.align  2
.global isSumCols
.type   isSumCols, %function
	@@ locals
	@@ 	r0 = running total

	@@ 	r2 = address to read value from
	@@ 	r3 = the value 
	@@ 	r4 = N
	@@ 	r5 = address
	@@ 	r6 = required sum
	@@ 	r8 = row loop counter
	@@  	r9 = column loop counter
	@@ 	r10 = index into matrix (linear 0 - N-1)
isSumCols:
        stmfd   sp!, {r4-r10, fp, lr}
	@@ setup
				@ save registers
	mov	r4, r0
	mov	r5, r1

				@ setup counters
	sub	r8, r4, #1
	mov	r9, r8
				@ get sum
	bl	calcSum
	mov	r6, r0
				@ index starts at 16 (decremented at top of loop)

2:				@ loop over rows
	sub	r9, r4, #1	@ reset column counter
	mov	r0, #0		@ reset running total
3:				@ loop over columns
	@@ calculate index (row * N + column)
	mul	r10, r8, r4
	add	r10, r10, r9
	add	r2, r5, r10
	
	ldrb	r3, [r2]
	
	add	r0, r0, r3

	subs	r9, #1
	bge	3b

	@@ check total
	cmp	r0, r6
	bne	0f

	subs	r8, #1
	bge	2b

	mov	r0, #1		@ sums correctly
	b	4f
	
0:				@ incorrect sum
	mov	r0, #0

4:	ldmfd   sp!, {r4-r10, fp, lr}
        bx      lr
        .size   isSumCols, .-isSumCols

@@@
@@@ Test data
@@@ 
.section 	.data
.align		2	
.magic:
.byte 0x10, 0x03, 0x02, 0x0D
.byte 0x05, 0x0A, 0x0B, 0x08
.byte 0x09, 0x06, 0x07, 0x0C
.byte 0x04, 0x0F, 0x0E, 0x01
.nonunique:	@ last value same as first
.byte 0x10, 0x03, 0x02, 0x0D
.byte 0x05, 0x0A, 0x0B, 0x08
.byte 0x09, 0x06, 0x07, 0x0C
.byte 0x04, 0x0F, 0x0E, 0x10
.magic3:
.byte 0x02, 0x07, 0x06
.byte 0x09, 0x05, 0x01
.byte 0x04, 0x03, 0x08
.nosumrow:
.byte 0x02, 0x07, 0x06
.byte 0x09, 0x05, 0x04
.byte 0x01, 0x03, 0x08
.nosumcol:
.byte 0x02, 0x07, 0x06
.byte 0x09, 0x05, 0x04
.byte 0x01, 0x08, 0x03	

.section 	.rodata
.align  	2
.debug:
.ascii	"value in r1 is: %p\012\000"
.debugIsSum:
.ascii	"reqSum: %d row: %d total: %d\012\000"
.testIsUniqueErr:
.ascii	"testIsUnique Error: %p\012\000"
.testCalcSumErr:
.ascii	"testCalcSum Error: n:%p exp:%p got:%p\012\000"
.testIsSumErr:
.ascii	"testIsSum Error: %p\012\000"
	
@@@ run tests
.text
.align  2
.global test
.type   test, %function
test:
        stmfd   sp!, {fp, lr}

	bl	testIsUnique
	bl	testCalcSum
	bl	testIsSum

        ldmfd   sp!, {fp, lr}
        bx      lr
        .size   test, .-test

	.text
.align  2
.global testIsUnique
.type   testIsUnique, %function
testIsUnique:
        stmfd   sp!, {fp, lr}

	ldr	r0, =4		@ N
	ldr	r1, =.magic
	bl	isUnique

	cmp	r0, #1
	movne	r1, #1		@ test 1
	blne	err

	ldr	r0, =4		@ N
	ldr	r1, =.nonunique
	bl	isUnique

	cmp	r0, #1
	movne	r1, #2		@ test 2
	bne	err

	b	out

err:	ldr	r0, =.testIsUniqueErr
	bl	printf

out:	ldmfd   sp!, {fp, lr}
        bx      lr
        .size   testIsUnique, .-testIsUnique

.text
.align  2
.global testCalcSum
.type   testCalcSum, %function
	@@ calcSum does n ( n * n + 1 ) / 2
testCalcSum:
	@@ locals
	@@ 	r4 = N
	@@ 	r5 = expected result
	@@ 	r6 = got (result)
        stmfd   sp!, {fp, lr}

	mov	r4, #2		@ N 
	mov	r5, #5		@ expected result
	mov	r0, r4
	bl	calcSum
	mov	r6, r0
	cmp	r5, r6		@ got vs expected
	bne	1f

	mov	r4, #4		@ N 
	mov	r5, #34		@ expected result
	mov	r0, r4
	bl	calcSum
	mov	r6, r0
	cmp	r5, r6		@ got vs expected
	bne	1f

	mov	r4, #9		@ N 
	ldr	r5, =#369	@ expected result
	mov	r0, r4
	bl	calcSum
	mov	r6, r0
	cmp	r5, r6		@ got vs expected
	bne	1f

	b	2f
	
1:	mov	r1, r4		@ r1 has N, r2 has expected, r3 has got
	mov	r2, r5
	mov	r3, r6
	ldr	r0, =.testCalcSumErr
	bl	printf
	
2:	ldmfd   sp!, {fp, lr}
        bx      lr
        .size   testCalcSum, .-testCalcSum

.text
.align  2
.global testIsSum
.type   testIsSum, %function
testIsSum:
        stmfd   sp!, {fp, lr}
	
	ldr	r0, =3		@ N
	ldr	r1, =.magic3
	bl	isSum

	cmp	r0, #1
	movne	r1, #1		@ test 1
	blne	1f
	
	ldr	r0, =3		@ N
	ldr	r1, =.nosumrow
	bl	isSum

	cmp	r0, #0
	movne	r1, #2		@ test 2 
	blne	1f
	
	ldr	r0, =3		@ N
	ldr	r1, =.nosumcol
	bl	isSum

	cmp	r0, #0
	movne	r1, #3		@ test 3
	blne	1f

	@@
	@@ did not implement check on diagonal
	@@
	
	@@ ldr	r0, =3		@ N
	@@ ldr	r1, =.nosumdiag
	@@ bl	isSum

	@@ cmp	r0, #0
	@@ movne	r1, #4		@ test 4
	@@ blne	1f

	b	out

1:	ldr	r0, =.testIsSumErr
	bl	printf

2:	ldmfd   sp!, {fp, lr}
        bx      lr
        .size   testIsSum, .-testIsSum
	
.text
.align  2
.global debug
.type   debug, %function
debug:
        stmfd   sp!, {r0-r10, fp, lr}

	ldr	r0, =.debug
	bl	printf
	
        ldmfd   sp!, {r0-r10, fp, lr}
        bx      lr
        .size   debug, .-debug

