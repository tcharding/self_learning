@@@ Binary Search
@@@ inputs:
@@@ 	r0 = starting address of data
@@@ 	r1 = number of elements
@@@ 	r2 = key to find
@@@ outputs:
@@@ 	r0 = index of elemen or -1 if not found

@@@
@@@	Currently supports only fixed sized key and data
@@@ 	
@@@	Keysize: 4 bytes
@@@ 	Datasize: 12 bytes

.equ	esize,	4		@ log 2 of entry size (16 bytes)
.equ	ksize,	2		@ log 2 of key size (4 bytes)
	
.text
.align  2
.global search
.type   search, %function
search:
	@@ locals:
	@@ 	r0 = first
	@@ 	r1 = last
	@@ 	r2 = middle
	@@ 	r3 = index
	@@ 	r4 = size of entries
	@@ 	r5 = key we are searching for
	@@ 	r6 = address of the list
	@@ 	r7 = tmp
        stmfd   sp!, {r4, r5, r6, fp, ip, lr}

	mov	r6, r0
	mov	r5, r2
	mov	r0, #0
	sub 	r1, #1		@ index = n - 1

loop:	cmp	r0, r1		@ middle > last, key not found
	movgt	r2, #-1
	bgt	done

	add	r2, r0, r1	@ calculate middle
	mov	r2, r2, asr #1	

	ldr	r7, [r6, r2, lsl #esize] @ load entry
	cmp	r5, r7
	addgt	r0, r2, #1	@ first = middle + 1
	sublt	r1, r2, #1	@ last = middle - 1
	bne 	loop

	mov	r0, r2
done:
	ldmfd	sp!, {r4, r5, r6, fp, ip, lr}
	bx	lr
        .size   search, .-search
