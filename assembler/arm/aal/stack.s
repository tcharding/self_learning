@@@ implement push/pop to stack without using stm/ldm instructions
.text
.align  2
.global push
.type   push, %function
push:
	@@ stack is full and grows downwards
	@@ 
	@@ r0 contains data to push
        stmfd   sp!, {fp, ip, lr}

	sub	sp, sp, #4	@ decrement stack
	mov	sp, r0

        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   push, .-push
	
.text
.align  2
.global pop
.type   pop, %function
pop:
	@@ stack is full and grows downwards
	@@ 
	@@ r0 contains data popped
        stmfd   sp!, {fp, ip, lr}

	mov	r0, sp
	add	sp, sp, #4	@ increment stack

        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   pop, .-pop
