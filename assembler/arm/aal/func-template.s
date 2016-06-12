.text
.align  2
.global name
.type   name, %function
name:
        stmfd   sp!, {fp, lr}

        ldmfd   sp!, {fp, lr}
        bx      lr
        .size   name, .-name
