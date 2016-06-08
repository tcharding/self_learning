.text
.align  2
.global name
.type   name, %function
name:
        stmfd   sp!, {fp, ip, lr}

        ldmfd   sp!, {fp, ip, lr}
        bx      lr
        .size   name, .-name
