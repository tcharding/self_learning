include ../Makefile.inc

GEN_EXE = psem_getvalue psem_create psem_post psem_unlink \
	  psem_timedwait psem_trywait psem_wait thread_incr_psem

LINUX_EXE =

EXE = ${GEN_EXE} ${LINUX_EXE}

all : ${EXE}

allgen : ${GEN_EXE}

CFLAGS = ${IMPL_CFLAGS} ${IMPL_THREAD_FLAGS}
LDLIBS = ${IMPL_LDLIBS} ${IMPL_THREAD_FLAGS}
	# POSIX semaphores need the NPTL thread library on Linux

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}

${EXE} : ${LPLIB}		# True as a rough approximation
