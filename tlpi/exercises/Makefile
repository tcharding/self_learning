include ../Makefile.inc

EXE = tee copy append atomic_append dup shared_fd readwritev \
	broken_longjmp env

all : ${EXE}

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}
