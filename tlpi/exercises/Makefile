include ../Makefile.inc

# executables by chapter
CH4 = tee copy
CH5 = append atomic_append dup shared_fd readwritev
EXE = ${CH4} ${CH5}

all : ${EXE}

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}
