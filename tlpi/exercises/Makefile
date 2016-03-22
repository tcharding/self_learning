include ../Makefile.inc

EXE = tee copy append atomic_append dup shared_fd readwritev \
	broken_longjmp env free_and_sbrk memory getpwnam idshow \
	initgroups ps pstree pathopen tail manyfiles t_stat access \
	readX chattr setfattr

all : ${EXE}

clean : 
	${RM} ${EXE} *.o

showall :
	@ echo ${EXE}
