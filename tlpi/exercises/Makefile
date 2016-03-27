include ../Makefile.inc

EXE = tee copy append atomic_append dup shared_fd readwritev \
	broken_longjmp env free_and_sbrk memory getpwnam idshow \
	initgroups ps pathopen tail manyfiles t_stat access \
	readX chattr setfattr getacl longrunner realpath list_files \
	getcwd filetypes multi_chdir notify sig_receiver sig_ignore \
	sig_flags

all : ${EXE}

getacl: getacl.o
	$(CC) -o $@ getacl.o -lacl -L.. -ltlpi

clean : 
	@for f in *.o $(EXE); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

showall :
	@ echo ${EXE}
