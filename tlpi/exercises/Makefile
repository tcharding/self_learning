include ../Makefile.inc

EXE = tee copy append atomic_append dup shared_fd readwritev \
	broken_longjmp env free_and_sbrk memory getpwnam idshow \
	initgroups ps pathopen tail manyfiles t_stat access \
	readX chattr setfattr getacl longrunner realpath list_files \
	getcwd filetypes multi_chdir notify sig_receiver sig_ignore \
	sig_flags sig_interupt sig_stop sig_catchall sig_speed_sigwaitinfo \
	sig_systemV abort alarm t_nanosleep posix_timer ptmr_sigev_signal \
	vfork noparent grandparent child_status make_zombie execlp exec \
	sig_chld_wait forktime thread_join_self thread_incr thread_one_time_init \
	basename_r dirname_r

all : ${EXE}

# Need thread library for these
#
thread_join_self: thread_join_self.o
	$(CC) -o $@ thread_join_self.o -L.. -ltlpi -pthread
thread_incr: thread_incr.o
	$(CC) -o $@ thread_incr.o -L.. -ltlpi -pthread
thread_one_time_init: thread_one_time_init.o
	$(CC) -o $@ thread_one_time_init.o -L.. -ltlpi -pthread

basename_r: basename_r.o
	$(CC) -o $@ basename_r.o -L.. -ltlpi -pthread
dirname_r: dirname_r.o
	$(CC) -o $@ dirname_r.o -L.. -ltlpi -pthread

# We need to link with extra libraries for these
#
getacl: getacl.o
	$(CC) -o $@ getacl.o -L.. -ltlpi -lacl

posix_timer: posix_timer.o
	$(CC) -o $@ posix_timer.o  -L.. -ltlpi -lrt

ptmr_sigev_signal: ptmr_sigev_signal.o
	$(CC) -o $@ ptmr_sigev_signal.o  -L.. -ltlpi -lrt

clean : 
	@for f in *.o $(EXE); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

showall :
	@ echo ${EXE}
