include ../Makefile.inc

EXE = changepgrp setsid disc_SIGHUP orphan orphancatch nice rtsched \
	fifoloop pipeaffinity rusage time xsoftlimit logger douser \
	check_password check_password_caps sched_set getlogin \
	utmpx_login loginout who dynload twinpipe popen fifo_seqnum_client \
	fifo_seqnum_server nonblockpipe

all : ${EXE}

douser: douser.o
	$(CC) -o $@ douser.o -L../.. -ltlpi -lcrypt
check_password: check_password.o
	$(CC) -o $@ check_password.o -L../.. -ltlpi -lcrypt
check_password_caps: check_password_caps.o
	$(CC) -o $@ check_password_caps.o -L../.. -ltlpi -lcrypt -lcap
sched_set: sched_set.o
	$(CC) -o $@ sched_set.o -L../.. -ltlpi -lcap
dynload: dynload.o
	$(CC) -o $@ dynload.o -L../.. -ltlpi -ldl

clean : 
	@for f in *.o $(EXE); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

showall :
	@ echo ${EXE}
