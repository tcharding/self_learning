include ../Makefile.inc

EXE = changepgrp setsid disc_SIGHUP orphan orphancatch nice rtsched \
	fifoloop pipeaffinity rusage time xsoftlimit

all : ${EXE}

clean : 
	@for f in *.o $(EXE); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

showall :
	@ echo ${EXE}
