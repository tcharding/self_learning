include ../Makefile.inc

EXE = changepgrp setsid disc_SIGHUP orphan orphancatch

all : ${EXE}

clean : 
	@for f in *.o $(EXE); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

showall :
	@ echo ${EXE}
