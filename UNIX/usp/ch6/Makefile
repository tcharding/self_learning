CC = gcc
CFLAG = -Wall -pedantic -D_GNU_SOURCE 
LFLAG = -L../../lib -ltch
INC = -I../../include

EXES = parentwritepipe synchronizefan pipeserver pipeclient \
	seqserverbad seqclientbad

.PHONY: all clean book 

all: $(EXES) 

# text book programs
parentwritepipe: parentwritepipe.o
	$(CC) -o $@ parentwritepipe.o $(LFLAG)

synchronizefan: synchronizefan.o restart.o
	$(CC) -o $@ synchronizefan.o restart.o $(LFLAG)

pipeserver: pipeserver.o restart.o
	$(CC) -o $@ pipeserver.o restart.o $(LFLAG)

pipeclient: pipeclient.o restart.o
	$(CC) -o $@ pipeclient.o restart.o $(LFLAG)

seqserverbad: seqserverbad.o 
	$(CC) -o $@ seqserverbad.o $(LFLAG)

seqclientbad: seqclientbad.o 
	$(CC) -o $@ seqclientbad.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
