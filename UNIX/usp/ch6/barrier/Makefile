CC = gcc
CFLAG = -Wall -pedantic -D_GNU_SOURCE 
LFLAG = -L../../../lib -ltch
INC = -I../../../include
EXES = srv cli		# barrier client/server

.PHONY: all clean 

all: $(EXES) 

# barrier client/server
srv: server.o barrier.o
	$(CC) -o $@ server.o barrier.o $(LFLAG)

cli: client.o barrier.o
	$(CC) -o $@ client.o barrier.o $(LFLAG)

barrier.o: barrier.c barrier.h
	$(CC) $(CFLAG) -c $< $(INC)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
