CC = gcc
CFLAG = -Wall -pedantic -D_GNU_SOURCE 
LFLAG = -L../../../lib -ltch
INC = -I../../../include
EXES = srv cli

.PHONY: all clean 

all: $(EXES) 

# request/reply client/server
srv: server.o rr.o
	$(CC) -o $@ server.o rr.o $(LFLAG)

cli: client.o rr.o
	$(CC) -o $@ client.o rr.o $(LFLAG)

rr: rr.o rr.h
	$(CC) $(CFLAG) -c $< $(INC)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in *.o $(EXES) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
