CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lsock

INC= -I../..
LIB= -L../../lib
SOBJ = srv.o str_echo.o sigchldwait.o
COBJ = cli.o str_cli.o

.PHONY: all clean

all: srv cli 

cli: $(COBJ)
	$(CC) -o $@ $(COBJ) $(LFLAG) $(LIB)

srv: $(SOBJ)
	$(CC) -o $@ $(SOBJ) $(LFLAG) $(LIB)

%.o: %.c
	$(CC) $(CFLAGS) -c $< $(INC)

clean:
	@for f in srv cli $(COBJ) $(SOBJ); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
