CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lsock
LIB = -L../../lib
INC = -I../..

COBJ = cli.o dg_cli.o

.PHONY: all clean

all: cli srv_simple srv_select

srv_select: srv_select.o str_echo.o sigchldwait.o
	$(CC) -o $@ srv_select.o str_echo.o sigchldwait.o $(LIB) $(LFLAG)

srv_simple: srv_simple.o dg_echo.o
	$(CC) -o $@ srv_simple.o dg_echo.o $(LIB) $(LFLAG)

cli: $(COBJ)
	$(CC) -o $@ $(COBJ) $(LIB) $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in $(SOBJ) $(COBJ) srv_select srv_simple cli *.o; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done


