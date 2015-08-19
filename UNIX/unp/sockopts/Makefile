CC = gcc
CFLAG = -Wall -pedantic -std=c99
LFLAG = -lsock
INC= -I..
LIB= -L../lib

#SRV_OBJ = srv.o str_echo.o 
CLI_OBJ = cli.o str_cli.o

.PHONY: all clean

all: checkopts cli-opts cli-linger showbuf srv

checkopts: checkopts.o
	$(CC) -o checkopts checkopts.o $(LFLAG) $(LIB) 

cli-opts: cli-opts.o str-cli.o getopts.o
	$(CC) -o $@ cli-opts.o str-cli.o getopts.o $(LFLAG) $(LIB)	

cli-linger: cli-linger.o str-cli.o getopts.o
	$(CC) -o $@ cli-linger.o str-cli.o getopts.o $(LFLAG) $(LIB)

showbuf: showbuf.o
	$(CC) -o $@ showbuf.o $(LFLAG) $(LIB)

srv: srv.o sigchldwait.o str-echo.o
	$(CC) -o $@ srv.o sigchldwait.o str-echo.o $(LFLAG) $(LIB)

cli: $(CLI_OBJ)
	$(CC) -o $@ $(CLI_OBJ) $(LFLAG) $(LIB)

%.o: %.c
	$(CC) $(CFLAGS) -c $< $(INC)

clean:
	@for f in checkopts cli-opts cli-linger showbuf srv *.o; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done
