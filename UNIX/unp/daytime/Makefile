CC = gcc
CFLAG = -Wall -pedantic -g -D_POSIX_C_SOURCE=200809L -DDEBUG
LFLAG = -L../lib -lsock
INC = -I..

.PHONY: all clean

all: tcpcli tcpsrv udpcli udpsrv tcpcli-gethostbyname tcpsrv-getnameinfo 

tcpsrv-getnameinfo: tcpsrv-getnameinfo.o 
	$(CC) -o $@ tcpsrv-getnameinfo.o  $(LFLAG)

tcpcli-gethostbyname: tcpcli-gethostbyname.o
	$(CC) -o $@ tcpcli-gethostbyname.o $(LFLAG)

tcpcli: tcpcli.o getnameinfo_timeo.o
	$(CC) -o $@ tcpcli.o getnameinfo_timeo.o $(LFLAG)

tcpsrv: tcpsrv.o
	$(CC) -o $@ tcpsrv.o $(LFLAG)

udpcli: udpcli.o
	$(CC) -o $@ udpcli.o $(LFLAG)

udpsrv: udpsrv.o
	$(CC) -o $@ udpsrv.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in tcpcli tcpsrv udpcli udpsrv tcpsrv-getnameinfo tcpcli-gethostbyname *.o ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

