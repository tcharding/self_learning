CC = gcc
CFLAG = -Wall -pedantic -g -D_POSIX_C_SOURCE=200809L -DDEBUG
LFLAG = -L../lib -lsock
INC = -I..

.PHONY: all clean

all: hostent hostent-mod echo-cli

echo-cli: udp_echocli.o dg_cli.o
	$(CC) -o $@ udp_echocli.o dg_cli.o $(LFLAG)

hostent: hostent.o
	$(CC) -o $@ hostent.o $(LFLAG)

hostent-mod: hostent-mod.o
	$(CC) -o $@ hostent-mod.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

clean:
	@for f in hostent *.o echo-cli hostent-mod; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

