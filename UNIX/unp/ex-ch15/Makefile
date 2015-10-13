CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS =  daytimetcpcli-ex-15.3.c daytimetcpsrv2-ex-15.3.c \
	backlog-unix.c backlog-tcp.c
OBJS = $(SRCS:.c=.o)
EXES =  daytimetcpcli-ex-15.3 daytimetcpsrv2-ex-15.3 daytimeunixcli daytimeunixsrv \
	backlog-unix backlog-tcp

.PHONY: all clean 
all: $(EXES) lint

backlog-unix: backlog-unix.o
	$(CC) -o $@ backlog-unix.o $(LFLAG)

backlog-tcp: backlog-tcp.o
	$(CC) -o $@ backlog-tcp.o $(LFLAG)

daytimetcpsrv2-ex-15.3: daytimetcpsrv2-ex-15.3.o
	$(CC) -o $@ daytimetcpsrv2-ex-15.3.o $(LFLAG)

daytimetcpcli-ex-15.3: daytimetcpcli-ex-15.3.o
	$(CC) -o $@ daytimetcpcli-ex-15.3.o $(LFLAG)

daytimeunixsrv: daytimeunixsrv.o
	$(CC) -o $@ daytimeunixsrv.o $(LFLAG)

daytimeunixcli: daytimeunixcli.o
	$(CC) -o $@ daytimeunixcli.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@echo '*** splint: lint.out ***' > $(SPLINT)
	@for f in $(SRCS) ; do \
	if [ -f $$f ]; then \
	splint $$f $(INC) -warnposix -nullderef -nullpass -unrecog -paramuse \
	>> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) $(SPLINT); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

