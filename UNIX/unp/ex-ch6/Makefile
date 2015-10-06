CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS =  tcpservselect01-ex-6.4.c tcpcli01-ex-6.5.c tcpservpoll01-ex-6.8.c
OBJS = $(SRCS:.c=.o)
EXES =  tcpservselect01-ex-6.4 tcpcli01-ex-6.5 chargen tcpservpoll01-ex-6.8

.PHONY: all clean 
all: $(EXES) lint

tcpservpoll01-ex-6.8: tcpservpoll01-ex-6.8.o
	$(CC) -o $@ tcpservpoll01-ex-6.8.o $(LFLAG)

chargen: chargen.o
	$(CC) -o $@ chargen.o $(LFLAG)

tcpcli01-ex-6.5: tcpcli01-ex-6.5.o
	$(CC) -o $@ tcpcli01-ex-6.5.o $(LFLAG)

tcpservselect01-ex-6.4: tcpservselect01-ex-6.4.o
	$(CC) -o $@ tcpservselect01-ex-6.4.o $(LFLAG)

#foo: foo.o
#	$(CC) -o $@ foo.o $(LFLAG)

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

