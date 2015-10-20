CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp -lpthread
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS =  strclithread-ex-26.3.c tcpcli01-ex-26.3.c  tcpserv01-ex-26.3.c readline-ex-26.4.c
OBJS = $(SRCS:.c=.o)
EXES =  tcpcli01-ex-26.3 tcpserv01-ex-26.3 tst-readline

.PHONY: all clean 
all: $(EXES) lint

tst-readline: readline-ex-26.4.o tst-readline.o
	$(CC) -o $@ readline-ex-26.4.o tst-readline.o $(LFLAG)

tcpserv01-ex-26.3: tcpserv01-ex-26.3.o
	$(CC) -o $@ tcpserv01-ex-26.3.o $(LFLAG)

tcpcli01-ex-26.3: strclithread-ex-26.3.o tcpcli01-ex-26.3.o
	$(CC) -o $@ strclithread-ex-26.3.o tcpcli01-ex-26.3.o $(LFLAG)

#foo: foo.o
#	$(CC) -o $@ foo.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC) $(LFLAG)

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

