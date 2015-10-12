CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS =  connect_timeo-ex-14.2.c daytimetcpcli-ex-14.3.c tcpserv01-ex-14.5.c  str_echo_stdio02-ex-14.5.c
OBJS = $(SRCS:.c=.o)
EXES =  daytimetcpcli-ex-14.3 tcpserv01-ex-14.5 

.PHONY: all clean 
all: $(EXES) lint

tcpserv01-ex-14.5: str_echo_stdio02-ex-14.5.o  tcpserv01-ex-14.5.o
	$(CC) -o $@ str_echo_stdio02-ex-14.5.o tcpserv01-ex-14.5.o $(LFLAG)

daytimetcpcli-ex-14.3: daytimetcpcli-ex-14.3.o
	$(CC) -o $@ daytimetcpcli-ex-14.3.o $(LFLAG)

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

