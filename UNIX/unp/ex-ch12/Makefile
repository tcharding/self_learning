CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS = ipv4tcpsrv.c ipv6tcpsrv.c
OBJS = $(SRCS:.c=.o)
EXES = ipv4tcpsrv ipv6tcpsrv

.PHONY: all clean 
all: $(EXES) lint

ipv4tcpsrv: ipv4tcpsrv.o
	$(CC) -o $@ ipv4tcpsrv.o $(LFLAG)


ipv6tcpsrv: ipv6tcpsrv.o
	$(CC) -o $@ ipv6tcpsrv.o $(LFLAG)

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

