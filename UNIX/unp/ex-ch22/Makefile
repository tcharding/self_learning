CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS =  dg_send_recv-ex-22.3.c udpserv03-ex-22.5.c udpcli01-ex-22.5.c
OBJS = $(SRCS:.c=.o)
EXES =  udpserv03-ex-22.5 udpcli01-ex-22.5

.PHONY: all clean 
all: $(EXES) lint

udpcli01-ex-22.5: udpcli01-ex-22.5.o
	$(CC) -o $@ udpcli01-ex-22.5.o $(LFLAG)

udpserv03-ex-22.5: udpserv03-ex-22.5.o
	$(CC) -o $@ udpserv03-ex-22.5.o $(LFLAG)

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

