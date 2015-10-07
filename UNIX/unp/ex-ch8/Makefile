CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L.. -lunp 
INC = -I../lib

# lint output file
SPLINT = lint.out

SRCS = overflow-udp.c udpservselect01-ex-8.9.c
OBJS = $(SRCS:.c=.o)
EXES = overflow-udp udpservselect01-ex-8.9

.PHONY: all clean 
all: $(EXES) lint

udpservselect01-ex-8.9: udpservselect01-ex-8.9.o
	$(CC) -o $@ udpservselect01-ex-8.9.o $(LFLAG)

overflow-udp: overflow-udp.o
	$(CC) -o $@ overflow-udp.o $(LFLAG)

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

