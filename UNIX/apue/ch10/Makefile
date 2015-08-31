CC = gcc
CFLAG = -Wall -pedantic -std=c99 
LFLAG = -L../lib -lapue 
INC = -I../include 
EXES = sig2string

all: $(EXES)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

sig2string: sig2string.o
	$(CC) -o $@ sig2string.o $(LFLAG)

# lint rules
lint_all:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass > $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

