CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../../lib -ltch
INC = -I../../include	
SPLINT = lint.out
EXES = sig-catcher showtimes

.PHONY: all clean 

all: $(EXES) lint_all

sig-catcher: sig-catcher.o
	$(CC) -o $@ sig-catcher.o $(LFLAG)

showtimes: showtimes.o
	$(CC) -o $@ showtimes.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass > $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

