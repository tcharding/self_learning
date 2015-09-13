CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../../lib -ltch -pthread
INC = -I../../include	
SPLINT = lint.out
EXES = cpdir

.PHONY: all clean 

all: $(EXES) lint_all

cpdir: cpdir.o
	$(CC) -o $@ cpdir.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

