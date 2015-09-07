CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../../lib -ltch
INC = -I../../include	
SPLINT = lint.out
SRCS = exec.c parse.c 
OBJS = $(SRCS:.c=.o)
EXES = ush
TESTS = tst-parse

.PHONY: all clean tests

all: $(EXES) lint_all
tests: $(TESTS)

ush: $(OBJS) ush.o ush.h
	$(CC) -o $@  $(OBJS) ush.o $(LFLAG)

tst-parse: $(OBJS) tst-parse.o
	$(CC) -o $@  $(OBJS) tst-parse.o $(LFLAG)

%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all: 
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT); \
	fi; done

lint_src: 
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(SRCS) ; do \
	if [ -f $$f ]; then \
	splint $$f -I../../include -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT); \
	fi; done



clean:
	@for f in *.o $(EXES) ; do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

