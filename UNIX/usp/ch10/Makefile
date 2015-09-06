CC = gcc
CFLAG = -Wall -pedantic -std=c99 -D_GNU_SOURCE
LFLAG = -L../../lib -ltch
INC = -I../../include	
SPLINT = lint.out
EXES =
TESTS = test-runner tst-virtualtimers
DEV = 

.PHONY: all clean dev tests

tests: $(TESTS)
dev: $(DEV) lint_dev
all: $(EXES) lint_all

# tests
test-runner: virtualtimers.o test-runner.o
	$(CC) -o $@ virtualtimers.o test-runner.o $(LFLAG)

tst-virtualtimers: virtualtimers.o tst-virtualtimers.o
	$(CC) -o $@ virtualtimers.o tst-virtualtimers.o $(LFLAG)

# object only rules
%.o: %.c
	$(CC) $(CFLAG) -c $< $(INC)

virtualtimers.o: virtualtimers.c virtualtimers.h
	$(CC) $(CFLAG) -c $< $(INC)

# lint rules
lint_all:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(EXES) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT); \
	fi; done

lint_dev:
	@if [ -f "$$SPLINT" ]; then rm "$$SPLINT"; fi
	@for f in $(DEV) ; do \
	if [ -f $$f ]; then \
	splint $$f.c -I../../include -warnposix -nullderef -nullpass -unrecog \
	>> $(SPLINT); \
	fi; done

clean:
	@for f in *.o $(EXES) $(DEV) $(TESTS); do \
	if [ -f $$f ]; then \
	rm "$$f"; \
	echo "rm $$f" ; \
	fi; done

